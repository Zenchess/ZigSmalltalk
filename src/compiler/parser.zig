const std = @import("std");
const lexer_mod = @import("lexer.zig");
const Lexer = lexer_mod.Lexer;
const Token = lexer_mod.Token;
const TokenType = lexer_mod.TokenType;

pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEnd,
    OutOfMemory,
    InvalidNumber,
    InvalidCharacter,
    MissingClosingQuote,
    InvalidAssignment,
};

/// AST Node types
pub const NodeType = enum {
    // Literals
    literal_integer,
    literal_float,
    literal_scaled_decimal,
    literal_string,
    literal_symbol,
    literal_character,
    literal_array,

    // Variables
    variable,
    pseudo_variable, // self, super, nil, true, false, thisContext

    // Expressions
    assignment,
    message_send,
    cascade,
    block,

    // Statements
    return_statement,
    sequence,

    // Method definition
    method,
};

/// Represents a parsed method definition
pub const MethodNode = struct {
    selector: []const u8,
    arguments: []const []const u8,
    temporaries: []const []const u8,
    statements: []const *ASTNode,
    primitive_index: u16, // 0 = no primitive
};

/// Scaled decimal data structure
pub const ScaledDecimalData = struct {
    value: f64,
    scale: u8,
};

/// AST Node
pub const ASTNode = struct {
    node_type: NodeType,
    token: Token, // The primary token for this node
    data: NodeData,

    pub const NodeData = union {
        // For literals
        integer: i64,
        float: f64,
        scaled_decimal: ScaledDecimalData,
        string: []const u8,

        // For variable
        name: []const u8,

        // For assignment
        assignment: struct {
            name: []const u8,
            value: *ASTNode,
        },

        // For message send
        message: struct {
            receiver: *ASTNode,
            selector: []const u8,
            arguments: []const *ASTNode,
            is_super: bool,
        },

        // For cascade
        cascade: struct {
            receiver: *ASTNode,
            messages: []const CascadeMessage,
        },

        // For block
        block: struct {
            parameters: []const []const u8,
            temporaries: []const []const u8,
            statements: []const *ASTNode,
        },

        // For return
        return_value: *ASTNode,

        // For sequence
        statements: []const *ASTNode,

        // For literal array
        elements: []const *ASTNode,

        // For method definition
        method_def: MethodNode,

        // No data
        none: void,
    };

    pub const CascadeMessage = struct {
        selector: []const u8,
        arguments: []const *ASTNode,
    };
};

pub const Parser = struct {
    lexer: Lexer,
    current: Token,
    previous: Token,
    allocator: std.mem.Allocator,
    had_error: bool,
    error_message: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        var parser = Parser{
            .lexer = Lexer.init(source),
            .current = undefined,
            .previous = undefined,
            .allocator = allocator,
            .had_error = false,
            .error_message = "",
        };
        parser.advance(); // Load first token
        return parser;
    }

    /// Parse a complete expression (for REPL doIt)
    /// Handles optional temporary declarations: | x y | expr
    pub fn parseExpression(self: *Parser) ParseError!*ASTNode {
        // Check for temporary variable declarations
        var temporaries: std.ArrayList([]const u8) = .{};
        errdefer temporaries.deinit(self.allocator);

        if (self.check(.bar)) {
            self.advance(); // consume first |
            // Parse temporary variable names
            while (self.check(.identifier)) {
                try temporaries.append(self.allocator, self.current.text);
                self.advance();
            }
            if (!self.match(.bar)) {
                return ParseError.UnexpectedToken;
            }
        }

        // Parse statements
        var statements: std.ArrayList(*ASTNode) = .{};
        errdefer statements.deinit(self.allocator);

        while (!self.isAtEnd()) {
            const stmt = try self.expression();
            try statements.append(self.allocator, stmt);

            // Statements are separated by periods
            if (!self.match(.period)) {
                break;
            }
        }

        // If we have temporaries, wrap in a block node
        if (temporaries.items.len > 0) {
            const block = try self.createNode(.block);
            block.data = .{
                .block = .{
                    .parameters = &[_][]const u8{},
                    .temporaries = try temporaries.toOwnedSlice(self.allocator),
                    .statements = try statements.toOwnedSlice(self.allocator),
                },
            };
            return block;
        }

        // No temporaries - return single expression or sequence
        if (statements.items.len == 1) {
            const single = statements.items[0];
            statements.deinit(self.allocator);
            return single;
        }

        const seq = try self.createNode(.sequence);
        seq.data = .{ .statements = try statements.toOwnedSlice(self.allocator) };
        return seq;
    }

    /// Parse a method definition
    /// Format: selector [| temps |] statements
    /// Examples:
    ///   yourself ^self
    ///   + aNumber ^self addNumber: aNumber
    ///   at: index put: value | temp | ...
    pub fn parseMethod(self: *Parser) ParseError!*ASTNode {
        // Parse method signature (selector and arguments)
        var selector_buf: std.ArrayList(u8) = .{};
        defer selector_buf.deinit(self.allocator);

        var arguments: std.ArrayList([]const u8) = .{};
        errdefer arguments.deinit(self.allocator);

        // Determine message type from first token
        // Note: .bar (|) is also a valid binary selector for 'or' method
        if (self.check(.binary_selector) or self.check(.bar)) {
            // Binary method: + aNumber, | operand
            try selector_buf.appendSlice(self.allocator, self.current.text);
            self.advance();

            if (!self.check(.identifier)) {
                return ParseError.UnexpectedToken;
            }
            try arguments.append(self.allocator, self.current.text);
            self.advance();
        } else if (self.check(.keyword)) {
            // Keyword method: at: index put: value
            while (self.check(.keyword)) {
                try selector_buf.appendSlice(self.allocator, self.current.text);
                self.advance();

                if (!self.check(.identifier)) {
                    return ParseError.UnexpectedToken;
                }
                try arguments.append(self.allocator, self.current.text);
                self.advance();
            }
        } else if (self.check(.identifier)) {
            // Unary method: yourself
            try selector_buf.appendSlice(self.allocator, self.current.text);
            self.advance();
        } else {
            return ParseError.UnexpectedToken;
        }

        // Parse primitive pragma if present: <primitive: N>
        var primitive_index: u16 = 0;
        if (self.check(.binary_selector) and std.mem.eql(u8, self.current.text, "<")) {
            self.advance();
            if (self.check(.keyword) and std.mem.eql(u8, self.current.text, "primitive:")) {
                self.advance();
                if (self.check(.integer)) {
                    const prim_text = self.current.text;
                    primitive_index = std.fmt.parseInt(u16, prim_text, 10) catch 0;
                    self.advance();
                }
                if (self.check(.binary_selector) and std.mem.eql(u8, self.current.text, ">")) {
                    self.advance();
                }
            }
        }

        // Parse temporary declarations: | temp1 temp2 |
        var temporaries: std.ArrayList([]const u8) = .{};
        errdefer temporaries.deinit(self.allocator);

        if (self.check(.bar)) {
            self.advance();
            while (self.check(.identifier)) {
                try temporaries.append(self.allocator, self.current.text);
                self.advance();
            }
            if (!self.match(.bar)) {
                return ParseError.UnexpectedToken;
            }
        }

        // Parse method body (statements)
        var statements: std.ArrayList(*ASTNode) = .{};
        errdefer statements.deinit(self.allocator);

        while (!self.isAtEnd()) {
            if (self.check(.return_op)) {
                self.advance();
                const value = try self.expression();
                const ret = try self.createNode(.return_statement);
                ret.data = .{ .return_value = value };
                try statements.append(self.allocator, ret);
                _ = self.match(.period);
                break;
            }

            const stmt = try self.expression();
            try statements.append(self.allocator, stmt);

            if (!self.match(.period)) {
                break;
            }
        }

        // Create method node
        const method_node = try self.createNode(.method);
        method_node.data = .{
            .method_def = .{
                .selector = try selector_buf.toOwnedSlice(self.allocator),
                .arguments = try arguments.toOwnedSlice(self.allocator),
                .temporaries = try temporaries.toOwnedSlice(self.allocator),
                .statements = try statements.toOwnedSlice(self.allocator),
                .primitive_index = primitive_index,
            },
        };

        return method_node;
    }

    /// Parse a sequence of statements
    pub fn parseStatements(self: *Parser) ParseError!*ASTNode {
        var statements: std.ArrayList(*ASTNode) = .{};
        errdefer statements.deinit(self.allocator);

        while (!self.isAtEnd()) {
            // Check for return statement
            if (self.check(.return_op)) {
                self.advance();
                const value = try self.expression();
                const ret = try self.createNode(.return_statement);
                ret.data = .{ .return_value = value };
                try statements.append(self.allocator, ret);

                // Optional period
                _ = self.match(.period);
                break;
            }

            const stmt = try self.expression();
            try statements.append(self.allocator, stmt);

            // Statements are separated by periods
            if (!self.match(.period)) {
                break;
            }
        }

        if (statements.items.len == 1) {
            const single = statements.items[0];
            statements.deinit(self.allocator);
            return single;
        }

        const seq = try self.createNode(.sequence);
        seq.data = .{ .statements = try statements.toOwnedSlice(self.allocator) };
        return seq;
    }

    fn expression(self: *Parser) ParseError!*ASTNode {
        return self.assignment();
    }

    fn assignment(self: *Parser) ParseError!*ASTNode {
        const expr = try self.cascade();

        if (self.match(.assignment)) {
            // Check that left side is a variable
            if (expr.node_type != .variable) {
                self.had_error = true;
                self.error_message = "Invalid assignment target";
                return ParseError.InvalidAssignment;
            }

            const value = try self.expression();
            const assign = try self.createNode(.assignment);
            assign.data = .{
                .assignment = .{
                    .name = expr.data.name,
                    .value = value,
                },
            };
            return assign;
        }

        return expr;
    }

    fn cascade(self: *Parser) ParseError!*ASTNode {
        var receiver = try self.keywordMessage();

        if (!self.check(.semicolon)) {
            return receiver;
        }

        // Parse cascade messages
        var messages: std.ArrayList(ASTNode.CascadeMessage) = .{};
        errdefer messages.deinit(self.allocator);

        // First, extract the message from the receiver if it's a message send
        if (receiver.node_type == .message_send) {
            try messages.append(self.allocator, .{
                .selector = receiver.data.message.selector,
                .arguments = receiver.data.message.arguments,
            });
            receiver = receiver.data.message.receiver;
        }

        while (self.match(.semicolon)) {
            const msg = try self.parseMessage();
            try messages.append(self.allocator, msg);
        }

        const casc = try self.createNode(.cascade);
        casc.data = .{
            .cascade = .{
                .receiver = receiver,
                .messages = try messages.toOwnedSlice(self.allocator),
            },
        };
        return casc;
    }

    fn keywordMessage(self: *Parser) ParseError!*ASTNode {
        const receiver = try self.binaryMessage();

        // Check for keyword message
        if (self.check(.keyword)) {
            var selector: std.ArrayList(u8) = .{};
            defer selector.deinit(self.allocator);

            var arguments: std.ArrayList(*ASTNode) = .{};
            errdefer arguments.deinit(self.allocator);

            while (self.check(.keyword)) {
                const keyword_token = self.current;
                self.advance();

                try selector.appendSlice(self.allocator, keyword_token.text);

                const arg = try self.binaryMessage();
                try arguments.append(self.allocator, arg);
            }

            // Check if receiver is 'super'
            const is_super = if (receiver.node_type == .pseudo_variable)
                std.mem.eql(u8, receiver.data.name, "super")
            else
                false;

            const send = try self.createNode(.message_send);
            send.data = .{
                .message = .{
                    .receiver = receiver,
                    .selector = try selector.toOwnedSlice(self.allocator),
                    .arguments = try arguments.toOwnedSlice(self.allocator),
                    .is_super = is_super,
                },
            };
            return send;
        }

        return receiver;
    }

    fn binaryMessage(self: *Parser) ParseError!*ASTNode {
        var receiver = try self.unaryMessage();

        // Note: .bar (|) is also a valid binary selector for 'or' method
        while (self.check(.binary_selector) or self.check(.bar)) {
            const op_token = self.current;
            self.advance();

            const arg = try self.unaryMessage();

            var args = try self.allocator.alloc(*ASTNode, 1);
            args[0] = arg;

            // Check if receiver is 'super'
            const is_super = if (receiver.node_type == .pseudo_variable)
                std.mem.eql(u8, receiver.data.name, "super")
            else
                false;

            const send = try self.createNode(.message_send);
            send.data = .{
                .message = .{
                    .receiver = receiver,
                    .selector = op_token.text,
                    .arguments = args,
                    .is_super = is_super,
                },
            };
            receiver = send;
        }

        return receiver;
    }

    fn unaryMessage(self: *Parser) ParseError!*ASTNode {
        var receiver = try self.primary();

        while (self.check(.identifier)) {
            // Make sure it's not a keyword (which contains ':')
            const text = self.current.text;
            if (std.mem.indexOfScalar(u8, text, ':') != null) {
                break;
            }

            const selector_token = self.current;
            self.advance();

            // Check if receiver is 'super'
            const is_super = if (receiver.node_type == .pseudo_variable)
                std.mem.eql(u8, receiver.data.name, "super")
            else
                false;

            const send = try self.createNode(.message_send);
            send.data = .{
                .message = .{
                    .receiver = receiver,
                    .selector = selector_token.text,
                    .arguments = &[_]*ASTNode{},
                    .is_super = is_super,
                },
            };
            receiver = send;
        }

        return receiver;
    }

    fn primary(self: *Parser) ParseError!*ASTNode {
        // Literals
        if (self.match(.integer)) {
            const node = try self.createNode(.literal_integer);
            node.data = .{ .integer = try self.parseInteger(self.previous.text) };
            return node;
        }

        if (self.match(.float)) {
            const node = try self.createNode(.literal_float);
            node.data = .{ .float = try self.parseFloat(self.previous.text) };
            return node;
        }

        if (self.match(.scaled_decimal)) {
            const node = try self.createNode(.literal_scaled_decimal);
            const parsed = try self.parseScaledDecimal(self.previous.text);
            node.data = .{ .scaled_decimal = parsed };
            return node;
        }

        if (self.match(.string)) {
            const node = try self.createNode(.literal_string);
            // Remove quotes
            const text = self.previous.text;
            node.data = .{ .string = text[1 .. text.len - 1] };
            return node;
        }

        if (self.match(.symbol)) {
            const node = try self.createNode(.literal_symbol);
            const text = self.previous.text;
            // Remove # and optional quotes
            // For quoted symbols like #'hello world', text is "#'hello world'"
            // Need at least 4 chars: #'x' (# ' x ')
            if (text.len >= 4 and text[1] == '\'' and text[text.len - 1] == '\'') {
                node.data = .{ .string = text[2 .. text.len - 1] };
            } else if (text.len > 1) {
                node.data = .{ .string = text[1..] };
            } else {
                node.data = .{ .string = "" };
            }
            return node;
        }

        if (self.match(.character)) {
            const node = try self.createNode(.literal_character);
            // Get the character after $
            if (self.previous.text.len >= 2) {
                node.data = .{ .integer = self.previous.text[1] };
            } else {
                return ParseError.InvalidCharacter;
            }
            return node;
        }

        // Literal array #(...)
        if (self.check(.hash) and self.lexer.peek() == '(') {
            return self.parseLiteralArray();
        }

        // Compile-time constant ##(expr) - treat as just (expr) for runtime eval
        if (self.match(.double_hash)) {
            if (self.match(.open_paren)) {
                const expr = try self.expression();
                if (!self.match(.close_paren)) {
                    return ParseError.UnexpectedToken;
                }
                return expr;
            }
            return ParseError.UnexpectedToken;
        }

        // Identifiers and pseudo-variables
        if (self.match(.identifier)) {
            const name = self.previous.text;

            // Check for pseudo-variables
            if (std.mem.eql(u8, name, "self") or
                std.mem.eql(u8, name, "super") or
                std.mem.eql(u8, name, "nil") or
                std.mem.eql(u8, name, "true") or
                std.mem.eql(u8, name, "false") or
                std.mem.eql(u8, name, "thisContext"))
            {
                const node = try self.createNode(.pseudo_variable);
                node.data = .{ .name = name };
                return node;
            }

            const node = try self.createNode(.variable);
            node.data = .{ .name = name };
            return node;
        }

        // Block [...]
        if (self.match(.open_bracket)) {
            return self.parseBlock();
        }

        // Parenthesized expression
        if (self.match(.open_paren)) {
            const expr = try self.expression();
            if (!self.match(.close_paren)) {
                return ParseError.UnexpectedToken;
            }
            return expr;
        }

        // Dynamic array {...}
        if (self.match(.open_brace)) {
            return self.parseDynamicArray();
        }

        return ParseError.UnexpectedToken;
    }

    fn parseBlock(self: *Parser) ParseError!*ASTNode {
        var parameters: std.ArrayList([]const u8) = .{};
        defer parameters.deinit(self.allocator);

        // Parse block parameters [:x :y | ...]
        if (self.check(.colon)) {
            while (self.match(.colon)) {
                if (!self.match(.identifier)) {
                    return ParseError.UnexpectedToken;
                }
                try parameters.append(self.allocator, self.previous.text);
            }
            // Expect | after parameters
            if (!self.match(.bar)) {
                return ParseError.UnexpectedToken;
            }
        }

        var temporaries: std.ArrayList([]const u8) = .{};
        defer temporaries.deinit(self.allocator);

        // Parse temporaries | temp1 temp2 |
        if (self.match(.bar)) {
            while (self.check(.identifier)) {
                try temporaries.append(self.allocator, self.current.text);
                self.advance();
            }
            if (!self.match(.bar)) {
                return ParseError.UnexpectedToken;
            }
        }

        // Parse statements
        var statements: std.ArrayList(*ASTNode) = .{};
        errdefer statements.deinit(self.allocator);

        while (!self.check(.close_bracket) and !self.isAtEnd()) {
            // Check for return
            if (self.check(.return_op)) {
                self.advance();
                const value = try self.expression();
                const ret = try self.createNode(.return_statement);
                ret.data = .{ .return_value = value };
                try statements.append(self.allocator, ret);
                _ = self.match(.period);
                break;
            }

            const stmt = try self.expression();
            try statements.append(self.allocator, stmt);

            if (!self.match(.period)) {
                break;
            }
        }

        if (!self.match(.close_bracket)) {
            return ParseError.UnexpectedToken;
        }

        const block = try self.createNode(.block);
        block.data = .{
            .block = .{
                .parameters = try parameters.toOwnedSlice(self.allocator),
                .temporaries = try temporaries.toOwnedSlice(self.allocator),
                .statements = try statements.toOwnedSlice(self.allocator),
            },
        };
        return block;
    }

    fn parseLiteralArray(self: *Parser) ParseError!*ASTNode {
        _ = self.match(.hash);
        if (!self.match(.open_paren)) {
            return ParseError.UnexpectedToken;
        }

        var elements: std.ArrayList(*ASTNode) = .{};
        errdefer elements.deinit(self.allocator);

        while (!self.check(.close_paren) and !self.isAtEnd()) {
            // Literal array can contain: numbers, strings, symbols, characters, other arrays
            if (self.check(.integer) or self.check(.float) or self.check(.scaled_decimal) or
                self.check(.string) or self.check(.symbol) or
                self.check(.character) or self.check(.identifier))
            {
                const elem = try self.primary();
                try elements.append(self.allocator, elem);
            } else if (self.check(.hash) or self.check(.open_paren)) {
                // Nested array
                if (self.check(.hash)) {
                    const nested = try self.parseLiteralArray();
                    try elements.append(self.allocator, nested);
                } else {
                    // Parenthesized nested array without # (shorthand in literal arrays)
                    _ = self.advance(); // consume (
                    // Recursively parse nested array content
                    var nested_elements: std.ArrayList(*ASTNode) = .{};
                    while (!self.check(.close_paren) and !self.isAtEnd()) {
                        if (self.check(.integer) or self.check(.float) or self.check(.scaled_decimal) or
                            self.check(.string) or self.check(.symbol) or
                            self.check(.character) or self.check(.identifier))
                        {
                            const elem = try self.primary();
                            try nested_elements.append(self.allocator, elem);
                        } else if (self.check(.hash) or self.check(.open_paren)) {
                            // Recursively handle nested arrays
                            if (self.check(.hash)) {
                                const nested = try self.parseLiteralArray();
                                try nested_elements.append(self.allocator, nested);
                            } else {
                                // Skip deeply nested parentheses for now
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    if (!self.match(.close_paren)) {
                        return ParseError.UnexpectedToken;
                    }
                    const nested_arr = try self.createNode(.literal_array);
                    nested_arr.data = .{ .elements = try nested_elements.toOwnedSlice(self.allocator) };
                    try elements.append(self.allocator, nested_arr);
                }
            } else {
                break;
            }
        }

        if (!self.match(.close_paren)) {
            return ParseError.UnexpectedToken;
        }

        const arr = try self.createNode(.literal_array);
        arr.data = .{ .elements = try elements.toOwnedSlice(self.allocator) };
        return arr;
    }

    fn parseDynamicArray(self: *Parser) ParseError!*ASTNode {
        var elements: std.ArrayList(*ASTNode) = .{};
        errdefer elements.deinit(self.allocator);

        while (!self.check(.close_brace) and !self.isAtEnd()) {
            const elem = try self.expression();
            try elements.append(self.allocator, elem);

            if (!self.match(.period)) {
                break;
            }
        }

        if (!self.match(.close_brace)) {
            return ParseError.UnexpectedToken;
        }

        const arr = try self.createNode(.literal_array);
        arr.data = .{ .elements = try elements.toOwnedSlice(self.allocator) };
        return arr;
    }

    fn parseMessage(self: *Parser) ParseError!ASTNode.CascadeMessage {
        // Parse a message (for cascade)
        if (self.check(.keyword)) {
            var selector: std.ArrayList(u8) = .{};
            defer selector.deinit(self.allocator);

            var arguments: std.ArrayList(*ASTNode) = .{};
            errdefer arguments.deinit(self.allocator);

            while (self.check(.keyword)) {
                const keyword_token = self.current;
                self.advance();
                try selector.appendSlice(self.allocator, keyword_token.text);

                const arg = try self.binaryMessage();
                try arguments.append(self.allocator, arg);
            }

            return .{
                .selector = try selector.toOwnedSlice(self.allocator),
                .arguments = try arguments.toOwnedSlice(self.allocator),
            };
        } else if (self.check(.binary_selector)) {
            const op_token = self.current;
            self.advance();

            const arg = try self.unaryMessage();
            var args = try self.allocator.alloc(*ASTNode, 1);
            args[0] = arg;

            return .{
                .selector = op_token.text,
                .arguments = args,
            };
        } else if (self.check(.identifier)) {
            const selector_token = self.current;
            self.advance();

            return .{
                .selector = selector_token.text,
                .arguments = &[_]*ASTNode{},
            };
        }

        return ParseError.UnexpectedToken;
    }

    fn parseInteger(self: *Parser, text: []const u8) ParseError!i64 {
        _ = self;
        // Handle radix notation (16rFF)
        if (std.mem.indexOfScalar(u8, text, 'r')) |r_pos| {
            const radix = std.fmt.parseInt(u8, text[0..r_pos], 10) catch {
                return ParseError.InvalidNumber;
            };
            const digits = text[r_pos + 1 ..];
            return std.fmt.parseInt(i64, digits, radix) catch {
                return ParseError.InvalidNumber;
            };
        }

        return std.fmt.parseInt(i64, text, 10) catch {
            return ParseError.InvalidNumber;
        };
    }

    fn parseFloat(self: *Parser, text: []const u8) ParseError!f64 {
        _ = self;
        // Handle double float suffix (e.g., "2.0d0")
        var float_text = text;
        for (text, 0..) |c, i| {
            if (c == 'd' or c == 'D') {
                float_text = text[0..i];
                break;
            }
        }
        return std.fmt.parseFloat(f64, float_text) catch {
            return ParseError.InvalidNumber;
        };
    }

    fn parseScaledDecimal(self: *Parser, text: []const u8) ParseError!ScaledDecimalData {
        _ = self;
        // Parse "2.0s3" format - value is 2.0, scale is 3
        var value_end: usize = 0;
        for (text, 0..) |c, i| {
            if (c == 's' or c == 'S') {
                value_end = i;
                break;
            }
        }
        if (value_end == 0) {
            return ParseError.InvalidNumber;
        }
        const value = std.fmt.parseFloat(f64, text[0..value_end]) catch {
            return ParseError.InvalidNumber;
        };
        const scale_str = text[value_end + 1 ..];
        const scale: u8 = if (scale_str.len > 0)
            std.fmt.parseInt(u8, scale_str, 10) catch 0
        else
            0;
        return .{ .value = value, .scale = scale };
    }

    fn createNode(self: *Parser, node_type: NodeType) ParseError!*ASTNode {
        const node = self.allocator.create(ASTNode) catch {
            return ParseError.OutOfMemory;
        };
        node.* = .{
            .node_type = node_type,
            .token = self.previous,
            .data = .{ .none = {} },
        };
        return node;
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.lexer.nextToken();
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (self.check(token_type)) {
            self.advance();
            return true;
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        return self.current.type == token_type;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.current.type == .eof;
    }
};

// Tests

test "Parser - integer literal" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "42");

    const node = try parser.parseExpression();
    defer allocator.destroy(node);

    try std.testing.expectEqual(NodeType.literal_integer, node.node_type);
    try std.testing.expectEqual(@as(i64, 42), node.data.integer);
}

test "Parser - binary message" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "3 + 4");

    const node = try parser.parseExpression();
    defer {
        // Clean up - this is simplified, real cleanup would be recursive
        allocator.destroy(node.data.message.receiver);
        allocator.free(node.data.message.arguments);
        allocator.destroy(node);
    }

    try std.testing.expectEqual(NodeType.message_send, node.node_type);
    try std.testing.expectEqualStrings("+", node.data.message.selector);
}

test "Parser - unary message" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "42 negated");

    const node = try parser.parseExpression();
    defer {
        allocator.destroy(node.data.message.receiver);
        allocator.destroy(node);
    }

    try std.testing.expectEqual(NodeType.message_send, node.node_type);
    try std.testing.expectEqualStrings("negated", node.data.message.selector);
}

test "Parser - keyword message" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "array at: 1");

    const node = try parser.parseExpression();
    defer {
        allocator.destroy(node.data.message.receiver);
        for (node.data.message.arguments) |arg| {
            allocator.destroy(arg);
        }
        allocator.free(@constCast(node.data.message.arguments));
        allocator.free(@constCast(node.data.message.selector));
        allocator.destroy(node);
    }

    try std.testing.expectEqual(NodeType.message_send, node.node_type);
    try std.testing.expectEqualStrings("at:", node.data.message.selector);
}

test "Parser - pseudo variables" {
    const allocator = std.testing.allocator;

    var parser1 = Parser.init(allocator, "nil");
    const nil_node = try parser1.parseExpression();
    defer allocator.destroy(nil_node);
    try std.testing.expectEqual(NodeType.pseudo_variable, nil_node.node_type);
    try std.testing.expectEqualStrings("nil", nil_node.data.name);

    var parser2 = Parser.init(allocator, "true");
    const true_node = try parser2.parseExpression();
    defer allocator.destroy(true_node);
    try std.testing.expectEqual(NodeType.pseudo_variable, true_node.node_type);
}
