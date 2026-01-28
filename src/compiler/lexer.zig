const std = @import("std");

pub const TokenType = enum {
    // Literals
    integer,
    float,
    scaled_decimal, // 2.0s3
    string,
    symbol,
    character,

    // Identifiers and keywords
    identifier, // foo, Bar, myVar
    keyword, // at:, ifTrue:ifFalse:
    binary_selector, // +, -, *, /, <, >, =, ~=, @

    // Special tokens
    assignment, // :=
    return_op, // ^
    open_paren, // (
    close_paren, // )
    open_bracket, // [
    close_bracket, // ]
    open_brace, // {
    close_brace, // }
    period, // .
    semicolon, // ;
    colon, // :
    bar, // |
    hash, // #
    double_hash, // ## (compile-time constant)

    // Special
    eof,
    error_token,
};

pub const Token = struct {
    type: TokenType,
    text: []const u8,
    line: usize,
    column: usize,

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}(\"{s}\")", .{ @tagName(self.type), self.text });
    }
};

pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: usize,
    column: usize,
    start_pos: usize,
    start_line: usize,
    start_column: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .start_pos = 0,
            .start_line = 1,
            .start_column = 1,
        };
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespaceAndComments();

        self.start_pos = self.pos;
        self.start_line = self.line;
        self.start_column = self.column;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        // Single-character tokens
        switch (c) {
            '(' => return self.makeToken(.open_paren),
            ')' => return self.makeToken(.close_paren),
            '[' => return self.makeToken(.open_bracket),
            ']' => return self.makeToken(.close_bracket),
            '{' => return self.makeToken(.open_brace),
            '}' => return self.makeToken(.close_brace),
            '.' => return self.makeToken(.period),
            ';' => return self.makeToken(.semicolon),
            '|' => return self.makeToken(.bar),
            '^' => return self.makeToken(.return_op),

            ':' => {
                if (self.match('=')) {
                    return self.makeToken(.assignment);
                }
                return self.makeToken(.colon);
            },

            '#' => {
                // Symbol or literal array
                if (self.peek() == '(' or self.peek() == '[') {
                    return self.makeToken(.hash);
                }
                // Compile-time constant ##(
                if (self.peek() == '#') {
                    // Check if it's ##( by looking two characters ahead
                    if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '(') {
                        _ = self.advance(); // consume second # only for ##(
                        return self.makeToken(.double_hash);
                    }
                    // Not ##( - just return the first # as hash
                    // The next call will see the second # and handle it
                    return self.makeToken(.hash);
                }
                if (self.peek() == '\'') {
                    // Quoted symbol #'...'
                    _ = self.advance(); // consume '
                    return self.scanQuotedSymbol();
                }
                // Regular symbol
                return self.scanSymbol();
            },

            '\'' => return self.scanString(),
            '$' => return self.scanCharacter(),

            else => {},
        }

        // Numbers
        if (isDigit(c) or (c == '-' and isDigit(self.peek()))) {
            return self.scanNumber();
        }

        // Identifiers and keywords
        if (isIdentifierStart(c)) {
            return self.scanIdentifierOrKeyword();
        }

        // Binary selectors
        if (isBinaryChar(c)) {
            return self.scanBinarySelector();
        }

        return self.errorToken("Unexpected character");
    }

    fn scanString(self: *Lexer) Token {
        // Scan string with proper handling of escaped quotes ('')
        // A quote is escaped if immediately followed by another quote
        while (true) {
            // Scan until we find a quote
            while (!self.isAtEnd() and self.peek() != '\'') {
                if (self.peek() == '\n') {
                    self.line += 1;
                    self.column = 0;
                }
                _ = self.advance();
            }

            if (self.isAtEnd()) {
                return self.errorToken("Unterminated string");
            }

            // Consume the quote
            _ = self.advance();

            // Check if it's an escaped quote ('')
            if (self.peek() == '\'') {
                // Escaped quote - consume it and continue scanning
                _ = self.advance();
                // Continue the outer loop to scan more content
            } else {
                // Not escaped - this was the closing quote
                break;
            }
        }

        return self.makeToken(.string);
    }

    fn scanQuotedSymbol(self: *Lexer) Token {
        while (!self.isAtEnd() and self.peek() != '\'') {
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated symbol");
        }

        _ = self.advance(); // consume closing quote
        return self.makeToken(.symbol);
    }

    fn scanSymbol(self: *Lexer) Token {
        // Symbol can be an identifier, keyword(s), or binary selector
        if (isIdentifierStart(self.peek())) {
            while (isIdentifierChar(self.peek())) {
                _ = self.advance();
            }
            // Check for keyword symbol
            while (self.peek() == ':') {
                _ = self.advance();
                while (isIdentifierChar(self.peek())) {
                    _ = self.advance();
                }
            }
        } else if (isBinaryChar(self.peek())) {
            while (isBinaryChar(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.symbol);
    }

    fn scanCharacter(self: *Lexer) Token {
        if (self.isAtEnd()) {
            return self.errorToken("Unexpected end of character literal");
        }
        _ = self.advance(); // consume the character
        return self.makeToken(.character);
    }

    fn scanNumber(self: *Lexer) Token {
        // Handle negative sign
        if (self.source[self.start_pos] == '-') {
            // Already consumed
        }

        // Integer part
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Check for radix notation (16rFF)
        if (self.peek() == 'r') {
            _ = self.advance();
            while (isHexDigit(self.peek())) {
                _ = self.advance();
            }
            return self.makeToken(.integer);
        }

        // Check for decimal point
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance(); // consume '.'
            while (isDigit(self.peek())) {
                _ = self.advance();
            }

            // Check for scaled decimal suffix (2.0s3)
            if (self.peek() == 's' or self.peek() == 'S') {
                _ = self.advance();
                while (isDigit(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.scaled_decimal);
            }

            // Check for double float suffix (2.0d0)
            if (self.peek() == 'd' or self.peek() == 'D') {
                _ = self.advance();
                if (self.peek() == '+' or self.peek() == '-') {
                    _ = self.advance();
                }
                while (isDigit(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.float); // treat double as float for now
            }

            // Check for exponent
            if (self.peek() == 'e' or self.peek() == 'E') {
                _ = self.advance();
                if (self.peek() == '+' or self.peek() == '-') {
                    _ = self.advance();
                }
                while (isDigit(self.peek())) {
                    _ = self.advance();
                }
            }

            return self.makeToken(.float);
        }

        return self.makeToken(.integer);
    }

    fn scanIdentifierOrKeyword(self: *Lexer) Token {
        while (isIdentifierChar(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == ':' and self.peekNext() != '=') {
            // It's a keyword
            _ = self.advance(); // consume ':'
            return self.makeToken(.keyword);
        }

        return self.makeToken(.identifier);
    }

    fn scanBinarySelector(self: *Lexer) Token {
        while (isBinaryChar(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.binary_selector);
    }

    fn skipWhitespaceAndComments(self: *Lexer) void {
        while (true) {
            if (self.isAtEnd()) return;

            const c = self.peek();

            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 1;
                },
                '"' => {
                    // Comment - skip until closing "
                    _ = self.advance();
                    while (!self.isAtEnd() and self.peek() != '"') {
                        if (self.peek() == '\n') {
                            self.line += 1;
                            self.column = 0;
                        }
                        _ = self.advance();
                    }
                    if (!self.isAtEnd()) {
                        _ = self.advance(); // consume closing "
                    }
                },
                else => return,
            }
        }
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        self.column += 1;
        return c;
    }

    pub fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.pos] != expected) return false;
        self.pos += 1;
        self.column += 1;
        return true;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.pos >= self.source.len;
    }

    fn makeToken(self: *Lexer, token_type: TokenType) Token {
        return .{
            .type = token_type,
            .text = self.source[self.start_pos..self.pos],
            .line = self.start_line,
            .column = self.start_column,
        };
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return .{
            .type = .error_token,
            .text = message,
            .line = self.line,
            .column = self.column,
        };
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isIdentifierStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentifierChar(c: u8) bool {
    return isIdentifierStart(c) or isDigit(c);
}

fn isBinaryChar(c: u8) bool {
    return switch (c) {
        '+', '-', '*', '/', '\\', '~', '<', '>', '=', '@', '%', '|', '&', '?', '!' => true,
        ',' => true, // for string concatenation
        else => false,
    };
}

// Tests

test "Lexer - basic tokens" {
    var lexer = Lexer.init("()[]{}");

    try std.testing.expectEqual(TokenType.open_paren, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.close_paren, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.open_bracket, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.close_bracket, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.open_brace, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.close_brace, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.eof, lexer.nextToken().type);
}

test "Lexer - numbers" {
    var lexer = Lexer.init("42 3.14 16rFF");

    const int_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.integer, int_tok.type);
    try std.testing.expectEqualStrings("42", int_tok.text);

    const float_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.float, float_tok.type);
    try std.testing.expectEqualStrings("3.14", float_tok.text);

    const hex_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.integer, hex_tok.type);
    try std.testing.expectEqualStrings("16rFF", hex_tok.text);
}

test "Lexer - strings and symbols" {
    var lexer = Lexer.init("'hello' #foo #'hello world'");

    const str_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.string, str_tok.type);
    try std.testing.expectEqualStrings("'hello'", str_tok.text);

    const sym_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.symbol, sym_tok.type);
    try std.testing.expectEqualStrings("#foo", sym_tok.text);

    const quoted_sym_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.symbol, quoted_sym_tok.type);
    try std.testing.expectEqualStrings("#'hello world'", quoted_sym_tok.text);
}

test "Lexer - identifiers and keywords" {
    var lexer = Lexer.init("foo at: ifTrue:ifFalse:");

    const id_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.identifier, id_tok.type);
    try std.testing.expectEqualStrings("foo", id_tok.text);

    const kw_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.keyword, kw_tok.type);
    try std.testing.expectEqualStrings("at:", kw_tok.text);

    const id2_tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.keyword, id2_tok.type);
    try std.testing.expectEqualStrings("ifTrue:", id2_tok.text);
}

test "Lexer - operators" {
    var lexer = Lexer.init("+ - * / := ^");

    try std.testing.expectEqual(TokenType.binary_selector, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.binary_selector, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.binary_selector, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.binary_selector, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.assignment, lexer.nextToken().type);
    try std.testing.expectEqual(TokenType.return_op, lexer.nextToken().type);
}

test "Lexer - simple expression" {
    var lexer = Lexer.init("3 + 4");

    const n1 = lexer.nextToken();
    try std.testing.expectEqual(TokenType.integer, n1.type);
    try std.testing.expectEqualStrings("3", n1.text);

    const op = lexer.nextToken();
    try std.testing.expectEqual(TokenType.binary_selector, op.type);
    try std.testing.expectEqualStrings("+", op.text);

    const n2 = lexer.nextToken();
    try std.testing.expectEqual(TokenType.integer, n2.type);
    try std.testing.expectEqualStrings("4", n2.text);
}

test "Lexer - comments" {
    var lexer = Lexer.init("\"this is a comment\" 42");

    const tok = lexer.nextToken();
    try std.testing.expectEqual(TokenType.integer, tok.type);
    try std.testing.expectEqualStrings("42", tok.text);
}
