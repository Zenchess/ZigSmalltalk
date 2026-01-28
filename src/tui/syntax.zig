//! Smalltalk Syntax Highlighter
//!
//! Provides syntax highlighting for Smalltalk code in the TUI.
//! Based on standard Smalltalk syntax elements:
//! - Comments: "..." (double quotes)
//! - Strings: '...' (single quotes)
//! - Symbols: #word or #'string'
//! - Keywords: self, super, true, false, nil, thisContext
//! - Block parameters: :param
//! - Character literals: $a
//! - Numbers: integers and floats
//! - Globals/Classes: Start with uppercase
//! - Operators: ^, :=, binary operators

const std = @import("std");
const style = @import("style.zig");
const Style = style.Style;

/// Token types for Smalltalk syntax
pub const TokenType = enum {
    normal,
    comment,
    string,
    symbol,
    character,
    number,
    keyword, // true, false, nil, thisContext
    self_super, // self, super
    block_param, // :param
    class_name, // Uppercase identifier
    operator, // ^, :=, binary operators
    assignment, // :=
    return_op, // ^
    bracket, // [ ] ( ) { }
    cascade, // ;
    period, // .
};

/// Get the style for a token type
pub fn styleForToken(token_type: TokenType) Style {
    return switch (token_type) {
        .comment => Style{ .fg = style.syntax.comment, .italic = true },
        .string => Style{ .fg = style.syntax.string },
        .symbol => Style{ .fg = style.syntax.symbol },
        .character => Style{ .fg = style.syntax.string },
        .number => Style{ .fg = style.syntax.number },
        .keyword => Style{ .fg = style.syntax.keyword, .bold = true },
        .self_super => Style{ .fg = style.syntax.self_var, .bold = true },
        .block_param => Style{ .fg = style.syntax.block },
        .class_name => Style{ .fg = style.syntax.class_name },
        .operator => Style{ .fg = style.syntax.operator },
        .assignment => Style{ .fg = style.syntax.operator, .bold = true },
        .return_op => Style{ .fg = style.syntax.operator, .bold = true },
        .bracket => Style{ .fg = style.syntax.block },
        .cascade => Style{ .fg = style.syntax.operator },
        .period => Style{ .fg = style.ui.foreground_dim },
        .normal => Style{ .fg = style.ui.foreground },
    };
}

/// Smalltalk syntax highlighter
/// Processes a line and returns token types for each character position
pub const SmalltalkHighlighter = struct {
    /// Highlight a single line of Smalltalk code
    /// Returns an array of token types, one per byte
    pub fn highlightLine(allocator: std.mem.Allocator, line: []const u8, in_comment: bool, in_string: bool) !HighlightResult {
        var tokens = try allocator.alloc(TokenType, line.len);
        @memset(tokens, .normal);

        var i: usize = 0;
        var still_in_comment = in_comment;
        var still_in_string = in_string;

        // Continue from previous line state
        if (still_in_comment) {
            while (i < line.len) {
                tokens[i] = .comment;
                if (line[i] == '"') {
                    still_in_comment = false;
                    i += 1;
                    break;
                }
                i += 1;
            }
        }

        if (still_in_string) {
            while (i < line.len) {
                tokens[i] = .string;
                if (line[i] == '\'') {
                    // Check for escaped quote ''
                    if (i + 1 < line.len and line[i + 1] == '\'') {
                        tokens[i + 1] = .string;
                        i += 2;
                        continue;
                    }
                    still_in_string = false;
                    i += 1;
                    break;
                }
                i += 1;
            }
        }

        while (i < line.len) {
            const c = line[i];

            // Comment: "..."
            if (c == '"') {
                const start = i;
                tokens[i] = .comment;
                i += 1;
                still_in_comment = true;
                while (i < line.len) {
                    tokens[i] = .comment;
                    if (line[i] == '"') {
                        still_in_comment = false;
                        i += 1;
                        break;
                    }
                    i += 1;
                }
                _ = start;
                continue;
            }

            // String: '...'
            if (c == '\'') {
                tokens[i] = .string;
                i += 1;
                still_in_string = true;
                while (i < line.len) {
                    tokens[i] = .string;
                    if (line[i] == '\'') {
                        // Check for escaped quote ''
                        if (i + 1 < line.len and line[i + 1] == '\'') {
                            tokens[i + 1] = .string;
                            i += 2;
                            continue;
                        }
                        still_in_string = false;
                        i += 1;
                        break;
                    }
                    i += 1;
                }
                continue;
            }

            // Symbol: #word or #'string'
            if (c == '#') {
                tokens[i] = .symbol;
                i += 1;
                if (i < line.len) {
                    if (line[i] == '\'') {
                        // Symbol string: #'...'
                        tokens[i] = .symbol;
                        i += 1;
                        while (i < line.len and line[i] != '\'') {
                            tokens[i] = .symbol;
                            i += 1;
                        }
                        if (i < line.len) {
                            tokens[i] = .symbol;
                            i += 1;
                        }
                    } else if (line[i] == '(') {
                        // Literal array: #(...)
                        tokens[i] = .symbol;
                        i += 1;
                    } else if (isIdentStart(line[i])) {
                        // Symbol identifier: #word
                        while (i < line.len and isIdentChar(line[i])) {
                            tokens[i] = .symbol;
                            i += 1;
                        }
                        // Allow colons in symbols (keyword symbols like #at:put:)
                        while (i < line.len and line[i] == ':') {
                            tokens[i] = .symbol;
                            i += 1;
                            while (i < line.len and isIdentChar(line[i])) {
                                tokens[i] = .symbol;
                                i += 1;
                            }
                        }
                    }
                }
                continue;
            }

            // Character literal: $a
            if (c == '$' and i + 1 < line.len) {
                tokens[i] = .character;
                tokens[i + 1] = .character;
                i += 2;
                continue;
            }

            // Block parameter: :param (but not :=)
            if (c == ':' and i + 1 < line.len and line[i + 1] != '=' and isIdentStart(line[i + 1])) {
                tokens[i] = .block_param;
                i += 1;
                while (i < line.len and isIdentChar(line[i])) {
                    tokens[i] = .block_param;
                    i += 1;
                }
                continue;
            }

            // Assignment: :=
            if (c == ':' and i + 1 < line.len and line[i + 1] == '=') {
                tokens[i] = .assignment;
                tokens[i + 1] = .assignment;
                i += 2;
                continue;
            }

            // Return: ^
            if (c == '^') {
                tokens[i] = .return_op;
                i += 1;
                continue;
            }

            // Brackets
            if (c == '[' or c == ']' or c == '(' or c == ')' or c == '{' or c == '}') {
                tokens[i] = .bracket;
                i += 1;
                continue;
            }

            // Cascade: ;
            if (c == ';') {
                tokens[i] = .cascade;
                i += 1;
                continue;
            }

            // Period: .
            if (c == '.') {
                tokens[i] = .period;
                i += 1;
                continue;
            }

            // Number
            if (isDigit(c) or (c == '-' and i + 1 < line.len and isDigit(line[i + 1]))) {
                while (i < line.len and (isDigit(line[i]) or line[i] == '.' or line[i] == 'e' or line[i] == 'E' or line[i] == '-' or line[i] == '+' or line[i] == 'r')) {
                    // Avoid treating method call . as part of number
                    if (line[i] == '.' and i + 1 < line.len and !isDigit(line[i + 1])) break;
                    tokens[i] = .number;
                    i += 1;
                }
                continue;
            }

            // Identifier or keyword
            if (isIdentStart(c)) {
                const start = i;
                while (i < line.len and isIdentChar(line[i])) {
                    i += 1;
                }
                const ident = line[start..i];

                // Check for keywords
                const token_type = identifierType(ident);
                for (start..i) |j| {
                    tokens[j] = token_type;
                }
                continue;
            }

            // Binary operators
            if (isBinaryChar(c)) {
                while (i < line.len and isBinaryChar(line[i])) {
                    tokens[i] = .operator;
                    i += 1;
                }
                continue;
            }

            // Skip whitespace and other characters
            i += 1;
        }

        return HighlightResult{
            .tokens = tokens,
            .continues_comment = still_in_comment,
            .continues_string = still_in_string,
        };
    }

    fn isIdentStart(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isIdentChar(c: u8) bool {
        return isIdentStart(c) or isDigit(c);
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isBinaryChar(c: u8) bool {
        return switch (c) {
            '+', '-', '*', '/', '\\', '~', '<', '>', '=', '@', '%', '|', '&', '?', '!' => true,
            else => false,
        };
    }

    fn identifierType(ident: []const u8) TokenType {
        // Self and super
        if (std.mem.eql(u8, ident, "self") or std.mem.eql(u8, ident, "super")) {
            return .self_super;
        }

        // Keywords (pseudo-variables and literals)
        if (std.mem.eql(u8, ident, "true") or
            std.mem.eql(u8, ident, "false") or
            std.mem.eql(u8, ident, "nil") or
            std.mem.eql(u8, ident, "thisContext"))
        {
            return .keyword;
        }

        // Class names start with uppercase
        if (ident.len > 0 and ident[0] >= 'A' and ident[0] <= 'Z') {
            return .class_name;
        }

        return .normal;
    }
};

/// Result of highlighting a line
pub const HighlightResult = struct {
    tokens: []TokenType,
    continues_comment: bool,
    continues_string: bool,

    pub fn deinit(self: *HighlightResult, allocator: std.mem.Allocator) void {
        allocator.free(self.tokens);
    }
};

/// Line-based highlighter that tracks multi-line state
pub const LineHighlighter = struct {
    allocator: std.mem.Allocator,
    line_states: std.ArrayList(LineState),

    const LineState = struct {
        in_comment: bool = false,
        in_string: bool = false,
    };

    pub fn init(allocator: std.mem.Allocator) LineHighlighter {
        return .{
            .allocator = allocator,
            .line_states = std.ArrayList(LineState).init(allocator),
        };
    }

    pub fn deinit(self: *LineHighlighter) void {
        self.line_states.deinit();
    }

    /// Reset state for a fresh document
    pub fn reset(self: *LineHighlighter) void {
        self.line_states.clearRetainingCapacity();
    }

    /// Get style for a specific character in a line
    pub fn getStyleAt(self: *LineHighlighter, lines: []const []const u8, line_idx: usize, col: usize) Style {
        if (line_idx >= lines.len) return style.styles.normal;
        const line = lines[line_idx];
        if (col >= line.len) return style.styles.normal;

        // Get state from previous line
        var in_comment = false;
        var in_string = false;
        if (line_idx > 0 and line_idx - 1 < self.line_states.items.len) {
            const prev_state = self.line_states.items[line_idx - 1];
            in_comment = prev_state.in_comment;
            in_string = prev_state.in_string;
        }

        // Highlight the line
        var result = SmalltalkHighlighter.highlightLine(self.allocator, line, in_comment, in_string) catch {
            return style.styles.normal;
        };
        defer result.deinit(self.allocator);

        // Update line state
        while (self.line_states.items.len <= line_idx) {
            self.line_states.append(.{}) catch break;
        }
        if (line_idx < self.line_states.items.len) {
            self.line_states.items[line_idx] = .{
                .in_comment = result.continues_comment,
                .in_string = result.continues_string,
            };
        }

        if (col < result.tokens.len) {
            return styleForToken(result.tokens[col]);
        }
        return style.styles.normal;
    }
};
