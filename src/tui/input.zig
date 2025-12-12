const std = @import("std");

pub const MouseButton = enum {
    left,
    middle,
    right,
    scroll_up,
    scroll_down,
    none,
};

pub const MouseEventType = enum {
    press,
    release,
    drag,
    move,
};

pub const MouseEvent = struct {
    button: MouseButton,
    event_type: MouseEventType,
    x: u16,
    y: u16,
    shift: bool = false,
    ctrl: bool = false,
    alt: bool = false,
};

pub const InputEvent = union(enum) {
    key: Key,
    mouse: MouseEvent,
    paste: []const u8, // Bracketed paste content
};

pub const Key = union(enum) {
    char: u21,
    ctrl: u8, // Ctrl+A = 1, Ctrl+B = 2, etc.
    alt: u21,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    f7,
    f8,
    f9,
    f10,
    f11,
    f12,
    up,
    down,
    left,
    right,
    home,
    end,
    page_up,
    page_down,
    insert,
    delete,
    backspace,
    tab,
    shift_tab,
    enter,
    escape,
    unknown,

    // Shift+arrow combinations
    shift_up,
    shift_down,
    shift_left,
    shift_right,
    shift_home,
    shift_end,
    shift_insert,

    // Ctrl+arrow combinations
    ctrl_up,
    ctrl_down,
    ctrl_left,
    ctrl_right,

    pub fn isCtrl(self: Key, char: u8) bool {
        return switch (self) {
            .ctrl => |c| c == char,
            else => false,
        };
    }

    pub fn format(self: Key, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .char => |c| try writer.print("'{c}'", .{@as(u8, @intCast(c))}),
            .ctrl => |c| try writer.print("Ctrl+{c}", .{c + 'A' - 1}),
            .alt => |c| try writer.print("Alt+{c}", .{@as(u8, @intCast(c))}),
            .f1 => try writer.writeAll("F1"),
            .f2 => try writer.writeAll("F2"),
            .f3 => try writer.writeAll("F3"),
            .f4 => try writer.writeAll("F4"),
            .f5 => try writer.writeAll("F5"),
            .f6 => try writer.writeAll("F6"),
            .f7 => try writer.writeAll("F7"),
            .f8 => try writer.writeAll("F8"),
            .f9 => try writer.writeAll("F9"),
            .f10 => try writer.writeAll("F10"),
            .f11 => try writer.writeAll("F11"),
            .f12 => try writer.writeAll("F12"),
            .up => try writer.writeAll("Up"),
            .down => try writer.writeAll("Down"),
            .left => try writer.writeAll("Left"),
            .right => try writer.writeAll("Right"),
            .home => try writer.writeAll("Home"),
            .end => try writer.writeAll("End"),
            .page_up => try writer.writeAll("PageUp"),
            .page_down => try writer.writeAll("PageDown"),
            .insert => try writer.writeAll("Insert"),
            .delete => try writer.writeAll("Delete"),
            .backspace => try writer.writeAll("Backspace"),
            .tab => try writer.writeAll("Tab"),
            .shift_tab => try writer.writeAll("Shift+Tab"),
            .enter => try writer.writeAll("Enter"),
            .escape => try writer.writeAll("Escape"),
            .shift_up => try writer.writeAll("Shift+Up"),
            .shift_down => try writer.writeAll("Shift+Down"),
            .shift_left => try writer.writeAll("Shift+Left"),
            .shift_right => try writer.writeAll("Shift+Right"),
            .shift_home => try writer.writeAll("Shift+Home"),
            .shift_end => try writer.writeAll("Shift+End"),
            .shift_insert => try writer.writeAll("Shift+Insert"),
            .ctrl_up => try writer.writeAll("Ctrl+Up"),
            .ctrl_down => try writer.writeAll("Ctrl+Down"),
            .ctrl_left => try writer.writeAll("Ctrl+Left"),
            .ctrl_right => try writer.writeAll("Ctrl+Right"),
            .unknown => try writer.writeAll("Unknown"),
        }
    }
};

pub const InputReader = struct {
    tty: std.fs.File,
    buf: [32]u8 = undefined,
    buf_len: usize = 0,
    allocator: std.mem.Allocator,

    // Paste buffer for bracketed paste mode
    paste_buf: ?[]u8 = null,

    pub fn init(terminal: *const @import("terminal.zig").Terminal, allocator: std.mem.Allocator) InputReader {
        return .{
            .tty = terminal.tty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *InputReader) void {
        if (self.paste_buf) |buf| {
            self.allocator.free(buf);
            self.paste_buf = null;
        }
    }

    /// Free paste buffer after handling paste event
    pub fn freePasteBuffer(self: *InputReader) void {
        if (self.paste_buf) |buf| {
            self.allocator.free(buf);
            self.paste_buf = null;
        }
    }

    pub fn readEvent(self: *InputReader) !?InputEvent {
        // Read available bytes
        const bytes_read = self.tty.read(&self.buf) catch |err| {
            if (err == error.WouldBlock) return null;
            return err;
        };

        if (bytes_read == 0) return null;

        self.buf_len = bytes_read;

        // Check for bracketed paste start (ESC [ 200 ~)
        if (self.buf_len >= 6 and self.buf[0] == 27 and self.buf[1] == '[' and
            self.buf[2] == '2' and self.buf[3] == '0' and self.buf[4] == '0' and self.buf[5] == '~')
        {
            if (self.readBracketedPaste()) |paste_content| {
                return .{ .paste = paste_content };
            }
        }

        // Check for mouse event (SGR format: ESC [ < ...)
        if (self.buf_len >= 3 and self.buf[0] == 27 and self.buf[1] == '[' and self.buf[2] == '<') {
            if (self.parseMouseEvent()) |mouse| {
                return .{ .mouse = mouse };
            }
        }

        return .{ .key = self.parseKey() };
    }

    fn readBracketedPaste(self: *InputReader) ?[]const u8 {
        // Free any previous paste buffer
        self.freePasteBuffer();

        // Allocate buffer for paste content
        var paste_data: std.ArrayList(u8) = .empty;
        defer paste_data.deinit(self.allocator);

        // Add any content after the start sequence
        if (self.buf_len > 6) {
            paste_data.appendSlice(self.allocator, self.buf[6..self.buf_len]) catch return null;
        }

        // Read until we find the end sequence (ESC [ 201 ~)
        var read_buf: [256]u8 = undefined;
        const end_seq = "\x1b[201~";
        const max_paste_size: usize = 1024 * 1024; // 1MB limit

        while (paste_data.items.len < max_paste_size) {
            // Check if we already have the end sequence
            if (paste_data.items.len >= 6) {
                if (std.mem.indexOf(u8, paste_data.items, end_seq)) |pos| {
                    // Found end sequence - extract content
                    const content = self.allocator.dupe(u8, paste_data.items[0..pos]) catch return null;
                    self.paste_buf = content;
                    return content;
                }
            }

            // Read more data
            const bytes_read = self.tty.read(&read_buf) catch break;
            if (bytes_read == 0) {
                std.Thread.sleep(1 * std.time.ns_per_ms);
                continue;
            }

            paste_data.appendSlice(self.allocator, read_buf[0..bytes_read]) catch break;
        }

        // Check one more time for end sequence
        if (std.mem.indexOf(u8, paste_data.items, end_seq)) |pos| {
            const content = self.allocator.dupe(u8, paste_data.items[0..pos]) catch return null;
            self.paste_buf = content;
            return content;
        }

        return null;
    }

    pub fn readKey(self: *InputReader) !?Key {
        const event = try self.readEvent();
        if (event) |e| {
            return switch (e) {
                .key => |k| k,
                .mouse => null, // Ignore mouse events in readKey
                .paste => null, // Ignore paste events in readKey
            };
        }
        return null;
    }

    fn parseMouseEvent(self: *InputReader) ?MouseEvent {
        // SGR format: ESC [ < button ; x ; y ; M/m
        // button encodes: bits 0-1 = button, bit 2 = shift, bit 3 = alt, bit 4 = ctrl
        // bit 5 = motion, bits 6-7 = scroll

        if (self.buf_len < 9) return null; // Minimum: ESC [ < 0 ; 1 ; 1 M

        // Find the parameters - start after '<'
        var pos: usize = 3;
        var params: [3]u16 = .{ 0, 0, 0 };
        var param_idx: usize = 0;

        while (pos < self.buf_len and param_idx < 3) {
            const c = self.buf[pos];
            if (c >= '0' and c <= '9') {
                params[param_idx] = params[param_idx] * 10 + (c - '0');
            } else if (c == ';') {
                param_idx += 1;
            } else if (c == 'M' or c == 'm') {
                // Found terminator
                const button_code = params[0];
                const x = if (params[1] > 0) params[1] - 1 else 0; // 1-based to 0-based
                const y = if (params[2] > 0) params[2] - 1 else 0;

                const is_release = (c == 'm');
                const is_motion = (button_code & 32) != 0;
                const is_scroll = (button_code & 64) != 0;

                const shift = (button_code & 4) != 0;
                const alt = (button_code & 8) != 0;
                const ctrl = (button_code & 16) != 0;

                var button: MouseButton = .none;
                var event_type: MouseEventType = .press;

                if (is_scroll) {
                    button = if ((button_code & 1) != 0) .scroll_down else .scroll_up;
                    event_type = .press;
                } else {
                    button = switch (button_code & 3) {
                        0 => .left,
                        1 => .middle,
                        2 => .right,
                        else => .none,
                    };

                    if (is_release) {
                        event_type = .release;
                    } else if (is_motion) {
                        event_type = if (button != .none) .drag else .move;
                    } else {
                        event_type = .press;
                    }
                }

                return MouseEvent{
                    .button = button,
                    .event_type = event_type,
                    .x = x,
                    .y = y,
                    .shift = shift,
                    .ctrl = ctrl,
                    .alt = alt,
                };
            } else {
                break; // Invalid character
            }
            pos += 1;
        }

        return null;
    }

    fn parseKey(self: *InputReader) Key {
        if (self.buf_len == 0) return .unknown;

        const c = self.buf[0];

        // Control characters (Ctrl+A through Ctrl+Z)
        if (c < 27 and c != 9 and c != 10 and c != 13) {
            return .{ .ctrl = c };
        }

        // Tab
        if (c == 9) return .tab;

        // Enter/Return
        if (c == 10 or c == 13) return .enter;

        // Escape or escape sequence
        if (c == 27) {
            if (self.buf_len == 1) return .escape;
            return self.parseEscapeSequence();
        }

        // Backspace (127 or 8)
        if (c == 127 or c == 8) return .backspace;

        // Regular character (possibly UTF-8)
        if (c >= 32 and c < 127) {
            return .{ .char = c };
        }

        // UTF-8 multi-byte character
        if (c >= 0xC0) {
            const codepoint = self.parseUtf8() catch return .unknown;
            return .{ .char = codepoint };
        }

        return .unknown;
    }

    fn parseEscapeSequence(self: *InputReader) Key {
        if (self.buf_len < 2) return .escape;

        // Alt+key
        if (self.buf[1] != '[' and self.buf[1] != 'O') {
            if (self.buf[1] >= 32 and self.buf[1] < 127) {
                return .{ .alt = self.buf[1] };
            }
            return .escape;
        }

        // CSI sequences (ESC [)
        if (self.buf[1] == '[') {
            if (self.buf_len < 3) return .escape;

            // Check for shift+tab (ESC [ Z)
            if (self.buf[2] == 'Z') return .shift_tab;

            // Arrow keys
            if (self.buf[2] == 'A') return .up;
            if (self.buf[2] == 'B') return .down;
            if (self.buf[2] == 'C') return .right;
            if (self.buf[2] == 'D') return .left;
            if (self.buf[2] == 'H') return .home;
            if (self.buf[2] == 'F') return .end;

            // Extended sequences (ESC [ n ~)
            if (self.buf_len >= 4 and self.buf[3] == '~') {
                return switch (self.buf[2]) {
                    '1' => .home,
                    '2' => .insert,
                    '3' => .delete,
                    '4' => .end,
                    '5' => .page_up,
                    '6' => .page_down,
                    else => .unknown,
                };
            }

            // Longer sequences (ESC [ n n ~)
            if (self.buf_len >= 5 and self.buf[4] == '~') {
                const num = (self.buf[2] - '0') * 10 + (self.buf[3] - '0');
                return switch (num) {
                    11 => .f1,
                    12 => .f2,
                    13 => .f3,
                    14 => .f4,
                    15 => .f5,
                    17 => .f6,
                    18 => .f7,
                    19 => .f8,
                    20 => .f9,
                    21 => .f10,
                    23 => .f11,
                    24 => .f12,
                    else => .unknown,
                };
            }

            // Modified insert/delete/etc (ESC [ n ; m ~) e.g., Shift+Insert = ESC [ 2 ; 2 ~
            if (self.buf_len >= 6 and self.buf[3] == ';' and self.buf[5] == '~') {
                const key_num = self.buf[2];
                const modifier = self.buf[4];

                // Shift modifier (2)
                if (modifier == '2') {
                    if (key_num == '2') return .shift_insert;
                }
            }

            // Modified keys (ESC [ 1 ; n X)
            if (self.buf_len >= 6 and self.buf[2] == '1' and self.buf[3] == ';') {
                const modifier = self.buf[4];
                const key_char = self.buf[5];

                // Shift modifier (2)
                if (modifier == '2') {
                    return switch (key_char) {
                        'A' => .shift_up,
                        'B' => .shift_down,
                        'C' => .shift_right,
                        'D' => .shift_left,
                        'H' => .shift_home,
                        'F' => .shift_end,
                        else => .unknown,
                    };
                }

                // Ctrl modifier (5)
                if (modifier == '5') {
                    return switch (key_char) {
                        'A' => .ctrl_up,
                        'B' => .ctrl_down,
                        'C' => .ctrl_right,
                        'D' => .ctrl_left,
                        else => .unknown,
                    };
                }
            }
        }

        // SS3 sequences (ESC O) - F1-F4 on some terminals
        if (self.buf[1] == 'O' and self.buf_len >= 3) {
            return switch (self.buf[2]) {
                'P' => .f1,
                'Q' => .f2,
                'R' => .f3,
                'S' => .f4,
                'H' => .home,
                'F' => .end,
                else => .unknown,
            };
        }

        return .escape;
    }

    fn parseUtf8(self: *InputReader) !u21 {
        const c = self.buf[0];

        // 2-byte sequence
        if (c >= 0xC0 and c < 0xE0) {
            if (self.buf_len < 2) return error.InvalidUtf8;
            const result = (@as(u21, c & 0x1F) << 6) | (self.buf[1] & 0x3F);
            return result;
        }

        // 3-byte sequence
        if (c >= 0xE0 and c < 0xF0) {
            if (self.buf_len < 3) return error.InvalidUtf8;
            const result = (@as(u21, c & 0x0F) << 12) |
                (@as(u21, self.buf[1] & 0x3F) << 6) |
                (self.buf[2] & 0x3F);
            return result;
        }

        // 4-byte sequence
        if (c >= 0xF0 and c < 0xF8) {
            if (self.buf_len < 4) return error.InvalidUtf8;
            const result = (@as(u21, c & 0x07) << 18) |
                (@as(u21, self.buf[1] & 0x3F) << 12) |
                (@as(u21, self.buf[2] & 0x3F) << 6) |
                (self.buf[3] & 0x3F);
            return result;
        }

        return error.InvalidUtf8;
    }
};
