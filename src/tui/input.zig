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

    // Ctrl+Shift combinations
    ctrl_shift_s, // Save image

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
            .ctrl_shift_s => try writer.writeAll("Ctrl+Shift+S"),
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

    // Track if mouse button is held down (for filtering garbage during drag)
    mouse_button_down: bool = false,

    pub fn init(terminal: *const @import("terminal.zig").Terminal, allocator: std.mem.Allocator) InputReader {
        return .{
            .tty = terminal.tty_input, // Use input handle (separate on Windows)
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
                // Track mouse button state
                if (mouse.event_type == .press) {
                    self.mouse_button_down = true;
                } else if (mouse.event_type == .release) {
                    self.mouse_button_down = false;
                }
                return .{ .mouse = mouse };
            }
            // If mouse parsing failed, it was still a mouse sequence - don't interpret as keys
            return null;
        }

        // Also check for partial mouse sequences that might have lost the ESC prefix
        if (self.buf_len >= 1 and self.buf[0] == '<') {
            return null;
        }

        // If mouse button is held down, discard characters that look like mouse sequence fragments
        // These are typically digits, semicolons, M, m that arrive as fragments during drag
        if (self.mouse_button_down and self.buf_len >= 1) {
            const c = self.buf[0];
            if (c == ';' or c == 'M' or c == 'm' or (c >= '0' and c <= '9')) {
                // Likely a mouse coordinate fragment - discard
                return null;
            }
        }

        const key = self.parseKey();
        self.logKeyToFile(key);
        return .{ .key = key };
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
        // SGR format: ESC [ < button ; x ; y M/m
        // button encodes: bits 0-1 = button, bit 2 = shift, bit 3 = alt, bit 4 = ctrl
        // bit 5 = motion, bits 6-7 = scroll

        // First check if we have a complete sequence (ends with M or m)
        var has_terminator = false;
        for (self.buf[0..self.buf_len]) |c| {
            if (c == 'M' or c == 'm') {
                has_terminator = true;
                break;
            }
        }

        // If no terminator, try to read more bytes to complete the sequence
        // Use non-blocking reads without sleep for responsiveness
        if (!has_terminator) {
            var attempts: usize = 0;
            while (attempts < 3 and self.buf_len < self.buf.len) {
                var temp_buf: [16]u8 = undefined;
                const bytes_read = self.tty.read(&temp_buf) catch break;
                if (bytes_read == 0) {
                    attempts += 1;
                    continue;
                }

                // Append to buffer
                const copy_len = @min(bytes_read, self.buf.len - self.buf_len);
                @memcpy(self.buf[self.buf_len..][0..copy_len], temp_buf[0..copy_len]);
                self.buf_len += copy_len;

                // Check if we now have a terminator
                for (temp_buf[0..bytes_read]) |c| {
                    if (c == 'M' or c == 'm') {
                        has_terminator = true;
                        break;
                    }
                }
                if (has_terminator) break;
            }
        }

        if (!has_terminator) return null;

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

    // Debug: store last key bytes for display (always, not just unknown)
    pub var debug_last_bytes: [32]u8 = undefined;
    pub var debug_last_len: usize = 0;

    // Debug: log key bytes to file
    const log_path = "keylog.txt";

    fn logKeyToFile(self: *InputReader, key: Key) void {
        const cwd = std.fs.cwd();
        // Open for append, or create if doesn't exist
        const file = cwd.openFile(log_path, .{ .mode = .write_only }) catch |err| {
            if (err == error.FileNotFound) {
                // Create the file if it doesn't exist
                return if (cwd.createFile(log_path, .{})) |f| {
                    self.writeKeyLog(f, key);
                    f.close();
                } else |_| {};
            }
            return;
        };
        defer file.close();
        file.seekFromEnd(0) catch return;
        self.writeKeyLog(file, key);
    }

    fn writeKeyLog(self: *InputReader, file: std.fs.File, key: Key) void {

        // Build the log line in a buffer
        var log_buf: [256]u8 = undefined;
        var pos: usize = 0;

        // Write "Key: "
        const prefix = "Key: ";
        @memcpy(log_buf[pos..][0..prefix.len], prefix);
        pos += prefix.len;

        // Write bytes as hex
        for (self.buf[0..self.buf_len]) |byte| {
            const hex = std.fmt.bufPrint(log_buf[pos..], "{X:0>2} ", .{byte}) catch break;
            pos += hex.len;
        }

        // Write bytes as chars
        log_buf[pos] = '\'';
        pos += 1;
        for (self.buf[0..self.buf_len]) |byte| {
            if (pos >= log_buf.len - 20) break;
            if (byte >= 32 and byte < 127) {
                log_buf[pos] = byte;
            } else {
                log_buf[pos] = '.';
            }
            pos += 1;
        }
        log_buf[pos] = '\'';
        pos += 1;

        // Write parsed key type
        const key_name: []const u8 = switch (key) {
            .char => "char",
            .ctrl => "ctrl",
            .alt => "alt",
            .f1 => "F1",
            .f2 => "F2",
            .f3 => "F3",
            .f4 => "F4",
            .f5 => "F5",
            .f6 => "F6",
            .f7 => "F7",
            .f8 => "F8",
            .f9 => "F9",
            .f10 => "F10",
            .f11 => "F11",
            .f12 => "F12",
            .up => "Up",
            .down => "Down",
            .left => "Left",
            .right => "Right",
            .home => "Home",
            .end => "End",
            .page_up => "PageUp",
            .page_down => "PageDown",
            .insert => "Insert",
            .delete => "Delete",
            .backspace => "Backspace",
            .tab => "Tab",
            .shift_tab => "Shift+Tab",
            .enter => "Enter",
            .escape => "Escape",
            .unknown => "Unknown",
            .shift_up => "Shift+Up",
            .shift_down => "Shift+Down",
            .shift_left => "Shift+Left",
            .shift_right => "Shift+Right",
            .shift_home => "Shift+Home",
            .shift_end => "Shift+End",
            .shift_insert => "Shift+Insert",
            .ctrl_up => "Ctrl+Up",
            .ctrl_down => "Ctrl+Down",
            .ctrl_left => "Ctrl+Left",
            .ctrl_right => "Ctrl+Right",
            .ctrl_shift_s => "Ctrl+Shift+S",
        };
        const arrow = " -> ";
        @memcpy(log_buf[pos..][0..arrow.len], arrow);
        pos += arrow.len;
        @memcpy(log_buf[pos..][0..key_name.len], key_name);
        pos += key_name.len;
        log_buf[pos] = '\n';
        pos += 1;

        // Write to file
        _ = file.write(log_buf[0..pos]) catch {};
    }

    fn parseKey(self: *InputReader) Key {
        // Always store debug bytes for any key press
        self.storeDebugBytes();
        if (self.buf_len == 0) return .unknown;

        const c = self.buf[0];

        // Check for Windows-specific special keys first (0x00 or 0xE0 prefix)
        if (c == 0x00 or c == 0xE0) {
            if (self.parseWindowsSpecialKey()) |key| {
                return key;
            }
        }

        // Control characters (Ctrl+A through Ctrl+Z)
        if (c < 27 and c != 9 and c != 10 and c != 13 and c != 0) {
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

        // Store debug info for unknown keys
        self.storeDebugBytes();
        return .unknown;
    }

    fn storeDebugBytes(self: *InputReader) void {
        const copy_len = @min(self.buf_len, debug_last_bytes.len);
        @memcpy(debug_last_bytes[0..copy_len], self.buf[0..copy_len]);
        debug_last_len = copy_len;
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

            // Windows Terminal / xterm F1-F4 sequences (ESC [ 1 P, ESC [ 1 Q, etc.)
            // Some terminals send ESC [ [ A for F1, ESC [ [ B for F2, etc.
            if (self.buf_len >= 4 and self.buf[2] == '[') {
                return switch (self.buf[3]) {
                    'A' => .f1,
                    'B' => .f2,
                    'C' => .f3,
                    'D' => .f4,
                    'E' => .f5,
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

            // CSI u sequences (ESC [ <codepoint> ; <modifier> u) - kitty/xterm extended key protocol
            // Look for 'u' terminator
            if (self.buf_len >= 5) {
                var u_pos: ?usize = null;
                for (self.buf[2..self.buf_len], 2..) |ch, idx| {
                    if (ch == 'u') {
                        u_pos = idx;
                        break;
                    }
                }
                if (u_pos) |end_pos| {
                    // Parse: ESC [ <codepoint> ; <modifier> u
                    // Find semicolon
                    var semi_pos: ?usize = null;
                    for (self.buf[2..end_pos], 2..) |ch, idx| {
                        if (ch == ';') {
                            semi_pos = idx;
                            break;
                        }
                    }
                    if (semi_pos) |sp| {
                        // Parse codepoint (digits before semicolon)
                        var codepoint: u32 = 0;
                        for (self.buf[2..sp]) |ch| {
                            if (ch >= '0' and ch <= '9') {
                                codepoint = codepoint * 10 + (ch - '0');
                            }
                        }
                        // Parse modifier (digits after semicolon)
                        var modifier: u32 = 0;
                        for (self.buf[sp + 1 .. end_pos]) |ch| {
                            if (ch >= '0' and ch <= '9') {
                                modifier = modifier * 10 + (ch - '0');
                            }
                        }
                        // Modifier 6 = Ctrl+Shift
                        if (modifier == 6) {
                            // Ctrl+Shift+S (83='S' or 115='s')
                            if (codepoint == 83 or codepoint == 115) {
                                return .ctrl_shift_s;
                            }
                        }
                    }
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
                else => blk: {
                    self.storeDebugBytes();
                    break :blk .unknown;
                },
            };
        }

        // Unknown escape sequence - store for debug
        self.storeDebugBytes();
        return .escape;
    }

    /// Check if running on Windows
    fn isWindows() bool {
        const builtin = @import("builtin");
        return builtin.os.tag == .windows;
    }

    /// Parse Windows-specific virtual key codes
    /// Windows console sends: 0x00 or 0xE0 followed by scan code
    fn parseWindowsSpecialKey(self: *InputReader) ?Key {
        if (!isWindows()) return null;
        if (self.buf_len < 2) return null;

        // Windows sends 0x00 or 0xE0 prefix for special keys
        if (self.buf[0] != 0x00 and self.buf[0] != 0xE0) return null;

        return switch (self.buf[1]) {
            // Function keys
            59 => .f1, // 0x3B
            60 => .f2, // 0x3C
            61 => .f3, // 0x3D
            62 => .f4, // 0x3E
            63 => .f5, // 0x3F
            64 => .f6, // 0x40
            65 => .f7, // 0x41
            66 => .f8, // 0x42
            67 => .f9, // 0x43
            68 => .f10, // 0x44
            133 => .f11, // 0x85
            134 => .f12, // 0x86
            // Arrow keys (with 0xE0 prefix)
            72 => .up, // 0x48
            80 => .down, // 0x50
            75 => .left, // 0x4B
            77 => .right, // 0x4D
            // Navigation keys
            71 => .home, // 0x47
            79 => .end, // 0x4F
            73 => .page_up, // 0x49
            81 => .page_down, // 0x51
            82 => .insert, // 0x52
            83 => .delete, // 0x53
            else => null,
        };
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
