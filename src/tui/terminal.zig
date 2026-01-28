const std = @import("std");
const builtin = @import("builtin");

// Platform-specific imports
const is_windows = builtin.os.tag == .windows;
const posix = if (!is_windows) std.posix else undefined;

pub const Size = struct {
    width: u16,
    height: u16,
};

pub const Terminal = struct {
    original_termios: if (!is_windows) posix.termios else void,
    original_console_mode: if (is_windows) u32 else void, // Store original Windows console mode
    tty: std.fs.File, // For output (and input on Unix)
    tty_input: std.fs.File, // For input (separate handle on Windows)
    width: u16,
    height: u16,

    var instance: ?*Terminal = null;

    pub fn init() !Terminal {
        if (is_windows) {
            // On Windows, use separate handles for input and output
            const out_handle = std.os.windows.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) catch return error.NoStdout;
            const in_handle = std.os.windows.GetStdHandle(std.os.windows.STD_INPUT_HANDLE) catch return error.NoStdin;

            // Get original console mode so we can restore it later
            var original_mode: u32 = 0;
            _ = std.os.windows.kernel32.GetConsoleMode(in_handle, &original_mode);

            var term = Terminal{
                .original_termios = {},
                .original_console_mode = original_mode,
                .tty = std.fs.File{ .handle = out_handle },
                .tty_input = std.fs.File{ .handle = in_handle },
                .width = 120,
                .height = 30,
            };
            const size = term.getSize() catch Size{ .width = 120, .height = 30 };
            term.width = size.width;
            term.height = size.height;
            return term;
        } else {
            // Open /dev/tty directly for proper terminal access
            const tty = try std.fs.openFileAbsolute("/dev/tty", .{ .mode = .read_write });
            const original = try posix.tcgetattr(tty.handle);

            var term = Terminal{
                .original_termios = original,
                .original_console_mode = {},
                .tty = tty,
                .tty_input = tty, // Same handle for Unix
                .width = 80,
                .height = 24,
            };

            const size = term.getSize() catch Size{ .width = 80, .height = 24 };
            term.width = size.width;
            term.height = size.height;

            return term;
        }
    }

    pub fn enableRawMode(self: *Terminal) !void {
        if (is_windows) {
            // Windows: Configure console for raw input mode
            const in_handle = self.tty_input.handle;
            const out_handle = self.tty.handle;

            // Console mode flags for input
            const ENABLE_WINDOW_INPUT: u32 = 0x0008;
            const ENABLE_MOUSE_INPUT: u32 = 0x0010;
            const ENABLE_VIRTUAL_TERMINAL_INPUT: u32 = 0x0200;

            // Console mode flags for output
            const ENABLE_PROCESSED_OUTPUT: u32 = 0x0001;
            const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;

            // Set input mode: disable echo/line input, enable VT input and mouse
            const input_mode: u32 = ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT;
            _ = std.os.windows.kernel32.SetConsoleMode(in_handle, input_mode);

            // Set output mode: enable VT processing for ANSI escape codes
            var output_mode: u32 = 0;
            _ = std.os.windows.kernel32.GetConsoleMode(out_handle, &output_mode);
            output_mode |= ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            _ = std.os.windows.kernel32.SetConsoleMode(out_handle, output_mode);

            instance = self;
            return;
        }

        var raw = self.original_termios;

        // Input modes: no break, no CR to NL, no parity check, no strip char, no start/stop output control
        raw.iflag.BRKINT = false;
        raw.iflag.ICRNL = false;
        raw.iflag.INPCK = false;
        raw.iflag.ISTRIP = false;
        raw.iflag.IXON = false;

        // Output modes: disable post processing
        raw.oflag.OPOST = false;

        // Control modes: set 8 bit chars
        raw.cflag.CSIZE = .CS8;

        // Local modes: echo off, canonical off, no extended functions, no signal chars
        raw.lflag.ECHO = false;
        raw.lflag.ICANON = false;
        raw.lflag.IEXTEN = false;
        raw.lflag.ISIG = false;

        // Control chars: set return condition min bytes and timer
        raw.cc[@intFromEnum(posix.V.MIN)] = 0;
        raw.cc[@intFromEnum(posix.V.TIME)] = 1; // 100ms timeout

        try posix.tcsetattr(self.tty.handle, .FLUSH, raw);

        instance = self;
    }

    pub fn disableRawMode(self: *Terminal) void {
        if (is_windows) {
            // Restore original console mode
            _ = std.os.windows.kernel32.SetConsoleMode(self.tty_input.handle, self.original_console_mode);
            instance = null;
            return;
        }
        posix.tcsetattr(self.tty.handle, .FLUSH, self.original_termios) catch {};
        instance = null;
    }

    pub fn deinit(self: *Terminal) void {
        self.disableRawMode();
        ansi.disableBracketedPaste();
        ansi.disableMouse();
        ansi.showCursor();
        ansi.exitAltScreen();
        ansi.resetStyle();
        if (!is_windows) {
            self.tty.close();
        }
    }

    pub fn getSize(self: *Terminal) !Size {
        _ = self;
        if (is_windows) {
            // On Windows, try to get console size via Windows API
            var csbi: std.os.windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
            const handle = std.os.windows.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) catch return error.GetSizeFailed;
            if (std.os.windows.kernel32.GetConsoleScreenBufferInfo(handle, &csbi) != 0) {
                const width: u16 = @intCast(csbi.srWindow.Right - csbi.srWindow.Left + 1);
                const height: u16 = @intCast(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);
                return Size{ .width = width, .height = height };
            }
            return Size{ .width = 120, .height = 30 };
        } else {
            var wsz: posix.winsize = undefined;
            const rc = std.posix.system.ioctl(std.posix.STDOUT_FILENO, posix.T.IOCGWINSZ, @intFromPtr(&wsz));
            if (rc == 0) {
                return Size{
                    .width = wsz.col,
                    .height = wsz.row,
                };
            }
            return error.GetSizeFailed;
        }
    }

    pub fn updateSize(self: *Terminal) void {
        const size = self.getSize() catch return;
        self.width = size.width;
        self.height = size.height;
    }
};

// ANSI escape code helpers
pub const ansi = struct {
    fn getStdout() std.fs.File {
        if (is_windows) {
            const handle = std.os.windows.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) catch unreachable;
            return std.fs.File{ .handle = handle };
        } else {
            return std.fs.File{ .handle = posix.STDOUT_FILENO };
        }
    }

    pub fn clear() void {
        // Clear screen and move cursor to home position (1,1)
        _ = getStdout().write("\x1b[2J\x1b[H") catch {};
    }

    pub fn clearLine() void {
        _ = getStdout().write("\x1b[2K") catch {};
    }

    pub fn moveCursor(row: u16, col: u16) void {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "\x1b[{d};{d}H", .{ row + 1, col + 1 }) catch return;
        _ = getStdout().write(s) catch {};
    }

    pub fn hideCursor() void {
        _ = getStdout().write("\x1b[?25l") catch {};
    }

    pub fn showCursor() void {
        _ = getStdout().write("\x1b[?25h") catch {};
    }

    pub fn enterAltScreen() void {
        _ = getStdout().write("\x1b[?1049h") catch {};
    }

    pub fn exitAltScreen() void {
        _ = getStdout().write("\x1b[?1049l") catch {};
    }

    pub fn enableMouse() void {
        // Enable mouse tracking:
        // 1000 = X10 compatibility mode (button press)
        // 1002 = Button-event tracking (press, release, motion while pressed)
        // 1006 = SGR extended mode (allows coordinates > 223)
        _ = getStdout().write("\x1b[?1000h\x1b[?1002h\x1b[?1006h") catch {};
    }

    pub fn disableMouse() void {
        _ = getStdout().write("\x1b[?1006l\x1b[?1002l\x1b[?1000l") catch {};
    }

    pub fn enableBracketedPaste() void {
        // Bracketed paste mode wraps pasted text in escape sequences
        // so we can distinguish paste from typed input
        _ = getStdout().write("\x1b[?2004h") catch {};
    }

    pub fn disableBracketedPaste() void {
        _ = getStdout().write("\x1b[?2004l") catch {};
    }

    pub fn resetStyle() void {
        _ = getStdout().write("\x1b[0m") catch {};
    }

    pub fn setBold() void {
        _ = getStdout().write("\x1b[1m") catch {};
    }

    pub fn setDim() void {
        _ = getStdout().write("\x1b[2m") catch {};
    }

    pub fn setItalic() void {
        _ = getStdout().write("\x1b[3m") catch {};
    }

    pub fn setUnderline() void {
        _ = getStdout().write("\x1b[4m") catch {};
    }

    pub fn setReverse() void {
        _ = getStdout().write("\x1b[7m") catch {};
    }

    pub fn setFg(color: Color) void {
        var buf: [16]u8 = undefined;
        const code = color.fgCode();
        const s = std.fmt.bufPrint(&buf, "\x1b[{d}m", .{code}) catch return;
        _ = getStdout().write(s) catch {};
    }

    pub fn setBg(color: Color) void {
        var buf: [16]u8 = undefined;
        const code = color.bgCode();
        const s = std.fmt.bufPrint(&buf, "\x1b[{d}m", .{code}) catch return;
        _ = getStdout().write(s) catch {};
    }

    pub fn setFgRgb(r: u8, g: u8, b: u8) void {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "\x1b[38;2;{d};{d};{d}m", .{ r, g, b }) catch return;
        _ = getStdout().write(s) catch {};
    }

    pub fn setBgRgb(r: u8, g: u8, b: u8) void {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "\x1b[48;2;{d};{d};{d}m", .{ r, g, b }) catch return;
        _ = getStdout().write(s) catch {};
    }

    pub fn write(text: []const u8) void {
        _ = getStdout().write(text) catch {};
    }

    pub fn writeChar(char: u21) void {
        var buf: [4]u8 = undefined;
        const len = std.unicode.utf8Encode(char, &buf) catch return;
        _ = getStdout().write(buf[0..len]) catch {};
    }

    pub fn flush() void {
        // stdout is typically unbuffered, but sync anyway
    }
};

pub const Color = enum(u8) {
    default = 0,
    black = 1,
    red = 2,
    green = 3,
    yellow = 4,
    blue = 5,
    magenta = 6,
    cyan = 7,
    white = 8,
    bright_black = 9,
    bright_red = 10,
    bright_green = 11,
    bright_yellow = 12,
    bright_blue = 13,
    bright_magenta = 14,
    bright_cyan = 15,
    bright_white = 16,

    pub fn fgCode(self: Color) u8 {
        return switch (self) {
            .default => 39,
            .black => 30,
            .red => 31,
            .green => 32,
            .yellow => 33,
            .blue => 34,
            .magenta => 35,
            .cyan => 36,
            .white => 37,
            .bright_black => 90,
            .bright_red => 91,
            .bright_green => 92,
            .bright_yellow => 93,
            .bright_blue => 94,
            .bright_magenta => 95,
            .bright_cyan => 96,
            .bright_white => 97,
        };
    }

    pub fn bgCode(self: Color) u8 {
        return switch (self) {
            .default => 49,
            .black => 40,
            .red => 41,
            .green => 42,
            .yellow => 43,
            .blue => 44,
            .magenta => 45,
            .cyan => 46,
            .white => 47,
            .bright_black => 100,
            .bright_red => 101,
            .bright_green => 102,
            .bright_yellow => 103,
            .bright_blue => 104,
            .bright_magenta => 105,
            .bright_cyan => 106,
            .bright_white => 107,
        };
    }
};

// RGB color type for true color support
pub const Rgb = struct {
    r: u8,
    g: u8,
    b: u8,

    pub fn init(r: u8, g: u8, b: u8) Rgb {
        return .{ .r = r, .g = g, .b = b };
    }

    // Parse hex color like "#1e1e2e"
    pub fn fromHex(hex: []const u8) Rgb {
        if (hex.len < 6) return .{ .r = 0, .g = 0, .b = 0 };
        const start: usize = if (hex[0] == '#') 1 else 0;
        const r = std.fmt.parseInt(u8, hex[start .. start + 2], 16) catch 0;
        const g = std.fmt.parseInt(u8, hex[start + 2 .. start + 4], 16) catch 0;
        const b = std.fmt.parseInt(u8, hex[start + 4 .. start + 6], 16) catch 0;
        return .{ .r = r, .g = g, .b = b };
    }
};
