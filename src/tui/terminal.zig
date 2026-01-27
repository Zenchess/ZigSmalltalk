
const std = @import("std");
const assert = std.debug.assert;

pub const ansi = struct {
    pub fn write(writer: anytype, comptime format: []const u8, args: anytype) !void {
        return writer.print(format, args);
    }

    pub fn resetStyle(writer: anytype) !void {
        try write(writer, "\x1b[0m", .{});
    }

    pub fn setBold(writer: anytype) !void {
        try write(writer, "\x1b[1m", .{});
    }

    pub fn setDim(writer: anytype) !void {
        try write(writer, "\x1b[2m", .{});
    }

    pub fn setItalic(writer: anytype) !void {
        try write(writer, "\x1b[3m", .{});
    }

    pub fn setUnderline(writer: anytype) !void {
        try write(writer, "\x1b[4m", .{});
    }

    pub fn setReverse(writer: anytype) !void {
        try write(writer, "\x1b[7m", .{});
    }

    pub fn setFgRgb(writer: anytype, r: u8, g: u8, b: u8) !void {
        try write(writer, "\x1b[38;2;{};{};{}m", .{r, g, b});
    }

    pub fn setBgRgb(writer: anytype, r: u8, g: u8, b: u8) !void {
        try write(writer, "\x1b[48;2;{};{};{}m", .{r, g, b});
    }

    pub fn showCursor(writer: anytype) !void {
        try write(writer, "\x1b[?25h", .{});
    }

    pub fn hideCursor(writer: anytype) !void {
        try write(writer, "\x1b[?25l", .{});
    }

    pub fn exitAltScreen(writer: anytype) !void {
        try write(writer, "\x1b[?1049l", .{});
    }

    pub fn enterAltScreen(writer: anytype) !void {
        try write(writer, "\x1b[?1049h", .{});
    }

    pub fn disableMouse(writer: anytype) !void {
        try write(writer, "\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1005l\x1b[?1015l\x1b[?1006l", .{});
    }

    pub fn enableMouse(writer: anytype) !void {
        try write(writer, "\x1b[?1003h\x1b[?1006h\x1b[?1015h", .{});
    }

    pub fn disableBracketedPaste(writer: anytype) !void {
        try write(writer, "\x1b[?2004l", .{});
    }

    pub fn enableBracketedPaste(writer: anytype) !void {
        try write(writer, "\x1b[?2004h", .{});
    }

    pub fn clear(writer: anytype) !void {
        try write(writer, "\x1b[2J\x1b[H", .{});
    }

    pub fn flush(writer: anytype) !void {
        try writer.flush();
    }

    pub fn cursorGoTo(writer: anytype, x: u16, y: u16) !void {
        try write(writer, "\x1b[{};{}H", .{y, x});
    }
};
pub const input_mod = @import("input.zig");
pub const Key = input_mod.Key;
pub const MouseEvent = input_mod.MouseEvent;

pub const Size = struct {
    width: u16,
    height: u16,
};

pub const Terminal = struct {
    reader: std.fs.File.Reader,
    writer: std.fs.File.Writer,
    tty: std.posix.TTY,
    original_termios: std.posix.termios,
    width: u16,
    height: u16,

    pub fn init() !Terminal {
        var tty = try std.posix.open(c"/dev/tty", std.posix.O.RDWR | std.posix.O.CLOEXEC);
        var original_termios = try tty.getTermios();
        const size = try Terminal.getWinSize(tty.fd);

        return Terminal{
            .reader = tty.reader(),
            .writer = tty.writer(),
            .tty = tty,
            .original_termios = original_termios,
            .width = size.width,
            .height = size.height,
        };
    }

    pub fn deinit(self: *Terminal) void {
        self.disableRawMode() catch {};
        ansi.showCursor(self.writer) catch {};
        ansi.exitAltScreen(self.writer) catch {};
        ansi.disableMouse(self.writer) catch {};
        ansi.disableBracketedPaste(self.writer) catch {};
        ansi.clear(self.writer) catch {};
        ansi.resetStyle(self.writer) catch {};
        ansi.flush(self.writer) catch {};
        self.tty.close();
    }

    pub fn enableRawMode(self: *Terminal) !void {
        var termios = self.original_termios;

        // Disable Ctrl-S, Ctrl-Q, Ctrl-Z, Ctrl-C
        termios.c_iflag &= ~(std.posix.IGNBRK | std.posix.BRKINT | std.posix.PARMRK | std.posix.ISTRIP
                              | std.posix.INLCR | std.posix.IGNCR | std.posix.ICRNL | std.posix.IXON);
        termios.c_oflag &= ~std.posix.OPOST;
        termios.c_lflag &= ~(std.posix.ECHO | std.posix.ECHONL | std.posix.ICANON | std.posix.ISIG | std.posix.IEXTEN);
        termios.c_cflag &= ~(std.posix.CSIZE | std.posix.PARENB);
        termios.c_cflag |= std.posix.CS8;

        termios.c_cc[std.posix.VMIN] = 0;
        termios.c_cc[std.posix.VTIME] = 1; // 100ms timeout for read

        try self.tty.setTermios(termios);
    }

    pub fn disableRawMode(self: *Terminal) !void {
        try self.tty.setTermios(self.original_termios);
    }

    pub fn write(self: *Terminal, bytes: []const u8) !void {
        try self.writer.writeAll(bytes);
    }

    pub fn read(self: *Terminal, buffer: []u8) !usize {
        return self.reader.read(buffer);
    }

    pub fn getChar(self: *Terminal) !u8 {
        var buf: [1]u8 = undefined;
        _ = try self.reader.readAtLeast(buf[0..1], 1);
        return buf[0];
    }

    pub fn getWinSize(fd: std.posix.fd_t) !Size {
        var ws: std.posix.winsize = undefined;
        _ = try std.posix.ioctl(fd, std.posix.TIOCGWINSZ, @ptrCast(&ws));
        return Size{
            .width = ws.ws_col,
            .height = ws.ws_row,
        };
    }

    pub fn getSize(self: *Terminal) !Size {
        return Terminal.getWinSize(self.tty.fd);
    }
};
