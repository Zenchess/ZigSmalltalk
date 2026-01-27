
const std = @import("std");
const assert = std.debug.assert;
const terminal_mod = @import("terminal.zig");
const style_mod = @import("style.zig");
const Style = style_mod.Style;
const Color = style_mod.Color;
const Attribute = style_mod.Attribute;

/// Represents a single cell in the terminal screen buffer.
pub const Cell = struct {
    ch: u8,
    style: Style,
};

/// Represents the terminal screen buffer.
pub const Screen = struct {
    terminal: *terminal_mod.Terminal,
    allocator: std.mem.Allocator,
    width: u16,
    height: u16,
    buffer: []Cell,
    prev_buffer: []Cell,
    dirty: []bool,
    full_redraw_pending: bool,

    pub fn init(allocator: std.mem.Allocator, terminal: *terminal_mod.Terminal, width: u16, height: u16) !Screen {
        const buffer_len = width * height;
        const buffer = try allocator.alloc(Cell, buffer_len);
        const prev_buffer = try allocator.alloc(Cell, buffer_len);
        const dirty = try allocator.alloc(bool, buffer_len);

        var screen = Screen{
            .allocator = allocator,
            .terminal = terminal,
            .width = width,
            .height = height,
            .buffer = buffer,
            .prev_buffer = prev_buffer,
            .dirty = dirty,
            .full_redraw_pending = true, // Force full redraw on first frame
        };
        screen.clear(); // Initialize buffers
        return screen;
    }

    pub fn deinit(self: *Screen) void {
        self.allocator.free(self.buffer);
        self.allocator.free(self.prev_buffer);
        self.allocator.free(self.dirty);
    }

    pub fn resize(self: *Screen, new_width: u16, new_height: u16) !void {
        if (new_width == self.width and new_height == self.height) return;

        const new_buffer_len = new_width * new_height;
        const new_buffer = try self.allocator.alloc(Cell, new_buffer_len);
        const new_prev_buffer = try self.allocator.alloc(Cell, new_buffer_len);
        const new_dirty = try self.allocator.alloc(bool, new_buffer_len);

        // Clear new buffers
        for (0..new_buffer_len) |i| {
            new_buffer[i] = Cell{ .ch = ' ', .style = Style.default() };
            new_prev_buffer[i] = Cell{ .ch = ' ', .style = Style.default() };
            new_dirty[i] = true;
        }

        // Copy existing content to the new buffer, clipping if necessary
        var y: u16 = 0;
        while (y < @min(self.height, new_height)) : (y += 1) {
            var x: u16 = 0;
            while (x < @min(self.width, new_width)) : (x += 1) {
                const old_idx = self.idx(x, y);
                const new_idx = (y * new_width) + x;
                new_buffer[new_idx] = self.buffer[old_idx];
            }
        }

        self.allocator.free(self.buffer);
        self.allocator.free(self.prev_buffer);
        self.allocator.free(self.dirty);

        self.buffer = new_buffer;
        self.prev_buffer = new_prev_buffer;
        self.dirty = new_dirty;
        self.width = new_width;
        self.height = new_height;
        self.full_redraw_pending = true;
    }

    /// Clears the screen buffer and marks all cells as dirty.
    pub fn clear(self: *Screen) void {
        const default_cell = Cell{ .ch = ' ', .style = Style.default() };
        for (0..self.buffer.len) |i| {
            self.buffer[i] = default_cell;
            self.prev_buffer[i] = default_cell; // Also clear prev buffer
            self.dirty[i] = true; // Mark all as dirty
        }
    }

    /// Sets a cell's character and style at the given coordinates.
    pub fn put(self: *Screen, x: u16, y: u16, ch: u8, style: Style) void {
        if (x >= self.width or y >= self.height) return;
        const i = self.idx(x, y);
        self.buffer[i] = Cell{ .ch = ch, .style = style };
    }

    /// Draws a string to the screen buffer.
    pub fn putStr(self: *Screen, x: u16, y: u16, text: []const u8, style: Style) void {
        var current_x = x;
        for (text) |ch| {
            if (current_x >= self.width) break;
            self.put(current_x, y, ch, style);
            current_x += 1;
        }
    }

    /// Draws a string to the screen buffer, truncating if it exceeds width.
    pub fn putStrTruncate(self: *Screen, x: u16, y: u16, text: []const u8, max_width: u16, style: Style) void {
        var current_x = x;
        for (text) |ch| {
            if (current_x >= self.width or (current_x - x) >= max_width) break;
            self.put(current_x, y, ch, style);
            current_x += 1;
        }
    }

    /// Draws a vertical line character.
    pub fn putVLine(self: *Screen, x: u16, y1: u16, y2: u16, ch: u8, style: Style) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            self.put(x, y, ch, style);
        }
    }

    /// Draws a horizontal line character.
    pub fn putHLine(self: *Screen, x1: u16, x2: u16, y: u16, ch: u8, style: Style) void {
        var x = x1;
        while (x <= x2) : (x += 1) {
            self.put(x, y, ch, style);
        }
    }

    /// Fills a rectangular region with a character and style.
    pub fn fill(self: *Screen, x: u16, y: u16, width: u16, height: u16, ch: u8, style: Style) void {
        var current_y = y;
        while (current_y < y + height) : (current_y += 1) {
            var current_x = x;
            while (current_x < x + width) : (current_x += 1) {
                self.put(current_x, current_y, ch, style);
            }
        }
    }

    /// Flushes the screen buffer to the terminal, only drawing changed cells.
    pub fn flush(self: *Screen) void {
        var current_style = Style.default;
        self.terminal.ansi.resetStyle(self.terminal.writer) catch {};

        var y: u16 = 0;
        while (y < self.height) : (y += 1) {
            var x: u16 = 0;
            while (x < self.width) : (x += 1) {
                const i = self.idx(x, y);
                const current_cell = self.buffer[i];
                const prev_cell = self.prev_buffer[i];

                if (self.full_redraw_pending or current_cell.ch != prev_cell.ch or !current_cell.style.eql(prev_cell.style)) {
                    self.terminal.ansi.cursorGoTo(self.terminal.writer, x + 1, y + 1) catch {};
                    if (!current_cell.style.eql(current_style)) {
                        current_cell.style.apply(self.terminal.writer);
                        current_style = current_cell.style;
                    }
                    self.terminal.writer.writeAll(&.{current_cell.ch}) catch {};
                    self.prev_buffer[i] = current_cell;
                }
            }
        }
        self.terminal.ansi.cursorGoTo(self.terminal.writer, 1, self.height) catch {};
        self.terminal.ansi.flush(self.terminal.writer) catch {};
        self.full_redraw_pending = false;
    }

    /// Forces a full redraw of the entire screen on the next flush.
    pub fn flushFull(self: *Screen) void {
        self.full_redraw_pending = true;
        self.flush();
    }

    /// Marks that a full redraw is needed on the next flush.
    pub fn forceFullRedraw(self: *Screen) void {
        self.full_redraw_pending = true;
    }

    fn idx(self: *Screen, x: u16, y: u16) usize {
        assert(x < self.width);
        assert(y < self.height);
        return (y * self.width) + x;
    }
};
