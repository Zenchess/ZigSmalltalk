const std = @import("std");
const terminal = @import("terminal.zig");
const style_mod = @import("style.zig");

const Style = style_mod.Style;
const Rgb = terminal.Rgb;
const ansi = terminal.ansi;

pub const Cell = struct {
    char: u21 = ' ',
    style: Style = Style.default,

    pub fn eql(self: Cell, other: Cell) bool {
        return self.char == other.char and self.style.eql(other.style);
    }
};

pub const Screen = struct {
    allocator: std.mem.Allocator,
    cells: []Cell,
    prev_cells: []Cell,
    width: u16,
    height: u16,
    cursor_x: u16 = 0,
    cursor_y: u16 = 0,
    cursor_visible: bool = true,

    pub fn init(allocator: std.mem.Allocator, width: u16, height: u16) !Screen {
        const size = @as(usize, width) * @as(usize, height);
        const cells = try allocator.alloc(Cell, size);
        const prev_cells = try allocator.alloc(Cell, size);

        for (cells) |*cell| {
            cell.* = Cell{};
        }
        for (prev_cells) |*cell| {
            cell.* = Cell{ .char = 0 }; // Different from default to force initial draw
        }

        return Screen{
            .allocator = allocator,
            .cells = cells,
            .prev_cells = prev_cells,
            .width = width,
            .height = height,
        };
    }

    pub fn deinit(self: *Screen) void {
        self.allocator.free(self.cells);
        self.allocator.free(self.prev_cells);
    }

    pub fn resize(self: *Screen, width: u16, height: u16) !void {
        const size = @as(usize, width) * @as(usize, height);

        self.allocator.free(self.cells);
        self.allocator.free(self.prev_cells);

        self.cells = try self.allocator.alloc(Cell, size);
        self.prev_cells = try self.allocator.alloc(Cell, size);

        for (self.cells) |*cell| {
            cell.* = Cell{};
        }
        for (self.prev_cells) |*cell| {
            cell.* = Cell{ .char = 0 };
        }

        self.width = width;
        self.height = height;
    }

    pub fn clear(self: *Screen) void {
        for (self.cells) |*cell| {
            cell.* = Cell{};
        }
    }

    pub fn clearWithStyle(self: *Screen, s: Style) void {
        for (self.cells) |*cell| {
            cell.* = Cell{ .char = ' ', .style = s };
        }
    }

    fn idx(self: *Screen, x: u16, y: u16) ?usize {
        if (x >= self.width or y >= self.height) return null;
        return @as(usize, y) * @as(usize, self.width) + @as(usize, x);
    }

    pub fn setCell(self: *Screen, x: u16, y: u16, char: u21, s: Style) void {
        if (self.idx(x, y)) |i| {
            self.cells[i] = Cell{ .char = char, .style = s };
        }
    }

    pub fn getCell(self: *Screen, x: u16, y: u16) ?Cell {
        if (self.idx(x, y)) |i| {
            return self.cells[i];
        }
        return null;
    }

    pub fn drawText(self: *Screen, x: u16, y: u16, text: []const u8, s: Style) void {
        var col = x;
        var i: usize = 0;
        while (i < text.len) {
            const len = std.unicode.utf8ByteSequenceLength(text[i]) catch 1;
            const codepoint = std.unicode.utf8Decode(text[i..@min(i + len, text.len)]) catch ' ';

            self.setCell(col, y, codepoint, s);
            col += 1;
            i += len;

            if (col >= self.width) break;
        }
    }

    pub fn drawTextClipped(self: *Screen, x: u16, y: u16, text: []const u8, max_width: u16, s: Style) void {
        var col = x;
        var i: usize = 0;
        const end_col = @min(x + max_width, self.width);

        while (i < text.len and col < end_col) {
            const len = std.unicode.utf8ByteSequenceLength(text[i]) catch 1;
            const codepoint = std.unicode.utf8Decode(text[i..@min(i + len, text.len)]) catch ' ';

            self.setCell(col, y, codepoint, s);
            col += 1;
            i += len;
        }
    }

    pub fn fillRect(self: *Screen, x: u16, y: u16, w: u16, h: u16, char: u21, s: Style) void {
        var row: u16 = y;
        while (row < y + h and row < self.height) : (row += 1) {
            var col: u16 = x;
            while (col < x + w and col < self.width) : (col += 1) {
                self.setCell(col, row, char, s);
            }
        }
    }

    pub fn drawHLine(self: *Screen, x: u16, y: u16, length: u16, char: u21, s: Style) void {
        var col = x;
        while (col < x + length and col < self.width) : (col += 1) {
            self.setCell(col, y, char, s);
        }
    }

    pub fn drawVLine(self: *Screen, x: u16, y: u16, length: u16, char: u21, s: Style) void {
        var row = y;
        while (row < y + length and row < self.height) : (row += 1) {
            self.setCell(x, row, char, s);
        }
    }

    pub fn drawBox(self: *Screen, x: u16, y: u16, w: u16, h: u16, s: Style) void {
        if (w < 2 or h < 2) return;

        const box = style_mod.box;

        // Corners
        self.drawText(x, y, box.top_left, s);
        self.drawText(x + w - 1, y, box.top_right, s);
        self.drawText(x, y + h - 1, box.bottom_left, s);
        self.drawText(x + w - 1, y + h - 1, box.bottom_right, s);

        // Horizontal lines
        var col: u16 = x + 1;
        while (col < x + w - 1) : (col += 1) {
            self.drawText(col, y, box.horizontal, s);
            self.drawText(col, y + h - 1, box.horizontal, s);
        }

        // Vertical lines
        var row: u16 = y + 1;
        while (row < y + h - 1) : (row += 1) {
            self.drawText(x, row, box.vertical, s);
            self.drawText(x + w - 1, row, box.vertical, s);
        }
    }

    pub fn drawBoxRounded(self: *Screen, x: u16, y: u16, w: u16, h: u16, s: Style) void {
        if (w < 2 or h < 2) return;

        const box = style_mod.box;

        // Rounded corners
        self.drawText(x, y, box.round_top_left, s);
        self.drawText(x + w - 1, y, box.round_top_right, s);
        self.drawText(x, y + h - 1, box.round_bottom_left, s);
        self.drawText(x + w - 1, y + h - 1, box.round_bottom_right, s);

        // Horizontal lines
        var col: u16 = x + 1;
        while (col < x + w - 1) : (col += 1) {
            self.drawText(col, y, box.horizontal, s);
            self.drawText(col, y + h - 1, box.horizontal, s);
        }

        // Vertical lines
        var row: u16 = y + 1;
        while (row < y + h - 1) : (row += 1) {
            self.drawText(x, row, box.vertical, s);
            self.drawText(x + w - 1, row, box.vertical, s);
        }
    }

    pub fn setCursor(self: *Screen, x: u16, y: u16) void {
        self.cursor_x = x;
        self.cursor_y = y;
    }

    pub fn flush(self: *Screen) void {
        var current_style: ?Style = null;

        var y: u16 = 0;
        while (y < self.height) : (y += 1) {
            var x: u16 = 0;
            var line_dirty = false;

            // Check if this line has any changes
            while (x < self.width) : (x += 1) {
                if (self.idx(x, y)) |i| {
                    if (!self.cells[i].eql(self.prev_cells[i])) {
                        line_dirty = true;
                        break;
                    }
                }
            }

            if (!line_dirty) continue;

            // Draw the dirty line
            ansi.moveCursor(y, 0);
            x = 0;
            while (x < self.width) : (x += 1) {
                if (self.idx(x, y)) |i| {
                    const cell = self.cells[i];

                    // Update style if changed
                    if (current_style == null or !current_style.?.eql(cell.style)) {
                        cell.style.apply();
                        current_style = cell.style;
                    }

                    // Write character
                    ansi.writeChar(cell.char);

                    // Update prev buffer
                    self.prev_cells[i] = cell;
                }
            }
        }

        // Reset style
        ansi.resetStyle();

        // Position cursor
        if (self.cursor_visible) {
            ansi.moveCursor(self.cursor_y, self.cursor_x);
            ansi.showCursor();
        } else {
            ansi.hideCursor();
        }
    }

    pub fn forceFullRedraw(self: *Screen) void {
        // Mark all prev_cells as different to force redraw
        for (self.prev_cells) |*cell| {
            cell.char = 0;
        }
    }
};
