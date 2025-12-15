const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style = @import("../style.zig");
const widget = @import("widget.zig");
const clipboard = @import("../clipboard.zig");

const Key = input.Key;
const Style = style.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const TextArea = struct {
    allocator: std.mem.Allocator,
    state: widget.WidgetState,

    // Content
    lines: std.ArrayList(std.ArrayList(u8)),

    // Cursor position
    cursor_line: usize = 0,
    cursor_col: usize = 0,

    // Selection
    selection_start_line: ?usize = null,
    selection_start_col: ?usize = null,
    selection_end_line: ?usize = null,
    selection_end_col: ?usize = null,

    // Scroll position
    scroll_y: usize = 0,
    scroll_x: usize = 0,

    // Options
    line_numbers: bool = true,
    readonly: bool = false,
    syntax_highlight: bool = true,
    wrap: bool = false,

    // Mouse selection state
    mouse_selecting: bool = false,

    // Callbacks
    on_change: ?*const fn (*TextArea) void = null,

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !TextArea {
        var lines: std.ArrayList(std.ArrayList(u8)) = .empty;
        // Start with one empty line
        var first_line: std.ArrayList(u8) = .empty;
        try lines.append(allocator, first_line);
        _ = &first_line;

        return TextArea{
            .allocator = allocator,
            .state = .{ .rect = rect },
            .lines = lines,
        };
    }

    pub fn deinit(self: *TextArea) void {
        for (self.lines.items) |*line| {
            line.deinit(self.allocator);
        }
        self.lines.deinit(self.allocator);
    }

    pub fn setText(self: *TextArea, text: []const u8) !void {
        // Clear existing lines
        for (self.lines.items) |*line| {
            line.deinit(self.allocator);
        }
        self.lines.clearRetainingCapacity();

        // Split text into lines (handle both \n and \r\n line endings)
        var iter = std.mem.splitScalar(u8, text, '\n');
        while (iter.next()) |line_text| {
            var line: std.ArrayList(u8) = .empty;
            // Strip control characters that could cause display issues
            // Keep only printable ASCII and valid UTF-8 continuation bytes
            for (line_text) |c| {
                // Skip \r, \t converted to space, skip other control chars (0-31 except tab)
                if (c == '\r') continue;
                if (c == '\t') {
                    // Convert tab to spaces
                    try line.append(self.allocator, ' ');
                    try line.append(self.allocator, ' ');
                    try line.append(self.allocator, ' ');
                    try line.append(self.allocator, ' ');
                } else if (c < 32 and c != '\t') {
                    // Skip other control characters
                    continue;
                } else {
                    try line.append(self.allocator, c);
                }
            }
            try self.lines.append(self.allocator, line);
        }

        // Ensure at least one line
        if (self.lines.items.len == 0) {
            const empty_line: std.ArrayList(u8) = .empty;
            try self.lines.append(self.allocator, empty_line);
        }

        // Reset cursor
        self.cursor_line = 0;
        self.cursor_col = 0;
        self.scroll_y = 0;
        self.scroll_x = 0;
        self.clearSelection();
    }

    pub fn getText(self: *TextArea, allocator: std.mem.Allocator) ![]u8 {
        var total_len: usize = 0;
        for (self.lines.items, 0..) |line, i| {
            total_len += line.items.len;
            if (i < self.lines.items.len - 1) total_len += 1; // newline
        }

        var result = try allocator.alloc(u8, total_len);
        var pos: usize = 0;

        for (self.lines.items, 0..) |line, i| {
            @memcpy(result[pos .. pos + line.items.len], line.items);
            pos += line.items.len;
            if (i < self.lines.items.len - 1) {
                result[pos] = '\n';
                pos += 1;
            }
        }

        return result;
    }

    pub fn getSelectedText(self: *TextArea, allocator: std.mem.Allocator) !?[]u8 {
        if (!self.hasSelection()) return null;

        const sel = self.normalizedSelection() orelse return null;

        if (sel.start_line == sel.end_line) {
            // Single line selection
            const line = self.lines.items[sel.start_line].items;
            const start = @min(sel.start_col, line.len);
            const end = @min(sel.end_col, line.len);
            if (start >= end) return null;

            const result = try allocator.alloc(u8, end - start);
            @memcpy(result, line[start..end]);
            return result;
        }

        // Multi-line selection
        var total_len: usize = 0;
        var line_idx = sel.start_line;
        while (line_idx <= sel.end_line) : (line_idx += 1) {
            const line = self.lines.items[line_idx].items;
            if (line_idx == sel.start_line) {
                total_len += line.len - @min(sel.start_col, line.len);
            } else if (line_idx == sel.end_line) {
                total_len += @min(sel.end_col, line.len);
            } else {
                total_len += line.len;
            }
            if (line_idx < sel.end_line) total_len += 1; // newline
        }

        var result = try allocator.alloc(u8, total_len);
        var pos: usize = 0;

        line_idx = sel.start_line;
        while (line_idx <= sel.end_line) : (line_idx += 1) {
            const line = self.lines.items[line_idx].items;
            var start: usize = 0;
            var end: usize = line.len;

            if (line_idx == sel.start_line) {
                start = @min(sel.start_col, line.len);
            }
            if (line_idx == sel.end_line) {
                end = @min(sel.end_col, line.len);
            }

            if (start < end) {
                @memcpy(result[pos .. pos + (end - start)], line[start..end]);
                pos += end - start;
            }

            if (line_idx < sel.end_line) {
                result[pos] = '\n';
                pos += 1;
            }
        }

        return result;
    }

    pub fn hasSelection(self: *TextArea) bool {
        return self.selection_start_line != null and self.selection_end_line != null;
    }

    fn normalizedSelection(self: *TextArea) ?struct {
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
    } {
        const sl = self.selection_start_line orelse return null;
        const sc = self.selection_start_col orelse return null;
        const el = self.selection_end_line orelse return null;
        const ec = self.selection_end_col orelse return null;

        if (sl < el or (sl == el and sc <= ec)) {
            return .{ .start_line = sl, .start_col = sc, .end_line = el, .end_col = ec };
        } else {
            return .{ .start_line = el, .start_col = ec, .end_line = sl, .end_col = sc };
        }
    }

    pub fn clearSelection(self: *TextArea) void {
        self.selection_start_line = null;
        self.selection_start_col = null;
        self.selection_end_line = null;
        self.selection_end_col = null;
    }

    fn startSelection(self: *TextArea) void {
        if (!self.hasSelection()) {
            self.selection_start_line = self.cursor_line;
            self.selection_start_col = self.cursor_col;
        }
    }

    fn extendSelection(self: *TextArea) void {
        self.selection_end_line = self.cursor_line;
        self.selection_end_col = self.cursor_col;
    }

    pub fn selectAll(self: *TextArea) void {
        self.selection_start_line = 0;
        self.selection_start_col = 0;
        self.selection_end_line = self.lines.items.len - 1;
        self.selection_end_col = self.lines.items[self.lines.items.len - 1].items.len;
    }

    /// Copy selected text to system clipboard
    pub fn copyToClipboard(self: *TextArea) void {
        const text = self.getSelectedText(self.allocator) catch return;
        if (text) |selected| {
            defer self.allocator.free(selected);
            clipboard.copy(selected) catch {};
        }
    }

    /// Paste text from system clipboard
    pub fn pasteFromClipboard(self: *TextArea) void {
        const text = clipboard.paste(self.allocator) catch return;
        defer self.allocator.free(text);

        // Delete any selected text first
        self.deleteSelection();

        // Insert pasted text character by character (handles newlines)
        for (text) |c| {
            if (c == '\n') {
                self.insertNewline();
            } else if (c != '\r') { // Skip carriage returns
                self.insertChar(c);
            }
        }
    }

    /// Paste text from primary selection (middle-click)
    pub fn pasteFromPrimarySelection(self: *TextArea) void {
        const text = clipboard.pastePrimary(self.allocator) catch return;
        defer self.allocator.free(text);

        // Delete any selected text first
        self.deleteSelection();

        // Insert pasted text character by character (handles newlines)
        for (text) |c| {
            if (c == '\n') {
                self.insertNewline();
            } else if (c != '\r') { // Skip carriage returns
                self.insertChar(c);
            }
        }
    }

    fn currentLine(self: *TextArea) *std.ArrayList(u8) {
        return &self.lines.items[self.cursor_line];
    }

    fn lineNumberWidth(self: *TextArea) u16 {
        if (!self.line_numbers) return 0;
        // Calculate width needed for line numbers (min 3 chars + space)
        var num_lines = self.lines.items.len;
        var width: u16 = 1;
        while (num_lines >= 10) : (width += 1) {
            num_lines /= 10;
        }
        return @max(width, 3) + 1; // +1 for separator
    }

    fn contentRect(self: *TextArea) Rect {
        var rect = self.state.contentRect();
        const ln_width = self.lineNumberWidth();
        rect.x += ln_width;
        rect.width -= @min(ln_width, rect.width);
        return rect;
    }

    fn ensureCursorVisible(self: *TextArea) void {
        const rect = self.contentRect();

        // Vertical scrolling
        if (self.cursor_line < self.scroll_y) {
            self.scroll_y = self.cursor_line;
        } else if (self.cursor_line >= self.scroll_y + rect.height) {
            self.scroll_y = self.cursor_line - rect.height + 1;
        }

        // Horizontal scrolling
        if (self.cursor_col < self.scroll_x) {
            self.scroll_x = self.cursor_col;
        } else if (self.cursor_col >= self.scroll_x + rect.width) {
            self.scroll_x = self.cursor_col - rect.width + 1;
        }
    }

    pub fn handleKey(self: *TextArea, key: Key) EventResult {
        switch (key) {
            .char => |c| {
                if (!self.readonly) {
                    self.deleteSelection();
                    self.insertChar(@intCast(c));
                    return .consumed;
                }
            },
            .enter => {
                if (!self.readonly) {
                    self.deleteSelection();
                    self.insertNewline();
                    return .consumed;
                }
            },
            .backspace => {
                if (!self.readonly) {
                    if (self.hasSelection()) {
                        self.deleteSelection();
                    } else {
                        self.deleteBackward();
                    }
                    return .consumed;
                }
            },
            .delete => {
                if (!self.readonly) {
                    if (self.hasSelection()) {
                        self.deleteSelection();
                    } else {
                        self.deleteForward();
                    }
                    return .consumed;
                }
            },
            .left => {
                self.clearSelection();
                self.moveCursorLeft();
                return .consumed;
            },
            .right => {
                self.clearSelection();
                self.moveCursorRight();
                return .consumed;
            },
            .up => {
                self.clearSelection();
                self.moveCursorUp();
                return .consumed;
            },
            .down => {
                self.clearSelection();
                self.moveCursorDown();
                return .consumed;
            },
            .home => {
                self.clearSelection();
                self.cursor_col = 0;
                self.ensureCursorVisible();
                return .consumed;
            },
            .end => {
                self.clearSelection();
                self.cursor_col = self.currentLine().items.len;
                self.ensureCursorVisible();
                return .consumed;
            },
            .page_up => {
                self.clearSelection();
                const rect = self.contentRect();
                if (self.cursor_line > rect.height) {
                    self.cursor_line -= rect.height;
                } else {
                    self.cursor_line = 0;
                }
                self.clampCursorCol();
                self.ensureCursorVisible();
                return .consumed;
            },
            .page_down => {
                self.clearSelection();
                const rect = self.contentRect();
                self.cursor_line += rect.height;
                if (self.cursor_line >= self.lines.items.len) {
                    self.cursor_line = self.lines.items.len - 1;
                }
                self.clampCursorCol();
                self.ensureCursorVisible();
                return .consumed;
            },
            .shift_left => {
                self.startSelection();
                self.moveCursorLeft();
                self.extendSelection();
                return .consumed;
            },
            .shift_right => {
                self.startSelection();
                self.moveCursorRight();
                self.extendSelection();
                return .consumed;
            },
            .shift_up => {
                self.startSelection();
                self.moveCursorUp();
                self.extendSelection();
                return .consumed;
            },
            .shift_down => {
                self.startSelection();
                self.moveCursorDown();
                self.extendSelection();
                return .consumed;
            },
            .shift_home => {
                self.startSelection();
                self.cursor_col = 0;
                self.extendSelection();
                self.ensureCursorVisible();
                return .consumed;
            },
            .shift_end => {
                self.startSelection();
                self.cursor_col = self.currentLine().items.len;
                self.extendSelection();
                self.ensureCursorVisible();
                return .consumed;
            },
            .shift_insert => {
                // Shift+Insert - Paste from clipboard
                if (!self.readonly) {
                    self.pasteFromClipboard();
                }
                return .consumed;
            },
            .ctrl => |c| {
                switch (c) {
                    1 => { // Ctrl+A - Select all
                        self.selectAll();
                        return .consumed;
                    },
                    3 => { // Ctrl+C - Copy
                        self.copyToClipboard();
                        return .consumed;
                    },
                    22 => { // Ctrl+V - Paste
                        if (!self.readonly) {
                            self.pasteFromClipboard();
                        }
                        return .consumed;
                    },
                    24 => { // Ctrl+X - Cut
                        if (!self.readonly) {
                            self.copyToClipboard();
                            self.deleteSelection();
                        }
                        return .consumed;
                    },
                    else => return .ignored,
                }
            },
            .tab => {
                if (!self.readonly) {
                    self.deleteSelection();
                    // Insert spaces instead of tab
                    var i: usize = 0;
                    while (i < 4) : (i += 1) {
                        self.insertChar(' ');
                    }
                    return .consumed;
                }
            },
            else => return .ignored,
        }
        return .ignored;
    }

    pub fn handleMouse(self: *TextArea, mouse: input.MouseEvent) EventResult {
        const text_rect = self.contentRect();

        // Handle scroll wheel
        if (mouse.button == .scroll_up) {
            if (self.scroll_y >= 3) {
                self.scroll_y -= 3;
            } else {
                self.scroll_y = 0;
            }
            return .consumed;
        } else if (mouse.button == .scroll_down) {
            const max_scroll = self.lines.items.len -| 1;
            self.scroll_y = @min(self.scroll_y + 3, max_scroll);
            return .consumed;
        }

        // Handle middle-click paste (primary selection)
        if (mouse.button == .middle and mouse.event_type == .press) {
            if (!self.readonly) {
                self.pasteFromPrimarySelection();
            }
            return .consumed;
        }

        // Only handle left button events
        if (mouse.button != .left) {
            return .ignored;
        }

        // Check if mouse is in content area
        if (!text_rect.contains(mouse.x, mouse.y)) {
            return .ignored;
        }

        // Calculate text position from screen coordinates
        const rel_x = mouse.x -| text_rect.x;
        const rel_y = mouse.y -| text_rect.y;
        const target_line = self.scroll_y + rel_y;
        const target_col = self.scroll_x + rel_x;

        switch (mouse.event_type) {
            .press => {
                // Start new selection (selection only, never moves text)
                if (target_line < self.lines.items.len) {
                    self.cursor_line = target_line;
                    const line_len = self.lines.items[target_line].items.len;
                    self.cursor_col = @min(target_col, line_len);

                    // Always clear existing selection and start fresh - no drag-to-move
                    self.selection_start_line = self.cursor_line;
                    self.selection_start_col = self.cursor_col;
                    self.selection_end_line = self.cursor_line;
                    self.selection_end_col = self.cursor_col;
                    self.mouse_selecting = true;
                }
                return .consumed;
            },
            .drag => {
                // Extend selection only - no text movement
                if (self.mouse_selecting) {
                    if (target_line < self.lines.items.len) {
                        self.cursor_line = target_line;
                        const line_len = self.lines.items[target_line].items.len;
                        self.cursor_col = @min(target_col, line_len);

                        // Update selection end only
                        self.selection_end_line = self.cursor_line;
                        self.selection_end_col = self.cursor_col;
                    }
                    return .consumed;
                }
                return .consumed; // Always consume drag events to prevent any side effects
            },
            .release => {
                // Finish selection - no text modification
                self.mouse_selecting = false;

                // If selection start equals end, clear selection (it was just a click)
                if (self.selection_start_line == self.selection_end_line and
                    self.selection_start_col == self.selection_end_col)
                {
                    self.clearSelection();
                }
                return .consumed;
            },
            .move => {
                // Ignore regular mouse movement
                return .consumed;
            },
        }

        return .ignored;
    }

    fn moveCursorLeft(self: *TextArea) void {
        if (self.cursor_col > 0) {
            self.cursor_col -= 1;
        } else if (self.cursor_line > 0) {
            self.cursor_line -= 1;
            self.cursor_col = self.currentLine().items.len;
        }
        self.ensureCursorVisible();
    }

    fn moveCursorRight(self: *TextArea) void {
        const line_len = self.currentLine().items.len;
        if (self.cursor_col < line_len) {
            self.cursor_col += 1;
        } else if (self.cursor_line < self.lines.items.len - 1) {
            self.cursor_line += 1;
            self.cursor_col = 0;
        }
        self.ensureCursorVisible();
    }

    fn moveCursorUp(self: *TextArea) void {
        if (self.cursor_line > 0) {
            self.cursor_line -= 1;
            self.clampCursorCol();
        }
        self.ensureCursorVisible();
    }

    fn moveCursorDown(self: *TextArea) void {
        if (self.cursor_line < self.lines.items.len - 1) {
            self.cursor_line += 1;
            self.clampCursorCol();
        }
        self.ensureCursorVisible();
    }

    fn clampCursorCol(self: *TextArea) void {
        const line_len = self.currentLine().items.len;
        if (self.cursor_col > line_len) {
            self.cursor_col = line_len;
        }
    }

    fn insertChar(self: *TextArea, c: u8) void {
        const line = self.currentLine();
        line.insert(self.allocator, self.cursor_col, c) catch return;
        self.cursor_col += 1;
        self.ensureCursorVisible();
        if (self.on_change) |cb| cb(self);
    }

    fn insertNewline(self: *TextArea) void {
        const line = self.currentLine();

        // Create new line with rest of current line
        var new_line: std.ArrayList(u8) = .empty;
        if (self.cursor_col < line.items.len) {
            new_line.appendSlice(self.allocator, line.items[self.cursor_col..]) catch return;
            line.shrinkRetainingCapacity(self.cursor_col);
        }

        self.lines.insert(self.allocator, self.cursor_line + 1, new_line) catch return;
        self.cursor_line += 1;
        self.cursor_col = 0;
        self.ensureCursorVisible();
        if (self.on_change) |cb| cb(self);
    }

    fn deleteBackward(self: *TextArea) void {
        if (self.cursor_col > 0) {
            const line = self.currentLine();
            _ = line.orderedRemove(self.cursor_col - 1);
            self.cursor_col -= 1;
        } else if (self.cursor_line > 0) {
            // Merge with previous line
            const prev_line = &self.lines.items[self.cursor_line - 1];
            const prev_len = prev_line.items.len;
            const curr_line = self.currentLine();

            prev_line.appendSlice(self.allocator, curr_line.items) catch return;
            curr_line.deinit(self.allocator);
            _ = self.lines.orderedRemove(self.cursor_line);

            self.cursor_line -= 1;
            self.cursor_col = prev_len;
        }
        self.ensureCursorVisible();
        if (self.on_change) |cb| cb(self);
    }

    fn deleteForward(self: *TextArea) void {
        const line = self.currentLine();
        if (self.cursor_col < line.items.len) {
            _ = line.orderedRemove(self.cursor_col);
        } else if (self.cursor_line < self.lines.items.len - 1) {
            // Merge with next line
            const next_line = &self.lines.items[self.cursor_line + 1];
            line.appendSlice(self.allocator, next_line.items) catch return;
            next_line.deinit(self.allocator);
            _ = self.lines.orderedRemove(self.cursor_line + 1);
        }
        if (self.on_change) |cb| cb(self);
    }

    fn deleteSelection(self: *TextArea) void {
        const sel = self.normalizedSelection() orelse return;

        if (sel.start_line == sel.end_line) {
            // Single line deletion
            const line = &self.lines.items[sel.start_line];
            const start = @min(sel.start_col, line.items.len);
            const end = @min(sel.end_col, line.items.len);

            var i: usize = 0;
            while (i < end - start) : (i += 1) {
                _ = line.orderedRemove(start);
            }
        } else {
            // Multi-line deletion
            // Keep start of first line
            const first_line = &self.lines.items[sel.start_line];
            const start_col = @min(sel.start_col, first_line.items.len);
            first_line.shrinkRetainingCapacity(start_col);

            // Append end of last line to first line
            const last_line = &self.lines.items[sel.end_line];
            const end_col = @min(sel.end_col, last_line.items.len);
            if (end_col < last_line.items.len) {
                first_line.appendSlice(self.allocator, last_line.items[end_col..]) catch {};
            }

            // Remove lines in between (including last line)
            var i = sel.end_line;
            while (i > sel.start_line) : (i -= 1) {
                self.lines.items[i].deinit(self.allocator);
                _ = self.lines.orderedRemove(i);
            }
        }

        self.cursor_line = sel.start_line;
        self.cursor_col = sel.start_col;
        self.clearSelection();
        self.ensureCursorVisible();
        if (self.on_change) |cb| cb(self);
    }

    pub fn draw(self: *TextArea, screen: *Screen) void {
        const rect = self.state.rect;
        const content = self.state.contentRect();

        // Draw border
        if (self.state.border) {
            widget.drawBorder(screen, rect, self.state.focused);
            if (self.state.title) |title| {
                widget.drawTitle(screen, rect, title, self.state.focused);
            }
        }

        // Clear content area
        widget.clearContent(screen, content);

        // Draw line numbers
        const ln_width = self.lineNumberWidth();
        if (self.line_numbers and ln_width > 0) {
            var row: u16 = 0;
            while (row < content.height) : (row += 1) {
                const line_idx = self.scroll_y + row;
                if (line_idx < self.lines.items.len) {
                    var buf: [16]u8 = undefined;
                    const num_str = std.fmt.bufPrint(&buf, "{d:>3} ", .{line_idx + 1}) catch "??? ";
                    const num_style = if (line_idx == self.cursor_line)
                        style.Style{ .fg = style.ui.foreground, .bg = style.ui.background }
                    else
                        style.styles.dim;
                    screen.drawText(content.x, content.y + row, num_str, num_style);
                }
            }
        }

        // Draw content
        const text_rect = self.contentRect();
        var row: u16 = 0;
        while (row < text_rect.height) : (row += 1) {
            const line_idx = self.scroll_y + row;
            if (line_idx >= self.lines.items.len) break;

            const line = self.lines.items[line_idx].items;

            // Iterate through UTF-8 codepoints, tracking both byte position and display column
            var byte_idx: usize = 0;
            var display_col: usize = 0;

            // Skip characters before scroll_x
            while (byte_idx < line.len and display_col < self.scroll_x) {
                const cp_len = std.unicode.utf8ByteSequenceLength(line[byte_idx]) catch 1;
                byte_idx += cp_len;
                display_col += 1;
            }

            // Draw visible characters
            var screen_col: u16 = 0;
            while (byte_idx < line.len and screen_col < text_rect.width) {
                // Safely decode UTF-8, treating invalid bytes as single characters
                const byte = line[byte_idx];
                var cp_len: usize = 1;
                var codepoint: u21 = '?';

                if (byte < 0x80) {
                    // ASCII - single byte
                    codepoint = byte;
                } else if (byte >= 0xC0 and byte < 0xF8) {
                    // Potential multi-byte sequence
                    cp_len = std.unicode.utf8ByteSequenceLength(byte) catch 1;
                    const end_idx = @min(byte_idx + cp_len, line.len);
                    if (end_idx - byte_idx == cp_len) {
                        // Have enough bytes, try to decode
                        codepoint = std.unicode.utf8Decode(line[byte_idx..end_idx]) catch '?';
                    } else {
                        // Not enough bytes - treat as single replacement char
                        cp_len = 1;
                    }
                } else {
                    // Invalid start byte (0x80-0xBF or 0xF8+) - skip as single byte
                    codepoint = '?';
                }

                var char_style = style.styles.normal;

                // Check if character is selected (using display column for selection)
                if (self.normalizedSelection()) |sel| {
                    const in_selection = if (line_idx > sel.start_line and line_idx < sel.end_line)
                        true
                    else if (line_idx == sel.start_line and line_idx == sel.end_line)
                        display_col >= sel.start_col and display_col < sel.end_col
                    else if (line_idx == sel.start_line)
                        display_col >= sel.start_col
                    else if (line_idx == sel.end_line)
                        display_col < sel.end_col
                    else
                        false;

                    if (in_selection) {
                        char_style = style.styles.selected;
                    }
                }

                screen.setCell(text_rect.x + screen_col, text_rect.y + row, codepoint, char_style);

                byte_idx += cp_len;
                display_col += 1;
                screen_col += 1;
            }
        }

        // Position cursor
        if (self.state.focused) {
            const cursor_screen_x = text_rect.x + @as(u16, @intCast(self.cursor_col)) - @as(u16, @intCast(self.scroll_x));
            const cursor_screen_y = text_rect.y + @as(u16, @intCast(self.cursor_line)) - @as(u16, @intCast(self.scroll_y));
            screen.setCursor(cursor_screen_x, cursor_screen_y);
            screen.cursor_visible = true;
        }
    }
};
