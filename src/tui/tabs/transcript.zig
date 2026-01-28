const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("../widgets/widget.zig");

const Key = input.Key;
const MouseEvent = input.MouseEvent;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const TranscriptLine = struct {
    text: []const u8,
    style: LineStyle = .normal,

    pub const LineStyle = enum {
        normal,
        error_style,
        warning,
        info,
        success,
    };
};

pub const TranscriptTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,

    lines: std.ArrayList(TranscriptLine),
    scroll_offset: usize = 0,
    auto_scroll: bool = true,
    selected_line: ?usize = null,

    // Buffer for building lines
    line_buffer: std.ArrayList(u8),

    // Callback to send selected line to workspace
    on_send_to_workspace: ?*const fn ([]const u8) void = null,

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !TranscriptTab {
        return TranscriptTab{
            .allocator = allocator,
            .rect = rect,
            .lines = .empty,
            .line_buffer = .empty,
        };
    }

    pub fn deinit(self: *TranscriptTab) void {
        for (self.lines.items) |line| {
            self.allocator.free(line.text);
        }
        self.lines.deinit(self.allocator);
        self.line_buffer.deinit(self.allocator);
    }

    pub fn clear(self: *TranscriptTab) void {
        for (self.lines.items) |line| {
            self.allocator.free(line.text);
        }
        self.lines.clearRetainingCapacity();
        self.scroll_offset = 0;
    }

    pub fn addLine(self: *TranscriptTab, text: []const u8, line_style: TranscriptLine.LineStyle) !void {
        const copy = try self.allocator.dupe(u8, text);
        try self.lines.append(self.allocator, .{ .text = copy, .style = line_style });

        if (self.auto_scroll) {
            self.scrollToBottom();
        }
    }

    pub fn addText(self: *TranscriptTab, text: []const u8) !void {
        // Split text by newlines and add each line
        var iter = std.mem.splitScalar(u8, text, '\n');
        var first = true;
        while (iter.next()) |part| {
            if (first and self.line_buffer.items.len > 0) {
                // Append to existing buffer
                try self.line_buffer.appendSlice(self.allocator, part);
                first = false;
            } else if (first) {
                try self.line_buffer.appendSlice(self.allocator, part);
                first = false;
            } else {
                // Flush previous line
                if (self.line_buffer.items.len > 0 or !first) {
                    try self.addLine(self.line_buffer.items, .normal);
                    self.line_buffer.clearRetainingCapacity();
                }
                try self.line_buffer.appendSlice(self.allocator, part);
            }
        }
    }

    pub fn newline(self: *TranscriptTab) !void {
        try self.addLine(self.line_buffer.items, .normal);
        self.line_buffer.clearRetainingCapacity();
    }

    pub fn addError(self: *TranscriptTab, text: []const u8) !void {
        try self.addLine(text, .error_style);
    }

    pub fn addInfo(self: *TranscriptTab, text: []const u8) !void {
        try self.addLine(text, .info);
    }

    pub fn addSuccess(self: *TranscriptTab, text: []const u8) !void {
        try self.addLine(text, .success);
    }

    fn visibleLines(self: *TranscriptTab) usize {
        const content_height = if (self.rect.height > 2) self.rect.height - 2 else 0;
        return content_height;
    }

    fn scrollToBottom(self: *TranscriptTab) void {
        const visible = self.visibleLines();
        if (self.lines.items.len > visible) {
            self.scroll_offset = self.lines.items.len - visible;
        } else {
            self.scroll_offset = 0;
        }
    }

    pub fn handleKey(self: *TranscriptTab, key: Key) EventResult {
        switch (key) {
            .up => {
                // Move selection up
                if (self.selected_line) |sel| {
                    if (sel > 0) {
                        self.selected_line = sel - 1;
                        // Scroll to keep selection visible
                        if (sel - 1 < self.scroll_offset) {
                            self.scroll_offset = sel - 1;
                        }
                    }
                } else if (self.lines.items.len > 0) {
                    // Select last visible line
                    const visible = self.visibleLines();
                    self.selected_line = @min(self.scroll_offset + visible - 1, self.lines.items.len - 1);
                }
                self.auto_scroll = false;
                return .consumed;
            },
            .down => {
                // Move selection down
                if (self.selected_line) |sel| {
                    if (sel + 1 < self.lines.items.len) {
                        self.selected_line = sel + 1;
                        // Scroll to keep selection visible
                        const visible = self.visibleLines();
                        if (sel + 1 >= self.scroll_offset + visible) {
                            self.scroll_offset = sel + 2 -| visible;
                        }
                    }
                } else if (self.lines.items.len > 0) {
                    // Select first visible line
                    self.selected_line = self.scroll_offset;
                }
                return .consumed;
            },
            .page_up => {
                const visible = self.visibleLines();
                if (self.scroll_offset > visible) {
                    self.scroll_offset -= visible;
                } else {
                    self.scroll_offset = 0;
                }
                self.auto_scroll = false;
                // Update selection to stay in view
                if (self.selected_line) |sel| {
                    if (sel >= self.scroll_offset + visible) {
                        self.selected_line = self.scroll_offset + visible - 1;
                    }
                }
                return .consumed;
            },
            .page_down => {
                const visible = self.visibleLines();
                self.scroll_offset += visible;
                if (self.scroll_offset >= self.lines.items.len -| visible) {
                    self.scroll_offset = self.lines.items.len -| visible;
                    self.auto_scroll = true;
                }
                // Update selection to stay in view
                if (self.selected_line) |sel| {
                    if (sel < self.scroll_offset) {
                        self.selected_line = self.scroll_offset;
                    }
                }
                return .consumed;
            },
            .home => {
                self.scroll_offset = 0;
                self.selected_line = if (self.lines.items.len > 0) 0 else null;
                self.auto_scroll = false;
                return .consumed;
            },
            .end => {
                self.scrollToBottom();
                self.selected_line = if (self.lines.items.len > 0) self.lines.items.len - 1 else null;
                self.auto_scroll = true;
                return .consumed;
            },
            .enter => {
                // Send selected line to workspace
                self.sendSelectedToWorkspace();
                return .consumed;
            },
            .char => |c| {
                if (c == 'w' or c == 'W') {
                    // Send selected line to workspace
                    self.sendSelectedToWorkspace();
                    return .consumed;
                }
            },
            .escape => {
                // Clear selection
                self.selected_line = null;
                return .consumed;
            },
            else => return .ignored,
        }
        return .ignored;
    }

    fn sendSelectedToWorkspace(self: *TranscriptTab) void {
        if (self.selected_line) |sel| {
            if (sel < self.lines.items.len) {
                const line = self.lines.items[sel];
                if (self.on_send_to_workspace) |callback| {
                    callback(line.text);
                }
            }
        }
    }

    pub fn handleMouse(self: *TranscriptTab, mouse: MouseEvent) void {
        const content = Rect.init(self.rect.x + 1, self.rect.y + 1, self.rect.width -| 2, self.rect.height -| 2);

        // Handle scroll wheel
        if (mouse.button == .scroll_up) {
            self.scroll(-3);
        } else if (mouse.button == .scroll_down) {
            self.scroll(3);
        }

        // Handle click in content area - select line
        if (mouse.event_type == .press and mouse.button == .left) {
            if (content.contains(mouse.x, mouse.y)) {
                self.auto_scroll = false;
                // Calculate which line was clicked
                const y_offset = mouse.y - content.y;
                const line_index = self.scroll_offset + y_offset;
                if (line_index < self.lines.items.len) {
                    self.selected_line = line_index;
                }
            }
        }

        // Double-click to send to workspace
        // Note: Would need to track click timing for true double-click
    }

    pub fn scroll(self: *TranscriptTab, delta: i32) void {
        const max_scroll = self.lines.items.len -| self.visibleLines();

        if (delta < 0) {
            // Scroll up
            const amount = @as(usize, @intCast(-delta));
            if (self.scroll_offset > amount) {
                self.scroll_offset -= amount;
            } else {
                self.scroll_offset = 0;
            }
            self.auto_scroll = false;
        } else {
            // Scroll down
            const amount = @as(usize, @intCast(delta));
            self.scroll_offset = @min(self.scroll_offset + amount, max_scroll);
            if (self.scroll_offset >= max_scroll) {
                self.auto_scroll = true;
            }
        }
    }

    pub fn draw(self: *TranscriptTab, screen: *Screen) void {
        const rect = self.rect;
        const content = Rect.init(rect.x + 1, rect.y + 1, rect.width -| 2, rect.height -| 2);

        // Draw border
        widget.drawBorderRounded(screen, rect, self.focused);
        widget.drawTitle(screen, rect, "Transcript", self.focused);

        // Clear content area
        widget.clearContent(screen, content);

        // Draw lines
        var row: u16 = 0;
        while (row < content.height) : (row += 1) {
            const line_idx = self.scroll_offset + row;
            if (line_idx >= self.lines.items.len) break;

            const line = self.lines.items[line_idx];
            const is_selected = self.selected_line != null and self.selected_line.? == line_idx;

            const line_style = if (is_selected)
                Style{ .fg = style_mod.ui.background, .bg = style_mod.ui.selection }
            else switch (line.style) {
                .normal => style_mod.styles.normal,
                .error_style => style_mod.styles.error_style,
                .warning => Style{ .fg = style_mod.ui.warning_text, .bg = style_mod.ui.background },
                .info => Style{ .fg = style_mod.ui.info_text, .bg = style_mod.ui.background },
                .success => Style{ .fg = style_mod.ui.success_text, .bg = style_mod.ui.background },
            };

            // For selected line, fill the entire line with highlight color
            if (is_selected) {
                var col: u16 = 0;
                while (col < content.width) : (col += 1) {
                    screen.setCell(content.x + col, content.y + row, ' ', line_style);
                }
            }

            screen.drawTextClipped(content.x, content.y + row, line.text, content.width, line_style);
        }

        // Draw scrollbar if needed
        if (self.lines.items.len > content.height) {
            self.drawScrollbar(screen, content);
        }
    }

    fn drawScrollbar(self: *TranscriptTab, screen: *Screen, content: Rect) void {
        const total = self.lines.items.len;
        const visible = @as(usize, content.height);
        if (total <= visible) return;

        const scrollbar_x = content.x + content.width;

        // Calculate thumb position and size
        const thumb_size = @max(1, (visible * content.height) / total);
        const thumb_pos = @as(u16, @intCast((self.scroll_offset * (content.height - thumb_size)) / (total - visible)));

        // Draw scrollbar track
        var row: u16 = 0;
        while (row < content.height) : (row += 1) {
            const char: u21 = if (row >= thumb_pos and row < thumb_pos + thumb_size)
                style_mod.box.block_full
            else
                style_mod.box.block_light;
            screen.setCell(scrollbar_x, content.y + row, char, style_mod.styles.dim);
        }
    }
};

// Global transcript instance for use by primitives
pub var global_transcript: ?*TranscriptTab = null;

pub fn setGlobalTranscript(transcript: ?*TranscriptTab) void {
    global_transcript = transcript;
}

pub fn getGlobalTranscript() ?*TranscriptTab {
    return global_transcript;
}
