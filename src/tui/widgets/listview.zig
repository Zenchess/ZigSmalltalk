const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("widget.zig");

const Key = input.Key;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const ListItem = struct {
    text: []const u8,
    text_owned: bool = false, // Whether we own (and should free) the text
    data: ?*anyopaque = null,
};

pub const ListView = struct {
    allocator: std.mem.Allocator,
    state: widget.WidgetState,

    items: std.ArrayList(ListItem),
    selected_index: ?usize = null,
    scroll_offset: usize = 0,

    // Callbacks
    on_select: ?*const fn (*ListView, usize) void = null,
    on_activate: ?*const fn (*ListView, usize) void = null,

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !ListView {
        return ListView{
            .allocator = allocator,
            .state = .{ .rect = rect },
            .items = .empty,
        };
    }

    pub fn deinit(self: *ListView) void {
        self.freeOwnedStrings();
        self.items.deinit(self.allocator);
    }

    fn freeOwnedStrings(self: *ListView) void {
        for (self.items.items) |item| {
            if (item.text_owned) {
                self.allocator.free(@constCast(item.text));
            }
        }
    }

    pub fn clear(self: *ListView) void {
        self.freeOwnedStrings();
        self.items.clearRetainingCapacity();
        self.selected_index = null;
        self.scroll_offset = 0;
    }

    /// Add an item, copying the text string
    pub fn addItem(self: *ListView, text: []const u8, data: ?*anyopaque) !void {
        const text_copy = try self.allocator.dupe(u8, text);
        try self.items.append(self.allocator, .{ .text = text_copy, .text_owned = true, .data = data });
    }

    /// Add an item without copying (caller must ensure text outlives ListView)
    pub fn addItemStatic(self: *ListView, text: []const u8, data: ?*anyopaque) !void {
        try self.items.append(self.allocator, .{ .text = text, .text_owned = false, .data = data });
    }

    pub fn setItems(self: *ListView, texts: []const []const u8) !void {
        self.clear();
        for (texts) |text| {
            try self.addItem(text, null);
        }
    }

    pub fn getSelectedItem(self: *ListView) ?ListItem {
        if (self.selected_index) |idx| {
            if (idx < self.items.items.len) {
                return self.items.items[idx];
            }
        }
        return null;
    }

    pub fn selectIndex(self: *ListView, index: usize) void {
        if (index < self.items.items.len) {
            self.selected_index = index;
            self.ensureVisible(index);
            if (self.on_select) |cb| cb(self, index);
        }
    }

    fn visibleCount(self: *ListView) usize {
        const content = self.state.contentRect();
        return content.height;
    }

    fn ensureVisible(self: *ListView, index: usize) void {
        const visible = self.visibleCount();
        if (visible == 0) return;

        if (index < self.scroll_offset) {
            self.scroll_offset = index;
        } else if (index >= self.scroll_offset + visible) {
            self.scroll_offset = index - visible + 1;
        }
    }

    pub fn handleKey(self: *ListView, key: Key) EventResult {
        if (self.items.items.len == 0) return .ignored;

        switch (key) {
            .up => {
                if (self.selected_index) |idx| {
                    if (idx > 0) {
                        self.selectIndex(idx - 1);
                    }
                } else {
                    self.selectIndex(0);
                }
                return .consumed;
            },
            .down => {
                if (self.selected_index) |idx| {
                    if (idx < self.items.items.len - 1) {
                        self.selectIndex(idx + 1);
                    }
                } else {
                    self.selectIndex(0);
                }
                return .consumed;
            },
            .home => {
                self.selectIndex(0);
                return .consumed;
            },
            .end => {
                self.selectIndex(self.items.items.len - 1);
                return .consumed;
            },
            .page_up => {
                const visible = self.visibleCount();
                if (self.selected_index) |idx| {
                    if (idx > visible) {
                        self.selectIndex(idx - visible);
                    } else {
                        self.selectIndex(0);
                    }
                }
                return .consumed;
            },
            .page_down => {
                const visible = self.visibleCount();
                if (self.selected_index) |idx| {
                    const new_idx = idx + visible;
                    if (new_idx < self.items.items.len) {
                        self.selectIndex(new_idx);
                    } else {
                        self.selectIndex(self.items.items.len - 1);
                    }
                }
                return .consumed;
            },
            .enter => {
                if (self.selected_index) |idx| {
                    if (self.on_activate) |cb| {
                        cb(self, idx);
                        return .consumed;
                    }
                }
                return .ignored;
            },
            else => return .ignored,
        }
    }

    pub fn draw(self: *ListView, screen: *Screen) void {
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

        // Draw items
        var row: u16 = 0;
        while (row < content.height) : (row += 1) {
            const item_idx = self.scroll_offset + row;
            if (item_idx >= self.items.items.len) break;

            const item = self.items.items[item_idx];
            const is_selected = self.selected_index != null and self.selected_index.? == item_idx;

            const item_style = if (is_selected)
                style_mod.styles.selected
            else
                style_mod.styles.normal;

            // Fill background for selected item
            if (is_selected) {
                screen.fillRect(content.x, content.y + row, content.width, 1, ' ', item_style);
            }

            // Draw item text (clipped to width)
            screen.drawTextClipped(content.x, content.y + row, item.text, content.width, item_style);
        }

        // Draw scrollbar if needed
        if (self.items.items.len > content.height) {
            self.drawScrollbar(screen, content);
        }
    }

    fn drawScrollbar(self: *ListView, screen: *Screen, content: Rect) void {
        const total = self.items.items.len;
        const visible = @as(usize, content.height);
        if (total <= visible) return;

        const scrollbar_x = content.x + content.width - 1;

        // Calculate thumb position and size
        const thumb_size = @max(1, (visible * content.height) / total);
        const thumb_pos = @as(u16, @intCast((self.scroll_offset * (content.height - thumb_size)) / (total - visible)));

        // Draw scrollbar track
        var row: u16 = 0;
        while (row < content.height) : (row += 1) {
            const char: u21 = if (row >= thumb_pos and row < thumb_pos + thumb_size)
                style_mod.box.block_full[0]
            else
                style_mod.box.block_light[0];
            screen.setCell(scrollbar_x, content.y + row, char, style_mod.styles.dim);
        }
    }
};
