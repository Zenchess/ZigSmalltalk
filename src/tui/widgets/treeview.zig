const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("widget.zig");

const Key = input.Key;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const TreeNode = struct {
    allocator: std.mem.Allocator,
    text: []const u8,
    data: ?*anyopaque = null,
    children: std.ArrayList(*TreeNode),
    parent: ?*TreeNode = null,
    expanded: bool = false,
    level: usize = 0,

    pub fn init(allocator: std.mem.Allocator, text: []const u8, data: ?*anyopaque) !*TreeNode {
        const node = try allocator.create(TreeNode);
        node.* = TreeNode{
            .allocator = allocator,
            .text = text,
            .data = data,
            .children = .empty,
        };
        return node;
    }

    pub fn deinit(self: *TreeNode, allocator: std.mem.Allocator) void {
        for (self.children.items) |child| {
            child.deinit(allocator);
        }
        self.children.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn addChild(self: *TreeNode, child: *TreeNode) !void {
        child.parent = self;
        child.level = self.level + 1;
        try self.children.append(self.allocator, child);
    }

    pub fn hasChildren(self: *TreeNode) bool {
        return self.children.items.len > 0;
    }

    pub fn toggle(self: *TreeNode) void {
        if (self.hasChildren()) {
            self.expanded = !self.expanded;
        }
    }
};

pub const TreeView = struct {
    allocator: std.mem.Allocator,
    state: widget.WidgetState,

    roots: std.ArrayList(*TreeNode),
    flat_list: std.ArrayList(*TreeNode), // Flattened visible nodes
    selected_index: ?usize = null,
    scroll_offset: usize = 0,

    // Callbacks
    on_select: ?*const fn (*TreeView, *TreeNode) void = null,
    on_activate: ?*const fn (*TreeView, *TreeNode) void = null,

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !TreeView {
        return TreeView{
            .allocator = allocator,
            .state = .{ .rect = rect },
            .roots = .empty,
            .flat_list = .empty,
        };
    }

    pub fn deinit(self: *TreeView) void {
        for (self.roots.items) |root| {
            root.deinit(self.allocator);
        }
        self.roots.deinit(self.allocator);
        self.flat_list.deinit(self.allocator);
    }

    pub fn clear(self: *TreeView) void {
        for (self.roots.items) |root| {
            root.deinit(self.allocator);
        }
        self.roots.clearRetainingCapacity();
        self.flat_list.clearRetainingCapacity();
        self.selected_index = null;
        self.scroll_offset = 0;
    }

    pub fn addRoot(self: *TreeView, node: *TreeNode) !void {
        node.level = 0;
        try self.roots.append(self.allocator, node);
        self.rebuildFlatList();
    }

    pub fn rebuildFlatList(self: *TreeView) void {
        self.flat_list.clearRetainingCapacity();

        for (self.roots.items) |root| {
            self.addToFlatList(root);
        }
    }

    fn addToFlatList(self: *TreeView, node: *TreeNode) void {
        self.flat_list.append(self.allocator, node) catch return;

        if (node.expanded) {
            for (node.children.items) |child| {
                self.addToFlatList(child);
            }
        }
    }

    pub fn getSelectedNode(self: *TreeView) ?*TreeNode {
        if (self.selected_index) |idx| {
            if (idx < self.flat_list.items.len) {
                return self.flat_list.items[idx];
            }
        }
        return null;
    }

    pub fn selectIndex(self: *TreeView, index: usize) void {
        if (index < self.flat_list.items.len) {
            self.selected_index = index;
            self.ensureVisible(index);
            if (self.on_select) |cb| {
                cb(self, self.flat_list.items[index]);
            }
        }
    }

    pub fn selectNode(self: *TreeView, node: *TreeNode) void {
        for (self.flat_list.items, 0..) |n, i| {
            if (n == node) {
                self.selectIndex(i);
                break;
            }
        }
    }

    fn visibleCount(self: *TreeView) usize {
        const content = self.state.contentRect();
        return content.height;
    }

    fn ensureVisible(self: *TreeView, index: usize) void {
        const visible = self.visibleCount();
        if (visible == 0) return;

        if (index < self.scroll_offset) {
            self.scroll_offset = index;
        } else if (index >= self.scroll_offset + visible) {
            self.scroll_offset = index - visible + 1;
        }
    }

    /// Handle mouse events. Returns true if a node was clicked (selection may have changed).
    /// The node_toggled flag indicates if the click was on an expansion arrow.
    pub fn handleMouse(self: *TreeView, mouse_x: u16, mouse_y: u16) struct { clicked: bool, toggled: bool } {
        const content = self.state.contentRect();

        // Check if click is within content area
        if (mouse_x < content.x or mouse_x >= content.x + content.width) {
            return .{ .clicked = false, .toggled = false };
        }
        if (mouse_y < content.y or mouse_y >= content.y + content.height) {
            return .{ .clicked = false, .toggled = false };
        }

        // Calculate which row was clicked
        const rel_y = mouse_y - content.y;
        const clicked_idx = self.scroll_offset + rel_y;

        if (clicked_idx >= self.flat_list.items.len) {
            return .{ .clicked = false, .toggled = false };
        }

        const node = self.flat_list.items[clicked_idx];

        // Calculate the x position of the expansion arrow for this node
        const indent: u16 = @intCast(node.level * 2);
        const arrow_start = content.x + indent;
        const arrow_end = arrow_start + 2; // Arrow is 2 characters wide

        // Check if click is on the expansion arrow
        if (mouse_x >= arrow_start and mouse_x < arrow_end and node.hasChildren()) {
            // Toggle expansion
            node.expanded = !node.expanded;
            self.rebuildFlatList();
            // Also select this node
            self.selected_index = clicked_idx;
            return .{ .clicked = true, .toggled = true };
        }

        // Normal click - just select the node
        self.selected_index = clicked_idx;
        if (self.on_select) |cb| {
            cb(self, node);
        }
        return .{ .clicked = true, .toggled = false };
    }

    pub fn handleKey(self: *TreeView, key: Key) EventResult {
        if (self.flat_list.items.len == 0) return .ignored;

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
                    if (idx < self.flat_list.items.len - 1) {
                        self.selectIndex(idx + 1);
                    }
                } else {
                    self.selectIndex(0);
                }
                return .consumed;
            },
            .left => {
                if (self.getSelectedNode()) |node| {
                    if (node.expanded and node.hasChildren()) {
                        node.expanded = false;
                        self.rebuildFlatList();
                    } else if (node.parent) |parent| {
                        self.selectNode(parent);
                    }
                }
                return .consumed;
            },
            .right => {
                if (self.getSelectedNode()) |node| {
                    if (node.hasChildren()) {
                        if (!node.expanded) {
                            node.expanded = true;
                            self.rebuildFlatList();
                        } else if (node.children.items.len > 0) {
                            // Move to first child
                            self.selectNode(node.children.items[0]);
                        }
                    }
                }
                return .consumed;
            },
            .enter => {
                if (self.getSelectedNode()) |node| {
                    if (node.hasChildren()) {
                        node.toggle();
                        self.rebuildFlatList();
                    } else if (self.on_activate) |cb| {
                        cb(self, node);
                    }
                }
                return .consumed;
            },
            .home => {
                self.selectIndex(0);
                return .consumed;
            },
            .end => {
                if (self.flat_list.items.len > 0) {
                    self.selectIndex(self.flat_list.items.len - 1);
                }
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
                    if (new_idx < self.flat_list.items.len) {
                        self.selectIndex(new_idx);
                    } else if (self.flat_list.items.len > 0) {
                        self.selectIndex(self.flat_list.items.len - 1);
                    }
                }
                return .consumed;
            },
            else => return .ignored,
        }
    }

    pub fn draw(self: *TreeView, screen: *Screen) void {
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

        // Draw nodes
        var row: u16 = 0;
        while (row < content.height) : (row += 1) {
            const node_idx = self.scroll_offset + row;
            if (node_idx >= self.flat_list.items.len) break;

            const node = self.flat_list.items[node_idx];
            const is_selected = self.selected_index != null and self.selected_index.? == node_idx;

            const item_style = if (is_selected)
                style_mod.styles.selected
            else
                style_mod.styles.normal;

            // Fill background for selected item
            if (is_selected) {
                screen.fillRect(content.x, content.y + row, content.width, 1, ' ', item_style);
            }

            // Calculate indent
            const indent: u16 = @intCast(node.level * 2);
            var col = content.x + indent;

            // Draw expand/collapse indicator
            if (node.hasChildren()) {
                const indicator = if (node.expanded)
                    style_mod.box.tree_expanded
                else
                    style_mod.box.tree_collapsed;
                screen.setCell(col, content.y + row, indicator, item_style);
                col += 2;
            } else {
                col += 2;
            }

            // Draw node text
            const max_width = if (col < content.x + content.width)
                content.x + content.width - col
            else
                0;
            screen.drawTextClipped(col, content.y + row, node.text, max_width, item_style);
        }

        // Draw scrollbar if needed
        if (self.flat_list.items.len > content.height) {
            self.drawScrollbar(screen, content);
        }
    }

    fn drawScrollbar(self: *TreeView, screen: *Screen, content: Rect) void {
        const total = self.flat_list.items.len;
        const visible = @as(usize, content.height);
        if (total <= visible or content.height == 0) return;

        const scrollbar_x = content.x + content.width - 1;

        // Calculate thumb size - ensure it's at least 1 but not larger than content height
        const raw_thumb_size = (visible * visible) / total;
        const thumb_size: u16 = @intCast(@max(1, @min(raw_thumb_size, visible)));

        // Calculate thumb position - ensure we don't overflow
        const scroll_range = total - visible;
        const track_range = content.height -| thumb_size; // Saturating subtraction
        const thumb_pos: u16 = if (scroll_range > 0 and track_range > 0)
            @intCast(@min(self.scroll_offset * track_range / scroll_range, track_range))
        else
            0;

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
