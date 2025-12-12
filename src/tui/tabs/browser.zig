const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("../widgets/widget.zig");
const TreeView = @import("../widgets/treeview.zig").TreeView;
const TreeNode = @import("../widgets/treeview.zig").TreeNode;
const ListView = @import("../widgets/listview.zig").ListView;
const TextArea = @import("../widgets/textarea.zig").TextArea;

const Key = input.Key;
const MouseEvent = input.MouseEvent;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const BrowserTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,

    // Panes
    class_tree: TreeView,
    instance_method_list: ListView,
    class_method_list: ListView,
    source_editor: TextArea,

    // State
    active_pane: Pane = .classes,
    show_class_side: bool = false,
    selected_class_name: []const u8 = "",
    selected_method_name: []const u8 = "",

    // Callbacks for loading data
    on_load_classes: ?*const fn (*BrowserTab) void = null,
    on_select_class: ?*const fn (*BrowserTab, []const u8) void = null,
    on_select_method: ?*const fn (*BrowserTab, []const u8, []const u8) void = null,
    on_save_method: ?*const fn (*BrowserTab, []const u8, []const u8, []const u8) void = null,

    pub const Pane = enum {
        classes,
        instance_methods,
        class_methods,
        source,
    };

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !BrowserTab {
        // Calculate pane sizes
        // Classes: 1/4, Instance Methods: 1/4, Class Methods: 1/4, Source: 1/4
        const pane_width = rect.width / 4;

        const class_rect = Rect.init(rect.x, rect.y, pane_width, rect.height);
        const inst_method_rect = Rect.init(rect.x + pane_width, rect.y, pane_width, rect.height);
        const class_method_rect = Rect.init(rect.x + pane_width * 2, rect.y, pane_width, rect.height);
        const source_rect = Rect.init(rect.x + pane_width * 3, rect.y, rect.width - pane_width * 3, rect.height);

        var class_tree = try TreeView.init(allocator, class_rect);
        class_tree.state.title = "Classes";

        var instance_method_list = try ListView.init(allocator, inst_method_rect);
        instance_method_list.state.title = "Instance";

        var class_method_list = try ListView.init(allocator, class_method_rect);
        class_method_list.state.title = "Class";

        var source_editor = try TextArea.init(allocator, Rect.init(source_rect.x + 1, source_rect.y + 1, source_rect.width -| 2, source_rect.height -| 2));
        source_editor.state.border = false;
        source_editor.line_numbers = true;
        source_editor.syntax_highlight = true;
        source_editor.readonly = true; // Read-only by default

        return BrowserTab{
            .allocator = allocator,
            .rect = rect,
            .class_tree = class_tree,
            .instance_method_list = instance_method_list,
            .class_method_list = class_method_list,
            .source_editor = source_editor,
        };
    }

    pub fn deinit(self: *BrowserTab) void {
        self.class_tree.deinit();
        self.instance_method_list.deinit();
        self.class_method_list.deinit();
        self.source_editor.deinit();
    }

    pub fn addClass(self: *BrowserTab, name: []const u8, parent_name: ?[]const u8) !void {
        const node = try TreeNode.init(self.allocator, name, null);

        if (parent_name) |pname| {
            // Find parent node
            for (self.class_tree.roots.items) |root| {
                if (self.findNode(root, pname)) |parent| {
                    try parent.addChild(node);
                    self.class_tree.rebuildFlatList();
                    return;
                }
            }
        }

        // Add as root
        try self.class_tree.addRoot(node);
    }

    fn findNode(_: *BrowserTab, node: *TreeNode, name: []const u8) ?*TreeNode {
        if (std.mem.eql(u8, node.text, name)) {
            return node;
        }

        for (node.children.items) |child| {
            if (findNodeRecursive(child, name)) |found| {
                return found;
            }
        }

        return null;
    }

    fn findNodeRecursive(node: *TreeNode, name: []const u8) ?*TreeNode {
        if (std.mem.eql(u8, node.text, name)) {
            return node;
        }

        for (node.children.items) |child| {
            if (findNodeRecursive(child, name)) |found| {
                return found;
            }
        }

        return null;
    }

    pub fn setInstanceMethods(self: *BrowserTab, methods: []const []const u8) !void {
        self.instance_method_list.clear();
        for (methods) |method| {
            try self.instance_method_list.addItem(method, null);
        }
    }

    pub fn setClassMethods(self: *BrowserTab, methods: []const []const u8) !void {
        self.class_method_list.clear();
        for (methods) |method| {
            try self.class_method_list.addItem(method, null);
        }
    }

    // Legacy compatibility
    pub fn setMethods(self: *BrowserTab, methods: []const []const u8) !void {
        try self.setInstanceMethods(methods);
    }

    pub fn setSource(self: *BrowserTab, source: []const u8) !void {
        try self.source_editor.setText(source);
    }

    pub fn updateRect(self: *BrowserTab, rect: Rect) void {
        self.rect = rect;

        // Recalculate pane sizes
        const pane_width = rect.width / 4;

        self.class_tree.state.rect = Rect.init(rect.x, rect.y, pane_width, rect.height);
        self.instance_method_list.state.rect = Rect.init(rect.x + pane_width, rect.y, pane_width, rect.height);
        self.class_method_list.state.rect = Rect.init(rect.x + pane_width * 2, rect.y, pane_width, rect.height);

        const source_rect = Rect.init(rect.x + pane_width * 3, rect.y, rect.width - pane_width * 3, rect.height);
        self.source_editor.state.rect = Rect.init(source_rect.x + 1, source_rect.y + 1, source_rect.width -| 2, source_rect.height -| 2);
    }

    fn nextPane(self: *BrowserTab) void {
        self.active_pane = switch (self.active_pane) {
            .classes => .instance_methods,
            .instance_methods => .class_methods,
            .class_methods => .source,
            .source => .classes,
        };
        self.updateFocus();
    }

    fn prevPane(self: *BrowserTab) void {
        self.active_pane = switch (self.active_pane) {
            .classes => .source,
            .instance_methods => .classes,
            .class_methods => .instance_methods,
            .source => .class_methods,
        };
        self.updateFocus();
    }

    fn updateFocus(self: *BrowserTab) void {
        self.class_tree.state.focused = self.focused and self.active_pane == .classes;
        self.instance_method_list.state.focused = self.focused and self.active_pane == .instance_methods;
        self.class_method_list.state.focused = self.focused and self.active_pane == .class_methods;
        self.source_editor.state.focused = self.focused and self.active_pane == .source;
    }

    pub fn handleKey(self: *BrowserTab, key: Key) EventResult {
        switch (key) {
            .tab => {
                self.nextPane();
                return .consumed;
            },
            .shift_tab => {
                self.prevPane();
                return .consumed;
            },
            .right => {
                // Right arrow moves to next pane (except in source where it's text navigation)
                if (self.active_pane != .source) {
                    self.nextPane();
                    return .consumed;
                }
            },
            .left => {
                // Left arrow moves to previous pane (except in source where it's text navigation)
                if (self.active_pane != .source) {
                    self.prevPane();
                    return .consumed;
                }
            },
            .ctrl => |c| {
                switch (c) {
                    19 => { // Ctrl+S - Save method
                        if (self.active_pane == .source and !self.source_editor.readonly) {
                            if (self.on_save_method) |cb| {
                                const source = self.source_editor.getText(self.allocator) catch return .ignored;
                                defer self.allocator.free(source);
                                cb(self, self.selected_class_name, self.selected_method_name, source);
                            }
                        }
                        return .consumed;
                    },
                    else => {},
                }
            },
            else => {},
        }

        // Pass to active pane
        return switch (self.active_pane) {
            .classes => self.handleClassKey(key),
            .instance_methods => self.handleInstanceMethodKey(key),
            .class_methods => self.handleClassMethodKey(key),
            .source => self.source_editor.handleKey(key),
        };
    }

    fn handleClassKey(self: *BrowserTab, key: Key) EventResult {
        const result = self.class_tree.handleKey(key);

        // Check if selection changed
        if (result == .consumed) {
            if (self.class_tree.getSelectedNode()) |node| {
                if (!std.mem.eql(u8, self.selected_class_name, node.text)) {
                    self.selected_class_name = node.text;
                    self.selected_method_name = "";
                    if (self.on_select_class) |cb| {
                        cb(self, node.text);
                    }
                }
            }
        }

        return result;
    }

    fn handleInstanceMethodKey(self: *BrowserTab, key: Key) EventResult {
        const result = self.instance_method_list.handleKey(key);

        // Check if selection changed
        if (result == .consumed) {
            if (self.instance_method_list.getSelectedItem()) |item| {
                if (!std.mem.eql(u8, self.selected_method_name, item.text)) {
                    self.selected_method_name = item.text;
                    self.show_class_side = false;
                    if (self.on_select_method) |cb| {
                        cb(self, self.selected_class_name, item.text);
                    }
                }
            }
        }

        return result;
    }

    fn handleClassMethodKey(self: *BrowserTab, key: Key) EventResult {
        const result = self.class_method_list.handleKey(key);

        // Check if selection changed
        if (result == .consumed) {
            if (self.class_method_list.getSelectedItem()) |item| {
                if (!std.mem.eql(u8, self.selected_method_name, item.text)) {
                    self.selected_method_name = item.text;
                    self.show_class_side = true;
                    if (self.on_select_method) |cb| {
                        cb(self, self.selected_class_name, item.text);
                    }
                }
            }
        }

        return result;
    }

    pub fn handleMouse(self: *BrowserTab, mouse: MouseEvent) void {
        const pane_width = self.rect.width / 4;

        // Calculate pane rects
        const class_rect = Rect.init(self.rect.x, self.rect.y, pane_width, self.rect.height);
        const inst_method_rect = Rect.init(self.rect.x + pane_width, self.rect.y, pane_width, self.rect.height);
        const class_method_rect = Rect.init(self.rect.x + pane_width * 2, self.rect.y, pane_width, self.rect.height);
        const source_rect = Rect.init(self.rect.x + pane_width * 3, self.rect.y, self.rect.width - pane_width * 3, self.rect.height);

        // Handle scroll wheel in active pane
        if (mouse.button == .scroll_up or mouse.button == .scroll_down) {
            const delta: i32 = if (mouse.button == .scroll_up) -3 else 3;

            // Determine which pane to scroll based on mouse position
            if (class_rect.contains(mouse.x, mouse.y)) {
                self.scrollClassTree(delta);
            } else if (inst_method_rect.contains(mouse.x, mouse.y)) {
                self.scrollInstanceMethods(delta);
            } else if (class_method_rect.contains(mouse.x, mouse.y)) {
                self.scrollClassMethods(delta);
            } else if (source_rect.contains(mouse.x, mouse.y)) {
                self.scrollSource(delta);
            }
            return;
        }

        // Handle left click
        if (mouse.event_type == .press and mouse.button == .left) {
            // Determine which pane was clicked and switch to it
            if (class_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .classes;
                self.updateFocus();

                // Calculate clicked item in tree (accounting for border)
                const content_y = mouse.y -| class_rect.y -| 1;
                const clicked_idx = self.class_tree.scroll_offset + content_y;

                if (clicked_idx < self.class_tree.flat_list.items.len) {
                    self.class_tree.selected_index = clicked_idx;

                    // Update selected class
                    if (self.class_tree.getSelectedNode()) |node| {
                        if (!std.mem.eql(u8, self.selected_class_name, node.text)) {
                            self.selected_class_name = node.text;
                            self.selected_method_name = "";
                        }
                    }
                }
            } else if (inst_method_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .instance_methods;
                self.updateFocus();

                // Calculate clicked item
                const content_y = mouse.y -| inst_method_rect.y -| 1;
                const clicked_idx = self.instance_method_list.scroll_offset + content_y;

                if (clicked_idx < self.instance_method_list.items.items.len) {
                    self.instance_method_list.selectIndex(clicked_idx);

                    // Update selected method
                    if (self.instance_method_list.getSelectedItem()) |item| {
                        self.selected_method_name = item.text;
                        self.show_class_side = false;
                    }
                }
            } else if (class_method_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .class_methods;
                self.updateFocus();

                // Calculate clicked item
                const content_y = mouse.y -| class_method_rect.y -| 1;
                const clicked_idx = self.class_method_list.scroll_offset + content_y;

                if (clicked_idx < self.class_method_list.items.items.len) {
                    self.class_method_list.selectIndex(clicked_idx);

                    // Update selected method
                    if (self.class_method_list.getSelectedItem()) |item| {
                        self.selected_method_name = item.text;
                        self.show_class_side = true;
                    }
                }
            } else if (source_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .source;
                self.updateFocus();

                // Position cursor in source editor
                const content = Rect.init(source_rect.x + 1, source_rect.y + 1, source_rect.width -| 2, source_rect.height -| 2);
                if (content.contains(mouse.x, mouse.y)) {
                    const line_num_width: u16 = if (self.source_editor.line_numbers) 5 else 0;
                    const rel_x = mouse.x -| content.x -| line_num_width;
                    const rel_y = mouse.y -| content.y;

                    const target_line = self.source_editor.scroll_y + rel_y;
                    const target_col = self.source_editor.scroll_x + rel_x;

                    if (target_line < self.source_editor.lines.items.len) {
                        self.source_editor.cursor_line = target_line;
                        const line_len = self.source_editor.lines.items[target_line].items.len;
                        self.source_editor.cursor_col = @min(target_col, line_len);
                    }
                }
            }
        }
    }

    pub fn scroll(self: *BrowserTab, delta: i32) void {
        // Scroll the active pane
        switch (self.active_pane) {
            .classes => self.scrollClassTree(delta),
            .instance_methods => self.scrollInstanceMethods(delta),
            .class_methods => self.scrollClassMethods(delta),
            .source => self.scrollSource(delta),
        }
    }

    fn scrollClassTree(self: *BrowserTab, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (self.class_tree.scroll_offset > amount) {
                self.class_tree.scroll_offset -= amount;
            } else {
                self.class_tree.scroll_offset = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.class_tree.flat_list.items.len -| 1;
            self.class_tree.scroll_offset = @min(self.class_tree.scroll_offset + amount, max_scroll);
        }
    }

    fn scrollInstanceMethods(self: *BrowserTab, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (self.instance_method_list.scroll_offset > amount) {
                self.instance_method_list.scroll_offset -= amount;
            } else {
                self.instance_method_list.scroll_offset = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.instance_method_list.items.items.len -| 1;
            self.instance_method_list.scroll_offset = @min(self.instance_method_list.scroll_offset + amount, max_scroll);
        }
    }

    fn scrollClassMethods(self: *BrowserTab, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (self.class_method_list.scroll_offset > amount) {
                self.class_method_list.scroll_offset -= amount;
            } else {
                self.class_method_list.scroll_offset = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.class_method_list.items.items.len -| 1;
            self.class_method_list.scroll_offset = @min(self.class_method_list.scroll_offset + amount, max_scroll);
        }
    }

    fn scrollSource(self: *BrowserTab, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (self.source_editor.scroll_y > amount) {
                self.source_editor.scroll_y -= amount;
            } else {
                self.source_editor.scroll_y = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.source_editor.lines.items.len -| 1;
            self.source_editor.scroll_y = @min(self.source_editor.scroll_y + amount, max_scroll);
        }
    }

    pub fn draw(self: *BrowserTab, screen: *Screen) void {
        // Update focus states
        self.updateFocus();

        // Calculate pane positions
        const pane_width = self.rect.width / 4;

        self.class_tree.state.rect = Rect.init(self.rect.x, self.rect.y, pane_width, self.rect.height);
        self.instance_method_list.state.rect = Rect.init(self.rect.x + pane_width, self.rect.y, pane_width, self.rect.height);
        self.class_method_list.state.rect = Rect.init(self.rect.x + pane_width * 2, self.rect.y, pane_width, self.rect.height);

        const source_rect = Rect.init(self.rect.x + pane_width * 3, self.rect.y, self.rect.width - pane_width * 3, self.rect.height);

        // Draw class tree
        self.class_tree.draw(screen);

        // Draw instance method list
        self.instance_method_list.draw(screen);

        // Draw class method list
        self.class_method_list.draw(screen);

        // Draw source pane border
        widget.drawBorderRounded(screen, source_rect, self.active_pane == .source and self.focused);

        // Build title with class/instance indicator
        const title = if (self.show_class_side) "Source (class)" else "Source (instance)";
        widget.drawTitle(screen, source_rect, title, self.active_pane == .source and self.focused);

        // Update source editor rect
        self.source_editor.state.rect = Rect.init(source_rect.x + 1, source_rect.y + 1, source_rect.width -| 2, source_rect.height -| 2);
        self.source_editor.state.focused = self.focused and self.active_pane == .source;

        // Draw source editor
        self.source_editor.draw(screen);
    }
};
