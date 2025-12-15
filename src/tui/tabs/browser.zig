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
    package_list: ListView,
    class_tree: TreeView,
    instance_method_list: ListView,
    class_method_list: ListView,
    source_editor: TextArea,

    // State
    active_pane: Pane = .packages,
    selected_package_name: []const u8 = "",
    show_class_side: bool = false,
    selected_class_name: []const u8 = "",
    selected_method_name: []const u8 = "",

    // Class definition editing - when no method selected, show class definition
    showing_class_definition: bool = false,
    class_def_dirty: bool = false,
    editing_new_method: bool = false, // True when source is ready for a new method
    has_template_text: bool = false, // True when source has template that should clear on first keystroke
    source_dirty: bool = false, // True when source has unsaved changes

    // Pending source storage - preserves edits when navigating away
    pending_source: ?[]u8 = null,
    pending_class_name: [64]u8 = [_]u8{0} ** 64,
    pending_class_name_len: usize = 0,
    pending_method_name: [128]u8 = [_]u8{0} ** 128,
    pending_method_name_len: usize = 0,
    pending_is_class_side: bool = false,

    // New class dialog state
    show_new_class_dialog: bool = false,
    dialog_class_name: [64]u8 = [_]u8{0} ** 64,
    dialog_class_name_len: usize = 0,
    dialog_superclass: [64]u8 = [_]u8{0} ** 64,
    dialog_superclass_len: usize = 0,
    dialog_inst_vars: [256]u8 = [_]u8{0} ** 256,
    dialog_inst_vars_len: usize = 0,
    dialog_field: DialogField = .class_name,

    // New package dialog state
    show_new_package_dialog: bool = false,
    dialog_package_name: [64]u8 = [_]u8{0} ** 64,
    dialog_package_name_len: usize = 0,

    // Status message
    status_message: [128]u8 = undefined,
    status_len: usize = 0,
    status_is_error: bool = false,

    // Callbacks for loading data
    on_load_classes: ?*const fn (*BrowserTab) void = null,
    on_select_package: ?*const fn (*BrowserTab, []const u8) void = null,
    on_select_class: ?*const fn (*BrowserTab, []const u8) void = null,
    on_select_method: ?*const fn (*BrowserTab, []const u8, []const u8) void = null,
    on_save_method: ?*const fn (*BrowserTab, []const u8, []const u8, []const u8) void = null,
    on_create_class: ?*const fn (*BrowserTab, []const u8, []const u8, []const u8) void = null,
    on_save_class_definition: ?*const fn (*BrowserTab, []const u8, []const u8) void = null,
    on_new_method: ?*const fn (*BrowserTab, bool) void = null, // Called when user wants to create new method (bool = class_side)
    on_save_package: ?*const fn (*BrowserTab, []const u8) void = null, // Called to save package to file
    on_create_package: ?*const fn (*BrowserTab, []const u8) void = null, // Called to create new package

    pub const DialogField = enum {
        class_name,
        superclass,
        inst_vars,
    };

    pub const Pane = enum {
        packages,
        classes,
        instance_methods,
        class_methods,
        source,
    };

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !BrowserTab {
        // Layout: top half has 4 panes, bottom half is source
        const top_height = rect.height / 2;
        const pane_width = rect.width / 4;

        const pkg_rect = Rect.init(rect.x, rect.y, pane_width, top_height);
        const class_rect = Rect.init(rect.x + pane_width, rect.y, pane_width, top_height);
        const inst_method_rect = Rect.init(rect.x + pane_width * 2, rect.y, pane_width, top_height);
        const class_method_rect = Rect.init(rect.x + pane_width * 3, rect.y, rect.width - pane_width * 3, top_height);
        const source_rect = Rect.init(rect.x, rect.y + top_height, rect.width, rect.height - top_height);

        var package_list = try ListView.init(allocator, pkg_rect);
        package_list.state.title = "Packages";

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

        var tab = BrowserTab{
            .allocator = allocator,
            .rect = rect,
            .package_list = package_list,
            .class_tree = class_tree,
            .instance_method_list = instance_method_list,
            .class_method_list = class_method_list,
            .source_editor = source_editor,
        };

        // Initialize dialog superclass default
        const default_super = "Object";
        @memcpy(tab.dialog_superclass[0..default_super.len], default_super);
        tab.dialog_superclass_len = default_super.len;

        return tab;
    }

    pub fn deinit(self: *BrowserTab) void {
        self.package_list.deinit();
        self.class_tree.deinit();
        self.instance_method_list.deinit();
        self.class_method_list.deinit();
        self.source_editor.deinit();
        if (self.pending_source) |ps| {
            self.allocator.free(ps);
        }
    }

    /// Save current source to pending storage (call before navigating away)
    pub fn savePendingSource(self: *BrowserTab) void {
        // Save if dirty OR if we have template text that was never modified
        if (!self.source_dirty and !self.editing_new_method) return;

        // Don't save template text
        if (self.has_template_text) return;

        // Free old pending source
        if (self.pending_source) |ps| {
            self.allocator.free(ps);
            self.pending_source = null;
        }

        // Save current source
        const source = self.source_editor.getText(self.allocator) catch return;

        // Don't save empty source
        if (source.len == 0) {
            self.allocator.free(source);
            return;
        }

        self.pending_source = source;

        // Save context - for new methods, save the class and side
        const class_len = @min(self.selected_class_name.len, self.pending_class_name.len);
        @memcpy(self.pending_class_name[0..class_len], self.selected_class_name[0..class_len]);
        self.pending_class_name_len = class_len;

        // For new methods, method name will be empty - that's fine
        const method_len = @min(self.selected_method_name.len, self.pending_method_name.len);
        @memcpy(self.pending_method_name[0..method_len], self.selected_method_name[0..method_len]);
        self.pending_method_name_len = method_len;

        self.pending_is_class_side = self.show_class_side;
    }

    /// Check if there's pending source for this context and restore it
    pub fn restorePendingSource(self: *BrowserTab) bool {
        if (self.pending_source == null) return false;

        // Check if context matches
        const class_matches = std.mem.eql(u8, self.selected_class_name, self.pending_class_name[0..self.pending_class_name_len]);
        const side_matches = self.show_class_side == self.pending_is_class_side;

        // For method matching: either both are same method, or both are new methods (empty)
        const pending_is_new = self.pending_method_name_len == 0;
        const current_is_new = self.selected_method_name.len == 0 or self.editing_new_method;
        const method_matches = if (pending_is_new and current_is_new)
            true // Both are new methods
        else if (!pending_is_new and !current_is_new)
            std.mem.eql(u8, self.selected_method_name, self.pending_method_name[0..self.pending_method_name_len])
        else
            false; // One is new, one isn't

        if (class_matches and method_matches and side_matches) {
            // Restore source
            self.source_editor.setText(self.pending_source.?) catch return false;
            self.source_dirty = true;
            self.has_template_text = false;
            return true;
        }

        return false;
    }

    /// Clear pending source after successful save
    pub fn clearPendingSource(self: *BrowserTab) void {
        if (self.pending_source) |ps| {
            self.allocator.free(ps);
            self.pending_source = null;
        }
        self.pending_class_name_len = 0;
        self.pending_method_name_len = 0;
        self.source_dirty = false;
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

    pub fn setPackages(self: *BrowserTab, packages: []const []const u8) !void {
        self.package_list.clear();
        for (packages) |pkg| {
            try self.package_list.addItem(pkg, null);
        }
    }

    pub fn addPackage(self: *BrowserTab, name: []const u8) !void {
        try self.package_list.addItem(name, null);
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
        self.showing_class_definition = false;
        self.class_def_dirty = false;
        self.source_dirty = false;
        self.has_template_text = false;
    }

    pub fn setClassDefinition(self: *BrowserTab, definition: []const u8) !void {
        try self.source_editor.setText(definition);
        self.showing_class_definition = true;
        self.class_def_dirty = false;
        self.source_dirty = false;
        self.has_template_text = false;
        self.source_editor.readonly = false; // Class definition is editable
    }

    pub fn setStatus(self: *BrowserTab, msg: []const u8, is_error: bool) void {
        const copy_len = @min(msg.len, self.status_message.len);
        @memcpy(self.status_message[0..copy_len], msg[0..copy_len]);
        self.status_len = copy_len;
        self.status_is_error = is_error;
    }

    pub fn updateRect(self: *BrowserTab, rect: Rect) void {
        self.rect = rect;
        // Rect updates happen in draw()
    }

    fn nextPane(self: *BrowserTab) void {
        const old_pane = self.active_pane;
        self.active_pane = switch (self.active_pane) {
            .packages => .classes,
            .classes => .instance_methods,
            .instance_methods => .class_methods,
            .class_methods => .source,
            .source => .packages,
        };
        self.updateFocus();

        // If entering methods pane from classes, prepare for new method
        if (old_pane == .classes and self.active_pane == .instance_methods) {
            self.enterMethodPane(false);
        } else if (old_pane == .instance_methods and self.active_pane == .class_methods) {
            self.enterMethodPane(true);
        }
    }

    fn prevPane(self: *BrowserTab) void {
        const old_pane = self.active_pane;
        self.active_pane = switch (self.active_pane) {
            .packages => .source,
            .classes => .packages,
            .instance_methods => .classes,
            .class_methods => .instance_methods,
            .source => .class_methods,
        };
        self.updateFocus();

        // If entering methods pane, prepare for new method
        if (old_pane == .source and self.active_pane == .class_methods) {
            self.enterMethodPane(true);
        } else if (old_pane == .class_methods and self.active_pane == .instance_methods) {
            self.enterMethodPane(false);
        }
    }

    fn enterMethodPane(self: *BrowserTab, class_side: bool) void {
        // When entering a method pane, always set up for new method editing
        // (user can navigate down to select existing methods)

        // Save current source if dirty BEFORE changing state flags
        // But don't save class definitions as method source
        if (!self.showing_class_definition) {
            self.savePendingSource();
        }

        // Now change state
        self.selected_method_name = "";
        self.show_class_side = class_side;
        self.editing_new_method = true;
        self.showing_class_definition = false;

        // Try to restore pending source for this context (new method on this class/side)
        if (self.restorePendingSource()) {
            self.source_editor.readonly = false;
            // Call callback for any additional work
            if (self.on_new_method) |cb| {
                cb(self, class_side);
            }
            return;
        }

        // No pending source - set up template
        if (self.selected_class_name.len > 0) {
            const template = "methodName\n\t\"Method comment\"\n\t^ self";
            self.source_editor.setText(template) catch {};
            self.source_editor.readonly = false;
            self.has_template_text = true;
            self.source_editor.cursor_line = 0;
            self.source_editor.cursor_col = 0;
        }

        // Call callback for any additional work
        if (self.on_new_method) |cb| {
            cb(self, class_side);
        }
    }

    fn prepareNewMethod(self: *BrowserTab, class_side: bool) void {
        // Only prepare for new method if no methods or no selection
        const list = if (class_side) &self.class_method_list else &self.instance_method_list;
        if (list.items.items.len == 0 or list.selected_index == null) {
            self.enterMethodPane(class_side);
        }
    }

    pub fn updateFocus(self: *BrowserTab) void {
        self.package_list.state.focused = self.focused and self.active_pane == .packages and !self.show_new_class_dialog;
        self.class_tree.state.focused = self.focused and self.active_pane == .classes and !self.show_new_class_dialog;
        self.instance_method_list.state.focused = self.focused and self.active_pane == .instance_methods and !self.show_new_class_dialog;
        self.class_method_list.state.focused = self.focused and self.active_pane == .class_methods and !self.show_new_class_dialog;
        self.source_editor.state.focused = self.focused and self.active_pane == .source and !self.show_new_class_dialog;
    }

    pub fn handleKey(self: *BrowserTab, key: Key) EventResult {
        // Handle new package dialog first
        if (self.show_new_package_dialog) {
            return self.handlePackageDialogKey(key);
        }

        // Handle new class dialog
        if (self.show_new_class_dialog) {
            return self.handleDialogKey(key);
        }

        switch (key) {
            .tab => {
                // In source pane, Tab inserts a tab character; use Shift+Tab to navigate
                if (self.active_pane == .source) {
                    // Let source editor handle the tab
                    return self.handleSourceKey(key);
                }
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
                    1 => { // Ctrl+A - New class dialog
                        self.openNewClassDialog();
                        return .consumed;
                    },
                    5 => { // Ctrl+E - Export/save package
                        if (self.selected_package_name.len > 0) {
                            if (self.on_save_package) |cb| {
                                cb(self, self.selected_package_name);
                            }
                        }
                        return .consumed;
                    },
                    19 => { // Ctrl+S - Save
                        self.save();
                        return .consumed;
                    },
                    else => {},
                }
            },
            else => {},
        }

        // Pass to active pane
        return switch (self.active_pane) {
            .packages => self.handlePackageKey(key),
            .classes => self.handleClassKey(key),
            .instance_methods => self.handleInstanceMethodKey(key),
            .class_methods => self.handleClassMethodKey(key),
            .source => self.handleSourceKey(key),
        };
    }

    fn openNewClassDialog(self: *BrowserTab) void {
        self.show_new_class_dialog = true;
        self.dialog_class_name_len = 0;
        // Keep superclass as "Object" by default
        const default_super = "Object";
        @memcpy(self.dialog_superclass[0..default_super.len], default_super);
        self.dialog_superclass_len = default_super.len;
        self.dialog_inst_vars_len = 0;
        self.dialog_field = .class_name;
    }

    fn handleDialogKey(self: *BrowserTab, key: Key) EventResult {
        switch (key) {
            .escape => {
                self.show_new_class_dialog = false;
                return .consumed;
            },
            .enter => {
                // Submit dialog - create the class
                if (self.dialog_class_name_len > 0) {
                    self.createClassFromDialog();
                    self.show_new_class_dialog = false;
                }
                return .consumed;
            },
            .tab => {
                // Switch field
                self.dialog_field = switch (self.dialog_field) {
                    .class_name => .superclass,
                    .superclass => .inst_vars,
                    .inst_vars => .class_name,
                };
                return .consumed;
            },
            .shift_tab => {
                // Switch field backwards
                self.dialog_field = switch (self.dialog_field) {
                    .class_name => .inst_vars,
                    .superclass => .class_name,
                    .inst_vars => .superclass,
                };
                return .consumed;
            },
            .backspace => {
                // Delete character
                switch (self.dialog_field) {
                    .class_name => {
                        if (self.dialog_class_name_len > 0) self.dialog_class_name_len -= 1;
                    },
                    .superclass => {
                        if (self.dialog_superclass_len > 0) self.dialog_superclass_len -= 1;
                    },
                    .inst_vars => {
                        if (self.dialog_inst_vars_len > 0) self.dialog_inst_vars_len -= 1;
                    },
                }
                return .consumed;
            },
            .char => |c| {
                // Add character to current field
                if (c < 128) {
                    const char: u8 = @intCast(c);
                    switch (self.dialog_field) {
                        .class_name => {
                            if (self.dialog_class_name_len < self.dialog_class_name.len - 1) {
                                self.dialog_class_name[self.dialog_class_name_len] = char;
                                self.dialog_class_name_len += 1;
                            }
                        },
                        .superclass => {
                            if (self.dialog_superclass_len < self.dialog_superclass.len - 1) {
                                self.dialog_superclass[self.dialog_superclass_len] = char;
                                self.dialog_superclass_len += 1;
                            }
                        },
                        .inst_vars => {
                            if (self.dialog_inst_vars_len < self.dialog_inst_vars.len - 1) {
                                self.dialog_inst_vars[self.dialog_inst_vars_len] = char;
                                self.dialog_inst_vars_len += 1;
                            }
                        },
                    }
                }
                return .consumed;
            },
            else => return .consumed,
        }
    }

    fn createClassFromDialog(self: *BrowserTab) void {
        if (self.on_create_class) |cb| {
            cb(
                self,
                self.dialog_class_name[0..self.dialog_class_name_len],
                self.dialog_superclass[0..self.dialog_superclass_len],
                self.dialog_inst_vars[0..self.dialog_inst_vars_len],
            );
        }
    }

    pub fn showNewPackageDialog(self: *BrowserTab) void {
        self.show_new_package_dialog = true;
        self.dialog_package_name_len = 0;
    }

    fn handlePackageDialogKey(self: *BrowserTab, key: Key) EventResult {
        switch (key) {
            .escape => {
                self.show_new_package_dialog = false;
                return .consumed;
            },
            .enter => {
                // Submit dialog - create the package
                if (self.dialog_package_name_len > 0) {
                    self.createPackageFromDialog();
                    self.show_new_package_dialog = false;
                }
                return .consumed;
            },
            .backspace => {
                if (self.dialog_package_name_len > 0) self.dialog_package_name_len -= 1;
                return .consumed;
            },
            .char => |c| {
                // Add character to package name (only alphanumeric)
                if (c < 128) {
                    const char: u8 = @intCast(c);
                    if ((char >= 'A' and char <= 'Z') or
                        (char >= 'a' and char <= 'z') or
                        (char >= '0' and char <= '9') or
                        char == '_' or char == '-')
                    {
                        if (self.dialog_package_name_len < self.dialog_package_name.len - 1) {
                            self.dialog_package_name[self.dialog_package_name_len] = char;
                            self.dialog_package_name_len += 1;
                        }
                    }
                }
                return .consumed;
            },
            else => return .consumed,
        }
    }

    fn createPackageFromDialog(self: *BrowserTab) void {
        if (self.on_create_package) |cb| {
            cb(self, self.dialog_package_name[0..self.dialog_package_name_len]);
        }
        // Add to browser's package list
        self.addPackage(self.dialog_package_name[0..self.dialog_package_name_len]) catch {};
    }

    fn save(self: *BrowserTab) void {
        if (self.showing_class_definition) {
            // Save class definition
            if (self.selected_class_name.len > 0 and self.on_save_class_definition != null) {
                const source = self.source_editor.getText(self.allocator) catch return;
                defer self.allocator.free(source);
                self.on_save_class_definition.?(self, self.selected_class_name, source);
                self.class_def_dirty = false;
                self.source_dirty = false;
            }
        } else {
            // Save method - can save from any pane, not just source
            if (!self.source_editor.readonly) {
                if (self.on_save_method) |cb| {
                    const source = self.source_editor.getText(self.allocator) catch return;
                    defer self.allocator.free(source);
                    cb(self, self.selected_class_name, self.selected_method_name, source);
                    // Clear dirty state and pending source on save attempt
                    self.clearPendingSource();
                }
            }
        }
    }

    fn handleSourceKey(self: *BrowserTab, key: Key) EventResult {
        // Check for up arrow at top of source - navigate to upper panes
        if (key == .up and self.source_editor.cursor_line == 0) {
            // Move to instance methods pane (or class methods if showing class side)
            if (self.show_class_side) {
                self.active_pane = .class_methods;
            } else {
                self.active_pane = .instance_methods;
            }
            self.updateFocus();
            return .consumed;
        }

        // Clear template text on first character typed
        if (self.has_template_text) {
            switch (key) {
                .char => {
                    // User is typing - clear the template and insert their character
                    self.source_editor.setText("") catch {};
                    self.source_editor.cursor_line = 0;
                    self.source_editor.cursor_col = 0;
                    self.has_template_text = false;
                },
                .backspace, .delete => {
                    // Clearing keys - just clear template
                    self.source_editor.setText("") catch {};
                    self.source_editor.cursor_line = 0;
                    self.source_editor.cursor_col = 0;
                    self.has_template_text = false;
                    return .consumed;
                },
                else => {},
            }
        }

        const result = self.source_editor.handleKey(key);

        // Track if source was modified
        if (result == .consumed) {
            // Check if this was a modifying key (not just navigation)
            const is_modifying = switch (key) {
                .char, .backspace, .delete, .enter, .tab => true,
                else => false,
            };
            if (is_modifying) {
                self.source_dirty = true;
                if (self.showing_class_definition) {
                    self.class_def_dirty = true;
                }
            }
        }

        return result;
    }

    fn handlePackageKey(self: *BrowserTab, key: Key) EventResult {
        const result = self.package_list.handleKey(key);

        // Check if selection changed
        if (result == .consumed) {
            if (self.package_list.getSelectedItem()) |item| {
                if (!std.mem.eql(u8, self.selected_package_name, item.text)) {
                    self.selected_package_name = item.text;
                    if (self.on_select_package) |cb| {
                        cb(self, item.text);
                    }
                }
            }
        }

        return result;
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
        // If list is empty or no selection, Enter starts new method
        if (key == .enter and self.instance_method_list.items.items.len == 0) {
            self.selected_method_name = "";
            self.show_class_side = false;
            self.editing_new_method = true;
            self.showing_class_definition = false;
            if (self.on_new_method) |cb| {
                cb(self, false);
            }
            return .consumed;
        }

        // Down arrow at bottom of list (or empty list) goes to source editor
        if (key == .down) {
            const items_len = self.instance_method_list.items.items.len;
            const at_bottom = if (self.instance_method_list.selected_index) |idx|
                idx >= items_len -| 1
            else
                true; // No selection = treat as at bottom

            if (at_bottom or items_len == 0) {
                self.active_pane = .source;
                self.updateFocus();
                return .consumed;
            }
        }

        const result = self.instance_method_list.handleKey(key);

        // Check if selection changed
        if (result == .consumed) {
            if (self.instance_method_list.getSelectedItem()) |item| {
                if (!std.mem.eql(u8, self.selected_method_name, item.text)) {
                    self.selected_method_name = item.text;
                    self.show_class_side = false;
                    self.editing_new_method = false;
                    self.showing_class_definition = false;
                    if (self.on_select_method) |cb| {
                        cb(self, self.selected_class_name, item.text);
                    }
                }
            }
        }

        return result;
    }

    fn handleClassMethodKey(self: *BrowserTab, key: Key) EventResult {
        // If list is empty or no selection, Enter starts new method
        if (key == .enter and self.class_method_list.items.items.len == 0) {
            self.selected_method_name = "";
            self.show_class_side = true;
            self.editing_new_method = true;
            self.showing_class_definition = false;
            if (self.on_new_method) |cb| {
                cb(self, true);
            }
            return .consumed;
        }

        // Down arrow at bottom of list (or empty list) goes to source editor
        if (key == .down) {
            const items_len = self.class_method_list.items.items.len;
            const at_bottom = if (self.class_method_list.selected_index) |idx|
                idx >= items_len -| 1
            else
                true; // No selection = treat as at bottom

            if (at_bottom or items_len == 0) {
                self.active_pane = .source;
                self.updateFocus();
                return .consumed;
            }
        }

        const result = self.class_method_list.handleKey(key);

        // Check if selection changed
        if (result == .consumed) {
            if (self.class_method_list.getSelectedItem()) |item| {
                if (!std.mem.eql(u8, self.selected_method_name, item.text)) {
                    self.selected_method_name = item.text;
                    self.show_class_side = true;
                    self.editing_new_method = false;
                    self.showing_class_definition = false;
                    if (self.on_select_method) |cb| {
                        cb(self, self.selected_class_name, item.text);
                    }
                }
            }
        }

        return result;
    }

    // Helper to calculate layout rects
    fn calcLayout(self: *BrowserTab) struct {
        pkg_rect: Rect,
        class_rect: Rect,
        inst_rect: Rect,
        class_method_rect: Rect,
        source_rect: Rect,
    } {
        const top_height = self.rect.height / 2;
        const bottom_height = self.rect.height - top_height;
        const pane_width = self.rect.width / 4;

        return .{
            .pkg_rect = Rect.init(self.rect.x, self.rect.y, pane_width, top_height),
            .class_rect = Rect.init(self.rect.x + pane_width, self.rect.y, pane_width, top_height),
            .inst_rect = Rect.init(self.rect.x + pane_width * 2, self.rect.y, pane_width, top_height),
            .class_method_rect = Rect.init(self.rect.x + pane_width * 3, self.rect.y, self.rect.width - pane_width * 3, top_height),
            .source_rect = Rect.init(self.rect.x, self.rect.y + top_height, self.rect.width, bottom_height),
        };
    }

    pub fn handleMouse(self: *BrowserTab, mouse: MouseEvent) void {
        // Handle dialog mouse clicks
        if (self.show_new_class_dialog) {
            if (mouse.event_type == .press and mouse.button == .left) {
                const dialog_width: u16 = 60;
                const dialog_x = (self.rect.width -| dialog_width) / 2 + self.rect.x;
                const dialog_y = (self.rect.height -| 12) / 2 + self.rect.y;

                // Check which field was clicked
                if (mouse.y == dialog_y + 2 and mouse.x >= dialog_x + 12 and mouse.x < dialog_x + dialog_width - 4) {
                    self.dialog_field = .class_name;
                } else if (mouse.y == dialog_y + 4 and mouse.x >= dialog_x + 12 and mouse.x < dialog_x + dialog_width - 4) {
                    self.dialog_field = .superclass;
                } else if (mouse.y == dialog_y + 6 and mouse.x >= dialog_x + 12 and mouse.x < dialog_x + dialog_width - 4) {
                    self.dialog_field = .inst_vars;
                }
            }
            return;
        }

        const layout = self.calcLayout();

        // Handle scroll wheel in active pane
        if (mouse.button == .scroll_up or mouse.button == .scroll_down) {
            const delta: i32 = if (mouse.button == .scroll_up) -3 else 3;

            // Determine which pane to scroll based on mouse position
            if (layout.pkg_rect.contains(mouse.x, mouse.y)) {
                self.scrollPackages(delta);
            } else if (layout.class_rect.contains(mouse.x, mouse.y)) {
                self.scrollClassTree(delta);
            } else if (layout.inst_rect.contains(mouse.x, mouse.y)) {
                self.scrollInstanceMethods(delta);
            } else if (layout.class_method_rect.contains(mouse.x, mouse.y)) {
                self.scrollClassMethods(delta);
            } else if (layout.source_rect.contains(mouse.x, mouse.y)) {
                self.scrollSource(delta);
            }
            return;
        }

        // Handle left click
        if (mouse.event_type == .press and mouse.button == .left) {
            // Clear source editor mouse selection state when clicking outside source
            if (!layout.source_rect.contains(mouse.x, mouse.y)) {
                self.source_editor.mouse_selecting = false;
            }

            // Determine which pane was clicked and switch to it
            if (layout.pkg_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .packages;
                self.updateFocus();

                // Calculate clicked item
                const content_y = mouse.y -| layout.pkg_rect.y -| 1;
                const clicked_idx = self.package_list.scroll_offset + content_y;

                if (clicked_idx < self.package_list.items.items.len) {
                    self.package_list.selectIndex(clicked_idx);

                    // Update selected package
                    if (self.package_list.getSelectedItem()) |item| {
                        self.selected_package_name = item.text;
                        if (self.on_select_package) |cb| {
                            cb(self, item.text);
                        }
                    }
                }
            } else if (layout.class_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .classes;
                self.updateFocus();

                // Use TreeView's handleMouse to handle clicks (including expansion arrows)
                const result = self.class_tree.handleMouse(mouse.x, mouse.y);

                if (result.clicked) {
                    // Update selected class
                    if (self.class_tree.getSelectedNode()) |node| {
                        if (!std.mem.eql(u8, self.selected_class_name, node.text)) {
                            self.selected_class_name = node.text;
                            self.selected_method_name = "";
                            // Trigger callback to load methods for newly selected class
                            if (self.on_select_class) |cb| {
                                cb(self, node.text);
                            }
                        }
                    }
                }
            } else if (layout.inst_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .instance_methods;
                self.updateFocus();

                // Calculate clicked item
                const content_y = mouse.y -| layout.inst_rect.y -| 1;
                const clicked_idx = self.instance_method_list.scroll_offset + content_y;

                if (clicked_idx < self.instance_method_list.items.items.len) {
                    self.instance_method_list.selectIndex(clicked_idx);

                    // Update selected method
                    if (self.instance_method_list.getSelectedItem()) |item| {
                        self.selected_method_name = item.text;
                        self.show_class_side = false;
                        self.editing_new_method = false;
                    }
                } else {
                    // Clicked in empty space - prepare for new instance method
                    self.selected_method_name = "";
                    self.show_class_side = false;
                    self.editing_new_method = true;
                    self.showing_class_definition = false;
                    if (self.on_new_method) |cb| {
                        cb(self, false); // false = instance side
                    }
                }
            } else if (layout.class_method_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .class_methods;
                self.updateFocus();

                // Calculate clicked item
                const content_y = mouse.y -| layout.class_method_rect.y -| 1;
                const clicked_idx = self.class_method_list.scroll_offset + content_y;

                if (clicked_idx < self.class_method_list.items.items.len) {
                    self.class_method_list.selectIndex(clicked_idx);

                    // Update selected method
                    if (self.class_method_list.getSelectedItem()) |item| {
                        self.selected_method_name = item.text;
                        self.show_class_side = true;
                        self.editing_new_method = false;
                    }
                } else {
                    // Clicked in empty space - prepare for new class method
                    self.selected_method_name = "";
                    self.show_class_side = true;
                    self.editing_new_method = true;
                    self.showing_class_definition = false;
                    if (self.on_new_method) |cb| {
                        cb(self, true); // true = class side
                    }
                }
            } else if (layout.source_rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .source;
                self.updateFocus();

                // Delegate to source editor for full mouse handling (including selection)
                _ = self.source_editor.handleMouse(mouse);
            }
        }

        // Handle drag/release events for ongoing source editor selection
        if (mouse.event_type == .drag or mouse.event_type == .release) {
            if (self.source_editor.mouse_selecting) {
                _ = self.source_editor.handleMouse(mouse);
            }
        }
    }

    pub fn scroll(self: *BrowserTab, delta: i32) void {
        // Scroll the active pane
        switch (self.active_pane) {
            .packages => self.scrollPackages(delta),
            .classes => self.scrollClassTree(delta),
            .instance_methods => self.scrollInstanceMethods(delta),
            .class_methods => self.scrollClassMethods(delta),
            .source => self.scrollSource(delta),
        }
    }

    fn scrollPackages(self: *BrowserTab, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (self.package_list.scroll_offset > amount) {
                self.package_list.scroll_offset -= amount;
            } else {
                self.package_list.scroll_offset = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.package_list.items.items.len -| 1;
            self.package_list.scroll_offset = @min(self.package_list.scroll_offset + amount, max_scroll);
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

        // Calculate layout
        const layout = self.calcLayout();

        // Update widget rects
        self.package_list.state.rect = layout.pkg_rect;
        self.class_tree.state.rect = layout.class_rect;
        self.instance_method_list.state.rect = layout.inst_rect;
        self.class_method_list.state.rect = layout.class_method_rect;

        // Draw top panes
        self.package_list.draw(screen);
        self.class_tree.draw(screen);
        self.instance_method_list.draw(screen);
        self.class_method_list.draw(screen);

        // Draw source pane border
        widget.drawBorderRounded(screen, layout.source_rect, self.active_pane == .source and self.focused);

        // Build title with class/method info and dirty indicator
        var title_buf: [80]u8 = undefined;
        const title = if (self.showing_class_definition) blk: {
            const dirty_marker = if (self.class_def_dirty or self.source_dirty) " *" else "";
            break :blk std.fmt.bufPrint(&title_buf, "{s} [Definition]{s}", .{
                self.selected_class_name,
                dirty_marker,
            }) catch "Class Definition";
        } else if (self.selected_method_name.len > 0) blk: {
            const side = if (self.show_class_side) " class" else "";
            const dirty_marker = if (self.source_dirty) " *" else "";
            break :blk std.fmt.bufPrint(&title_buf, "{s}{s} >> {s}{s}", .{
                self.selected_class_name,
                side,
                self.selected_method_name,
                dirty_marker,
            }) catch "Source";
        } else if (self.editing_new_method) blk: {
            const side = if (self.show_class_side) " class" else "";
            const dirty_marker = if (self.source_dirty) " *" else "";
            break :blk std.fmt.bufPrint(&title_buf, "{s}{s} >> [New Method]{s}", .{
                self.selected_class_name,
                side,
                dirty_marker,
            }) catch "New Method";
        } else if (self.selected_class_name.len > 0)
            self.selected_class_name
        else
            "Source";

        widget.drawTitle(screen, layout.source_rect, title, self.active_pane == .source and self.focused);

        // Update source editor rect (inside border)
        self.source_editor.state.rect = Rect.init(
            layout.source_rect.x + 1,
            layout.source_rect.y + 1,
            layout.source_rect.width -| 2,
            layout.source_rect.height -| 2,
        );
        self.source_editor.state.focused = self.focused and self.active_pane == .source;

        // Draw source editor
        self.source_editor.draw(screen);

        // Draw status message if present
        if (self.status_len > 0) {
            const status_y = layout.source_rect.y + layout.source_rect.height -| 1;
            const status_style = if (self.status_is_error)
                Style{ .fg = style_mod.ui.error_text }
            else
                Style{ .fg = style_mod.ui.success_text };
            screen.drawText(layout.source_rect.x + 1, status_y, self.status_message[0..self.status_len], status_style);
        }

        // Draw new class dialog if visible
        if (self.show_new_class_dialog) {
            self.drawNewClassDialog(screen);
        }

        // Draw new package dialog if visible
        if (self.show_new_package_dialog) {
            self.drawNewPackageDialog(screen);
        }
    }

    fn drawNewPackageDialog(self: *BrowserTab, screen: *Screen) void {
        const dialog_width: u16 = 50;
        const dialog_height: u16 = 7;
        const dialog_x = (self.rect.width -| dialog_width) / 2 + self.rect.x;
        const dialog_y = (self.rect.height -| dialog_height) / 2 + self.rect.y;

        const dialog_rect = Rect.init(dialog_x, dialog_y, dialog_width, dialog_height);

        // Draw dialog background
        screen.fillRect(dialog_rect.x, dialog_rect.y, dialog_rect.width, dialog_rect.height, ' ', style_mod.styles.normal);
        widget.drawBorderRounded(screen, dialog_rect, true);
        widget.drawTitle(screen, dialog_rect, "New Package (Ctrl+N)", true);

        const label_style = style_mod.styles.normal;
        const field_style = Style{ .fg = style_mod.theme.text, .bg = style_mod.theme.surface1, .bold = false };

        // Package name field
        screen.drawText(dialog_x + 2, dialog_y + 2, "Name:", label_style);
        screen.fillRect(dialog_x + 8, dialog_y + 2, dialog_width - 12, 1, ' ', field_style);
        screen.drawTextClipped(dialog_x + 8, dialog_y + 2, self.dialog_package_name[0..self.dialog_package_name_len], dialog_width - 12, field_style);

        // Help text
        screen.drawText(dialog_x + 2, dialog_y + 4, "Enter: create | Esc: cancel", style_mod.styles.dim);

        // Position cursor at end of name field
        screen.cursor_visible = true;
        screen.setCursor(dialog_x + 8 + @as(u16, @intCast(self.dialog_package_name_len)), dialog_y + 2);
    }

    fn drawNewClassDialog(self: *BrowserTab, screen: *Screen) void {
        const dialog_width: u16 = 60;
        const dialog_height: u16 = 12;
        const dialog_x = (self.rect.width -| dialog_width) / 2 + self.rect.x;
        const dialog_y = (self.rect.height -| dialog_height) / 2 + self.rect.y;

        const dialog_rect = Rect.init(dialog_x, dialog_y, dialog_width, dialog_height);

        // Draw dialog background
        screen.fillRect(dialog_rect.x, dialog_rect.y, dialog_rect.width, dialog_rect.height, ' ', style_mod.styles.normal);
        widget.drawBorderRounded(screen, dialog_rect, true);
        widget.drawTitle(screen, dialog_rect, "New Class (Ctrl+A)", true);

        const label_style = style_mod.styles.normal;
        const field_style = Style{ .fg = style_mod.theme.text, .bg = style_mod.theme.surface1, .bold = false };
        const active_style = Style{ .fg = style_mod.theme.base, .bg = style_mod.theme.blue, .bold = true };

        // Class name field
        const name_style = if (self.dialog_field == .class_name) active_style else field_style;
        screen.drawText(dialog_x + 2, dialog_y + 2, "Class Name:", label_style);
        screen.fillRect(dialog_x + 14, dialog_y + 2, dialog_width - 18, 1, ' ', name_style);
        screen.drawTextClipped(dialog_x + 14, dialog_y + 2, self.dialog_class_name[0..self.dialog_class_name_len], dialog_width - 18, name_style);

        // Superclass field
        const super_style = if (self.dialog_field == .superclass) active_style else field_style;
        screen.drawText(dialog_x + 2, dialog_y + 4, "Superclass:", label_style);
        screen.fillRect(dialog_x + 14, dialog_y + 4, dialog_width - 18, 1, ' ', super_style);
        screen.drawTextClipped(dialog_x + 14, dialog_y + 4, self.dialog_superclass[0..self.dialog_superclass_len], dialog_width - 18, super_style);

        // Instance variables field
        const vars_style = if (self.dialog_field == .inst_vars) active_style else field_style;
        screen.drawText(dialog_x + 2, dialog_y + 6, "Inst Vars:", label_style);
        screen.fillRect(dialog_x + 14, dialog_y + 6, dialog_width - 18, 1, ' ', vars_style);
        screen.drawTextClipped(dialog_x + 14, dialog_y + 6, self.dialog_inst_vars[0..self.dialog_inst_vars_len], dialog_width - 18, vars_style);
        screen.drawText(dialog_x + 2, dialog_y + 7, "(space-separated)", style_mod.styles.dim);

        // Help text
        const help_y = dialog_y + 9;
        screen.drawText(dialog_x + 2, help_y, "Tab: next field | Enter: create | Esc: cancel", style_mod.styles.dim);

        // Position cursor at end of active field
        screen.cursor_visible = true;
        switch (self.dialog_field) {
            .class_name => screen.setCursor(dialog_x + 14 + @as(u16, @intCast(self.dialog_class_name_len)), dialog_y + 2),
            .superclass => screen.setCursor(dialog_x + 14 + @as(u16, @intCast(self.dialog_superclass_len)), dialog_y + 4),
            .inst_vars => screen.setCursor(dialog_x + 14 + @as(u16, @intCast(self.dialog_inst_vars_len)), dialog_y + 6),
        }
    }
};
