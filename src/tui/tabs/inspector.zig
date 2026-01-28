const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("../widgets/widget.zig");
const TextArea = @import("../widgets/textarea.zig").TextArea;
const transcript_mod = @import("transcript.zig");

const Key = input.Key;
const MouseEvent = input.MouseEvent;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

// Import VM types
const Value = @import("../../vm/object.zig").Value;
const Object = @import("../../vm/object.zig").Object;
const Heap = @import("../../vm/memory.zig").Heap;

/// A single inspection session - tracks one inspected object and its drill-down history
pub const InspectionSession = struct {
    root_value: Value, // The original value that started this session
    current_value: Value, // Current value being viewed (may have drilled down)
    history: std.ArrayList(Value), // Drill-down history within this session

    pub fn init(value: Value) InspectionSession {
        return .{
            .root_value = value,
            .current_value = value,
            .history = .empty,
        };
    }

    pub fn deinit(self: *InspectionSession, allocator: std.mem.Allocator) void {
        self.history.deinit(allocator);
    }
};

pub const InspectorTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,
    heap: *Heap,

    // Multiple inspection sessions
    sessions: std.ArrayList(InspectionSession),
    current_session: usize = 0,

    // Variable list (top pane)
    selected_index: usize = 0,
    scroll_offset: usize = 0,
    var_names: std.ArrayList([]const u8),
    var_values: std.ArrayList(Value),

    // Expression editor (bottom pane)
    editor: TextArea,

    // Which pane is focused: false = var list, true = editor
    editor_focused: bool = false,

    // Callback to send to workspace
    on_send_to_workspace: ?*const fn ([]const u8) void = null,
    // Callback to evaluate expression with self bound to inspected value
    on_do_it: ?*const fn ([]const u8, Value) void = null,
    on_print_it: ?*const fn ([]const u8, Value) ?[]const u8 = null,
    on_inspect_it: ?*const fn ([]const u8, Value) void = null,
    // Callback to browse the class in the Browser tab
    on_browse: ?*const fn ([]const u8) void = null,

    pub fn init(allocator: std.mem.Allocator, rect: Rect, heap: *Heap) !InspectorTab {
        // Calculate editor rect (bottom half)
        const editor_rect = calculateEditorRect(rect);
        var editor = try TextArea.init(allocator, editor_rect);
        editor.state.border = false;
        editor.line_numbers = false;
        editor.syntax_highlight = true;

        return InspectorTab{
            .allocator = allocator,
            .rect = rect,
            .heap = heap,
            .sessions = .empty,
            .var_names = .empty,
            .var_values = .empty,
            .editor = editor,
        };
    }

    fn calculateEditorRect(rect: Rect) Rect {
        // Bottom half of the inspector, minus borders
        const half_height = rect.height / 2;
        const editor_y = rect.y + rect.height - half_height;
        return Rect.init(
            rect.x + 1,
            editor_y + 1,
            rect.width -| 2,
            half_height -| 2,
        );
    }

    fn calculateVarListHeight(rect: Rect) usize {
        // Top half minus header (3 lines) and border
        const half_height = rect.height / 2;
        if (half_height > 5) {
            return half_height - 5;
        }
        return 1;
    }

    pub fn deinit(self: *InspectorTab) void {
        for (self.sessions.items) |*session| {
            session.deinit(self.allocator);
        }
        self.sessions.deinit(self.allocator);
        self.clearVarCache();
        self.var_names.deinit(self.allocator);
        self.var_values.deinit(self.allocator);
        self.editor.deinit();
    }

    fn clearVarCache(self: *InspectorTab) void {
        for (self.var_names.items) |name| {
            self.allocator.free(name);
        }
        self.var_names.clearRetainingCapacity();
        self.var_values.clearRetainingCapacity();
    }

    /// Get the currently inspected value (from current session)
    fn currentValue(self: *InspectorTab) Value {
        if (self.sessions.items.len == 0) return Value.nil;
        return self.sessions.items[self.current_session].current_value;
    }

    /// Get mutable reference to current session
    fn currentSession(self: *InspectorTab) ?*InspectionSession {
        if (self.sessions.items.len == 0) return null;
        return &self.sessions.items[self.current_session];
    }

    /// Open a new inspection session (adds to the list, switches to it)
    pub fn inspect(self: *InspectorTab, value: Value) !void {
        // Create new session
        const session = InspectionSession.init(value);
        try self.sessions.append(self.allocator, session);
        self.current_session = self.sessions.items.len - 1;
        self.selected_index = 0;
        self.scroll_offset = 0;
        try self.refreshVarList();
    }

    /// Drill down into a value within the current session
    pub fn drillDown(self: *InspectorTab, value: Value) !void {
        if (self.currentSession()) |session| {
            // Push current to history
            try session.history.append(self.allocator, session.current_value);
            session.current_value = value;
            self.selected_index = 0;
            self.scroll_offset = 0;
            try self.refreshVarList();
        }
    }

    /// Go back in drill-down history within current session
    pub fn goBack(self: *InspectorTab) !void {
        if (self.currentSession()) |session| {
            if (session.history.items.len > 0) {
                const prev = session.history.items[session.history.items.len - 1];
                session.history.items.len -= 1;
                session.current_value = prev;
                self.selected_index = 0;
                self.scroll_offset = 0;
                try self.refreshVarList();
            }
        }
    }

    /// Switch to next inspection session
    pub fn nextSession(self: *InspectorTab) !void {
        if (self.sessions.items.len > 1) {
            self.current_session = (self.current_session + 1) % self.sessions.items.len;
            self.selected_index = 0;
            self.scroll_offset = 0;
            try self.refreshVarList();
        }
    }

    /// Switch to previous inspection session
    pub fn prevSession(self: *InspectorTab) !void {
        if (self.sessions.items.len > 1) {
            if (self.current_session == 0) {
                self.current_session = self.sessions.items.len - 1;
            } else {
                self.current_session -= 1;
            }
            self.selected_index = 0;
            self.scroll_offset = 0;
            try self.refreshVarList();
        }
    }

    /// Close the current inspection session
    pub fn closeSession(self: *InspectorTab) !void {
        if (self.sessions.items.len > 0) {
            var session = self.sessions.orderedRemove(self.current_session);
            session.deinit(self.allocator);
            // Adjust current_session index
            if (self.current_session >= self.sessions.items.len and self.sessions.items.len > 0) {
                self.current_session = self.sessions.items.len - 1;
            }
            self.selected_index = 0;
            self.scroll_offset = 0;
            if (self.sessions.items.len > 0) {
                try self.refreshVarList();
            } else {
                self.clearVarCache();
            }
        }
    }

    /// Get the drill-down depth of current session
    fn currentDepth(self: *InspectorTab) usize {
        if (self.currentSession()) |session| {
            return session.history.items.len;
        }
        return 0;
    }

    fn refreshVarList(self: *InspectorTab) !void {
        self.clearVarCache();

        const value = self.currentValue();

        // Add "self" entry
        try self.var_names.append(self.allocator, try self.allocator.dupe(u8, "self"));
        try self.var_values.append(self.allocator, value);

        if (value.isSmallInt()) {
            const val = value.asSmallInt();
            const str = try std.fmt.allocPrint(self.allocator, "value: {d}", .{val});
            try self.var_names.append(self.allocator, str);
            try self.var_values.append(self.allocator, value);
        } else if (value == Value.true or value == Value.false) {
            const str = if (value == Value.true) "true" else "false";
            const copy = try self.allocator.dupe(u8, str);
            try self.var_names.append(self.allocator, copy);
            try self.var_values.append(self.allocator, value);
        } else if (value == Value.nil) {
            const copy = try self.allocator.dupe(u8, "nil");
            try self.var_names.append(self.allocator, copy);
            try self.var_values.append(self.allocator, value);
        } else if (value.isCharacter()) {
            const char_val = value.asCharacter();
            const str = try std.fmt.allocPrint(self.allocator, "character: code {d}", .{@as(u32, char_val)});
            try self.var_names.append(self.allocator, str);
            try self.var_values.append(self.allocator, value);
        } else if (value.isObject()) {
            const obj = value.asObject();

            // Check for String
            if (obj.header.class_index == Heap.CLASS_STRING or obj.header.class_index == Heap.CLASS_SYMBOL) {
                const str_bytes = obj.bytes(obj.header.size);
                const display = try std.fmt.allocPrint(self.allocator, "value: '{s}'", .{str_bytes});
                try self.var_names.append(self.allocator, display);
                try self.var_values.append(self.allocator, value);
            } else if (obj.header.class_index == Heap.CLASS_FLOAT) {
                // Float stores f64 as bytes
                const float_bytes = obj.bytes(@sizeOf(f64));
                const float_val: f64 = @bitCast(float_bytes[0..8].*);
                const str = try std.fmt.allocPrint(self.allocator, "value: {d}", .{float_val});
                try self.var_names.append(self.allocator, str);
                try self.var_values.append(self.allocator, value);
            } else if (obj.header.class_index == Heap.CLASS_ARRAY) {
                // Array - show indexed elements using fields()
                const size = obj.header.size;
                const fields = obj.fields(size);
                for (0..@min(size, 50)) |i| {
                    const elem = fields[i];
                    const name = try std.fmt.allocPrint(self.allocator, "[{d}]: {s}", .{ i + 1, self.briefValue(elem) });
                    try self.var_names.append(self.allocator, name);
                    try self.var_values.append(self.allocator, elem);
                }
                if (size > 50) {
                    const more = try std.fmt.allocPrint(self.allocator, "... and {d} more elements", .{size - 50});
                    try self.var_names.append(self.allocator, more);
                    try self.var_values.append(self.allocator, Value.nil);
                }
            } else if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) {
                // ByteArray - show bytes
                const size = obj.header.size;
                const byte_data = obj.bytes(size);
                for (0..@min(size, 50)) |i| {
                    const name = try std.fmt.allocPrint(self.allocator, "[{d}]: {d}", .{ i + 1, byte_data[i] });
                    try self.var_names.append(self.allocator, name);
                    try self.var_values.append(self.allocator, Value.fromSmallInt(@intCast(byte_data[i])));
                }
                if (size > 50) {
                    const more = try std.fmt.allocPrint(self.allocator, "... and {d} more bytes", .{size - 50});
                    try self.var_names.append(self.allocator, more);
                    try self.var_values.append(self.allocator, Value.nil);
                }
            } else {
                // Regular object - show instance variables using fields()
                const num_fields = obj.header.size;
                const fields = obj.fields(num_fields);

                for (0..num_fields) |i| {
                    const var_val = fields[i];
                    const name = try std.fmt.allocPrint(self.allocator, "instVar{d}: {s}", .{ i + 1, self.briefValue(var_val) });
                    try self.var_names.append(self.allocator, name);
                    try self.var_values.append(self.allocator, var_val);
                }
            }
        }
    }

    fn briefValue(self: *InspectorTab, value: Value) []const u8 {
        _ = self;
        if (value.isSmallInt()) {
            return "<int>";
        } else if (value == Value.true) {
            return "true";
        } else if (value == Value.false) {
            return "false";
        } else if (value == Value.nil) {
            return "nil";
        } else if (value.isCharacter()) {
            return "<char>";
        } else if (value.isObject()) {
            const obj = value.asObject();
            if (obj.header.class_index == Heap.CLASS_STRING) {
                return "<String>";
            } else if (obj.header.class_index == Heap.CLASS_SYMBOL) {
                return "<Symbol>";
            } else if (obj.header.class_index == Heap.CLASS_ARRAY) {
                return "<Array>";
            } else if (obj.header.class_index == Heap.CLASS_FLOAT) {
                return "<Float>";
            }
            return "<Object>";
        }
        return "?";
    }

    fn getClassName(self: *InspectorTab, value: Value) []const u8 {
        if (value.isSmallInt()) {
            return "SmallInteger";
        } else if (value == Value.true) {
            return "True";
        } else if (value == Value.false) {
            return "False";
        } else if (value == Value.nil) {
            return "UndefinedObject";
        } else if (value.isCharacter()) {
            return "Character";
        } else if (value.isObject()) {
            const obj = value.asObject();
            if (obj.header.class_index == Heap.CLASS_STRING) return "String";
            if (obj.header.class_index == Heap.CLASS_SYMBOL) return "Symbol";
            if (obj.header.class_index == Heap.CLASS_ARRAY) return "Array";
            if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) return "ByteArray";
            if (obj.header.class_index == Heap.CLASS_FLOAT) return "Float";

            // Try to get class name from class object
            const class_val = self.heap.getClass(obj.header.class_index);
            if (class_val.isObject()) {
                const class_obj = class_val.asObject();
                const num_fields = class_obj.header.size;
                if (num_fields > 6) {
                    const class_fields = class_obj.fields(num_fields);
                    const name_val = class_fields[6];
                    if (name_val.isObject()) {
                        const name_obj = name_val.asObject();
                        if (name_obj.header.class_index == Heap.CLASS_SYMBOL or
                            name_obj.header.class_index == Heap.CLASS_STRING)
                        {
                            return name_obj.bytes(name_obj.header.size);
                        }
                    }
                }
            }
            return "Object";
        }
        return "Unknown";
    }

    pub fn handleKey(self: *InspectorTab, key: Key) EventResult {
        // Tab switches between var list and editor
        if (key == .tab) {
            self.editor_focused = !self.editor_focused;
            return .consumed;
        }

        // Session navigation with Left/Right arrows (only from var list pane)
        if (!self.editor_focused and self.sessions.items.len > 1) {
            switch (key) {
                .left => {
                    self.prevSession() catch {};
                    return .consumed;
                },
                .right => {
                    self.nextSession() catch {};
                    return .consumed;
                },
                else => {},
            }
        }

        if (self.editor_focused) {
            return self.handleEditorKey(key);
        } else {
            return self.handleVarListKey(key);
        }
    }

    fn handleVarListKey(self: *InspectorTab, key: Key) EventResult {
        const visible = calculateVarListHeight(self.rect);

        switch (key) {
            .up => {
                if (self.selected_index > 0) {
                    self.selected_index -= 1;
                    if (self.selected_index < self.scroll_offset) {
                        self.scroll_offset = self.selected_index;
                    }
                }
                return .consumed;
            },
            .down => {
                if (self.selected_index + 1 < self.var_names.items.len) {
                    self.selected_index += 1;
                    if (self.selected_index >= self.scroll_offset + visible) {
                        self.scroll_offset = self.selected_index + 1 - visible;
                    }
                }
                return .consumed;
            },
            .enter => {
                // Drill down into selected value
                if (self.selected_index < self.var_values.items.len) {
                    const val = self.var_values.items[self.selected_index];
                    if (val.isObject()) {
                        self.drillDown(val) catch {};
                    }
                }
                return .consumed;
            },
            .backspace => {
                self.goBack() catch {};
                return .consumed;
            },
            .char => |c| {
                if (c == 'w' or c == 'W') {
                    self.sendToWorkspace();
                    return .consumed;
                }
                // Browse class in Browser tab
                if (c == 'b' or c == 'B') {
                    self.browseClass();
                    return .consumed;
                }
                // Close current session
                if (c == 'x' or c == 'X') {
                    self.closeSession() catch {};
                    return .consumed;
                }
                // Any other printable character: switch to editor and type there
                if (c >= 32 and c < 127) {
                    self.editor_focused = true;
                    _ = self.editor.handleKey(.{ .char = c });
                    return .consumed;
                }
            },
            .ctrl => |c| {
                // Ctrl+D/P/I should work from var list too - switch to editor first
                switch (c) {
                    4 => { // Ctrl+D - Do It
                        self.editor_focused = true;
                        self.doIt();
                        return .consumed;
                    },
                    16 => { // Ctrl+P - Print It
                        self.editor_focused = true;
                        self.printIt();
                        return .consumed;
                    },
                    9 => { // Ctrl+I - Inspect It
                        self.editor_focused = true;
                        self.inspectIt();
                        return .consumed;
                    },
                    else => {},
                }
            },
            .page_up => {
                if (self.scroll_offset > visible) {
                    self.scroll_offset -= visible;
                } else {
                    self.scroll_offset = 0;
                }
                if (self.selected_index >= self.scroll_offset + visible) {
                    self.selected_index = self.scroll_offset + visible - 1;
                }
                return .consumed;
            },
            .page_down => {
                const max_scroll = if (self.var_names.items.len > visible) self.var_names.items.len - visible else 0;
                self.scroll_offset = @min(self.scroll_offset + visible, max_scroll);
                if (self.selected_index < self.scroll_offset) {
                    self.selected_index = self.scroll_offset;
                }
                return .consumed;
            },
            else => {},
        }
        return .ignored;
    }

    fn handleEditorKey(self: *InspectorTab, key: Key) EventResult {
        // Handle Ctrl+D/P/I for evaluation
        switch (key) {
            .ctrl => |c| {
                switch (c) {
                    4 => { // Ctrl+D - Do It
                        self.doIt();
                        return .consumed;
                    },
                    16 => { // Ctrl+P - Print It
                        self.printIt();
                        return .consumed;
                    },
                    9 => { // Ctrl+I - Inspect It
                        self.inspectIt();
                        return .consumed;
                    },
                    else => {},
                }
            },
            else => {},
        }

        // Pass to editor
        self.editor.state.focused = self.focused and self.editor_focused;
        return self.editor.handleKey(key);
    }

    fn getSelectedOrCurrentLine(self: *InspectorTab) ?[]u8 {
        // First try to get selected text
        if (self.editor.getSelectedText(self.allocator) catch null) |text| {
            if (text.len > 0) return text;
            self.allocator.free(text);
        }

        // Otherwise get all text from editor
        const text = self.editor.getText(self.allocator) catch return null;
        if (text.len > 0) return text;
        self.allocator.free(text);
        return null;
    }

    fn doIt(self: *InspectorTab) void {
        const code = self.getSelectedOrCurrentLine() orelse return;
        defer self.allocator.free(code);

        if (self.on_do_it) |callback| {
            callback(code, self.currentValue());
        }
    }

    fn printIt(self: *InspectorTab) void {
        const code = self.getSelectedOrCurrentLine() orelse {
            if (transcript_mod.global_transcript) |t| {
                t.addLine("Inspector: no code to execute", .error_style) catch {};
            }
            return;
        };
        defer self.allocator.free(code);

        if (code.len == 0) return;

        if (self.on_print_it) |callback| {
            if (callback(code, self.currentValue())) |result| {
                defer self.allocator.free(result);
                // Insert result after cursor as a comment
                self.insertResult(result);
            }
        }
    }

    fn inspectIt(self: *InspectorTab) void {
        const code = self.getSelectedOrCurrentLine() orelse return;
        defer self.allocator.free(code);

        if (self.on_inspect_it) |callback| {
            callback(code, self.currentValue());
        }
    }

    fn browseClass(self: *InspectorTab) void {
        const class_name = self.getClassName(self.currentValue());
        if (self.on_browse) |callback| {
            callback(class_name);
        }
    }

    fn insertResult(self: *InspectorTab, result: []const u8) void {
        // Move cursor to end of selection or current line
        if (self.editor.hasSelection()) {
            self.editor.clearSelection();
        }

        // Move to end of line
        self.editor.cursor_col = self.editor.lines.items[self.editor.cursor_line].items.len;

        // Insert space and result as comment
        const text = std.fmt.allocPrint(self.allocator, " \" {s} \"", .{result}) catch return;
        defer self.allocator.free(text);

        for (text) |c| {
            _ = self.editor.handleKey(.{ .char = c });
        }
    }

    fn sendToWorkspace(self: *InspectorTab) void {
        if (self.on_send_to_workspace) |callback| {
            if (self.selected_index < self.var_names.items.len) {
                callback(self.var_names.items[self.selected_index]);
            }
        }
    }

    pub fn handleMouse(self: *InspectorTab, mouse: MouseEvent) void {
        const half_height = self.rect.height / 2;
        const divider_y = self.rect.y + half_height;

        // Determine which pane was clicked
        if (mouse.event_type == .press and mouse.button == .left) {
            if (mouse.y < divider_y) {
                // Clicked in var list area
                self.editor_focused = false;

                // Calculate which line was clicked
                const content_y = self.rect.y + 4;
                if (mouse.y >= content_y and mouse.y < divider_y) {
                    const y_offset = mouse.y - content_y;
                    const new_index = self.scroll_offset + y_offset;
                    if (new_index < self.var_names.items.len) {
                        self.selected_index = new_index;
                    }
                }
            } else {
                // Clicked in editor area
                self.editor_focused = true;
            }
        }

        // Forward mouse events to editor if in editor area
        if (mouse.y >= divider_y) {
            self.editor.state.focused = self.focused and self.editor_focused;
            _ = self.editor.handleMouse(mouse);
        }
    }

    pub fn scroll(self: *InspectorTab, delta: i32) void {
        if (self.editor_focused) {
            // Scroll editor
            if (delta < 0) {
                const amount = @as(usize, @intCast(-delta));
                if (self.editor.scroll_y > amount) {
                    self.editor.scroll_y -= amount;
                } else {
                    self.editor.scroll_y = 0;
                }
            } else {
                self.editor.scroll_y += @as(usize, @intCast(delta));
            }
        } else {
            // Scroll var list
            const visible = calculateVarListHeight(self.rect);
            const max_scroll = if (self.var_names.items.len > visible)
                self.var_names.items.len - visible
            else
                0;

            if (delta < 0) {
                const abs_delta = @as(usize, @intCast(-delta));
                if (self.scroll_offset > abs_delta) {
                    self.scroll_offset -= abs_delta;
                } else {
                    self.scroll_offset = 0;
                }
            } else {
                self.scroll_offset = @min(self.scroll_offset + @as(usize, @intCast(delta)), max_scroll);
            }
        }
    }

    pub fn updateRect(self: *InspectorTab, rect: Rect) void {
        self.rect = rect;
        self.editor.state.rect = calculateEditorRect(rect);
    }

    pub fn draw(self: *InspectorTab, screen: *Screen) void {
        const rect = self.rect;
        const half_height = rect.height / 2;

        // Draw outer border
        const border_style = if (self.focused) style_mod.styles.border_focused_style else style_mod.styles.border_style;
        screen.drawBox(rect.x, rect.y, rect.width, rect.height, border_style);

        // Draw title with session counter
        var title_buf: [32]u8 = undefined;
        const title = if (self.sessions.items.len > 1)
            std.fmt.bufPrint(&title_buf, " Inspector ({d}/{d}) ", .{ self.current_session + 1, self.sessions.items.len }) catch " Inspector "
        else
            " Inspector ";
        const title_x = rect.x + (rect.width -| @as(u16, @intCast(title.len))) / 2;
        screen.drawText(title_x, rect.y, title, style_mod.styles.title);

        // Draw class name header
        const class_name = self.getClassName(self.currentValue());
        var header_buf: [64]u8 = undefined;
        const header = std.fmt.bufPrint(&header_buf, "Class: {s}", .{class_name}) catch "Class: ?";
        screen.drawText(rect.x + 2, rect.y + 1, header, Style{ .fg = style_mod.ui.info_text, .bg = style_mod.ui.background });

        // Draw navigation/hint line
        const depth = self.currentDepth();
        if (self.sessions.items.len > 1 or depth > 0) {
            var hint_buf: [72]u8 = undefined;
            const hint = if (self.sessions.items.len > 1 and depth > 0)
                std.fmt.bufPrint(&hint_buf, "[Left/Right: switch] [Bksp: back] Depth: {d}", .{depth}) catch ""
            else if (self.sessions.items.len > 1)
                std.fmt.bufPrint(&hint_buf, "[Left/Right: switch] [X: close]", .{}) catch ""
            else
                std.fmt.bufPrint(&hint_buf, "[Backspace: back] Depth: {d}", .{depth}) catch "";
            screen.drawText(rect.x + 2, rect.y + 2, hint, style_mod.styles.dim);
        } else {
            screen.drawText(rect.x + 2, rect.y + 2, "[Tab: switch panes] [Enter: drill down]", style_mod.styles.dim);
        }

        // Draw separator line
        screen.drawText(rect.x + 1, rect.y + 3, "---", style_mod.styles.dim);

        // Draw variable list (top half)
        const var_list_focused = self.focused and !self.editor_focused;
        const content_y = rect.y + 4;
        const content_height = calculateVarListHeight(rect);
        const content_width = rect.width -| 3;

        var row: usize = 0;
        while (row < content_height) : (row += 1) {
            const item_idx = self.scroll_offset + row;
            if (item_idx >= self.var_names.items.len) break;

            const name = self.var_names.items[item_idx];
            const is_selected = item_idx == self.selected_index and var_list_focused;

            const line_style = if (is_selected)
                Style{ .fg = style_mod.ui.background, .bg = style_mod.ui.selection }
            else
                style_mod.styles.normal;

            // Clear line for selection highlight
            if (is_selected) {
                var col: u16 = 0;
                while (col < content_width) : (col += 1) {
                    screen.setCell(rect.x + 2 + col, content_y + @as(u16, @intCast(row)), ' ', line_style);
                }
            }

            screen.drawTextClipped(rect.x + 2, content_y + @as(u16, @intCast(row)), name, content_width, line_style);
        }

        // Draw horizontal divider between panes
        const divider_y = rect.y + half_height;
        var div_col: u16 = 0;
        while (div_col < rect.width -| 2) : (div_col += 1) {
            screen.setCell(rect.x + 1 + div_col, divider_y, style_mod.box.horizontal, style_mod.styles.dim);
        }
        // Draw T-junctions at edges
        screen.setCell(rect.x, divider_y, style_mod.box.t_right, border_style);
        screen.setCell(rect.x + rect.width - 1, divider_y, style_mod.box.t_left, border_style);

        // Draw pane label
        const expr_label = " self >> ";
        const label_style = if (self.editor_focused)
            Style{ .fg = style_mod.ui.info_text, .bg = style_mod.ui.background, .bold = true }
        else
            style_mod.styles.dim;
        screen.drawText(rect.x + 2, divider_y, expr_label, label_style);

        // Draw editor (bottom half)
        self.editor.state.focused = self.focused and self.editor_focused;
        self.editor.draw(screen);

        // Draw hints at bottom
        const hints = if (self.editor_focused)
            "[Ctrl+D: do it] [Ctrl+P: print it] [Ctrl+I: inspect it]"
        else if (self.sessions.items.len > 1)
            "[Left/Right: switch] [X: close] [B: browse] [Enter: drill]"
        else
            "[B: browse class] [W: to workspace] [Enter: drill down]";
        screen.drawTextClipped(rect.x + 2, rect.y + rect.height - 1, hints, content_width, style_mod.styles.dim);
    }
};
