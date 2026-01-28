const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("../widgets/widget.zig");
const ListView = @import("../widgets/listview.zig").ListView;
const TextArea = @import("../widgets/textarea.zig").TextArea;

const Key = input.Key;
const MouseEvent = input.MouseEvent;
const Style = style_mod.Style;
const Rect = widget.Rect;

// Import VM debugger types
const debugger_mod = @import("../../vm/debugger.zig");
const Debugger = debugger_mod.Debugger;
const StackFrameInfo = debugger_mod.StackFrameInfo;
const LocalVariable = debugger_mod.LocalVariable;
const memory = @import("../../vm/memory.zig");
const Heap = memory.Heap;
const Value = @import("../../vm/object.zig").Value;

pub const DebuggerTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,
    heap: *Heap,

    // Panes
    stack_list: ListView,
    var_list: ListView,
    source_view: TextArea,

    // State
    active_pane: Pane = .stack,
    is_halted: bool = false,
    selected_frame: usize = 0,

    // Cached data (owned slices - must be freed)
    stack_frames: []StackFrameInfo = &[_]StackFrameInfo{},
    stack_frames_owned: bool = false,
    current_vars: []LocalVariable = &[_]LocalVariable{},
    current_vars_owned: bool = false,
    current_source: []const u8 = "",
    current_line: usize = 0,

    // Temporary buffers for display
    var_display_buf: [4096]u8 = undefined,

    // Status message
    status_message: [128]u8 = undefined,
    status_len: usize = 0,

    // Callbacks
    on_step_into: ?*const fn () void = null,
    on_step_over: ?*const fn () void = null,
    on_step_out: ?*const fn () void = null,
    on_continue: ?*const fn () void = null,
    on_restart: ?*const fn (usize) void = null,

    pub const Pane = enum {
        stack,
        vars,
        source,
    };

    pub fn init(allocator: std.mem.Allocator, rect: Rect, heap: *Heap) !DebuggerTab {
        // Layout: left side has stack/vars stacked, right side is source
        const left_width = rect.width / 3;
        const stack_height = rect.height / 2;

        const stack_rect = Rect.init(rect.x, rect.y, left_width, stack_height);
        const var_rect = Rect.init(rect.x, rect.y + stack_height, left_width, rect.height - stack_height);
        const source_rect = Rect.init(rect.x + left_width, rect.y, rect.width - left_width, rect.height);

        var stack_list = try ListView.init(allocator, stack_rect);
        stack_list.state.title = "Stack";

        var var_list = try ListView.init(allocator, var_rect);
        var_list.state.title = "Variables";

        var source_view = try TextArea.init(allocator, Rect.init(
            source_rect.x + 1,
            source_rect.y + 1,
            source_rect.width -| 2,
            source_rect.height -| 2,
        ));
        source_view.state.border = false;
        source_view.line_numbers = true;
        source_view.syntax_highlight = true;
        source_view.readonly = true;

        return DebuggerTab{
            .allocator = allocator,
            .rect = rect,
            .heap = heap,
            .stack_list = stack_list,
            .var_list = var_list,
            .source_view = source_view,
        };
    }

    pub fn deinit(self: *DebuggerTab) void {
        self.stack_list.deinit();
        self.var_list.deinit();
        self.source_view.deinit();
        if (self.stack_frames_owned) {
            self.allocator.free(self.stack_frames);
        }
        if (self.current_vars_owned) {
            self.allocator.free(self.current_vars);
        }
    }

    pub fn updateRect(self: *DebuggerTab, rect: Rect) void {
        self.rect = rect;

        // Recalculate layout
        const left_width = rect.width / 3;
        const stack_height = rect.height / 2;

        const stack_rect = Rect.init(rect.x, rect.y, left_width, stack_height);
        const var_rect = Rect.init(rect.x, rect.y + stack_height, left_width, rect.height - stack_height);
        const source_rect = Rect.init(rect.x + left_width, rect.y, rect.width - left_width, rect.height);

        self.stack_list.state.rect = stack_rect;
        self.var_list.state.rect = var_rect;
        self.source_view.state.rect = Rect.init(
            source_rect.x + 1,
            source_rect.y + 1,
            source_rect.width -| 2,
            source_rect.height -| 2,
        );
    }

    /// Called when debugger halts - refresh all display
    pub fn onHalt(self: *DebuggerTab) void {
        self.is_halted = true;
        self.selected_frame = 0;
        self.refresh();
    }

    /// Called when execution continues
    pub fn onContinue(self: *DebuggerTab) void {
        self.is_halted = false;
        self.setStatus("Running...");
    }

    /// Refresh all debugger display from VM state
    pub fn refresh(self: *DebuggerTab) void {
        if (debugger_mod.globalDebugger) |dbg| {
            // Get stack frames - free previous if owned
            if (self.stack_frames_owned) {
                self.allocator.free(self.stack_frames);
                self.stack_frames_owned = false;
            }
            if (dbg.getStackFrames(self.allocator)) |frames| {
                self.stack_frames = frames;
                self.stack_frames_owned = true;
            } else |_| {
                self.stack_frames = &[_]StackFrameInfo{};
                self.stack_frames_owned = false;
            }

            // Ensure selected_frame is in bounds
            if (self.selected_frame >= self.stack_frames.len) {
                self.selected_frame = 0;
            }

            // Update stack list
            self.stack_list.clear();
            for (self.stack_frames, 0..) |frame, i| {
                var buf: [128]u8 = undefined;
                const prefix: []const u8 = if (i == self.selected_frame) "> " else "  ";
                const text = std.fmt.bufPrint(&buf, "{s}{s}>>{s}", .{
                    prefix,
                    frame.class_name,
                    frame.selector[0..@min(frame.selector.len, 40)],
                }) catch "<error>";
                self.stack_list.addItem(text, null) catch {};
            }

            // Update variables for selected frame
            self.refreshVariables();

            // Update source for selected frame
            self.refreshSource();

            self.setStatus("Halted");
        }
    }

    fn refreshVariables(self: *DebuggerTab) void {
        self.var_list.clear();

        if (debugger_mod.globalDebugger) |dbg| {
            if (self.selected_frame < self.stack_frames.len) {
                const frame = self.stack_frames[self.selected_frame];

                // Add "self" first
                var buf: [256]u8 = undefined;
                const self_str = std.fmt.bufPrint(&buf, "self: {s}", .{
                    self.formatValue(frame.receiver),
                }) catch "<error>";
                self.var_list.addItem(self_str, null) catch {};

                // Get locals - free previous if owned
                if (self.current_vars_owned) {
                    self.allocator.free(self.current_vars);
                    self.current_vars_owned = false;
                }
                if (dbg.getLocals(frame, self.allocator)) |vars| {
                    self.current_vars = vars;
                    self.current_vars_owned = true;
                } else |_| {
                    self.current_vars = &[_]LocalVariable{};
                    self.current_vars_owned = false;
                }

                for (self.current_vars) |local| {
                    const kind: []const u8 = if (local.is_argument) "arg" else "temp";
                    const text = std.fmt.bufPrint(&buf, "{s}[{d}]: {s}", .{
                        kind,
                        local.index,
                        self.formatValue(local.value),
                    }) catch "<error>";
                    self.var_list.addItem(text, null) catch {};
                }

                // Get instance variables
                const inst_vars = dbg.getInstanceVars(frame.receiver, self.allocator) catch &[_]LocalVariable{};
                defer if (inst_vars.len > 0) self.allocator.free(inst_vars);

                if (inst_vars.len > 0) {
                    self.var_list.addItem("--- Instance Vars ---", null) catch {};
                    for (inst_vars) |ivar| {
                        const text = std.fmt.bufPrint(&buf, "  [{d}]: {s}", .{
                            ivar.index,
                            self.formatValue(ivar.value),
                        }) catch "<error>";
                        self.var_list.addItem(text, null) catch {};
                    }
                }
            }
        }
    }

    fn refreshSource(self: *DebuggerTab) void {
        if (debugger_mod.globalDebugger) |dbg| {
            if (self.selected_frame < self.stack_frames.len) {
                const frame = self.stack_frames[self.selected_frame];
                const source = dbg.getMethodSource(frame.method);

                self.source_view.setText("") catch {};
                if (source.len > 0) {
                    self.source_view.setText(source) catch {};
                    // TODO: Highlight current line based on IP
                    // For now just show source
                } else {
                    self.source_view.setText("(no source available)") catch {};
                }
            }
        }
    }

    fn formatValue(self: *DebuggerTab, value: Value) []const u8 {
        const buf = &self.var_display_buf;

        if (value.isNil()) {
            return "nil";
        } else if (value.isTrue()) {
            return "true";
        } else if (value.isFalse()) {
            return "false";
        } else if (value.isSmallInt()) {
            const result = std.fmt.bufPrint(buf, "{d}", .{value.asSmallInt()}) catch return "<int>";
            return result;
        } else if (value.isCharacter()) {
            const cp = value.asCharacter();
            if (cp >= 32 and cp < 127) {
                const result = std.fmt.bufPrint(buf, "${c}", .{@as(u8, @intCast(cp))}) catch return "<char>";
                return result;
            } else {
                return "$...";
            }
        } else if (value.isObject()) {
            const obj = value.asObject();
            const class_index = obj.header.class_index;

            if (class_index == Heap.CLASS_STRING) {
                const bytes = obj.bytes(obj.header.size);
                const max_len = @min(bytes.len, 30);
                if (bytes.len > 30) {
                    const result = std.fmt.bufPrint(buf, "'{s}...'", .{bytes[0..max_len]}) catch return "<string>";
                    return result;
                } else {
                    const result = std.fmt.bufPrint(buf, "'{s}'", .{bytes[0..max_len]}) catch return "<string>";
                    return result;
                }
            } else if (class_index == Heap.CLASS_SYMBOL) {
                const bytes = obj.bytes(obj.header.size);
                const result = std.fmt.bufPrint(buf, "#{s}", .{bytes[0..@min(bytes.len, 30)]}) catch return "<symbol>";
                return result;
            } else if (class_index == Heap.CLASS_ARRAY) {
                const result = std.fmt.bufPrint(buf, "#(size={d})", .{obj.header.size}) catch return "<array>";
                return result;
            } else if (class_index == Heap.CLASS_BLOCK_CLOSURE) {
                return "aBlockClosure";
            } else {
                // Get class name
                const class = self.heap.getClass(class_index);
                if (class.isObject()) {
                    const class_obj = class.asObject();
                    const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                    if (name_val.isObject()) {
                        const name_obj = name_val.asObject();
                        if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            const class_name = name_obj.bytes(name_obj.header.size);
                            const result = std.fmt.bufPrint(buf, "a{s}", .{class_name}) catch return "<object>";
                            return result;
                        }
                    }
                }
                return "<object>";
            }
        }
        return "<?>";
    }

    fn setStatus(self: *DebuggerTab, msg: []const u8) void {
        const len = @min(msg.len, self.status_message.len);
        @memcpy(self.status_message[0..len], msg[0..len]);
        self.status_len = len;
    }

    pub fn handleKey(self: *DebuggerTab, key: Key) bool {
        // Global debugger keys
        switch (key) {
            .f7 => {
                // Step Into
                if (self.is_halted) {
                    if (debugger_mod.globalDebugger) |dbg| {
                        dbg.stepInto();
                        self.setStatus("Stepping...");
                    }
                }
                return true;
            },
            .f8 => {
                // Step Over
                if (self.is_halted) {
                    if (debugger_mod.globalDebugger) |dbg| {
                        dbg.stepOver();
                        self.setStatus("Stepping over...");
                    }
                }
                return true;
            },
            .f9 => {
                // Continue
                if (self.is_halted) {
                    if (debugger_mod.globalDebugger) |dbg| {
                        dbg.continueExecution();
                        self.onContinue();
                    }
                }
                return true;
            },
            .char => |c| {
                if (c == 'r' or c == 'R') {
                    // Restart current frame
                    if (self.is_halted and self.selected_frame < self.stack_frames.len) {
                        if (debugger_mod.globalDebugger) |dbg| {
                            const frame = self.stack_frames[self.selected_frame];
                            dbg.restartAtContext(frame.context_index);
                            self.refresh();
                            self.setStatus("Restarted method");
                        }
                    }
                    return true;
                }
            },
            .tab => {
                // Switch panes
                self.active_pane = switch (self.active_pane) {
                    .stack => .vars,
                    .vars => .source,
                    .source => .stack,
                };
                self.updateFocus();
                return true;
            },
            .up => {
                switch (self.active_pane) {
                    .stack => {
                        if (self.stack_list.selected_index) |idx| {
                            if (idx > 0) {
                                self.stack_list.selectIndex(idx - 1);
                                self.selected_frame = idx - 1;
                                self.refreshVariables();
                                self.refreshSource();
                            }
                        }
                    },
                    .vars => {
                        if (self.var_list.selected_index) |idx| {
                            if (idx > 0) {
                                self.var_list.selectIndex(idx - 1);
                            }
                        }
                    },
                    .source => {
                        if (self.source_view.scroll_y > 0) {
                            self.source_view.scroll_y -= 1;
                        }
                    },
                }
                return true;
            },
            .down => {
                switch (self.active_pane) {
                    .stack => {
                        if (self.stack_list.selected_index) |idx| {
                            if (idx + 1 < self.stack_list.items.items.len) {
                                self.stack_list.selectIndex(idx + 1);
                                self.selected_frame = idx + 1;
                                self.refreshVariables();
                                self.refreshSource();
                            }
                        }
                    },
                    .vars => {
                        if (self.var_list.selected_index) |idx| {
                            if (idx + 1 < self.var_list.items.items.len) {
                                self.var_list.selectIndex(idx + 1);
                            }
                        }
                    },
                    .source => {
                        self.source_view.scroll_y += 1;
                    },
                }
                return true;
            },
            else => {},
        }

        return false;
    }

    pub fn handleMouse(self: *DebuggerTab, mouse: MouseEvent) void {
        if (mouse.event_type != .press) return;

        const layout = self.calcLayout();

        if (layout.stack_rect.contains(mouse.x, mouse.y)) {
            self.active_pane = .stack;
            self.updateFocus();
            // Calculate clicked item
            const content_y = mouse.y -| layout.stack_rect.y -| 1;
            const clicked_idx = self.stack_list.scroll_offset + content_y;
            if (clicked_idx < self.stack_list.items.items.len) {
                self.stack_list.selectIndex(clicked_idx);
                self.selected_frame = clicked_idx;
                self.refreshVariables();
                self.refreshSource();
            }
        } else if (layout.var_rect.contains(mouse.x, mouse.y)) {
            self.active_pane = .vars;
            self.updateFocus();
            const content_y = mouse.y -| layout.var_rect.y -| 1;
            const clicked_idx = self.var_list.scroll_offset + content_y;
            if (clicked_idx < self.var_list.items.items.len) {
                self.var_list.selectIndex(clicked_idx);
            }
        } else if (layout.source_rect.contains(mouse.x, mouse.y)) {
            self.active_pane = .source;
            self.updateFocus();
        }
    }

    pub fn scroll(self: *DebuggerTab, delta: i32) void {
        switch (self.active_pane) {
            .stack => self.scrollList(&self.stack_list, delta),
            .vars => self.scrollList(&self.var_list, delta),
            .source => self.scrollSource(delta),
        }
    }

    fn scrollList(_: *DebuggerTab, list: *ListView, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (list.scroll_offset > amount) {
                list.scroll_offset -= amount;
            } else {
                list.scroll_offset = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = list.items.items.len -| 1;
            list.scroll_offset = @min(list.scroll_offset + amount, max_scroll);
        }
    }

    fn scrollSource(self: *DebuggerTab, delta: i32) void {
        if (delta < 0) {
            const amount = @as(usize, @intCast(-delta));
            if (self.source_view.scroll_y > amount) {
                self.source_view.scroll_y -= amount;
            } else {
                self.source_view.scroll_y = 0;
            }
        } else {
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.source_view.lines.items.len -| 1;
            self.source_view.scroll_y = @min(self.source_view.scroll_y + amount, max_scroll);
        }
    }

    const Layout = struct {
        stack_rect: Rect,
        var_rect: Rect,
        source_rect: Rect,
    };

    fn calcLayout(self: *DebuggerTab) Layout {
        const left_width = self.rect.width / 3;
        const stack_height = self.rect.height / 2;

        return .{
            .stack_rect = Rect.init(self.rect.x, self.rect.y, left_width, stack_height),
            .var_rect = Rect.init(self.rect.x, self.rect.y + stack_height, left_width, self.rect.height - stack_height),
            .source_rect = Rect.init(self.rect.x + left_width, self.rect.y, self.rect.width - left_width, self.rect.height),
        };
    }

    fn updateFocus(self: *DebuggerTab) void {
        self.stack_list.state.focused = (self.active_pane == .stack);
        self.var_list.state.focused = (self.active_pane == .vars);
        self.source_view.state.focused = (self.active_pane == .source);
    }

    pub fn draw(self: *DebuggerTab, screen: *Screen) void {
        self.updateFocus();

        const layout = self.calcLayout();

        // Update widget rects
        self.stack_list.state.rect = layout.stack_rect;
        self.var_list.state.rect = layout.var_rect;

        // Draw panes
        self.stack_list.draw(screen);
        self.var_list.draw(screen);

        // Draw source with border
        self.drawSourcePane(screen, layout.source_rect);

        // Draw help bar at bottom of source
        if (self.is_halted) {
            self.drawHelpBar(screen, layout.source_rect);
        } else {
            // Show "not halted" message
            const msg = "Debugger: Not currently halted. Use 'self halt' in code.";
            const x = layout.source_rect.x + 2;
            const y = layout.source_rect.y + layout.source_rect.height / 2;
            screen.drawText(x, y, msg, style_mod.styles.dim);
        }
    }

    fn drawSourcePane(self: *DebuggerTab, screen: *Screen, rect: Rect) void {
        // Draw border
        const title = "Source";
        const border_style = if (self.active_pane == .source)
            style_mod.styles.border_focused_style
        else
            style_mod.styles.border_style;

        // Top border
        screen.setCell(rect.x, rect.y, style_mod.box.top_left, border_style);
        var x = rect.x + 1;
        while (x < rect.x + rect.width - 1) : (x += 1) {
            screen.setCell(x, rect.y, style_mod.box.horizontal, border_style);
        }
        screen.setCell(rect.x + rect.width - 1, rect.y, style_mod.box.top_right, border_style);

        // Title
        const title_x = rect.x + 2;
        screen.drawText(title_x, rect.y, title, border_style);

        // Side borders
        var y = rect.y + 1;
        while (y < rect.y + rect.height - 1) : (y += 1) {
            screen.setCell(rect.x, y, style_mod.box.vertical, border_style);
            screen.setCell(rect.x + rect.width - 1, y, style_mod.box.vertical, border_style);
        }

        // Bottom border
        screen.setCell(rect.x, rect.y + rect.height - 1, style_mod.box.bottom_left, border_style);
        x = rect.x + 1;
        while (x < rect.x + rect.width - 1) : (x += 1) {
            screen.setCell(x, rect.y + rect.height - 1, style_mod.box.horizontal, border_style);
        }
        screen.setCell(rect.x + rect.width - 1, rect.y + rect.height - 1, style_mod.box.bottom_right, border_style);

        // Draw source content
        self.source_view.state.rect = Rect.init(
            rect.x + 1,
            rect.y + 1,
            rect.width -| 2,
            rect.height -| 3, // Leave room for help bar
        );
        self.source_view.draw(screen);
    }

    fn drawHelpBar(self: *DebuggerTab, screen: *Screen, rect: Rect) void {
        _ = self;
        const y = rect.y + rect.height - 2;
        const help = " F7:Into  F8:Over  Shift+F8:Out  F9:Continue  R:Restart ";
        screen.drawText(rect.x + 1, y, help, style_mod.styles.status);
    }
};
