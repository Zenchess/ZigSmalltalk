const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("../widgets/widget.zig");
const ListView = @import("../widgets/listview.zig").ListView;
const TextArea = @import("../widgets/textarea.zig").TextArea;

const Key = input.Key;
const MouseEvent = input.MouseEvent;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const ViewComposerTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,

    palette_list: ListView,
    widget_list: ListView,
    code_preview: TextArea,

    canvas_rect: Rect,
    properties_rect: Rect,

    active_pane: Pane = .canvas,
    selected_palette: usize = 0,
    selected_widget: ?usize = null,

    widgets: std.ArrayList(WidgetSpec),

    window_name: [64]u8 = [_]u8{0} ** 64,
    window_name_len: usize = 0,
    class_name: [64]u8 = [_]u8{0} ** 64,
    class_name_len: usize = 0,

    status_message: [160]u8 = [_]u8{0} ** 160,
    status_len: usize = 0,

    edit_mode: EditMode = .none,
    edit_buffer: [96]u8 = [_]u8{0} ** 96,
    edit_len: usize = 0,

    drag_widget: ?usize = null,
    drag_offset_x: i16 = 0,
    drag_offset_y: i16 = 0,

    next_widget_id: usize = 1,
    generated_code: []u8,

    on_export_code: ?*const fn (*ViewComposerTab, []const u8) void = null,
    on_run_code: ?*const fn (*ViewComposerTab, []const u8) void = null,

    const palette_names = [_][]const u8{
        "Button",
        "Label",
        "TextField",
        "Slider",
        "Checkbox",
        "List",
        "Tabs",
        "Container",
    };

    pub const Pane = enum {
        palette,
        canvas,
        widgets,
        code,
    };

    pub const EditMode = enum {
        none,
        class_name,
        window_name,
        widget_name,
        widget_text,
        widget_event,
    };

    pub const WidgetKind = enum {
        button,
        label,
        text_field,
        slider,
        checkbox,
        list,
        tabs,
        container,

        fn fromPaletteIndex(index: usize) WidgetKind {
            return switch (index) {
                0 => .button,
                1 => .label,
                2 => .text_field,
                3 => .slider,
                4 => .checkbox,
                5 => .list,
                6 => .tabs,
                else => .container,
            };
        }

        fn displayName(self: WidgetKind) []const u8 {
            return switch (self) {
                .button => "Button",
                .label => "Label",
                .text_field => "TextField",
                .slider => "Slider",
                .checkbox => "Checkbox",
                .list => "List",
                .tabs => "Tabs",
                .container => "Container",
            };
        }

        fn defaultSize(self: WidgetKind) struct { w: u16, h: u16 } {
            return switch (self) {
                .button => .{ .w = 14, .h = 3 },
                .label => .{ .w = 18, .h = 1 },
                .text_field => .{ .w = 24, .h = 3 },
                .slider => .{ .w = 22, .h = 1 },
                .checkbox => .{ .w = 18, .h = 1 },
                .list => .{ .w = 24, .h = 6 },
                .tabs => .{ .w = 28, .h = 8 },
                .container => .{ .w = 30, .h = 10 },
            };
        }

        fn defaultText(self: WidgetKind) []const u8 {
            return switch (self) {
                .button => "Button",
                .label => "Label",
                .text_field => "",
                .slider => "",
                .checkbox => "Check",
                .list => "",
                .tabs => "Tabs",
                .container => "Panel",
            };
        }

        fn defaultEvent(self: WidgetKind) []const u8 {
            return switch (self) {
                .button => "onClick",
                .text_field => "onChange",
                .slider => "onChange",
                .checkbox => "onToggle",
                .list => "onSelect",
                else => "",
            };
        }
    };

    pub const WidgetSpec = struct {
        kind: WidgetKind,
        name: [32]u8 = [_]u8{0} ** 32,
        name_len: usize = 0,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        text: [48]u8 = [_]u8{0} ** 48,
        text_len: usize = 0,
        event_selector: [32]u8 = [_]u8{0} ** 32,
        event_len: usize = 0,
    };

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !ViewComposerTab {
        const pane = computeLayout(rect);

        var palette_list = try ListView.init(allocator, pane.palette_rect.inner(1));
        palette_list.state.title = "Palette";

        var widget_list = try ListView.init(allocator, pane.widget_list_rect.inner(1));
        widget_list.state.title = "Widgets";

        var code_preview = try TextArea.init(allocator, pane.code_rect.inner(1));
        code_preview.state.border = false;
        code_preview.readonly = true;
        code_preview.line_numbers = true;

        var tab = ViewComposerTab{
            .allocator = allocator,
            .rect = rect,
            .palette_list = palette_list,
            .widget_list = widget_list,
            .code_preview = code_preview,
            .canvas_rect = pane.canvas_rect,
            .properties_rect = pane.properties_rect,
            .widgets = .empty,
            .generated_code = try allocator.alloc(u8, 0),
        };

        tab.setFixedText(&tab.window_name, &tab.window_name_len, "Main Window");
        tab.setFixedText(&tab.class_name, &tab.class_name_len, "ComposedWindowPresenter");
        tab.setStatus("Composer ready. Click canvas to add selected widget.");
        try tab.populatePalette();
        tab.updateCodePreview();

        return tab;
    }

    pub fn deinit(self: *ViewComposerTab) void {
        self.palette_list.deinit();
        self.widget_list.deinit();
        self.code_preview.deinit();
        self.widgets.deinit(self.allocator);
        self.allocator.free(self.generated_code);
    }

    pub fn getGeneratedCode(self: *ViewComposerTab, allocator: std.mem.Allocator) ![]u8 {
        return allocator.dupe(u8, self.generated_code);
    }

    pub fn updateRect(self: *ViewComposerTab, rect: Rect) void {
        self.rect = rect;
        const pane = computeLayout(rect);
        self.canvas_rect = pane.canvas_rect;
        self.properties_rect = pane.properties_rect;
        self.palette_list.state.rect = pane.palette_rect.inner(1);
        self.widget_list.state.rect = pane.widget_list_rect.inner(1);
        self.code_preview.state.rect = pane.code_rect.inner(1);
    }

    fn computeLayout(rect: Rect) struct {
        palette_rect: Rect,
        canvas_rect: Rect,
        widget_list_rect: Rect,
        properties_rect: Rect,
        code_rect: Rect,
    } {
        const outer = rect.inner(1);
        const split_left = outer.splitVerticalFixed(@min(24, outer.width));
        const right_width = split_left.right.width;
        const canvas_width = if (right_width > 34) right_width - 34 else right_width;
        const split_right = split_left.right.splitVerticalFixed(canvas_width);
        const right_column = split_right.right;

        const right_top = right_column.splitHorizontalFixed(@min(10, right_column.height));
        const right_mid = right_top.bottom.splitHorizontalFixed(@min(10, right_top.bottom.height));

        return .{
            .palette_rect = split_left.left,
            .canvas_rect = split_right.left,
            .widget_list_rect = right_top.top,
            .properties_rect = right_mid.top,
            .code_rect = right_mid.bottom,
        };
    }

    fn populatePalette(self: *ViewComposerTab) !void {
        self.palette_list.clear();
        for (palette_names) |name| {
            try self.palette_list.addItem(name, null);
        }
        self.palette_list.selectIndex(0);
        self.selected_palette = 0;
    }

    fn setFixedText(_: *ViewComposerTab, buffer: []u8, len_ptr: *usize, text: []const u8) void {
        const len = @min(buffer.len, text.len);
        @memcpy(buffer[0..len], text[0..len]);
        len_ptr.* = len;
    }

    fn setStatus(self: *ViewComposerTab, msg: []const u8) void {
        const len = @min(self.status_message.len, msg.len);
        @memcpy(self.status_message[0..len], msg[0..len]);
        self.status_len = len;
    }

    fn refreshWidgetList(self: *ViewComposerTab) void {
        self.widget_list.clear();
        for (self.widgets.items, 0..) |spec, i| {
            var line: [128]u8 = undefined;
            const text = std.fmt.bufPrint(&line, "{d}. {s} ({d},{d}) {d}x{d}", .{
                i + 1,
                spec.name[0..spec.name_len],
                spec.x,
                spec.y,
                spec.width,
                spec.height,
            }) catch "<widget>";
            self.widget_list.addItem(text, null) catch {};
        }

        if (self.widgets.items.len == 0) {
            self.selected_widget = null;
            return;
        }

        if (self.selected_widget) |idx| {
            const bounded = @min(idx, self.widgets.items.len - 1);
            self.selected_widget = bounded;
            self.widget_list.selectIndex(bounded);
        } else {
            self.selected_widget = 0;
            self.widget_list.selectIndex(0);
        }
    }

    fn updateCodePreview(self: *ViewComposerTab) void {
        var out: std.ArrayList(u8) = .empty;
        defer out.deinit(self.allocator);
        var escaped_title: [128]u8 = undefined;

        out.writer(self.allocator).print(
            "\"Generated by ZigSmalltalk View Composer\"\nObject subclass: #{s}\n    instanceVariableNames: '",
            .{self.class_name[0..self.class_name_len]},
        ) catch {};
        self.appendInstanceVars(&out);
        out.writer(self.allocator).writeAll("'\n    classVariableNames: ''\n    poolDictionaries: ''!\n\n") catch {};

        out.writer(self.allocator).print("!{s} methodsFor!\n", .{self.class_name[0..self.class_name_len]}) catch {};
        out.writer(self.allocator).writeAll("buildView\n") catch {};
        out.writer(self.allocator).print(
            "    | shell |\n    shell := ComposerShell title: '{s}'.\n",
            .{escapeSmalltalkString(self.window_name[0..self.window_name_len], &escaped_title)},
        ) catch {};

        for (self.widgets.items) |spec| {
            self.appendWidgetBuilder(&out, spec);
        }

        out.writer(self.allocator).writeAll("    self bindEvents.\n    ^shell!\n\n") catch {};

        out.writer(self.allocator).writeAll("bindEvents\n") catch {};
        var emitted_binding = false;
        for (self.widgets.items) |spec| {
            if (spec.event_len == 0) continue;
            emitted_binding = true;
            var handler_buf: [96]u8 = undefined;
            const handler = makeHandlerName(spec, &handler_buf);
            out.writer(self.allocator).print(
                "    {s} when: #{s} send: #{s} to: self.\n",
                .{ spec.name[0..spec.name_len], spec.event_selector[0..spec.event_len], handler },
            ) catch {};
        }
        if (!emitted_binding) {
            out.writer(self.allocator).writeAll("    \"No event wiring yet\".\n") catch {};
        }
        out.writer(self.allocator).writeAll("    ^self!\n\n") catch {};

        for (self.widgets.items) |spec| {
            if (spec.event_len == 0) continue;
            var handler_buf: [96]u8 = undefined;
            const handler = makeHandlerName(spec, &handler_buf);
            out.writer(self.allocator).print(
                "{s}\n    \"TODO: event handler for {s} ({s})\"\n    ^self!\n\n",
                .{ handler, spec.name[0..spec.name_len], spec.event_selector[0..spec.event_len] },
            ) catch {};
        }

        out.writer(self.allocator).writeAll(
            "open\n    ^self new buildView open!\n\nrun\n    | shell |\n    shell := self new buildView.\n    shell open.\n    shell runNativeLoop.\n    ^shell!\n\n!\n",
        ) catch {};

        const owned = out.toOwnedSlice(self.allocator) catch {
            self.setStatus("Failed to generate code");
            return;
        };

        self.allocator.free(self.generated_code);
        self.generated_code = owned;
        self.code_preview.setText(self.generated_code) catch {};
    }

    fn saveSpec(self: *ViewComposerTab) void {
        var file = std.fs.cwd().createFile("composer-spec.json", .{}) catch {
            self.setStatus("Save failed: cannot open composer-spec.json");
            return;
        };
        defer file.close();

        file.writeAll("{\n  \"class\": \"") catch return;
        file.writeAll(self.class_name[0..self.class_name_len]) catch return;
        file.writeAll("\",\n  \"window\": \"") catch return;
        file.writeAll(self.window_name[0..self.window_name_len]) catch return;
        file.writeAll("\",\n  \"widgets\": [\n") catch return;

        for (self.widgets.items, 0..) |spec, i| {
            var line: [512]u8 = undefined;
            const text = std.fmt.bufPrint(&line,
                "    {{\"kind\":\"{s}\",\"name\":\"{s}\",\"x\":{d},\"y\":{d},\"width\":{d},\"height\":{d},\"text\":\"{s}\",\"event\":\"{s}\"}}{s}\n",
                .{
                    spec.kind.displayName(),
                    spec.name[0..spec.name_len],
                    spec.x,
                    spec.y,
                    spec.width,
                    spec.height,
                    spec.text[0..spec.text_len],
                    spec.event_selector[0..spec.event_len],
                    if (i + 1 < self.widgets.items.len) "," else "",
                },
            ) catch {
                self.setStatus("Save failed: format error");
                return;
            };
            file.writeAll(text) catch {
                self.setStatus("Save failed: write error");
                return;
            };
        }

        file.writeAll("  ]\n}\n") catch {
            self.setStatus("Save failed: write error");
            return;
        };
        self.setStatus("Saved composer-spec.json");
    }

    fn parseWidgetKind(kind: []const u8) WidgetKind {
        if (std.mem.eql(u8, kind, "Button")) return .button;
        if (std.mem.eql(u8, kind, "Label")) return .label;
        if (std.mem.eql(u8, kind, "TextField")) return .text_field;
        if (std.mem.eql(u8, kind, "Slider")) return .slider;
        if (std.mem.eql(u8, kind, "Checkbox")) return .checkbox;
        if (std.mem.eql(u8, kind, "List")) return .list;
        if (std.mem.eql(u8, kind, "Tabs")) return .tabs;
        return .container;
    }

    fn loadSpec(self: *ViewComposerTab) void {
        const content = std.fs.cwd().readFileAlloc(self.allocator, "composer-spec.json", 1024 * 1024) catch {
            self.setStatus("Load failed: composer-spec.json not found");
            return;
        };
        defer self.allocator.free(content);

        const parsed = std.json.parseFromSlice(std.json.Value, self.allocator, content, .{}) catch {
            self.setStatus("Load failed: invalid JSON");
            return;
        };
        defer parsed.deinit();

        if (parsed.value != .object) {
            self.setStatus("Load failed: invalid root object");
            return;
        }

        const root = parsed.value.object;

        if (root.get("class")) |v| {
            if (v == .string) self.sanitizeIdentifierInto(v.string, &self.class_name, &self.class_name_len);
        }
        if (root.get("window")) |v| {
            if (v == .string) self.setFixedText(&self.window_name, &self.window_name_len, v.string);
        }

        self.widgets.clearRetainingCapacity();
        self.selected_widget = null;
        self.next_widget_id = 1;

        if (root.get("widgets")) |widgets_val| {
            if (widgets_val == .array) {
                for (widgets_val.array.items) |wv| {
                    if (wv != .object) continue;
                    const obj = wv.object;

                    var spec = WidgetSpec{
                        .kind = .button,
                        .x = 2,
                        .y = 2,
                        .width = 12,
                        .height = 2,
                    };

                    if (obj.get("kind")) |v| {
                        if (v == .string) spec.kind = parseWidgetKind(v.string);
                    }
                    if (obj.get("name")) |v| {
                        if (v == .string) self.sanitizeIdentifierInto(v.string, &spec.name, &spec.name_len);
                    }
                    if (obj.get("x")) |v| {
                        if (v == .integer) spec.x = @intCast(@max(0, v.integer));
                    }
                    if (obj.get("y")) |v| {
                        if (v == .integer) spec.y = @intCast(@max(0, v.integer));
                    }
                    if (obj.get("width")) |v| {
                        if (v == .integer) spec.width = @intCast(@max(1, v.integer));
                    }
                    if (obj.get("height")) |v| {
                        if (v == .integer) spec.height = @intCast(@max(1, v.integer));
                    }
                    if (obj.get("text")) |v| {
                        if (v == .string) self.setFixedText(&spec.text, &spec.text_len, v.string);
                    }
                    if (obj.get("event")) |v| {
                        if (v == .string) self.sanitizeIdentifierInto(v.string, &spec.event_selector, &spec.event_len);
                    }

                    if (spec.name_len == 0) {
                        var name_buf: [32]u8 = undefined;
                        const name = std.fmt.bufPrint(&name_buf, "{s}{d}", .{ spec.kind.displayName(), self.next_widget_id }) catch "Widget";
                        self.setFixedText(&spec.name, &spec.name_len, name);
                    }
                    self.next_widget_id += 1;
                    self.widgets.append(self.allocator, spec) catch {};
                }
            }
        }

        self.refreshWidgetList();
        self.updateCodePreview();
        self.setStatus("Loaded composer-spec.json");
    }

    fn appendInstanceVars(self: *ViewComposerTab, out: *std.ArrayList(u8)) void {
        for (self.widgets.items, 0..) |spec, i| {
            out.writer(self.allocator).writeAll(spec.name[0..spec.name_len]) catch {};
            if (i + 1 < self.widgets.items.len) {
                out.writer(self.allocator).writeByte(' ') catch {};
            }
        }
    }

    fn makeHandlerName(spec: WidgetSpec, buf: *[96]u8) []const u8 {
        const suffix = switch (spec.kind) {
            .button => "Clicked",
            .checkbox => "Toggled",
            .slider => "Changed",
            .text_field => "Changed",
            .list => "Selected",
            else => "Changed",
        };

        const n: usize = @min(spec.name_len, buf.len);
        @memcpy(buf[0..n], spec.name[0..n]);
        var pos: usize = n;
        const rem: usize = buf.len - pos;
        const suffix_take: usize = @min(suffix.len, rem);
        @memcpy(buf[pos .. pos + suffix_take], suffix[0..suffix_take]);
        pos += suffix_take;
        return buf[0..pos];
    }

    fn appendWidgetBuilder(self: *ViewComposerTab, out: *std.ArrayList(u8), spec: WidgetSpec) void {
        const class_name = switch (spec.kind) {
            .button => "ComposerButton",
            .label => "ComposerLabel",
            .text_field => "ComposerTextField",
            .slider => "ComposerSlider",
            .checkbox => "ComposerCheckbox",
            .list => "ComposerList",
            .tabs => "ComposerTabs",
            .container => "ComposerContainer",
        };
        var escaped_name: [96]u8 = undefined;
        var escaped_label: [96]u8 = undefined;

        out.writer(self.allocator).print(
            "    {s} := {s} new\n        name: '{s}';\n        bounds: #({d} {d} {d} {d});\n        label: '{s}';\n        yourself.\n    shell add: {s}.\n",
            .{
                spec.name[0..spec.name_len],
                class_name,
                escapeSmalltalkString(spec.name[0..spec.name_len], &escaped_name),
                spec.x,
                spec.y,
                spec.width,
                spec.height,
                escapeSmalltalkString(spec.text[0..spec.text_len], &escaped_label),
                spec.name[0..spec.name_len],
            },
        ) catch {};
    }

    fn escapeSmalltalkString(src: []const u8, out: []u8) []const u8 {
        var pos: usize = 0;
        for (src) |c| {
            if (c == '\'') {
                if (pos + 2 > out.len) break;
                out[pos] = '\'';
                out[pos + 1] = '\'';
                pos += 2;
            } else {
                if (pos + 1 > out.len) break;
                out[pos] = c;
                pos += 1;
            }
        }
        return out[0..pos];
    }

    fn addWidget(self: *ViewComposerTab, kind: WidgetKind, x: u16, y: u16) void {
        var spec = WidgetSpec{
            .kind = kind,
            .x = x,
            .y = y,
            .width = kind.defaultSize().w,
            .height = kind.defaultSize().h,
        };

        var name_buf: [32]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buf, "{s}{d}", .{ kind.displayName(), self.next_widget_id }) catch "Widget";
        self.next_widget_id += 1;

        self.setFixedText(&spec.name, &spec.name_len, name);
        self.setFixedText(&spec.text, &spec.text_len, kind.defaultText());
        self.setFixedText(&spec.event_selector, &spec.event_len, kind.defaultEvent());

        self.widgets.append(self.allocator, spec) catch {
            self.setStatus("Failed to add widget (out of memory)");
            return;
        };
        self.selected_widget = self.widgets.items.len - 1;
        self.refreshWidgetList();
        self.updateCodePreview();
        self.setStatus("Widget added");
    }

    fn addWidgetAtCanvas(self: *ViewComposerTab, mouse_x: u16, mouse_y: u16) void {
        const canvas = self.canvas_rect.inner(1);
        if (!canvas.contains(mouse_x, mouse_y)) return;

        const kind = WidgetKind.fromPaletteIndex(self.selected_palette);
        const rel_x = mouse_x - canvas.x;
        const rel_y = mouse_y - canvas.y;
        self.addWidget(kind, rel_x, rel_y);
    }

    fn deleteSelectedWidget(self: *ViewComposerTab) void {
        if (self.selected_widget) |idx| {
            if (idx < self.widgets.items.len) {
                _ = self.widgets.orderedRemove(idx);
                if (self.widgets.items.len == 0) {
                    self.selected_widget = null;
                } else if (idx >= self.widgets.items.len) {
                    self.selected_widget = self.widgets.items.len - 1;
                } else {
                    self.selected_widget = idx;
                }
                self.refreshWidgetList();
                self.updateCodePreview();
                self.setStatus("Widget removed");
            }
        }
    }

    fn clampPosition(spec: *WidgetSpec, max_w: u16, max_h: u16) void {
        if (spec.x > max_w) spec.x = max_w;
        if (spec.y > max_h) spec.y = max_h;
    }

    fn moveSelected(self: *ViewComposerTab, dx: i16, dy: i16) void {
        if (self.selected_widget) |idx| {
            if (idx >= self.widgets.items.len) return;
            var spec = &self.widgets.items[idx];

            const new_x_signed = @as(i32, @intCast(spec.x)) + dx;
            const new_y_signed = @as(i32, @intCast(spec.y)) + dy;

            spec.x = @intCast(@max(0, new_x_signed));
            spec.y = @intCast(@max(0, new_y_signed));
            const canvas = self.canvas_rect.inner(1);
            clampPosition(spec, canvas.width -| 1, canvas.height -| 1);

            self.refreshWidgetList();
            self.updateCodePreview();
        }
    }

    fn resizeSelected(self: *ViewComposerTab, dw: i16, dh: i16) void {
        if (self.selected_widget) |idx| {
            if (idx >= self.widgets.items.len) return;
            var spec = &self.widgets.items[idx];

            const new_w = @as(i32, @intCast(spec.width)) + dw;
            const new_h = @as(i32, @intCast(spec.height)) + dh;

            spec.width = @intCast(@max(1, new_w));
            spec.height = @intCast(@max(1, new_h));

            self.refreshWidgetList();
            self.updateCodePreview();
        }
    }

    fn cycleEventSelector(self: *ViewComposerTab) void {
        if (self.selected_widget) |idx| {
            if (idx >= self.widgets.items.len) return;
            var spec = &self.widgets.items[idx];

            const current = spec.event_selector[0..spec.event_len];
            if (std.mem.eql(u8, current, "")) {
                self.setFixedText(&spec.event_selector, &spec.event_len, "onClick");
            } else if (std.mem.eql(u8, current, "onClick")) {
                self.setFixedText(&spec.event_selector, &spec.event_len, "onChange");
            } else if (std.mem.eql(u8, current, "onChange")) {
                self.setFixedText(&spec.event_selector, &spec.event_len, "onToggle");
            } else if (std.mem.eql(u8, current, "onToggle")) {
                self.setFixedText(&spec.event_selector, &spec.event_len, "onSelect");
            } else {
                self.setFixedText(&spec.event_selector, &spec.event_len, "");
            }

            self.updateCodePreview();
            self.setStatus("Event selector cycled");
        }
    }

    fn startEdit(self: *ViewComposerTab, mode: EditMode, initial: []const u8) void {
        self.edit_mode = mode;
        self.edit_len = @min(initial.len, self.edit_buffer.len);
        @memcpy(self.edit_buffer[0..self.edit_len], initial[0..self.edit_len]);
        self.setStatus("Editing... Enter to apply, Esc to cancel");
    }

    fn beginEdit(self: *ViewComposerTab, mode: EditMode) void {
        switch (mode) {
            .class_name => self.startEdit(.class_name, self.class_name[0..self.class_name_len]),
            .window_name => self.startEdit(.window_name, self.window_name[0..self.window_name_len]),
            .widget_name, .widget_text, .widget_event => {
                if (self.selected_widget) |idx| {
                    if (idx >= self.widgets.items.len) return;
                    const spec = self.widgets.items[idx];
                    const txt = switch (mode) {
                        .widget_name => spec.name[0..spec.name_len],
                        .widget_text => spec.text[0..spec.text_len],
                        .widget_event => spec.event_selector[0..spec.event_len],
                        else => "",
                    };
                    self.startEdit(mode, txt);
                }
            },
            else => {},
        }
    }

    fn commitEdit(self: *ViewComposerTab) void {
        const edited = self.edit_buffer[0..self.edit_len];

        switch (self.edit_mode) {
            .class_name => {
                self.sanitizeIdentifierInto(edited, &self.class_name, &self.class_name_len);
            },
            .window_name => {
                self.setFixedText(&self.window_name, &self.window_name_len, edited);
            },
            .widget_name => {
                if (self.selected_widget) |idx| {
                    if (idx < self.widgets.items.len) {
                        self.sanitizeIdentifierInto(edited, &self.widgets.items[idx].name, &self.widgets.items[idx].name_len);
                    }
                }
            },
            .widget_text => {
                if (self.selected_widget) |idx| {
                    if (idx < self.widgets.items.len) {
                        self.setFixedText(&self.widgets.items[idx].text, &self.widgets.items[idx].text_len, edited);
                    }
                }
            },
            .widget_event => {
                if (self.selected_widget) |idx| {
                    if (idx < self.widgets.items.len) {
                        self.sanitizeIdentifierInto(edited, &self.widgets.items[idx].event_selector, &self.widgets.items[idx].event_len);
                    }
                }
            },
            else => {},
        }

        self.edit_mode = .none;
        self.refreshWidgetList();
        self.updateCodePreview();
        self.setStatus("Edit applied");
    }

    fn cancelEdit(self: *ViewComposerTab) void {
        self.edit_mode = .none;
        self.setStatus("Edit canceled");
    }

    fn sanitizeIdentifierInto(self: *ViewComposerTab, src: []const u8, dest: []u8, len_ptr: *usize) void {
        var pos: usize = 0;
        for (src) |c| {
            if (pos >= dest.len) break;
            if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_') {
                dest[pos] = c;
                pos += 1;
            } else if (c == ' ' or c == '-') {
                dest[pos] = '_';
                pos += 1;
            }
        }
        if (pos == 0) {
            self.setFixedText(dest, len_ptr, "item");
        } else {
            len_ptr.* = pos;
        }
    }

    fn handleEditKey(self: *ViewComposerTab, key: Key) EventResult {
        switch (key) {
            .escape => {
                self.cancelEdit();
                return .consumed;
            },
            .enter => {
                self.commitEdit();
                return .consumed;
            },
            .backspace => {
                if (self.edit_len > 0) self.edit_len -= 1;
                return .consumed;
            },
            .char => |c| {
                if (c >= 32 and c < 127 and self.edit_len < self.edit_buffer.len) {
                    self.edit_buffer[self.edit_len] = @intCast(c);
                    self.edit_len += 1;
                }
                return .consumed;
            },
            else => return .consumed,
        }
    }

    pub fn handleKey(self: *ViewComposerTab, key: Key) EventResult {
        if (self.edit_mode != .none) {
            return self.handleEditKey(key);
        }

        // Pane-local key routing first.
        switch (self.active_pane) {
            .palette => {
                if (self.palette_list.handleKey(key) == .consumed) {
                    if (self.palette_list.selected_index) |idx| {
                        self.selected_palette = idx;
                    }
                    return .consumed;
                }
            },
            .widgets => {
                if (self.widget_list.handleKey(key) == .consumed) {
                    self.selected_widget = self.widget_list.selected_index;
                    return .consumed;
                }
            },
            .code => {
                if (self.code_preview.handleKey(key) == .consumed) {
                    return .consumed;
                }
            },
            else => {},
        }

        switch (key) {
            .ctrl => |c| {
                switch (c) {
                    7 => { // Ctrl+G
                        if (self.on_export_code) |cb| cb(self, self.generated_code);
                        self.setStatus("Generated code exported");
                        return .consumed;
                    },
                    18 => { // Ctrl+R
                        if (self.on_run_code) |cb| cb(self, self.generated_code);
                        self.setStatus("Generated code executed");
                        return .consumed;
                    },
                    else => {},
                }
            },
            .tab => {
                self.active_pane = switch (self.active_pane) {
                    .palette => .canvas,
                    .canvas => .widgets,
                    .widgets => .code,
                    .code => .palette,
                };
                self.setStatus("Focus switched");
                return .consumed;
            },
            .shift_tab => {
                self.active_pane = switch (self.active_pane) {
                    .palette => .code,
                    .canvas => .palette,
                    .widgets => .canvas,
                    .code => .widgets,
                };
                self.setStatus("Focus switched");
                return .consumed;
            },
            .enter => {
                if (self.active_pane == .palette) {
                    const kind = WidgetKind.fromPaletteIndex(self.selected_palette);
                    self.addWidget(kind, 2, 2);
                    return .consumed;
                }
            },
            .delete, .backspace => {
                self.deleteSelectedWidget();
                return .consumed;
            },
            .left => {
                self.moveSelected(-1, 0);
                return .consumed;
            },
            .right => {
                self.moveSelected(1, 0);
                return .consumed;
            },
            .up => {
                self.moveSelected(0, -1);
                return .consumed;
            },
            .down => {
                self.moveSelected(0, 1);
                return .consumed;
            },
            .char => |c| {
                switch (c) {
                    'a', 'A' => {
                        const kind = WidgetKind.fromPaletteIndex(self.selected_palette);
                        self.addWidget(kind, 2, 2);
                        return .consumed;
                    },
                    'x', 'X' => {
                        self.deleteSelectedWidget();
                        return .consumed;
                    },
                    '+' => {
                        self.resizeSelected(1, 0);
                        return .consumed;
                    },
                    '-' => {
                        self.resizeSelected(-1, 0);
                        return .consumed;
                    },
                    ']',
                    '>' => {
                        self.resizeSelected(0, 1);
                        return .consumed;
                    },
                    '[',
                    '<' => {
                        self.resizeSelected(0, -1);
                        return .consumed;
                    },
                    'e', 'E' => {
                        self.cycleEventSelector();
                        return .consumed;
                    },
                    'g', 'G' => {
                        if (self.on_export_code) |cb| cb(self, self.generated_code);
                        self.setStatus("Generated code exported");
                        return .consumed;
                    },
                    'r', 'R' => {
                        if (self.on_run_code) |cb| cb(self, self.generated_code);
                        self.setStatus("Generated code executed");
                        return .consumed;
                    },
                    's', 'S' => {
                        self.saveSpec();
                        return .consumed;
                    },
                    'l', 'L' => {
                        self.loadSpec();
                        return .consumed;
                    },
                    'n', 'N' => {
                        self.beginEdit(.widget_name);
                        return .consumed;
                    },
                    't', 'T' => {
                        self.beginEdit(.widget_text);
                        return .consumed;
                    },
                    'c', 'C' => {
                        self.beginEdit(.class_name);
                        return .consumed;
                    },
                    'w', 'W' => {
                        self.beginEdit(.window_name);
                        return .consumed;
                    },
                    'v', 'V' => {
                        self.beginEdit(.widget_event);
                        return .consumed;
                    },
                    else => {},
                }
            },
            else => {},
        }

        return .ignored;
    }

    pub fn handleMouse(self: *ViewComposerTab, mouse: MouseEvent) void {
        // Drag move handling.
        if (self.drag_widget != null and (mouse.event_type == .drag or mouse.event_type == .move)) {
            self.dragSelectedTo(mouse.x, mouse.y);
            return;
        }
        if (mouse.event_type == .release) {
            self.drag_widget = null;
            return;
        }

        // Scroll code preview when hovering that pane.
        if (self.code_preview.state.rect.contains(mouse.x, mouse.y)) {
            _ = self.code_preview.handleMouse(mouse);
            if (mouse.button == .scroll_up or mouse.button == .scroll_down) return;
        }

        if (mouse.event_type != .press) return;

        if (mouse.button == .left) {
            if (self.palette_list.state.rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .palette;
                self.selectFromListClick(&self.palette_list, mouse.y);
                if (self.palette_list.selected_index) |idx| {
                    self.selected_palette = idx;
                }
                return;
            }

            if (self.widget_list.state.rect.contains(mouse.x, mouse.y)) {
                self.active_pane = .widgets;
                self.selectFromListClick(&self.widget_list, mouse.y);
                self.selected_widget = self.widget_list.selected_index;
                return;
            }

            if (self.canvas_rect.inner(1).contains(mouse.x, mouse.y)) {
                self.active_pane = .canvas;
                if (self.selectWidgetAt(mouse.x, mouse.y)) |idx| {
                    self.selected_widget = idx;
                    self.widget_list.selectIndex(idx);
                    self.startDrag(idx, mouse.x, mouse.y);
                } else {
                    self.addWidgetAtCanvas(mouse.x, mouse.y);
                }
                return;
            }
        }

        if (mouse.button == .scroll_up and self.widget_list.state.rect.contains(mouse.x, mouse.y)) {
            _ = self.widget_list.handleKey(.page_up);
            self.selected_widget = self.widget_list.selected_index;
        } else if (mouse.button == .scroll_down and self.widget_list.state.rect.contains(mouse.x, mouse.y)) {
            _ = self.widget_list.handleKey(.page_down);
            self.selected_widget = self.widget_list.selected_index;
        }
    }

    fn startDrag(self: *ViewComposerTab, idx: usize, mouse_x: u16, mouse_y: u16) void {
        if (idx >= self.widgets.items.len) return;
        const canvas = self.canvas_rect.inner(1);
        const spec = self.widgets.items[idx];
        const local_x = @as(i16, @intCast(mouse_x - canvas.x));
        const local_y = @as(i16, @intCast(mouse_y - canvas.y));
        self.drag_widget = idx;
        self.drag_offset_x = local_x - @as(i16, @intCast(spec.x));
        self.drag_offset_y = local_y - @as(i16, @intCast(spec.y));
    }

    fn dragSelectedTo(self: *ViewComposerTab, mouse_x: u16, mouse_y: u16) void {
        const idx = self.drag_widget orelse return;
        if (idx >= self.widgets.items.len) return;
        const canvas = self.canvas_rect.inner(1);
        if (!canvas.contains(mouse_x, mouse_y)) return;

        var spec = &self.widgets.items[idx];
        const local_x = @as(i32, @intCast(mouse_x - canvas.x));
        const local_y = @as(i32, @intCast(mouse_y - canvas.y));

        const target_x = local_x - self.drag_offset_x;
        const target_y = local_y - self.drag_offset_y;

        spec.x = @intCast(@max(0, target_x));
        spec.y = @intCast(@max(0, target_y));
        clampPosition(spec, canvas.width -| 1, canvas.height -| 1);

        self.refreshWidgetList();
        self.updateCodePreview();
    }

    fn selectFromListClick(self: *ViewComposerTab, list: *ListView, mouse_y: u16) void {
        const content = list.state.contentRect();
        if (mouse_y < content.y or mouse_y >= content.y + content.height) return;
        const row = mouse_y - content.y;
        const idx = list.scroll_offset + row;
        if (idx < list.items.items.len) {
            list.selectIndex(idx);
        }
        _ = self;
    }

    fn selectWidgetAt(self: *ViewComposerTab, mouse_x: u16, mouse_y: u16) ?usize {
        const canvas = self.canvas_rect.inner(1);
        if (!canvas.contains(mouse_x, mouse_y)) return null;

        const local_x = mouse_x - canvas.x;
        const local_y = mouse_y - canvas.y;

        var found: ?usize = null;
        for (self.widgets.items, 0..) |spec, i| {
            if (local_x >= spec.x and local_x < spec.x + spec.width and local_y >= spec.y and local_y < spec.y + spec.height) {
                found = i;
            }
        }

        if (found != null) self.setStatus("Widget selected");
        return found;
    }

    pub fn draw(self: *ViewComposerTab, screen: *Screen) void {
        const rect = self.rect;
        widget.drawBorderRounded(screen, rect, self.focused);
        widget.drawTitle(screen, rect, "View Composer", self.focused);

        const pane = computeLayout(rect);
        self.canvas_rect = pane.canvas_rect;
        self.properties_rect = pane.properties_rect;

        self.drawPaneFrame(screen, pane.palette_rect, "Palette", self.active_pane == .palette);
        self.drawPaneFrame(screen, pane.canvas_rect, "Shell Canvas", self.active_pane == .canvas);
        self.drawPaneFrame(screen, pane.widget_list_rect, "Widgets", self.active_pane == .widgets);
        self.drawPaneFrame(screen, pane.properties_rect, "Properties", false);
        self.drawPaneFrame(screen, pane.code_rect, "Generated Smalltalk", self.active_pane == .code);

        self.palette_list.state.rect = pane.palette_rect.inner(1);
        self.palette_list.state.focused = self.focused and self.active_pane == .palette;
        self.palette_list.draw(screen);

        self.widget_list.state.rect = pane.widget_list_rect.inner(1);
        self.widget_list.state.focused = self.focused and self.active_pane == .widgets;
        self.widget_list.draw(screen);

        self.drawCanvas(screen, pane.canvas_rect.inner(1));
        self.drawProperties(screen, pane.properties_rect.inner(1));

        self.code_preview.state.rect = pane.code_rect.inner(1);
        self.code_preview.state.focused = self.focused and self.active_pane == .code;
        self.code_preview.draw(screen);

        const help_y = rect.y + rect.height -| 3;
        screen.fillRect(rect.x + 1, help_y, rect.width -| 2, 1, ' ', style_mod.styles.status);
        screen.drawTextClipped(
            rect.x + 2,
            help_y,
            "Tab focus | Click add/drag | Arrows move | +/- width | [] height | N/T/V edit | S save | L load | G export | R run",
            rect.width -| 4,
            style_mod.styles.status,
        );

        const status_y = rect.y + rect.height -| 2;
        screen.fillRect(rect.x + 1, status_y, rect.width -| 2, 1, ' ', style_mod.styles.status);

        if (self.edit_mode != .none) {
            var prompt: [170]u8 = undefined;
            const field = switch (self.edit_mode) {
                .class_name => "class",
                .window_name => "window",
                .widget_name => "widget name",
                .widget_text => "widget label",
                .widget_event => "widget event",
                else => "",
            };
            const line = std.fmt.bufPrint(&prompt, "Editing {s}: {s}", .{ field, self.edit_buffer[0..self.edit_len] }) catch "Editing";
            screen.drawTextClipped(rect.x + 2, status_y, line, rect.width -| 4, style_mod.styles.status);
        } else {
            screen.drawTextClipped(rect.x + 2, status_y, self.status_message[0..self.status_len], rect.width -| 4, style_mod.styles.status);
        }
    }

    fn drawPaneFrame(_: *ViewComposerTab, screen: *Screen, pane_rect: Rect, title: []const u8, focused: bool) void {
        widget.drawBorder(screen, pane_rect, focused);
        widget.drawTitle(screen, pane_rect, title, focused);
    }

    fn drawCanvas(self: *ViewComposerTab, screen: *Screen, canvas: Rect) void {
        widget.clearContent(screen, canvas);

        // Subtle grid for positioning feedback.
        var y: u16 = 0;
        while (y < canvas.height) : (y += 2) {
            var x: u16 = 0;
            while (x < canvas.width) : (x += 4) {
                screen.setCell(canvas.x + x, canvas.y + y, '.', style_mod.styles.dim);
            }
        }

        for (self.widgets.items, 0..) |spec, i| {
            const selected = self.selected_widget != null and self.selected_widget.? == i;
            self.drawWidgetSpec(screen, canvas, spec, selected);
        }
    }

    fn drawWidgetSpec(_: *ViewComposerTab, screen: *Screen, canvas: Rect, spec: WidgetSpec, selected: bool) void {
        const x = canvas.x + spec.x;
        const y = canvas.y + spec.y;
        const w = @max(@as(u16, 1), spec.width);
        const h = @max(@as(u16, 1), spec.height);

        if (x >= canvas.x + canvas.width or y >= canvas.y + canvas.height) return;

        const style = if (selected) style_mod.styles.selected else style_mod.styles.normal;

        if (h > 1 and w > 1) {
            screen.drawBox(x, y, @min(w, canvas.width - (x - canvas.x)), @min(h, canvas.height - (y - canvas.y)), style);
        } else {
            const fill_w = @min(w, canvas.width - (x - canvas.x));
            screen.fillRect(x, y, fill_w, 1, '-', style);
        }

        var label: [96]u8 = undefined;
        const text = std.fmt.bufPrint(&label, "{s}:{s}", .{ spec.kind.displayName(), spec.name[0..spec.name_len] }) catch spec.name[0..spec.name_len];
        if (h > 1 and w > 2) {
            screen.drawTextClipped(x + 1, y + @min(@as(u16, 1), h - 1), text, w - 1, style);
        } else {
            screen.drawTextClipped(x, y, text, w, style);
        }
    }

    fn drawProperties(self: *ViewComposerTab, screen: *Screen, props: Rect) void {
        widget.clearContent(screen, props);

        const normal = style_mod.styles.normal;
        const dim = style_mod.styles.dim;

        screen.drawText(props.x, props.y, "Class:", dim);
        screen.drawTextClipped(props.x + 7, props.y, self.class_name[0..self.class_name_len], props.width -| 7, normal);
        screen.drawText(props.x, props.y + 1, "Window:", dim);
        screen.drawTextClipped(props.x + 8, props.y + 1, self.window_name[0..self.window_name_len], props.width -| 8, normal);

        if (self.selected_widget == null or self.selected_widget.? >= self.widgets.items.len) {
            screen.drawText(props.x, props.y + 3, "Select a widget to edit bounds/events.", dim);
            return;
        }

        const spec = self.widgets.items[self.selected_widget.?];
        var line: [128]u8 = undefined;

        const hdr = std.fmt.bufPrint(&line, "Name: {s}  (N edit)", .{spec.name[0..spec.name_len]}) catch "Name: <err>";
        screen.drawTextClipped(props.x, props.y + 3, hdr, props.width, normal);

        const kind_line = std.fmt.bufPrint(&line, "Type: {s}", .{spec.kind.displayName()}) catch "Type: <err>";
        screen.drawTextClipped(props.x, props.y + 4, kind_line, props.width, normal);

        const bounds_line = std.fmt.bufPrint(&line, "Bounds: {d},{d} {d}x{d}", .{ spec.x, spec.y, spec.width, spec.height }) catch "Bounds: <err>";
        screen.drawTextClipped(props.x, props.y + 5, bounds_line, props.width, normal);

        const text_line = std.fmt.bufPrint(&line, "Label: '{s}'  (T edit)", .{spec.text[0..spec.text_len]}) catch "Label: <err>";
        screen.drawTextClipped(props.x, props.y + 6, text_line, props.width, normal);

        const evt_line = std.fmt.bufPrint(&line, "Event: #{s}  (V edit / E cycle)", .{spec.event_selector[0..spec.event_len]}) catch "Event: <err>";
        screen.drawTextClipped(props.x, props.y + 7, evt_line, props.width, normal);
    }

    pub fn scroll(self: *ViewComposerTab, delta: i32) void {
        if (delta < 0) {
            _ = self.code_preview.handleKey(.page_up);
        } else if (delta > 0) {
            _ = self.code_preview.handleKey(.page_down);
        }
    }
};
