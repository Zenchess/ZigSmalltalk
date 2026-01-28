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

// Import VM types
const scheduler_mod = @import("../../vm/scheduler.zig");
const memory = @import("../../vm/memory.zig");
const Heap = memory.Heap;
const Value = @import("../../vm/object.zig").Value;

pub const ProcessTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,
    heap: *Heap,

    // Panes
    process_list: ListView,
    details_area: TextArea,
    stack_area: TextArea,

    // State
    active_pane: Pane = .processes,
    selected_index: usize = 0,

    // Process info cache
    process_info: std.ArrayList(ProcessInfo),

    // Status
    status_message: [128]u8 = undefined,
    status_len: usize = 0,

    // Vital process names (cannot be killed)
    vital_names: [8][]const u8 = .{
        "main",
        "idle",
        "gc",
        "scheduler",
        "",
        "",
        "",
        "",
    },
    num_vital: usize = 4,

    // Callbacks
    on_debug_process: ?*const fn (usize) void = null,

    pub const ProcessInfo = struct {
        name: [64]u8 = [_]u8{0} ** 64,
        name_len: usize = 0,
        state: []const u8,
        priority: i64,
        is_vital: bool,
        process_id: usize,
    };

    pub const Pane = enum {
        processes,
        details,
        stack,
    };

    pub fn init(allocator: std.mem.Allocator, rect: Rect, heap: *Heap) !ProcessTab {
        // Layout: left side is process list, right side has details and stack
        const left_width = rect.width / 3;
        const right_width = rect.width - left_width;
        const details_height = 8;

        const process_rect = Rect.init(rect.x, rect.y, left_width, rect.height);
        const details_rect = Rect.init(rect.x + left_width, rect.y, right_width, details_height);
        const stack_rect = Rect.init(rect.x + left_width, rect.y + details_height, right_width, rect.height - details_height);

        var process_list = try ListView.init(allocator, process_rect);
        process_list.state.title = "Processes";

        var details_area = try TextArea.init(allocator, Rect.init(
            details_rect.x + 1,
            details_rect.y + 1,
            details_rect.width -| 2,
            details_rect.height -| 2,
        ));
        details_area.state.border = false;
        details_area.readonly = true;

        var stack_area = try TextArea.init(allocator, Rect.init(
            stack_rect.x + 1,
            stack_rect.y + 1,
            stack_rect.width -| 2,
            stack_rect.height -| 2,
        ));
        stack_area.state.border = false;
        stack_area.readonly = true;

        return ProcessTab{
            .allocator = allocator,
            .rect = rect,
            .heap = heap,
            .process_list = process_list,
            .details_area = details_area,
            .stack_area = stack_area,
            .process_info = .empty,
        };
    }

    pub fn deinit(self: *ProcessTab) void {
        self.process_list.deinit();
        self.details_area.deinit();
        self.stack_area.deinit();
        self.process_info.deinit(self.allocator);
    }

    pub fn updateRect(self: *ProcessTab, rect: Rect) void {
        self.rect = rect;

        const left_width = rect.width / 3;
        const right_width = rect.width - left_width;
        const details_height = 8;

        const process_rect = Rect.init(rect.x, rect.y, left_width, rect.height);
        const details_rect = Rect.init(rect.x + left_width, rect.y, right_width, details_height);
        const stack_rect = Rect.init(rect.x + left_width, rect.y + details_height, right_width, rect.height - details_height);

        self.process_list.state.rect = process_rect;
        self.details_area.state.rect = Rect.init(
            details_rect.x + 1,
            details_rect.y + 1,
            details_rect.width -| 2,
            details_rect.height -| 2,
        );
        self.stack_area.state.rect = Rect.init(
            stack_rect.x + 1,
            stack_rect.y + 1,
            stack_rect.width -| 2,
            stack_rect.height -| 2,
        );
    }

    /// Refresh process list from scheduler
    pub fn refresh(self: *ProcessTab) void {
        self.process_list.clear();
        self.process_info.clearRetainingCapacity();

        // For now, show a placeholder since we need scheduler integration
        // In a full implementation, we'd iterate through scheduler.ready_list
        // and scheduler.waiting_list

        // Add a placeholder for the main process
        var info = ProcessInfo{
            .state = "running",
            .priority = 4,
            .is_vital = true,
            .process_id = 0,
        };
        const name = "Main Process";
        @memcpy(info.name[0..name.len], name);
        info.name_len = name.len;

        self.process_info.append(self.allocator, info) catch {};

        // Update display
        self.updateProcessList();
        self.updateDetails();
    }

    fn updateProcessList(self: *ProcessTab) void {
        self.process_list.clear();

        for (self.process_info.items, 0..) |info, i| {
            var buf: [128]u8 = undefined;
            const state_char: u8 = switch (info.state[0]) {
                'r' => 'R', // running
                's' => 'S', // suspended
                'w' => 'W', // waiting
                't' => 'T', // terminated
                else => '?',
            };
            const vital_str: []const u8 = if (info.is_vital) " (vital)" else "";
            const prefix: []const u8 = if (i == self.selected_index) "> " else "  ";
            const name_slice = info.name[0..info.name_len];

            const text = std.fmt.bufPrint(&buf, "{s}[{c}] {s}{s}", .{
                prefix,
                state_char,
                name_slice,
                vital_str,
            }) catch "<error>";

            self.process_list.addItem(text, null) catch {};
        }
    }

    fn updateDetails(self: *ProcessTab) void {
        self.details_area.setText("") catch {};
        self.stack_area.setText("") catch {};

        if (self.selected_index < self.process_info.items.len) {
            const info = self.process_info.items[self.selected_index];
            var buf: [512]u8 = undefined;

            const name_slice = info.name[0..info.name_len];
            const details = std.fmt.bufPrint(&buf,
                \\Process: {s}
                \\State: {s}
                \\Priority: {d}
                \\ID: {d}
                \\Vital: {s}
            , .{
                name_slice,
                info.state,
                info.priority,
                info.process_id,
                if (info.is_vital) "yes" else "no",
            }) catch "<error>";

            self.details_area.setText(details) catch {};

            // Stack info placeholder
            self.stack_area.setText("(Select a process and press 'D' to debug)") catch {};
        }
    }

    fn isVital(self: *ProcessTab, name: []const u8) bool {
        for (self.vital_names[0..self.num_vital]) |vital| {
            if (std.mem.eql(u8, name, vital)) {
                return true;
            }
        }
        return false;
    }

    fn setStatus(self: *ProcessTab, msg: []const u8) void {
        const len = @min(msg.len, self.status_message.len);
        @memcpy(self.status_message[0..len], msg[0..len]);
        self.status_len = len;
    }

    pub fn handleKey(self: *ProcessTab, key: Key) bool {
        switch (key) {
            .char => |c| {
                switch (c) {
                    'r', 'R' => {
                        self.refresh();
                        self.setStatus("Refreshed");
                        return true;
                    },
                    'p', 'P' => {
                        // Pause/suspend selected process
                        if (self.selected_index < self.process_info.items.len) {
                            const info = self.process_info.items[self.selected_index];
                            if (!info.is_vital) {
                                self.setStatus("Suspended process");
                                // TODO: Actually suspend the process
                            } else {
                                self.setStatus("Cannot suspend vital process");
                            }
                        }
                        return true;
                    },
                    's', 'S' => {
                        // Resume selected process
                        if (self.selected_index < self.process_info.items.len) {
                            self.setStatus("Resumed process");
                            // TODO: Actually resume the process
                        }
                        return true;
                    },
                    'd', 'D' => {
                        // Debug selected process
                        if (self.selected_index < self.process_info.items.len) {
                            if (self.on_debug_process) |cb| {
                                cb(self.selected_index);
                            }
                            self.setStatus("Opening debugger...");
                        }
                        return true;
                    },
                    'k', 'K' => {
                        // Kill selected process
                        if (self.selected_index < self.process_info.items.len) {
                            const info = self.process_info.items[self.selected_index];
                            if (!info.is_vital) {
                                self.setStatus("Terminated process");
                                // TODO: Actually terminate the process
                            } else {
                                self.setStatus("Cannot kill vital process!");
                            }
                        }
                        return true;
                    },
                    else => {},
                }
            },
            .tab => {
                self.active_pane = switch (self.active_pane) {
                    .processes => .details,
                    .details => .stack,
                    .stack => .processes,
                };
                self.updateFocus();
                return true;
            },
            .up => {
                if (self.active_pane == .processes) {
                    if (self.selected_index > 0) {
                        self.selected_index -= 1;
                        self.process_list.selectIndex(self.selected_index);
                        self.updateDetails();
                    }
                } else if (self.active_pane == .stack) {
                    if (self.stack_area.scroll_y > 0) {
                        self.stack_area.scroll_y -= 1;
                    }
                }
                return true;
            },
            .down => {
                if (self.active_pane == .processes) {
                    if (self.selected_index + 1 < self.process_info.items.len) {
                        self.selected_index += 1;
                        self.process_list.selectIndex(self.selected_index);
                        self.updateDetails();
                    }
                } else if (self.active_pane == .stack) {
                    self.stack_area.scroll_y += 1;
                }
                return true;
            },
            else => {},
        }
        return false;
    }

    pub fn handleMouse(self: *ProcessTab, mouse: MouseEvent) void {
        if (mouse.event_type != .press) return;

        const layout = self.calcLayout();

        if (layout.process_rect.contains(mouse.x, mouse.y)) {
            self.active_pane = .processes;
            self.updateFocus();
            const content_y = mouse.y -| layout.process_rect.y -| 1;
            const clicked_idx = self.process_list.scroll_offset + content_y;
            if (clicked_idx < self.process_info.items.len) {
                self.selected_index = clicked_idx;
                self.process_list.selectIndex(clicked_idx);
                self.updateDetails();
            }
        } else if (layout.details_rect.contains(mouse.x, mouse.y)) {
            self.active_pane = .details;
            self.updateFocus();
        } else if (layout.stack_rect.contains(mouse.x, mouse.y)) {
            self.active_pane = .stack;
            self.updateFocus();
        }
    }

    pub fn scroll(self: *ProcessTab, delta: i32) void {
        switch (self.active_pane) {
            .processes => {
                if (delta < 0) {
                    const amount = @as(usize, @intCast(-delta));
                    if (self.process_list.scroll_offset > amount) {
                        self.process_list.scroll_offset -= amount;
                    } else {
                        self.process_list.scroll_offset = 0;
                    }
                } else {
                    const amount = @as(usize, @intCast(delta));
                    const max_scroll = self.process_info.items.len -| 1;
                    self.process_list.scroll_offset = @min(self.process_list.scroll_offset + amount, max_scroll);
                }
            },
            .details => {},
            .stack => {
                if (delta < 0) {
                    const amount = @as(usize, @intCast(-delta));
                    if (self.stack_area.scroll_y > amount) {
                        self.stack_area.scroll_y -= amount;
                    } else {
                        self.stack_area.scroll_y = 0;
                    }
                } else {
                    const amount = @as(usize, @intCast(delta));
                    self.stack_area.scroll_y += amount;
                }
            },
        }
    }

    const Layout = struct {
        process_rect: Rect,
        details_rect: Rect,
        stack_rect: Rect,
    };

    fn calcLayout(self: *ProcessTab) Layout {
        const left_width = self.rect.width / 3;
        const right_width = self.rect.width - left_width;
        const details_height = 8;

        return .{
            .process_rect = Rect.init(self.rect.x, self.rect.y, left_width, self.rect.height),
            .details_rect = Rect.init(self.rect.x + left_width, self.rect.y, right_width, details_height),
            .stack_rect = Rect.init(self.rect.x + left_width, self.rect.y + details_height, right_width, self.rect.height - details_height),
        };
    }

    fn updateFocus(self: *ProcessTab) void {
        self.process_list.state.focused = (self.active_pane == .processes);
        self.details_area.state.focused = (self.active_pane == .details);
        self.stack_area.state.focused = (self.active_pane == .stack);
    }

    pub fn draw(self: *ProcessTab, screen: *Screen) void {
        self.updateFocus();

        const layout = self.calcLayout();

        // Update widget rects
        self.process_list.state.rect = layout.process_rect;

        // Draw process list
        self.process_list.draw(screen);

        // Draw details pane with border
        self.drawDetailsPane(screen, layout.details_rect);

        // Draw stack pane with border
        self.drawStackPane(screen, layout.stack_rect);

        // Draw help bar
        self.drawHelpBar(screen);
    }

    fn drawDetailsPane(self: *ProcessTab, screen: *Screen, rect: Rect) void {
        const title = "Details";
        const border_style = if (self.active_pane == .details)
            style_mod.styles.border_focused_style
        else
            style_mod.styles.border_style;

        self.drawBorderedPane(screen, rect, title, border_style);

        self.details_area.state.rect = Rect.init(
            rect.x + 1,
            rect.y + 1,
            rect.width -| 2,
            rect.height -| 2,
        );
        self.details_area.draw(screen);
    }

    fn drawStackPane(self: *ProcessTab, screen: *Screen, rect: Rect) void {
        const title = "Stack";
        const border_style = if (self.active_pane == .stack)
            style_mod.styles.border_focused_style
        else
            style_mod.styles.border_style;

        self.drawBorderedPane(screen, rect, title, border_style);

        self.stack_area.state.rect = Rect.init(
            rect.x + 1,
            rect.y + 1,
            rect.width -| 2,
            rect.height -| 3, // Room for help bar
        );
        self.stack_area.draw(screen);
    }

    fn drawBorderedPane(_: *ProcessTab, screen: *Screen, rect: Rect, title: []const u8, border_style: style_mod.Style) void {
        // Top border
        screen.setCell(rect.x, rect.y, style_mod.box.top_left, border_style);
        var x = rect.x + 1;
        while (x < rect.x + rect.width - 1) : (x += 1) {
            screen.setCell(x, rect.y, style_mod.box.horizontal, border_style);
        }
        screen.setCell(rect.x + rect.width - 1, rect.y, style_mod.box.top_right, border_style);

        // Title
        screen.drawText(rect.x + 2, rect.y, title, border_style);

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
    }

    fn drawHelpBar(self: *ProcessTab, screen: *Screen) void {
        const layout = self.calcLayout();
        const y = layout.stack_rect.y + layout.stack_rect.height - 2;
        const help = " R:Refresh  P:Pause  S:Resume  D:Debug  K:Kill ";
        screen.drawText(layout.stack_rect.x + 1, y, help, style_mod.styles.status);
    }
};
