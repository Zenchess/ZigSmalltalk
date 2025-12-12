const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("../widgets/widget.zig");
const TextArea = @import("../widgets/textarea.zig").TextArea;

const Key = input.Key;
const MouseEvent = input.MouseEvent;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const WorkspaceTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,

    editor: TextArea,

    // Callbacks
    on_do_it: ?*const fn (*WorkspaceTab, []const u8) void = null,
    on_print_it: ?*const fn (*WorkspaceTab, []const u8) void = null,
    on_inspect_it: ?*const fn (*WorkspaceTab, []const u8) void = null,

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !WorkspaceTab {
        const content = Rect.init(rect.x + 1, rect.y + 1, rect.width -| 2, rect.height -| 2);
        var editor = try TextArea.init(allocator, content);
        editor.state.border = false;
        editor.line_numbers = true;
        editor.syntax_highlight = true;

        return WorkspaceTab{
            .allocator = allocator,
            .rect = rect,
            .editor = editor,
        };
    }

    pub fn deinit(self: *WorkspaceTab) void {
        self.editor.deinit();
    }

    pub fn setText(self: *WorkspaceTab, text: []const u8) !void {
        try self.editor.setText(text);
    }

    pub fn getText(self: *WorkspaceTab) ![]u8 {
        return self.editor.getText(self.allocator);
    }

    pub fn getSelectedOrCurrentLine(self: *WorkspaceTab) ![]u8 {
        // If there's a selection, return that
        const maybe_selected = try self.editor.getSelectedText(self.allocator);
        if (maybe_selected) |selected| {
            return selected;
        }

        // Otherwise return current line
        if (self.editor.cursor_line < self.editor.lines.items.len) {
            const line = self.editor.lines.items[self.editor.cursor_line];
            return self.allocator.dupe(u8, line.items);
        }

        return self.allocator.alloc(u8, 0);
    }

    pub fn insertResult(self: *WorkspaceTab, result: []const u8) !void {
        // Move cursor to end of selection or current line
        if (self.editor.hasSelection()) {
            // Delete selection first (result replaces it)
            self.editor.clearSelection();
        }

        // Move to end of line
        self.editor.cursor_col = self.editor.lines.items[self.editor.cursor_line].items.len;

        // Insert space and result
        const text = try std.fmt.allocPrint(self.allocator, " \" {s} \"", .{result});
        defer self.allocator.free(text);

        for (text) |c| {
            _ = self.editor.handleKey(.{ .char = c });
        }
    }

    pub fn updateRect(self: *WorkspaceTab, rect: Rect) void {
        self.rect = rect;
        const content = Rect.init(rect.x + 1, rect.y + 1, rect.width -| 2, rect.height -| 2);
        self.editor.state.rect = content;
    }

    pub fn handleKey(self: *WorkspaceTab, key: Key) EventResult {
        switch (key) {
            .ctrl => |c| {
                switch (c) {
                    4 => { // Ctrl+D - Do It
                        const code = self.getSelectedOrCurrentLine() catch return .ignored;
                        defer self.allocator.free(code);
                        if (self.on_do_it) |cb| {
                            cb(self, code);
                        }
                        return .consumed;
                    },
                    16 => { // Ctrl+P - Print It
                        const code = self.getSelectedOrCurrentLine() catch return .ignored;
                        defer self.allocator.free(code);
                        if (self.on_print_it) |cb| {
                            cb(self, code);
                        }
                        return .consumed;
                    },
                    9 => { // Ctrl+I - Inspect It
                        const code = self.getSelectedOrCurrentLine() catch return .ignored;
                        defer self.allocator.free(code);
                        if (self.on_inspect_it) |cb| {
                            cb(self, code);
                        }
                        return .consumed;
                    },
                    else => {},
                }
            },
            else => {},
        }

        // Pass to editor
        self.editor.state.focused = self.focused;
        return self.editor.handleKey(key);
    }

    pub fn handleMouse(self: *WorkspaceTab, mouse: MouseEvent) void {
        // Update editor state
        self.editor.state.focused = self.focused;

        // Delegate to editor for all mouse handling (click, drag selection, scroll)
        _ = self.editor.handleMouse(mouse);
    }

    pub fn scroll(self: *WorkspaceTab, delta: i32) void {
        if (delta < 0) {
            // Scroll up
            const amount = @as(usize, @intCast(-delta));
            if (self.editor.scroll_y > amount) {
                self.editor.scroll_y -= amount;
            } else {
                self.editor.scroll_y = 0;
            }
        } else {
            // Scroll down
            const amount = @as(usize, @intCast(delta));
            const max_scroll = self.editor.lines.items.len -| 1;
            self.editor.scroll_y = @min(self.editor.scroll_y + amount, max_scroll);
        }
    }

    pub fn draw(self: *WorkspaceTab, screen: *Screen) void {
        const rect = self.rect;

        // Draw border
        widget.drawBorderRounded(screen, rect, self.focused);
        widget.drawTitle(screen, rect, "Workspace", self.focused);

        // Update editor rect
        const content = Rect.init(rect.x + 1, rect.y + 1, rect.width -| 2, rect.height -| 2);
        self.editor.state.rect = content;
        self.editor.state.focused = self.focused;

        // Draw editor
        self.editor.draw(screen);
    }
};
