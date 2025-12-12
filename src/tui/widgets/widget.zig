const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style = @import("../style.zig");

const Key = input.Key;
const Style = style.Style;

// Rect represents a rectangular area
pub const Rect = struct {
    x: u16,
    y: u16,
    width: u16,
    height: u16,

    pub fn init(x: u16, y: u16, width: u16, height: u16) Rect {
        return .{ .x = x, .y = y, .width = width, .height = height };
    }

    pub fn contains(self: Rect, px: u16, py: u16) bool {
        return px >= self.x and px < self.x + self.width and
            py >= self.y and py < self.y + self.height;
    }

    pub fn inner(self: Rect, margin: u16) Rect {
        return .{
            .x = self.x + margin,
            .y = self.y + margin,
            .width = if (self.width > margin * 2) self.width - margin * 2 else 0,
            .height = if (self.height > margin * 2) self.height - margin * 2 else 0,
        };
    }

    pub fn splitHorizontal(self: Rect, ratio: f32) struct { top: Rect, bottom: Rect } {
        const top_height = @as(u16, @intFromFloat(@as(f32, @floatFromInt(self.height)) * ratio));
        return .{
            .top = Rect.init(self.x, self.y, self.width, top_height),
            .bottom = Rect.init(self.x, self.y + top_height, self.width, self.height - top_height),
        };
    }

    pub fn splitVertical(self: Rect, ratio: f32) struct { left: Rect, right: Rect } {
        const left_width = @as(u16, @intFromFloat(@as(f32, @floatFromInt(self.width)) * ratio));
        return .{
            .left = Rect.init(self.x, self.y, left_width, self.height),
            .right = Rect.init(self.x + left_width, self.y, self.width - left_width, self.height),
        };
    }

    pub fn splitVerticalFixed(self: Rect, left_width: u16) struct { left: Rect, right: Rect } {
        const actual_left = @min(left_width, self.width);
        return .{
            .left = Rect.init(self.x, self.y, actual_left, self.height),
            .right = Rect.init(self.x + actual_left, self.y, self.width - actual_left, self.height),
        };
    }

    pub fn splitHorizontalFixed(self: Rect, top_height: u16) struct { top: Rect, bottom: Rect } {
        const actual_top = @min(top_height, self.height);
        return .{
            .top = Rect.init(self.x, self.y, self.width, actual_top),
            .bottom = Rect.init(self.x, self.y + actual_top, self.width, self.height - actual_top),
        };
    }
};

// Base event handler result
pub const EventResult = enum {
    ignored, // Event not handled, pass to parent
    consumed, // Event handled, stop propagation
    quit, // Request to quit the application
};

// Common widget properties
pub const WidgetState = struct {
    rect: Rect,
    focused: bool = false,
    visible: bool = true,
    title: ?[]const u8 = null,
    border: bool = true,

    pub fn contentRect(self: WidgetState) Rect {
        if (self.border) {
            return self.rect.inner(1);
        }
        return self.rect;
    }
};

// Widget interface using tagged union for type-erased widgets
pub const WidgetType = enum {
    text_area,
    list_view,
    tree_view,
    tab_bar,
    status_bar,
    label,
    custom,
};

// Helper functions for common widget operations
pub fn drawTitle(screen: *Screen, rect: Rect, title: []const u8, focused: bool) void {
    const border_style = if (focused) style.styles.border_focused_style else style.styles.border_style;
    const title_style = style.styles.title;

    // Draw title with padding
    if (title.len > 0 and rect.width > 4) {
        const start_x = rect.x + 2;
        screen.drawText(start_x, rect.y, " ", border_style);
        screen.drawText(start_x + 1, rect.y, title, title_style);
        const end_x = start_x + 1 + @as(u16, @intCast(@min(title.len, rect.width - 6)));
        screen.drawText(end_x, rect.y, " ", border_style);
    }
}

pub fn drawBorder(screen: *Screen, rect: Rect, focused: bool) void {
    const border_style = if (focused) style.styles.border_focused_style else style.styles.border_style;
    screen.drawBox(rect.x, rect.y, rect.width, rect.height, border_style);
}

pub fn drawBorderRounded(screen: *Screen, rect: Rect, focused: bool) void {
    const border_style = if (focused) style.styles.border_focused_style else style.styles.border_style;
    screen.drawBoxRounded(rect.x, rect.y, rect.width, rect.height, border_style);
}

pub fn clearContent(screen: *Screen, rect: Rect) void {
    screen.fillRect(rect.x, rect.y, rect.width, rect.height, ' ', style.styles.normal);
}
