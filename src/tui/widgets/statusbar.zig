const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const style_mod = @import("../style.zig");
const widget = @import("widget.zig");

const Style = style_mod.Style;
const Rect = widget.Rect;

pub const StatusItem = struct {
    text: []const u8,
    key: ?[]const u8 = null, // e.g., "Ctrl+D"
};

pub const StatusBar = struct {
    state: widget.WidgetState,
    message: []const u8 = "",
    items: []const StatusItem = &[_]StatusItem{},

    pub fn init(rect: Rect) StatusBar {
        return StatusBar{
            .state = .{ .rect = rect, .border = false },
        };
    }

    pub fn setMessage(self: *StatusBar, msg: []const u8) void {
        self.message = msg;
    }

    pub fn setItems(self: *StatusBar, items: []const StatusItem) void {
        self.items = items;
    }

    pub fn draw(self: *StatusBar, screen: *Screen) void {
        const rect = self.state.rect;

        // Draw background
        screen.fillRect(rect.x, rect.y, rect.width, rect.height, ' ', style_mod.styles.status);

        // Draw message on the left
        if (self.message.len > 0) {
            screen.drawTextClipped(rect.x + 1, rect.y, self.message, rect.width / 3, style_mod.styles.status);
        }

        // Draw items on the right
        var x = rect.x + rect.width;
        var i = self.items.len;
        while (i > 0) {
            i -= 1;
            const item = self.items[i];

            // Calculate item width
            var item_width: u16 = @intCast(item.text.len);
            if (item.key) |key| {
                item_width += @as(u16, @intCast(key.len)) + 1; // +1 for separator
            }
            item_width += 3; // padding

            if (x < item_width + rect.x) break;

            x -= item_width;

            // Draw key binding
            var draw_x = x + 1;
            if (item.key) |key| {
                screen.drawText(draw_x, rect.y, key, style_mod.styles.status_key);
                draw_x += @as(u16, @intCast(key.len)) + 1;
            }

            // Draw text
            screen.drawText(draw_x, rect.y, item.text, style_mod.styles.status);

            // Draw separator
            if (i > 0 and x > rect.x + 1) {
                screen.setCell(x - 1, rect.y, style_mod.box.vertical[0], style_mod.styles.status);
            }
        }
    }
};
