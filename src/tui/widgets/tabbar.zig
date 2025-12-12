const std = @import("std");
const Screen = @import("../screen.zig").Screen;
const input = @import("../input.zig");
const style_mod = @import("../style.zig");
const widget = @import("widget.zig");

const Key = input.Key;
const Style = style_mod.Style;
const Rect = widget.Rect;
const EventResult = widget.EventResult;

pub const Tab = struct {
    title: []const u8,
    shortcut: ?u8 = null, // e.g., '1' for Ctrl+1
};

pub const TabBar = struct {
    state: widget.WidgetState,

    tabs: []const Tab,
    active_tab: usize = 0,

    // Callbacks
    on_change: ?*const fn (*TabBar, usize) void = null,

    pub fn init(rect: Rect, tabs: []const Tab) TabBar {
        return TabBar{
            .state = .{ .rect = rect, .border = false },
            .tabs = tabs,
        };
    }

    pub fn setActiveTab(self: *TabBar, index: usize) void {
        if (index < self.tabs.len and index != self.active_tab) {
            self.active_tab = index;
            if (self.on_change) |cb| cb(self, index);
        }
    }

    pub fn nextTab(self: *TabBar) void {
        self.setActiveTab((self.active_tab + 1) % self.tabs.len);
    }

    pub fn prevTab(self: *TabBar) void {
        if (self.active_tab > 0) {
            self.setActiveTab(self.active_tab - 1);
        } else {
            self.setActiveTab(self.tabs.len - 1);
        }
    }

    pub fn handleKey(self: *TabBar, key: Key) EventResult {
        switch (key) {
            .tab => {
                self.nextTab();
                return .consumed;
            },
            .shift_tab => {
                self.prevTab();
                return .consumed;
            },
            .ctrl => |c| {
                // Ctrl+1 through Ctrl+9
                if (c >= 1 and c <= 9) {
                    const idx = c - 1;
                    if (idx < self.tabs.len) {
                        self.setActiveTab(idx);
                        return .consumed;
                    }
                }
                return .ignored;
            },
            else => return .ignored,
        }
    }

    pub fn draw(self: *TabBar, screen: *Screen) void {
        const rect = self.state.rect;

        // Draw background
        screen.fillRect(rect.x, rect.y, rect.width, rect.height, ' ', style_mod.styles.normal);

        // Draw tabs
        var x = rect.x;
        for (self.tabs, 0..) |tab, i| {
            const is_active = i == self.active_tab;

            // Tab style
            const tab_style = if (is_active)
                style_mod.styles.tab_active_style
            else
                style_mod.styles.tab_inactive_style;

            // Calculate tab width
            const tab_width = @as(u16, @intCast(tab.title.len)) + 4; // padding

            // Draw tab background
            screen.fillRect(x, rect.y, tab_width, rect.height, ' ', tab_style);

            // Draw tab text centered
            screen.drawText(x + 2, rect.y, tab.title, tab_style);

            // Draw separator between tabs
            if (!is_active and i < self.tabs.len - 1) {
                screen.setCell(x + tab_width - 1, rect.y, ' ', style_mod.styles.normal);
            }

            x += tab_width;

            if (x >= rect.x + rect.width) break;
        }

        // Fill remaining space
        if (x < rect.x + rect.width) {
            screen.fillRect(x, rect.y, rect.x + rect.width - x, rect.height, ' ', style_mod.styles.normal);
        }

        // Draw bottom border line
        var col = rect.x;
        while (col < rect.x + rect.width) : (col += 1) {
            screen.drawText(col, rect.y + rect.height - 1, style_mod.box.horizontal, style_mod.styles.border_style);
        }
    }
};
