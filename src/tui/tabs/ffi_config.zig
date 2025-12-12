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
const EventResult = widget.EventResult;

pub const FFILibrary = struct {
    name: []const u8,
    headers: []const []const u8,
    link: []const u8,
    enabled: bool = true,

    // For owned strings when loaded from file
    name_owned: ?[]u8 = null,
    headers_owned: ?[][]u8 = null,
    link_owned: ?[]u8 = null,
};

pub const FFIConfigTab = struct {
    allocator: std.mem.Allocator,
    rect: Rect,
    focused: bool = false,

    libraries: std.ArrayList(FFILibrary),
    library_list: ListView,

    status_message: []const u8 = "Ready",
    config_path: []const u8 = "ffi-config.json",
    dirty: bool = false, // Config has unsaved changes

    // Add library dialog state
    show_add_dialog: bool = false,
    dialog_name: [64]u8 = [_]u8{0} ** 64,
    dialog_name_len: usize = 0,
    dialog_headers: [256]u8 = [_]u8{0} ** 256,
    dialog_headers_len: usize = 0,
    dialog_link: [64]u8 = [_]u8{0} ** 64,
    dialog_link_len: usize = 0,
    dialog_field: DialogField = .name,

    pub const DialogField = enum {
        name,
        headers,
        link,
    };

    pub fn init(allocator: std.mem.Allocator, rect: Rect) !FFIConfigTab {
        const list_rect = Rect.init(rect.x + 2, rect.y + 4, rect.width - 4, rect.height - 10);
        var library_list = try ListView.init(allocator, list_rect);
        library_list.state.title = "Configured Libraries";

        var tab = FFIConfigTab{
            .allocator = allocator,
            .rect = rect,
            .libraries = .empty,
            .library_list = library_list,
        };

        // Load config from file
        tab.loadConfig() catch {
            // If no config file, add defaults
            try tab.addDefaultLibraries();
        };

        return tab;
    }

    pub fn deinit(self: *FFIConfigTab) void {
        // Free owned strings
        for (self.libraries.items) |*lib| {
            if (lib.name_owned) |owned| {
                self.allocator.free(owned);
            }
            if (lib.headers_owned) |owned| {
                for (owned) |h| {
                    self.allocator.free(h);
                }
                self.allocator.free(owned);
            }
            if (lib.link_owned) |owned| {
                self.allocator.free(owned);
            }
        }
        self.libraries.deinit(self.allocator);
        self.library_list.deinit();
    }

    fn addDefaultLibraries(self: *FFIConfigTab) !void {
        // LibC
        try self.libraries.append(self.allocator, .{
            .name = "LibC",
            .headers = &[_][]const u8{ "stdio.h", "stdlib.h", "string.h" },
            .link = "c",
            .enabled = true,
        });

        // LibMath
        try self.libraries.append(self.allocator, .{
            .name = "LibMath",
            .headers = &[_][]const u8{"math.h"},
            .link = "m",
            .enabled = true,
        });

        self.refreshList();
    }

    pub fn loadConfig(self: *FFIConfigTab) !void {
        const file = std.fs.cwd().openFile(self.config_path, .{}) catch return error.FileNotFound;
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(content);

        const parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, content, .{});
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) return error.InvalidFormat;

        const libs_val = root.object.get("libraries") orelse return error.InvalidFormat;
        if (libs_val != .array) return error.InvalidFormat;

        // Clear existing
        for (self.libraries.items) |*lib| {
            if (lib.name_owned) |owned| self.allocator.free(owned);
            if (lib.headers_owned) |owned| {
                for (owned) |h| self.allocator.free(h);
                self.allocator.free(owned);
            }
            if (lib.link_owned) |owned| self.allocator.free(owned);
        }
        self.libraries.clearRetainingCapacity();

        for (libs_val.array.items) |lib_val| {
            if (lib_val != .object) continue;

            const name_val = lib_val.object.get("name") orelse continue;
            const headers_val = lib_val.object.get("headers") orelse continue;
            const link_val = lib_val.object.get("link") orelse continue;
            const enabled_val = lib_val.object.get("enabled");

            if (name_val != .string or headers_val != .array or link_val != .string) continue;

            // Copy name
            const name_owned = try self.allocator.dupe(u8, name_val.string);

            // Copy headers
            var headers_list: std.ArrayList([]u8) = .empty;
            for (headers_val.array.items) |h| {
                if (h == .string) {
                    try headers_list.append(self.allocator, try self.allocator.dupe(u8, h.string));
                }
            }
            const headers_owned = try headers_list.toOwnedSlice(self.allocator);

            // Build const slice for headers field
            var headers_const = try self.allocator.alloc([]const u8, headers_owned.len);
            for (headers_owned, 0..) |h, i| {
                headers_const[i] = h;
            }

            // Copy link
            const link_owned = try self.allocator.dupe(u8, link_val.string);

            const enabled = if (enabled_val) |ev| (ev == .bool and ev.bool) else true;

            try self.libraries.append(self.allocator, .{
                .name = name_owned,
                .headers = headers_const,
                .link = link_owned,
                .enabled = enabled,
                .name_owned = name_owned,
                .headers_owned = headers_owned,
                .link_owned = link_owned,
            });
        }

        self.refreshList();
        self.status_message = "Config loaded";
    }

    pub fn saveConfig(self: *FFIConfigTab) !void {
        var file = try std.fs.cwd().createFile(self.config_path, .{});
        defer file.close();

        try file.writeAll("{\n  \"libraries\": [\n");

        for (self.libraries.items, 0..) |lib, i| {
            try file.writeAll("    {\n");

            var buf: [512]u8 = undefined;
            var len = std.fmt.bufPrint(&buf, "      \"name\": \"{s}\",\n", .{lib.name}) catch continue;
            try file.writeAll(len);

            try file.writeAll("      \"headers\": [");
            for (lib.headers, 0..) |h, j| {
                if (j > 0) try file.writeAll(", ");
                len = std.fmt.bufPrint(&buf, "\"{s}\"", .{h}) catch continue;
                try file.writeAll(len);
            }
            try file.writeAll("],\n");

            len = std.fmt.bufPrint(&buf, "      \"link\": \"{s}\",\n", .{lib.link}) catch continue;
            try file.writeAll(len);

            len = std.fmt.bufPrint(&buf, "      \"enabled\": {}\n", .{lib.enabled}) catch continue;
            try file.writeAll(len);

            try file.writeAll("    }");
            if (i < self.libraries.items.len - 1) {
                try file.writeAll(",");
            }
            try file.writeAll("\n");
        }

        try file.writeAll("  ]\n}\n");

        self.dirty = false;
        self.status_message = "Config saved - run 'zig build' to generate bindings";
    }

    fn refreshList(self: *FFIConfigTab) void {
        self.library_list.clear();

        for (self.libraries.items) |lib| {
            var buf: [256]u8 = undefined;
            const enabled = if (lib.enabled) "[x]" else "[ ]";

            const headers_str = blk: {
                var h_buf: [128]u8 = undefined;
                var h_pos: usize = 0;
                for (lib.headers, 0..) |header, i| {
                    if (i > 0 and h_pos < h_buf.len - 2) {
                        h_buf[h_pos] = ',';
                        h_buf[h_pos + 1] = ' ';
                        h_pos += 2;
                    }
                    const to_copy = @min(header.len, h_buf.len - h_pos);
                    @memcpy(h_buf[h_pos .. h_pos + to_copy], header[0..to_copy]);
                    h_pos += to_copy;
                }
                break :blk h_buf[0..h_pos];
            };

            const display = std.fmt.bufPrint(&buf, "{s} {s} (-l{s}) - {s}", .{
                enabled,
                lib.name,
                lib.link,
                headers_str,
            }) catch lib.name;

            self.library_list.addItem(display, null) catch {};
        }
    }

    pub fn updateRect(self: *FFIConfigTab, rect: Rect) void {
        self.rect = rect;
        self.library_list.state.rect = Rect.init(rect.x + 2, rect.y + 4, rect.width - 4, rect.height - 10);
    }

    pub fn handleKey(self: *FFIConfigTab, key: Key) EventResult {
        // Handle add dialog
        if (self.show_add_dialog) {
            return self.handleDialogKey(key);
        }

        switch (key) {
            .enter => {
                // Toggle enable/disable
                if (self.library_list.selected_index) |idx| {
                    if (idx < self.libraries.items.len) {
                        self.libraries.items[idx].enabled = !self.libraries.items[idx].enabled;
                        self.dirty = true;
                        self.refreshList();
                        self.library_list.selectIndex(idx);
                    }
                }
                return .consumed;
            },
            .char => |c| {
                if (c == 'a' or c == 'A') {
                    // Open add dialog
                    self.show_add_dialog = true;
                    self.dialog_name_len = 0;
                    self.dialog_headers_len = 0;
                    self.dialog_link_len = 0;
                    self.dialog_field = .name;
                    return .consumed;
                }
                if (c == 'd' or c == 'D') {
                    // Delete selected library
                    if (self.library_list.selected_index) |idx| {
                        if (idx < self.libraries.items.len) {
                            const lib = &self.libraries.items[idx];
                            if (lib.name_owned) |owned| self.allocator.free(owned);
                            if (lib.headers_owned) |owned| {
                                for (owned) |h| self.allocator.free(h);
                                self.allocator.free(owned);
                            }
                            if (lib.link_owned) |owned| self.allocator.free(owned);
                            _ = self.libraries.orderedRemove(idx);
                            self.dirty = true;
                            self.refreshList();
                            self.status_message = "Library removed";
                        }
                    }
                    return .consumed;
                }
            },
            .ctrl => |c| {
                switch (c) {
                    19 => { // Ctrl+S - Save config
                        self.saveConfig() catch {
                            self.status_message = "Failed to save config";
                        };
                        return .consumed;
                    },
                    else => {},
                }
            },
            .delete => {
                // Delete selected library
                if (self.library_list.selected_index) |idx| {
                    if (idx < self.libraries.items.len) {
                        const lib = &self.libraries.items[idx];
                        if (lib.name_owned) |owned| self.allocator.free(owned);
                        if (lib.headers_owned) |owned| {
                            for (owned) |h| self.allocator.free(h);
                            self.allocator.free(owned);
                        }
                        if (lib.link_owned) |owned| self.allocator.free(owned);
                        _ = self.libraries.orderedRemove(idx);
                        self.dirty = true;
                        self.refreshList();
                        self.status_message = "Library removed";
                    }
                }
                return .consumed;
            },
            else => {},
        }

        // Pass to list
        self.library_list.state.focused = self.focused;
        return self.library_list.handleKey(key);
    }

    fn handleDialogKey(self: *FFIConfigTab, key: Key) EventResult {
        switch (key) {
            .escape => {
                self.show_add_dialog = false;
                return .consumed;
            },
            .enter => {
                // Submit dialog - add the library
                if (self.dialog_name_len > 0 and self.dialog_headers_len > 0) {
                    self.addLibraryFromDialog() catch {
                        self.status_message = "Failed to add library";
                    };
                    self.show_add_dialog = false;
                }
                return .consumed;
            },
            .tab => {
                // Switch field
                self.dialog_field = switch (self.dialog_field) {
                    .name => .headers,
                    .headers => .link,
                    .link => .name,
                };
                return .consumed;
            },
            .shift_tab => {
                // Switch field backwards
                self.dialog_field = switch (self.dialog_field) {
                    .name => .link,
                    .headers => .name,
                    .link => .headers,
                };
                return .consumed;
            },
            .backspace => {
                // Delete character
                switch (self.dialog_field) {
                    .name => {
                        if (self.dialog_name_len > 0) self.dialog_name_len -= 1;
                    },
                    .headers => {
                        if (self.dialog_headers_len > 0) self.dialog_headers_len -= 1;
                    },
                    .link => {
                        if (self.dialog_link_len > 0) self.dialog_link_len -= 1;
                    },
                }
                return .consumed;
            },
            .char => |c| {
                // Add character to current field
                if (c < 128) {
                    const char: u8 = @intCast(c);
                    switch (self.dialog_field) {
                        .name => {
                            if (self.dialog_name_len < self.dialog_name.len - 1) {
                                self.dialog_name[self.dialog_name_len] = char;
                                self.dialog_name_len += 1;
                            }
                        },
                        .headers => {
                            if (self.dialog_headers_len < self.dialog_headers.len - 1) {
                                self.dialog_headers[self.dialog_headers_len] = char;
                                self.dialog_headers_len += 1;
                            }
                        },
                        .link => {
                            if (self.dialog_link_len < self.dialog_link.len - 1) {
                                self.dialog_link[self.dialog_link_len] = char;
                                self.dialog_link_len += 1;
                            }
                        },
                    }
                }
                return .consumed;
            },
            else => return .consumed,
        }
    }

    fn addLibraryFromDialog(self: *FFIConfigTab) !void {
        // Parse headers (comma or space separated)
        var headers_list: std.ArrayList([]u8) = .empty;
        errdefer {
            for (headers_list.items) |h| self.allocator.free(h);
            headers_list.deinit(self.allocator);
        }

        var iter = std.mem.tokenizeAny(u8, self.dialog_headers[0..self.dialog_headers_len], ", ");
        while (iter.next()) |header| {
            const trimmed = std.mem.trim(u8, header, " \t");
            if (trimmed.len > 0) {
                try headers_list.append(self.allocator, try self.allocator.dupe(u8, trimmed));
            }
        }

        if (headers_list.items.len == 0) return error.NoHeaders;

        const headers_owned = try headers_list.toOwnedSlice(self.allocator);

        // Build const slice
        var headers_const = try self.allocator.alloc([]const u8, headers_owned.len);
        for (headers_owned, 0..) |h, i| {
            headers_const[i] = h;
        }

        // Copy name and link
        const name_owned = try self.allocator.dupe(u8, self.dialog_name[0..self.dialog_name_len]);
        const link_owned = try self.allocator.dupe(u8, self.dialog_link[0..self.dialog_link_len]);

        try self.libraries.append(self.allocator, .{
            .name = name_owned,
            .headers = headers_const,
            .link = link_owned,
            .enabled = true,
            .name_owned = name_owned,
            .headers_owned = headers_owned,
            .link_owned = link_owned,
        });

        self.dirty = true;
        self.refreshList();
        self.status_message = "Library added - Ctrl+S to save";
    }

    pub fn handleMouse(self: *FFIConfigTab, mouse: MouseEvent) void {
        if (self.show_add_dialog) return;

        const list_rect = Rect.init(self.rect.x + 2, self.rect.y + 4, self.rect.width - 4, self.rect.height - 10);

        // Handle scroll wheel
        if (mouse.button == .scroll_up) {
            if (self.library_list.scroll_offset > 3) {
                self.library_list.scroll_offset -= 3;
            } else {
                self.library_list.scroll_offset = 0;
            }
        } else if (mouse.button == .scroll_down) {
            const max_scroll = self.library_list.items.items.len -| 1;
            self.library_list.scroll_offset = @min(self.library_list.scroll_offset + 3, max_scroll);
        }

        // Handle left click in list area
        if (mouse.event_type == .press and mouse.button == .left) {
            if (list_rect.contains(mouse.x, mouse.y)) {
                const content_y = mouse.y -| list_rect.y -| 1;
                const clicked_idx = self.library_list.scroll_offset + content_y;

                if (clicked_idx < self.library_list.items.items.len) {
                    self.library_list.selectIndex(clicked_idx);
                }
            }
        }
    }

    pub fn draw(self: *FFIConfigTab, screen: *Screen) void {
        const rect = self.rect;

        // Draw main border
        widget.drawBorderRounded(screen, rect, self.focused);
        widget.drawTitle(screen, rect, "FFI Configuration", self.focused);

        // Draw header text
        const header_style = style_mod.styles.normal;
        screen.drawText(rect.x + 2, rect.y + 2, "Configure C library bindings for Smalltalk FFI:", header_style);

        // Update and draw library list
        self.library_list.state.rect = Rect.init(rect.x + 2, rect.y + 4, rect.width - 4, rect.height - 10);
        self.library_list.state.focused = self.focused and !self.show_add_dialog;
        self.library_list.draw(screen);

        // Draw buttons/help at bottom
        const button_y = rect.y + rect.height - 4;
        const button_style = style_mod.styles.status;
        const key_style = style_mod.styles.status_key;

        screen.drawText(rect.x + 2, button_y, "Enter", key_style);
        screen.drawText(rect.x + 8, button_y, "Toggle", button_style);

        screen.drawText(rect.x + 18, button_y, "A", key_style);
        screen.drawText(rect.x + 20, button_y, "Add", button_style);

        screen.drawText(rect.x + 26, button_y, "D/Del", key_style);
        screen.drawText(rect.x + 32, button_y, "Delete", button_style);

        screen.drawText(rect.x + 42, button_y, "Ctrl+S", key_style);
        screen.drawText(rect.x + 49, button_y, "Save", button_style);

        // Draw status bar
        const status_y = rect.y + rect.height - 2;
        screen.fillRect(rect.x + 1, status_y, rect.width - 2, 1, ' ', style_mod.styles.status);

        var status_buf: [128]u8 = undefined;
        const dirty_indicator = if (self.dirty) " [unsaved]" else "";
        const status = std.fmt.bufPrint(&status_buf, "Status: {s}{s}", .{
            self.status_message,
            dirty_indicator,
        }) catch self.status_message;

        screen.drawTextClipped(rect.x + 2, status_y, status, rect.width - 4, style_mod.styles.status);

        // Draw add dialog if visible
        if (self.show_add_dialog) {
            self.drawAddDialog(screen);
        }
    }

    fn drawAddDialog(self: *FFIConfigTab, screen: *Screen) void {
        const dialog_width: u16 = 60;
        const dialog_height: u16 = 13;
        const dialog_x = (self.rect.width -| dialog_width) / 2 + self.rect.x;
        const dialog_y = (self.rect.height -| dialog_height) / 2 + self.rect.y;

        const dialog_rect = Rect.init(dialog_x, dialog_y, dialog_width, dialog_height);

        // Draw dialog background
        screen.fillRect(dialog_rect.x, dialog_rect.y, dialog_rect.width, dialog_rect.height, ' ', style_mod.styles.normal);
        widget.drawBorderRounded(screen, dialog_rect, true);
        widget.drawTitle(screen, dialog_rect, "Add Library", true);

        const label_style = style_mod.styles.normal;
        const field_style = style_mod.styles.selected;
        const active_style = Style{ .fg = style_mod.ui.foreground, .bg = style_mod.ui.tab_active, .bold = false };

        // Name field
        const name_style = if (self.dialog_field == .name) active_style else field_style;
        screen.drawText(dialog_x + 2, dialog_y + 2, "Name:", label_style);
        screen.fillRect(dialog_x + 10, dialog_y + 2, dialog_width - 14, 1, ' ', name_style);
        screen.drawTextClipped(dialog_x + 10, dialog_y + 2, self.dialog_name[0..self.dialog_name_len], dialog_width - 14, name_style);

        // Headers field
        const headers_style = if (self.dialog_field == .headers) active_style else field_style;
        screen.drawText(dialog_x + 2, dialog_y + 4, "Headers:", label_style);
        screen.fillRect(dialog_x + 10, dialog_y + 4, dialog_width - 14, 1, ' ', headers_style);
        screen.drawTextClipped(dialog_x + 10, dialog_y + 4, self.dialog_headers[0..self.dialog_headers_len], dialog_width - 14, headers_style);
        screen.drawText(dialog_x + 2, dialog_y + 5, "(comma separated, e.g. SDL.h, SDL_image.h)", style_mod.styles.dim);

        // Link library field
        const link_style = if (self.dialog_field == .link) active_style else field_style;
        screen.drawText(dialog_x + 2, dialog_y + 7, "Link (-l):", label_style);
        screen.fillRect(dialog_x + 12, dialog_y + 7, dialog_width - 16, 1, ' ', link_style);
        screen.drawTextClipped(dialog_x + 12, dialog_y + 7, self.dialog_link[0..self.dialog_link_len], dialog_width - 16, link_style);

        // Help text
        screen.drawText(dialog_x + 2, dialog_y + 9, "Tab: next field | Enter: add | Esc: cancel", style_mod.styles.dim);
        screen.drawText(dialog_x + 2, dialog_y + 10, "All functions auto-discovered on rebuild", style_mod.styles.dim);
    }
};
