const std = @import("std");
const terminal_mod = @import("terminal.zig");
const screen_mod = @import("screen.zig");
const input_mod = @import("input.zig");
const style_mod = @import("style.zig");
const widget = @import("widgets/widget.zig");
const TabBar = @import("widgets/tabbar.zig").TabBar;
const StatusBar = @import("widgets/statusbar.zig").StatusBar;
const StatusItem = @import("widgets/statusbar.zig").StatusItem;
const TranscriptTab = @import("tabs/transcript.zig").TranscriptTab;
const WorkspaceTab = @import("tabs/workspace.zig").WorkspaceTab;
const BrowserTab = @import("tabs/browser.zig").BrowserTab;
const FFIConfigTab = @import("tabs/ffi_config.zig").FFIConfigTab;
const transcript_mod = @import("tabs/transcript.zig");

const Terminal = terminal_mod.Terminal;
const Screen = screen_mod.Screen;
const InputReader = input_mod.InputReader;
const Key = input_mod.Key;
const MouseEvent = input_mod.MouseEvent;
const InputEvent = input_mod.InputEvent;
const Rect = widget.Rect;
const clipboard = @import("clipboard.zig");

// Import VM types from parent
const memory = @import("../vm/memory.zig");
const interpreter_mod = @import("../vm/interpreter.zig");
const parser_mod = @import("../compiler/parser.zig");
const codegen_mod = @import("../compiler/codegen.zig");
const filein_mod = @import("../image/filein.zig");
const build_options = @import("build_options");
const ffi_autogen = if (build_options.ffi_enabled) @import("../vm/ffi_autogen.zig") else undefined;

const Heap = memory.Heap;
const Interpreter = interpreter_mod.Interpreter;
const Parser = parser_mod.Parser;
const CodeGenerator = codegen_mod.CodeGenerator;
const Value = @import("../vm/object.zig").Value;
const filein = filein_mod;
const snapshot = @import("../image/snapshot.zig");
const packages_mod = @import("packages.zig");
const PackageRegistry = packages_mod.PackageRegistry;

// Global transcript buffer for TUI mode - transcript primitives write here
var g_transcript_buffer: [8192]u8 = undefined;
var g_transcript_len: usize = 0;
var g_transcript_mutex: std.Thread.Mutex = .{};

// Global App pointer for primitive access
pub var g_app: ?*App = null;

fn transcriptCallback(text: []const u8) void {
    g_transcript_mutex.lock();
    defer g_transcript_mutex.unlock();

    const space_left = g_transcript_buffer.len - g_transcript_len;
    const to_copy = @min(text.len, space_left);
    if (to_copy > 0) {
        @memcpy(g_transcript_buffer[g_transcript_len..][0..to_copy], text[0..to_copy]);
        g_transcript_len += to_copy;
    }
}

pub const App = struct {
    allocator: std.mem.Allocator,
    terminal: Terminal,
    screen: Screen,
    input: InputReader,

    // VM reference
    heap: *Heap,
    interpreter: *Interpreter,

    // UI Components
    tabbar: TabBar,
    statusbar: StatusBar,

    // Tabs
    transcript: TranscriptTab,
    workspace: WorkspaceTab,
    browser: BrowserTab,
    ffi_config: FFIConfigTab,

    // Package management
    package_registry: PackageRegistry,

    active_tab: usize = 0,
    running: bool = true,

    // Status items for different tabs
    workspace_items: []const StatusItem,
    browser_items: []const StatusItem,
    default_items: []const StatusItem,

    // Status message buffer (for dynamic messages)
    status_message_buf: [128]u8 = undefined,

    // Save Image As dialog state
    show_save_as_dialog: bool = false,
    save_as_filename: [128]u8 = undefined,
    save_as_filename_len: usize = 0,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, interp: *Interpreter) !*App {
        var term = try Terminal.init();

        // Enable raw mode and set up terminal
        try term.enableRawMode();
        terminal_mod.ansi.enterAltScreen();
        terminal_mod.ansi.hideCursor();
        terminal_mod.ansi.enableMouse();
        terminal_mod.ansi.enableBracketedPaste();
        terminal_mod.ansi.clear();

        // Initialize clipboard with TTY for OSC 52 support
        clipboard.init(term.tty);

        const width = term.width;
        const height = term.height;

        const screen = try Screen.init(allocator, width, height);

        // Calculate layout regions
        const tabbar_rect = Rect.init(0, 0, width, 1);
        const content_rect = Rect.init(0, 1, width, height -| 2);
        const statusbar_rect = Rect.init(0, height -| 1, width, 1);

        const tabs = [_]@import("widgets/tabbar.zig").Tab{
            .{ .title = "Transcript", .shortcut = '1' },
            .{ .title = "Workspace", .shortcut = '2' },
            .{ .title = "Browser", .shortcut = '3' },
            .{ .title = "FFI Config", .shortcut = '4' },
        };
        const tabbar = TabBar.init(tabbar_rect, &tabs);

        var statusbar = StatusBar.init(statusbar_rect);
        statusbar.setMessage("Ready");

        // Create tabs
        const transcript = try TranscriptTab.init(allocator, content_rect);
        const workspace = try WorkspaceTab.init(allocator, content_rect);
        var browser = try BrowserTab.init(allocator, content_rect);
        const ffi_config = try FFIConfigTab.init(allocator, content_rect);

        // Create package registry with system packages
        var package_registry = PackageRegistry.init(allocator);
        try package_registry.initSystemPackages();

        // Add "All" option first, then all packages to browser
        try browser.addPackage("All");
        for (package_registry.packages.items) |pkg| {
            try browser.addPackage(pkg.name);
        }

        const app = try allocator.create(App);
        app.* = App{
            .allocator = allocator,
            .terminal = term,
            .screen = screen,
            .input = InputReader.init(&term, allocator),
            .heap = heap,
            .interpreter = interp,
            .tabbar = tabbar,
            .statusbar = statusbar,
            .transcript = transcript,
            .workspace = workspace,
            .browser = browser,
            .ffi_config = ffi_config,
            .package_registry = package_registry,
            .workspace_items = &[_]StatusItem{
                .{ .text = "Execute", .key = "Ctrl+D" },
                .{ .text = "Print", .key = "Ctrl+P" },
                .{ .text = "Inspect", .key = "Ctrl+I" },
                .{ .text = "Quit", .key = "Ctrl+Q" },
            },
            .browser_items = &[_]StatusItem{
                .{ .text = "Save", .key = "Ctrl+S" },
                .{ .text = "Export", .key = "Ctrl+E" },
                .{ .text = "New Pkg", .key = "Ctrl+N" },
                .{ .text = "New Class", .key = "Ctrl+A" },
            },
            .default_items = &[_]StatusItem{
                .{ .text = "Quit", .key = "Ctrl+Q" },
            },
        };

        // Add welcome message to transcript
        try app.transcript.addInfo("Zig Smalltalk TUI v0.1");
        try app.transcript.addLine("", .normal);
        try app.transcript.addLine("Switch Tabs:", .normal);
        try app.transcript.addLine("  F1 - Transcript    F2 - Workspace    F3 - Browser    F4 - FFI Config", .normal);
        try app.transcript.addLine("", .normal);
        try app.transcript.addLine("Global Shortcuts:", .normal);
        try app.transcript.addLine("  F9 - Save Image    F12 - Save Image As    Ctrl+Q - Quit", .normal);
        try app.transcript.addLine("", .normal);
        try app.transcript.addLine("Workspace: Ctrl+D execute | Ctrl+P print | Ctrl+I inspect", .normal);
        try app.transcript.addLine("Browser:   Ctrl+S save | Ctrl+E export pkg | Ctrl+N new pkg | Ctrl+A new class", .normal);
        try app.transcript.addLine("", .normal);
        try app.transcript.addLine("Packages: System packages in system-packages/, user packages in packages/", .normal);

        // Set global transcript for primitive output redirection
        transcript_mod.setGlobalTranscript(&app.transcript);

        // Set interpreter transcript callback for primitive output
        interp.setTranscriptCallback(&transcriptCallback);

        // Load classes into browser
        try app.loadClasses();

        // Set up browser callbacks
        app.browser.on_create_class = &browserCreateClass;
        app.browser.on_save_class_definition = &browserSaveClassDefinition;
        app.browser.on_new_method = &browserNewMethod;
        app.browser.on_select_package = &browserSelectPackage;
        app.browser.on_save_package = &browserSavePackage;
        app.browser.on_create_package = &browserCreatePackage;

        // Set global app pointer for callbacks
        g_app = app;

        return app;
    }

    pub fn deinit(self: *App) void {
        // Clear transcript callback
        self.interpreter.setTranscriptCallback(null);
        transcript_mod.setGlobalTranscript(null);
        self.input.deinit();
        self.transcript.deinit();
        self.workspace.deinit();
        self.browser.deinit();
        self.ffi_config.deinit();
        self.package_registry.deinit();
        self.screen.deinit();
        self.terminal.deinit();
        self.allocator.destroy(self);
    }

    pub fn run(self: *App) !void {
        // Initial render
        self.render();
        self.screen.flush();

        // Track last known size to detect changes
        var last_width = self.screen.width;
        var last_height = self.screen.height;

        while (self.running) {
            // Check for window resize EVERY iteration (before and after input)
            const new_size = self.terminal.getSize() catch terminal_mod.Size{ .width = last_width, .height = last_height };
            if (new_size.width != last_width or new_size.height != last_height) {
                last_width = new_size.width;
                last_height = new_size.height;
                self.handleResize(new_size.width, new_size.height);
            }

            // Run pending background processes (give them ~500 instructions per iteration)
            // This enables forked processes to run concurrently with the UI
            _ = self.interpreter.runPendingProcesses(500);

            // Check for transcript output from primitives
            self.flushTranscriptBuffer();

            // Read input (key, mouse, or paste event)
            if (try self.input.readEvent()) |event| {
                switch (event) {
                    .key => |key| self.handleKey(key),
                    .mouse => |mouse| self.handleMouse(mouse),
                    .paste => |text| {
                        self.handlePaste(text);
                        self.input.freePasteBuffer();
                    },
                }

                // Re-render after input
                self.render();
                self.screen.flush();
            }

            // Small sleep to prevent busy-waiting
            std.Thread.sleep(10 * std.time.ns_per_ms);
        }
    }

    /// Run the TUI with integrated Smalltalk process scheduling.
    /// This interleaves UI handling with running Smalltalk processes.
    pub fn runWithScheduler(self: *App) !void {
        // Initial render
        self.render();
        self.screen.flush();

        // Track last known size
        var last_width = self.screen.width;
        var last_height = self.screen.height;

        while (self.running) {
            // Check for window resize
            const new_size = self.terminal.getSize() catch terminal_mod.Size{ .width = last_width, .height = last_height };
            if (new_size.width != last_width or new_size.height != last_height) {
                last_width = new_size.width;
                last_height = new_size.height;
                self.handleResize(new_size.width, new_size.height);
            }

            // Run a small batch of Smalltalk processes
            _ = self.interpreter.runPendingProcesses(200);

            // Flush transcript output
            self.flushTranscriptBuffer();

            // Read and handle input (blocks up to ~100ms via VTIME if no input)
            if (try self.input.readEvent()) |event| {
                switch (event) {
                    .key => |key| self.handleKey(key),
                    .mouse => |mouse| self.handleMouse(mouse),
                    .paste => |text| {
                        self.handlePaste(text);
                        self.input.freePasteBuffer();
                    },
                }

                // Re-render after input
                self.render();
                self.screen.flush();
            }
        }
    }

    /// Process one iteration of the UI loop. Called from Smalltalk UIProcess.
    /// Returns true if UI should continue, false if quit was requested.
    pub fn processOneIteration(self: *App) bool {
        // Check for window resize
        const new_size = self.terminal.getSize() catch terminal_mod.Size{ .width = self.screen.width, .height = self.screen.height };
        if (new_size.width != self.screen.width or new_size.height != self.screen.height) {
            self.handleResize(new_size.width, new_size.height);
        }

        // Flush transcript buffer
        self.flushTranscriptBuffer();

        // Check and handle input (non-blocking)
        if (self.input.readEvent() catch null) |event| {
            switch (event) {
                .key => |key| self.handleKey(key),
                .mouse => |mouse| self.handleMouse(mouse),
                .paste => |text| {
                    self.handlePaste(text);
                    self.input.freePasteBuffer();
                },
            }

            // Re-render after input
            self.render();
            self.screen.flush();
        }

        return self.running;
    }

    /// Check if the UI is still running
    pub fn isRunning(self: *App) bool {
        return self.running;
    }

    fn handleResize(self: *App, width: u16, height: u16) void {
        // Update terminal size
        self.terminal.width = width;
        self.terminal.height = height;

        // Clear the physical terminal first
        terminal_mod.ansi.clear();
        terminal_mod.ansi.moveCursor(0, 0);

        // Resize screen buffer
        self.screen.resize(width, height) catch return;

        // Force full redraw
        self.screen.forceFullRedraw();

        // Re-render and flush immediately
        self.render();
        self.screen.flushFull();
    }

    fn flushTranscriptBuffer(self: *App) void {
        g_transcript_mutex.lock();
        defer g_transcript_mutex.unlock();

        if (g_transcript_len > 0) {
            // Add buffered transcript content to transcript tab
            const content = g_transcript_buffer[0..g_transcript_len];
            self.transcript.addLine(content, .normal) catch {};
            g_transcript_len = 0;

            // Force re-render to show new content
            self.render();
            self.screen.flush();
        }
    }

    fn handleKey(self: *App, key: Key) void {
        // Handle Save As dialog first if visible
        if (self.show_save_as_dialog) {
            self.handleSaveAsDialogKey(key);
            return;
        }

        // Global shortcuts first
        switch (key) {
            .ctrl => |c| {
                switch (c) {
                    3, 17 => { // Ctrl+C or Ctrl+Q - Quit
                        self.running = false;
                        return;
                    },
                    else => {},
                }
            },
            .f1 => {
                self.active_tab = 0;
                self.tabbar.setActiveTab(0);
                self.updateFocus();
                return;
            },
            .f2 => {
                self.active_tab = 1;
                self.tabbar.setActiveTab(1);
                self.updateFocus();
                return;
            },
            .f3 => {
                self.active_tab = 2;
                self.tabbar.setActiveTab(2);
                self.updateFocus();
                return;
            },
            .f4 => {
                self.active_tab = 3;
                self.tabbar.setActiveTab(3);
                self.updateFocus();
                return;
            },
            .ctrl_shift_s, .f9 => {
                // F9 for Save Image (F11 gets captured by terminal for fullscreen)
                // Copy path to local buffer to avoid issues with heap.image_path being modified
                var path_buf: [256]u8 = undefined;
                const src_path = self.heap.image_path orelse "smalltalk.image";
                const len = @min(src_path.len, path_buf.len);
                @memcpy(path_buf[0..len], src_path[0..len]);
                self.saveImage(path_buf[0..len]);
                return;
            },
            .f12 => {
                // Save Image As - show dialog
                self.showSaveAsDialog();
                return;
            },
            // Alt+1-4 as alternative to F1-F4 (works better in PowerShell)
            .alt => |c| {
                switch (c) {
                    '1' => {
                        self.active_tab = 0;
                        self.tabbar.setActiveTab(0);
                        self.updateFocus();
                        return;
                    },
                    '2' => {
                        self.active_tab = 1;
                        self.tabbar.setActiveTab(1);
                        self.updateFocus();
                        return;
                    },
                    '3' => {
                        self.active_tab = 2;
                        self.tabbar.setActiveTab(2);
                        self.updateFocus();
                        return;
                    },
                    '4' => {
                        self.active_tab = 3;
                        self.tabbar.setActiveTab(3);
                        self.updateFocus();
                        return;
                    },
                    else => {},
                }
            },
            .unknown => {
                // Unknown key - already shown in status bar, just return
                return;
            },
            .escape => {
                // Escape - already shown in status bar, just return
                return;
            },
            else => {},
        }

        // Handle workspace shortcuts BEFORE tab bar (Ctrl+D/P/I conflict with Ctrl+1-9)
        if (self.active_tab == 1) {
            switch (key) {
                .ctrl => |c| {
                    switch (c) {
                        4 => { // Ctrl+D - Do It
                            const code = self.workspace.getSelectedOrCurrentLine() catch return;
                            defer self.allocator.free(code);
                            self.executeCode(code, false);
                            return;
                        },
                        16 => { // Ctrl+P - Print It
                            const code = self.workspace.getSelectedOrCurrentLine() catch return;
                            defer self.allocator.free(code);
                            self.executeCode(code, true);
                            return;
                        },
                        9 => { // Ctrl+I - Inspect It
                            const code = self.workspace.getSelectedOrCurrentLine() catch return;
                            defer self.allocator.free(code);
                            self.executeCode(code, true);
                            return;
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }

        // Handle browser BEFORE tab bar so Tab works for pane switching
        if (self.active_tab == 2) {
            // Handle Ctrl+key shortcuts for browser
            if (key == .ctrl) {
                switch (key.ctrl) {
                    19 => { // Ctrl+S - Save method
                        self.saveMethod();
                        return;
                    },
                    14 => { // Ctrl+N - New package
                        self.browser.showNewPackageDialog();
                        return;
                    },
                    else => {},
                }
            }

            const old_class = self.browser.selected_class_name;
            const old_method = self.browser.selected_method_name;
            const old_class_side = self.browser.show_class_side;

            const result = self.browser.handleKey(key);

            // Check if class selection changed
            if (!std.mem.eql(u8, old_class, self.browser.selected_class_name)) {
                self.loadMethodsForClass(self.browser.selected_class_name);
            }

            // Check if method selection changed (or class side toggled)
            if (!std.mem.eql(u8, old_method, self.browser.selected_method_name) or old_class_side != self.browser.show_class_side) {
                self.loadMethodSource(self.browser.selected_class_name, self.browser.selected_method_name, self.browser.show_class_side);
            }

            if (result == .consumed) return;
            if (result == .quit) {
                self.running = false;
                return;
            }
            // Fall through to tab bar if not consumed
        }

        // Handle FFI config BEFORE tab bar so Tab works in Add Library dialog
        if (self.active_tab == 3) {
            const result = self.ffi_config.handleKey(key);
            if (result == .consumed) return;
            if (result == .quit) {
                self.running = false;
                return;
            }
            // Fall through to tab bar if not consumed
        }

        // Handle tab bar input (for switching between main tabs)
        const tab_result = self.tabbar.handleKey(key);
        if (tab_result == .consumed) {
            self.active_tab = self.tabbar.active_tab;
            self.updateFocus();
            return;
        }

        // Pass to active tab (browser and ffi_config handled above)
        const result = switch (self.active_tab) {
            0 => self.transcript.handleKey(key),
            1 => self.workspace.handleKey(key),
            else => .ignored,
        };

        if (result == .quit) {
            self.running = false;
        }
    }

    fn handleMouse(self: *App, mouse: MouseEvent) void {
        const width = self.screen.width;
        const height = self.screen.height;

        // Check if click is in tab bar (row 0) - only for press events
        if (mouse.event_type == .press and mouse.y == 0) {
            // Calculate which tab was clicked based on x position
            var x: u16 = 0;
            for (self.tabbar.tabs, 0..) |tab, i| {
                const tab_width = @as(u16, @intCast(tab.title.len)) + 4;
                if (mouse.x >= x and mouse.x < x + tab_width) {
                    self.active_tab = i;
                    self.tabbar.setActiveTab(i);
                    self.updateFocus();
                    return;
                }
                x += tab_width;
            }
            return;
        }

        // Check if click is in content area (between tab bar and status bar)
        const content_rect = Rect.init(0, 1, width, height -| 2);
        if (content_rect.contains(mouse.x, mouse.y)) {
            // Pass to active tab
            switch (self.active_tab) {
                0 => self.transcript.handleMouse(mouse),
                1 => self.workspace.handleMouse(mouse),
                2 => {
                    const old_class = self.browser.selected_class_name;
                    const old_method = self.browser.selected_method_name;
                    const old_class_side = self.browser.show_class_side;

                    self.browser.handleMouse(mouse);

                    // Check if class selection changed
                    if (!std.mem.eql(u8, old_class, self.browser.selected_class_name)) {
                        self.loadMethodsForClass(self.browser.selected_class_name);
                    }

                    // Check if method selection changed
                    if (!std.mem.eql(u8, old_method, self.browser.selected_method_name) or old_class_side != self.browser.show_class_side) {
                        self.loadMethodSource(self.browser.selected_class_name, self.browser.selected_method_name, self.browser.show_class_side);
                    }
                },
                3 => self.ffi_config.handleMouse(mouse),
                else => {},
            }
            return;
        }

        // Handle scroll wheel anywhere for scrolling the current tab
        if (mouse.button == .scroll_up or mouse.button == .scroll_down) {
            const scroll_dir: i32 = if (mouse.button == .scroll_up) -3 else 3;
            switch (self.active_tab) {
                0 => self.transcript.scroll(scroll_dir),
                1 => self.workspace.scroll(scroll_dir),
                2 => self.browser.scroll(scroll_dir),
                else => {},
            }
        }
    }

    fn handlePaste(self: *App, text: []const u8) void {
        // Paste into the active tab's text area
        switch (self.active_tab) {
            1 => {
                // Workspace - insert pasted text
                for (text) |c| {
                    if (c == '\n') {
                        _ = self.workspace.editor.handleKey(.enter);
                    } else if (c != '\r') {
                        _ = self.workspace.editor.handleKey(.{ .char = c });
                    }
                }
            },
            else => {},
        }
    }

    fn updateFocus(self: *App) void {
        self.transcript.focused = self.active_tab == 0;
        self.workspace.focused = self.active_tab == 1;
        self.browser.focused = self.active_tab == 2;
        self.ffi_config.focused = self.active_tab == 3;
    }

    fn render(self: *App) void {
        const width = self.screen.width;
        const height = self.screen.height;

        // Clear screen and force full redraw to avoid rendering artifacts
        self.screen.clear();
        self.screen.forceFullRedraw();

        // Update rects based on current size
        const tabbar_rect = Rect.init(0, 0, width, 1);
        const content_rect = Rect.init(0, 1, width, height -| 2);
        const statusbar_rect = Rect.init(0, height -| 1, width, 1);

        // Draw tab bar
        self.tabbar.state.rect = tabbar_rect;
        self.tabbar.draw(&self.screen);

        // Update and draw active tab
        self.updateFocus();

        switch (self.active_tab) {
            0 => {
                self.transcript.rect = content_rect;
                self.transcript.draw(&self.screen);
                self.statusbar.setItems(self.default_items);
            },
            1 => {
                self.workspace.updateRect(content_rect);
                self.workspace.draw(&self.screen);
                self.statusbar.setItems(self.workspace_items);
            },
            2 => {
                self.browser.updateRect(content_rect);
                self.browser.draw(&self.screen);
                self.statusbar.setItems(self.browser_items);
            },
            3 => {
                self.ffi_config.updateRect(content_rect);
                self.ffi_config.draw(&self.screen);
                self.statusbar.setItems(self.default_items);
            },
            else => {},
        }

        // Draw status bar
        self.statusbar.state.rect = statusbar_rect;
        self.statusbar.draw(&self.screen);

        // Draw Save As dialog if visible (on top of everything)
        if (self.show_save_as_dialog) {
            self.drawSaveAsDialog();
        }
    }

    fn loadClasses(self: *App) !void {
        // Iterate through class table and add to browser
        const class_table = self.heap.class_table.items;

        for (class_table) |class_val| {
            if (class_val.isNil() or !class_val.isObject()) continue;

            const class_obj = class_val.asObject();

            // Verify class object is valid
            if (class_obj.header.class_index == 0) continue;

            // Use actual object size for field access (old classes may have fewer fields)
            const actual_fields = class_obj.header.size;

            // Get class name
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, actual_fields);
            if (name_val.isNil() or !name_val.isObject()) continue;

            const name_obj = name_val.asObject();
            if (name_obj.header.class_index != Heap.CLASS_SYMBOL) continue;

            const name_size = name_obj.header.size;
            if (name_size == 0 or name_size > 1000) continue;

            const name = name_obj.bytes(name_size);
            if (name.len == 0) continue;

            // Get superclass to determine hierarchy
            const super_val = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, actual_fields);
            var parent_name: ?[]const u8 = null;

            if (!super_val.isNil() and super_val.isObject()) {
                const super_obj = super_val.asObject();
                if (super_obj.header.class_index != 0) {
                    const super_num_fields = super_obj.header.size;
                    const super_name_val = super_obj.getField(Heap.CLASS_FIELD_NAME, super_num_fields);
                    if (!super_name_val.isNil() and super_name_val.isObject()) {
                        const super_name_obj = super_name_val.asObject();
                        if (super_name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            const super_size = super_name_obj.header.size;
                            if (super_size > 0 and super_size < 1000) {
                                parent_name = super_name_obj.bytes(super_size);
                            }
                        }
                    }
                }
            }

            self.browser.addClass(name, parent_name) catch {};

            // Get category from class object (stored in the image)
            // Check if class has the category field (old images may have fewer fields)
            var category_name: []const u8 = "Unpackaged";

            if (actual_fields > Heap.CLASS_FIELD_CATEGORY) {
                const category_val = class_obj.getField(Heap.CLASS_FIELD_CATEGORY, actual_fields);
                if (!category_val.isNil() and category_val.isObject()) {
                    const category_obj = category_val.asObject();
                    if (category_obj.header.class_index == Heap.CLASS_SYMBOL) {
                        const cat_size = category_obj.header.size;
                        if (cat_size > 0 and cat_size < 256) {
                            category_name = category_obj.bytes(cat_size);
                        }
                    }
                }
            }

            // Add class to the package from its category field
            self.package_registry.addClassToPackage(category_name, name) catch {};
        }

        // Sync any new packages from class categories to the browser's package list
        self.syncPackagesToBrowser();

        // Add FFI libraries to the browser
        self.loadFFILibraries();
    }

    /// Sync packages from the registry to the browser's package list
    /// This ensures user packages created from class categories show up
    fn syncPackagesToBrowser(self: *App) void {
        for (self.package_registry.packages.items) |pkg| {
            // Check if package is already in browser's package list
            var found = false;
            for (self.browser.package_list.items.items) |item| {
                if (std.mem.eql(u8, item.text, pkg.name)) {
                    found = true;
                    break;
                }
            }
            // Add if not found
            if (!found) {
                self.browser.addPackage(pkg.name) catch {};
            }
        }
    }

    fn loadFFILibraries(self: *App) void {
        // Skip if FFI is disabled
        if (!build_options.ffi_enabled) return;

        // Add a root node for FFI libraries
        self.browser.addClass("FFI Libraries", null) catch return;

        // Add "FFI Libraries" to the FFI package
        self.package_registry.addClassToPackage("FFI", "FFI Libraries") catch {};

        // Add each FFI library as a child
        for (ffi_autogen.available_libraries) |lib_name| {
            self.browser.addClass(lib_name, "FFI Libraries") catch {};
            // Add each library to the FFI package
            self.package_registry.addClassToPackage("FFI", lib_name) catch {};
        }

        // Expand the FFI Libraries node so children are visible
        for (self.browser.class_tree.roots.items) |root| {
            if (std.mem.eql(u8, root.text, "FFI Libraries")) {
                root.expanded = true;
                self.browser.class_tree.rebuildFlatList();
                break;
            }
        }
    }

    fn isFFILibrary(name: []const u8) bool {
        if (!build_options.ffi_enabled) return false;

        for (ffi_autogen.available_libraries) |lib_name| {
            if (std.mem.eql(u8, name, lib_name)) return true;
        }
        return false;
    }

    fn saveImage(self: *App, path: []const u8) void {
        // TODO: Add FFI cleanup before save - clear external pointers
        // This would iterate through objects and nullify FFI handles

        // Save the snapshot
        snapshot.saveToFile(self.heap, self.allocator, path) catch |err| {
            // Show error in transcript with the path for debugging
            var err_buf: [256]u8 = undefined;
            const err_msg = switch (err) {
                snapshot.SnapshotError.IoError => std.fmt.bufPrint(&err_buf, "I/O error saving to: {s}", .{path}) catch "I/O error saving image",
                snapshot.SnapshotError.OutOfMemory => "Out of memory saving image",
                else => "Error saving image",
            };
            self.transcript.addError(err_msg) catch {};
            self.statusbar.setMessage(err_msg);
            return;
        };

        // Update the heap's image_path so SessionManager >> imagePath works
        // Only update if the path is different from the current one
        const should_update = if (self.heap.image_path) |current|
            !std.mem.eql(u8, current, path)
        else
            true;

        if (should_update) {
            const new_path = self.allocator.dupe(u8, path) catch null;
            if (new_path) |np| {
                if (self.heap.image_path) |old_path| {
                    self.allocator.free(old_path);
                }
                self.heap.image_path = np;
            }
        }

        // Show success message in both transcript and status bar
        var msg_buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "Image saved to {s}", .{path}) catch "Image saved";
        self.transcript.addSuccess(msg) catch {};
        self.statusbar.setMessage(msg);
    }

    fn showSaveAsDialog(self: *App) void {
        self.show_save_as_dialog = true;
        // Pre-fill with current image path or default
        if (self.heap.image_path) |path| {
            const len = @min(path.len, self.save_as_filename.len);
            @memcpy(self.save_as_filename[0..len], path[0..len]);
            self.save_as_filename_len = len;
        } else {
            const default = "smalltalk.image";
            @memcpy(self.save_as_filename[0..default.len], default);
            self.save_as_filename_len = default.len;
        }
    }

    fn handleSaveAsDialogKey(self: *App, key: Key) void {
        switch (key) {
            .escape => {
                self.show_save_as_dialog = false;
            },
            .enter => {
                // Save with the entered filename
                if (self.save_as_filename_len > 0) {
                    self.saveImage(self.save_as_filename[0..self.save_as_filename_len]);
                    self.show_save_as_dialog = false;
                }
            },
            .backspace => {
                if (self.save_as_filename_len > 0) {
                    self.save_as_filename_len -= 1;
                }
            },
            .char => |c| {
                // Only accept ASCII characters for filenames
                if (c < 128 and self.save_as_filename_len < self.save_as_filename.len - 1) {
                    self.save_as_filename[self.save_as_filename_len] = @intCast(c);
                    self.save_as_filename_len += 1;
                }
            },
            else => {},
        }
    }

    fn drawSaveAsDialog(self: *App) void {
        const dialog_width: u16 = 50;
        const dialog_height: u16 = 7;
        const dialog_x = (self.screen.width -| dialog_width) / 2;
        const dialog_y = (self.screen.height -| dialog_height) / 2;

        const dialog_rect = Rect.init(dialog_x, dialog_y, dialog_width, dialog_height);

        // Draw dialog background
        self.screen.fillRect(dialog_rect.x, dialog_rect.y, dialog_rect.width, dialog_rect.height, ' ', style_mod.styles.normal);
        widget.drawBorderRounded(&self.screen, dialog_rect, true);
        widget.drawTitle(&self.screen, dialog_rect, "Save Image As (F12)", true);

        const label_style = style_mod.styles.normal;
        const field_style = style_mod.Style{ .fg = style_mod.theme.text, .bg = style_mod.theme.surface1, .bold = false };

        // File name field
        self.screen.drawText(dialog_x + 2, dialog_y + 2, "Filename:", label_style);
        self.screen.fillRect(dialog_x + 12, dialog_y + 2, dialog_width - 16, 1, ' ', field_style);
        self.screen.drawTextClipped(dialog_x + 12, dialog_y + 2, self.save_as_filename[0..self.save_as_filename_len], dialog_width - 16, field_style);

        // Help text
        self.screen.drawText(dialog_x + 2, dialog_y + 4, "Enter: save | Esc: cancel", style_mod.styles.dim);

        // Show cursor at end of filename
        self.screen.setCursor(dialog_x + 12 + @as(u16, @intCast(self.save_as_filename_len)), dialog_y + 2);
    }

    pub fn executeCode(self: *App, code: []const u8, print_result: bool) void {
        const trimmed = std.mem.trim(u8, code, " \t\r\n");

        // Handle special "filein" command
        if (std.mem.startsWith(u8, trimmed, "filein ")) {
            const path = std.mem.trim(u8, trimmed[7..], " \t\r\n");
            self.loadFileIn(path);
            return;
        }

        // Add execution message to transcript
        self.transcript.addInfo("Evaluating...") catch {};

        // Compile and execute
        const result = self.compileAndExecute(code) catch |err| {
            var buf: [256]u8 = undefined;
            const err_msg = std.fmt.bufPrint(&buf, "Error: {s}", .{@errorName(err)}) catch "Error";
            self.transcript.addError(err_msg) catch {};
            return;
        };

        if (print_result) {
            // Format and display result
            var buf: [512]u8 = undefined;
            const result_str = self.formatValue(&buf, result);
            self.transcript.addSuccess(result_str) catch {};

            // Also insert into workspace
            self.workspace.insertResult(result_str) catch {};
        } else {
            self.transcript.addSuccess("Done") catch {};
        }
    }

    /// Load a .st file using filein
    fn loadFileIn(self: *App, path: []const u8) void {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Loading {s}...", .{path}) catch "Loading...";
        self.transcript.addInfo(msg) catch {};

        var file_in = filein.FileIn.init(self.allocator, self.heap);
        defer file_in.deinit();

        file_in.loadFile(path) catch |err| {
            const err_msg = switch (err) {
                filein.FileInError.FileNotFound => "File not found",
                filein.FileInError.ClassNotFound => "Class not found",
                filein.FileInError.InvalidMethodDefinition => "Invalid method definition",
                filein.FileInError.CompilationFailed => "Compilation failed",
                else => "Unknown error",
            };
            var err_buf: [256]u8 = undefined;
            const full_err = std.fmt.bufPrint(&err_buf, "Error loading {s}: {s}", .{ path, err_msg }) catch "Load error";
            self.transcript.addError(full_err) catch {};
            return;
        };

        var result_buf: [256]u8 = undefined;
        const result_msg = std.fmt.bufPrint(&result_buf, "Loaded {d} methods, {d} classes from {s}", .{ file_in.methods_loaded, file_in.classes_defined, path }) catch "Loaded";
        self.transcript.addSuccess(result_msg) catch {};

        // Refresh the browser class list
        self.loadClasses() catch {};
    }

    fn compileAndExecute(self: *App, source: []const u8) !Value {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const temp_alloc = arena.allocator();

        // Parse
        var p = Parser.init(temp_alloc, source);
        const ast = try p.parseExpression();

        // Compile
        var gen = CodeGenerator.init(temp_alloc, self.heap, temp_alloc);
        defer gen.deinit();

        const method = try gen.compileDoIt(ast);

        // Execute
        return try self.interpreter.execute(method, Value.nil, &[_]Value{});
    }

    fn formatValue(self: *App, buf: []u8, value: Value) []const u8 {
        if (value.isNil()) {
            return "nil";
        } else if (value.isTrue()) {
            return "true";
        } else if (value.isFalse()) {
            return "false";
        } else if (value.isSmallInt()) {
            return std.fmt.bufPrint(buf, "{d}", .{value.asSmallInt()}) catch "?";
        } else if (value.isCharacter()) {
            const cp = value.asCharacter();
            if (cp < 128) {
                return std.fmt.bufPrint(buf, "${c}", .{@as(u8, @intCast(cp))}) catch "?";
            } else {
                return std.fmt.bufPrint(buf, "$\\u{{{x}}}", .{cp}) catch "?";
            }
        } else if (value.isObject()) {
            const obj = value.asObject();
            const class_index = obj.header.class_index;

            if (class_index == Heap.CLASS_STRING or class_index == Heap.CLASS_SYMBOL) {
                const size = obj.header.size;
                if (size > 0) {
                    const bytes_slice = obj.bytes(size);
                    if (class_index == Heap.CLASS_SYMBOL) {
                        return std.fmt.bufPrint(buf, "#'{s}'", .{bytes_slice}) catch "#'...'";
                    } else {
                        return std.fmt.bufPrint(buf, "'{s}'", .{bytes_slice}) catch "'...'";
                    }
                } else {
                    return if (class_index == Heap.CLASS_SYMBOL) "#''" else "''";
                }
            } else if (class_index == Heap.CLASS_ARRAY) {
                return "#(...)";
            } else {
                // Get class name
                const class = self.heap.getClass(class_index);
                var class_name: []const u8 = "Object";
                if (class.isObject()) {
                    const class_obj = class.asObject();
                    const class_num_fields = class_obj.header.size;
                    const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, class_num_fields);
                    if (name_val.isObject()) {
                        const name_obj = name_val.asObject();
                        if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            class_name = name_obj.bytes(name_obj.header.size);
                        }
                    }
                }
                return std.fmt.bufPrint(buf, "<{s} @{x}>", .{ class_name, @intFromPtr(obj) }) catch "<?>";
            }
        }
        return std.fmt.bufPrint(buf, "<Unknown: {x}>", .{value.bits}) catch "?";
    }

    fn loadMethodsForClass(self: *App, class_name: []const u8) void {
        // Skip empty class names
        if (class_name.len == 0) return;

        // Save any pending source before switching classes
        self.browser.savePendingSource();

        // Check if it's an FFI library
        if (isFFILibrary(class_name)) {
            self.loadFFIFunctions(class_name);
            return;
        }

        // Skip special category nodes
        if (std.mem.eql(u8, class_name, "FFI Libraries")) {
            self.browser.setInstanceMethods(&[_][]const u8{}) catch {};
            self.browser.setClassMethods(&[_][]const u8{}) catch {};
            self.browser.setSource("\"Select an FFI library to view its functions\"") catch {};
            return;
        }

        // Find class in heap
        const class_val = self.findClass(class_name) orelse return;
        if (!class_val.isObject()) return;

        const class_obj = class_val.asObject();
        const num_fields = class_obj.header.size;

        // Load instance methods
        var instance_methods: std.ArrayList([]const u8) = .empty;
        defer instance_methods.deinit(self.allocator);
        self.collectMethodsFromDict(class_obj, &instance_methods);
        // Sort alphabetically
        std.mem.sort([]const u8, instance_methods.items, {}, struct {
            fn lessThan(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.order(u8, a, b) == .lt;
            }
        }.lessThan);
        self.browser.setInstanceMethods(instance_methods.items) catch {};

        // Load class methods from metaclass
        var class_methods: std.ArrayList([]const u8) = .empty;
        defer class_methods.deinit(self.allocator);

        const metaclass_val = class_obj.getField(Heap.CLASS_FIELD_METACLASS, num_fields);
        if (!metaclass_val.isNil() and metaclass_val.isObject()) {
            const metaclass_obj = metaclass_val.asObject();
            self.collectMethodsFromDict(metaclass_obj, &class_methods);
        }
        // Sort alphabetically
        std.mem.sort([]const u8, class_methods.items, {}, struct {
            fn lessThan(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.order(u8, a, b) == .lt;
            }
        }.lessThan);
        self.browser.setClassMethods(class_methods.items) catch {};

        // Show class definition in source pane (editable)
        self.showClassDefinition(class_name, class_obj);
    }

    fn showClassDefinition(self: *App, class_name: []const u8, class_obj: *@import("../vm/object.zig").Object) void {
        // Build class definition string
        var def_buf: [1024]u8 = undefined;

        // Use actual object size for field access
        const num_fields = class_obj.header.size;

        // Get superclass name
        const superclass_val = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, num_fields);
        var super_name: []const u8 = "nil";
        if (!superclass_val.isNil() and superclass_val.isObject()) {
            const super_obj = superclass_val.asObject();
            const super_num_fields = super_obj.header.size;
            const name_val = super_obj.getField(Heap.CLASS_FIELD_NAME, super_num_fields);
            if (!name_val.isNil() and name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL or name_obj.header.class_index == Heap.CLASS_STRING) {
                    const size = name_obj.header.size;
                    if (size > 0 and size < 256) {
                        super_name = name_obj.bytes(size);
                    }
                }
            }
        }

        // Get instance variable names
        // INST_VARS can be either a String or an Array of Symbols
        var inst_var_buf: [512]u8 = undefined;
        var inst_var_names: []const u8 = "";
        const inst_vars_val = class_obj.getField(Heap.CLASS_FIELD_INST_VARS, num_fields);
        if (!inst_vars_val.isNil() and inst_vars_val.isObject()) {
            const inst_vars_obj = inst_vars_val.asObject();
            const class_idx = inst_vars_obj.header.class_index;

            if (class_idx == Heap.CLASS_STRING or class_idx == Heap.CLASS_SYMBOL) {
                // It's a String - read bytes directly
                const size = inst_vars_obj.header.size;
                if (size > 0 and size < 512) {
                    inst_var_names = inst_vars_obj.bytes(size);
                }
            } else if (class_idx == Heap.CLASS_ARRAY) {
                // It's an Array of Symbols - iterate and build names
                const count = inst_vars_obj.header.size;
                var pos: usize = 0;
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const sym_val = inst_vars_obj.getField(i, count);
                    if (sym_val.isObject()) {
                        const sym_obj = sym_val.asObject();
                        if (sym_obj.header.class_index == Heap.CLASS_SYMBOL or
                            sym_obj.header.class_index == Heap.CLASS_STRING)
                        {
                            const sym_size = sym_obj.header.size;
                            if (sym_size > 0 and sym_size < 128 and pos + sym_size + 1 < inst_var_buf.len) {
                                if (pos > 0) {
                                    inst_var_buf[pos] = ' ';
                                    pos += 1;
                                }
                                const sym_bytes = sym_obj.bytes(sym_size);
                                @memcpy(inst_var_buf[pos .. pos + sym_size], sym_bytes);
                                pos += sym_size;
                            }
                        }
                    }
                }
                if (pos > 0) {
                    inst_var_names = inst_var_buf[0..pos];
                }
            }
        }

        // Build the definition
        const definition = std.fmt.bufPrint(&def_buf,
            \\"{s} class definition"
            \\"Edit and press Ctrl+S to save changes"
            \\
            \\{s} subclass: #{s}
            \\    instanceVariableNames: '{s}'
            \\    classVariableNames: ''
            \\    poolDictionaries: ''
        , .{ class_name, super_name, class_name, inst_var_names }) catch class_name;

        self.browser.setClassDefinition(definition) catch {};
    }

    fn loadFFIFunctions(self: *App, lib_name: []const u8) void {
        // Skip if FFI is disabled
        if (!build_options.ffi_enabled) {
            self.browser.setInstanceMethods(&[_][]const u8{}) catch {};
            self.browser.setClassMethods(&[_][]const u8{}) catch {};
            self.browser.setSource("FFI is not available in this build.") catch {};
            return;
        }

        // Get functions for this library
        if (ffi_autogen.getLibraryFunctions(lib_name)) |functions| {
            // Populate instance methods with FFI selectors (Smalltalk-style)
            var func_names: std.ArrayList([]const u8) = .empty;
            defer func_names.deinit(self.allocator);

            for (functions) |func| {
                // Build Smalltalk selector from function name and arg count
                const selector = buildFFISelector(func.name, func.arg_count);
                // Need to dupe since buildFFISelector uses static buffer
                const duped = self.allocator.dupe(u8, selector) catch continue;
                func_names.append(self.allocator, duped) catch {};
            }

            self.browser.setInstanceMethods(func_names.items) catch {};
            self.browser.setClassMethods(&[_][]const u8{}) catch {};

            // Show summary in source pane
            var buf: [512]u8 = undefined;
            const summary = std.fmt.bufPrint(&buf,
                \\"{s} - FFI Library"
                \\"Available functions: {d}"
                \\""
                \\"Usage from Smalltalk:"
                \\"  '{s}' ffiCall: #functionName with: {{ arg1. arg2 }}"
                \\""
                \\"Example:"
                \\"  '{s}' ffiCall: #sin with: {{ 1.5708 }}"
            , .{ lib_name, functions.len, lib_name, lib_name }) catch lib_name;

            self.browser.setSource(summary) catch {};
        } else {
            self.browser.setInstanceMethods(&[_][]const u8{}) catch {};
            self.browser.setClassMethods(&[_][]const u8{}) catch {};
            self.browser.setSource("\"FFI library not found\"") catch {};
        }
    }

    fn collectMethodsFromDict(self: *App, class_obj: *@import("../vm/object.zig").Object, methods: *std.ArrayList([]const u8)) void {
        const CompiledMethod = @import("../vm/object.zig").CompiledMethod;
        const num_fields = class_obj.header.size;
        const method_dict_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, num_fields);
        if (method_dict_val.isNil()) return;
        if (!method_dict_val.isObject()) return;

        const dict_obj = method_dict_val.asObject();
        const dict_size = dict_obj.header.size;

        // Safety check for reasonable dictionary size
        if (dict_size > 10000) return;

        // Method dictionary is a flat array: [selector1, method1, selector2, method2, ...]
        var i: usize = 0;
        while (i + 1 < dict_size) : (i += 2) {
            const selector_val = dict_obj.getField(i, dict_size);
            const method_val = dict_obj.getField(i + 1, dict_size);

            // Skip nil entries
            if (selector_val.isNil()) continue;
            if (!selector_val.isObject()) continue;

            // Skip auto-generated primitive methods without source code
            if (method_val.isObject()) {
                const method_obj = method_val.asObject();
                const cm: *CompiledMethod = @ptrCast(@alignCast(method_obj));
                // Skip if it's a primitive method without stored source
                if (cm.header.primitive_index != 0 and !cm.header.flags.has_source) {
                    continue;
                }
            }

            const selector_obj = selector_val.asObject();
            if (selector_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const selector_size = selector_obj.header.size;
                if (selector_size > 0 and selector_size < 1000) {
                    const selector = selector_obj.bytes(selector_size);
                    methods.append(self.allocator, selector) catch {};
                }
            }
        }
    }

    fn loadMethodSource(self: *App, class_name: []const u8, method_name: []const u8, class_side: bool) void {
        // Skip empty names
        if (class_name.len == 0 or method_name.len == 0) return;

        // Save pending source before navigating away
        self.browser.savePendingSource();

        // Try to restore pending source if navigating back to same context
        if (self.browser.restorePendingSource()) {
            self.browser.source_editor.readonly = false;
            return;
        }

        // Check if it's an FFI library
        if (isFFILibrary(class_name)) {
            self.loadFFIFunctionSource(class_name, method_name);
            return;
        }

        // Find class
        const class_val = self.findClass(class_name) orelse return;
        if (!class_val.isObject()) return;

        var class_obj = class_val.asObject();
        var obj_num_fields = class_obj.header.size;

        // If looking for class-side method, use metaclass
        if (class_side) {
            const metaclass_val = class_obj.getField(Heap.CLASS_FIELD_METACLASS, obj_num_fields);
            if (metaclass_val.isNil() or !metaclass_val.isObject()) {
                self.browser.setSource("\"No metaclass\"") catch {};
                return;
            }
            class_obj = metaclass_val.asObject();
            obj_num_fields = class_obj.header.size;
        }

        const method_dict_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, obj_num_fields);
        if (method_dict_val.isNil() or !method_dict_val.isObject()) {
            self.browser.setSource("\"No methods\"") catch {};
            return;
        }

        // Look up method by selector
        const method_val = self.lookupMethod(method_dict_val, method_name) orelse {
            var buf: [256]u8 = undefined;
            const placeholder = std.fmt.bufPrint(&buf, "{s}\n    \"Method not found\"\n    ^ self", .{method_name}) catch method_name;
            self.browser.setSource(placeholder) catch {};
            return;
        };
        if (!method_val.isObject()) return;

        const method_obj = method_val.asObject();

        // Cast to CompiledMethod and get source
        const cm = @as(*@import("../vm/object.zig").CompiledMethod, @ptrCast(@alignCast(method_obj)));

        // Use the proper getSource method which checks the has_source flag
        if (cm.getSource()) |source| {
            self.browser.setSource(source) catch {};
            self.browser.source_editor.readonly = false; // Enable editing
            return;
        }

        // No source stored - check if it's a primitive method
        var buf: [512]u8 = undefined;
        if (cm.header.primitive_index != 0) {
            // Primitive method - show primitive info
            const info = std.fmt.bufPrint(&buf,
                "{s}\n    \"Primitive method\"\n    <primitive: {d}>\n    ^ self primitiveFailed",
                .{ method_name, cm.header.primitive_index }
            ) catch method_name;
            self.browser.setSource(info) catch {};
        } else {
            // Regular method without stored source
            const info = std.fmt.bufPrint(&buf,
                "{s}\n    \"Source not stored\"\n    \"Bytecodes: {d}, Literals: {d}\"\n    ^ self",
                .{ method_name, cm.header.bytecode_size, cm.header.num_literals }
            ) catch method_name;
            self.browser.setSource(info) catch {};
        }
    }

    fn saveMethod(self: *App) void {
        const class_name = self.browser.selected_class_name;
        const class_side = self.browser.show_class_side;

        // Check if we have a class selected
        if (class_name.len == 0) {
            self.setStatusMessage("No class selected");
            return;
        }

        // Check if source editor is readonly (no editable method)
        if (self.browser.source_editor.readonly) {
            self.setStatusMessage("No editable method");
            return;
        }

        // Get the source code from the editor
        const source = self.browser.source_editor.getText(self.allocator) catch {
            self.setStatusMessage("Error: Could not get source");
            return;
        };
        defer self.allocator.free(source);

        if (source.len == 0) {
            self.setStatusMessage("Error: Empty source");
            return;
        }

        // Compile and install the method
        self.compileAndInstallMethod(class_name, source, class_side) catch |err| {
            // Show error in status bar
            const msg = std.fmt.bufPrint(&self.status_message_buf, "Compile error: {s}", .{@errorName(err)}) catch "Compile error";
            self.statusbar.setMessage(msg);
            return;
        };

        // Success - extract selector for display
        const selector = filein.extractSelector(source) orelse "method";
        const msg = std.fmt.bufPrint(&self.status_message_buf, "Saved: {s} >> {s}", .{ class_name, selector }) catch "Saved";
        self.statusbar.setMessage(msg);

        // Update the selected method name if it changed
        if (filein.extractSelector(source)) |sel| {
            // Find or create an owned copy - for now just update if same
            self.browser.selected_method_name = sel;
        }

        // Reload methods to reflect any changes
        self.loadMethodsForClass(class_name);
    }

    fn compileAndInstallMethod(self: *App, class_name: []const u8, source: []const u8, class_side: bool) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const temp_alloc = arena.allocator();

        // Find the class
        const class_val = self.findClass(class_name) orelse return error.ClassNotFound;
        if (!class_val.isObject()) return error.ClassNotFound;

        var target_class = class_val.asObject();
        var target_num_fields = target_class.header.size;

        // If class-side, use metaclass
        if (class_side) {
            const metaclass_val = target_class.getField(Heap.CLASS_FIELD_METACLASS, target_num_fields);
            if (metaclass_val.isNil() or !metaclass_val.isObject()) return error.NoMetaclass;
            target_class = metaclass_val.asObject();
            target_num_fields = target_class.header.size;
        }

        // Extract selector from source
        const selector = filein.extractSelector(source) orelse return error.InvalidSelector;

        // Parse the method
        var parser = Parser.init(temp_alloc, source);
        const ast = parser.parseMethod() catch return error.ParseError;

        // Set up code generator with instance variables
        var gen = CodeGenerator.init(temp_alloc, self.heap, temp_alloc);
        defer gen.deinit();

        // Collect instance variables from the class hierarchy
        var inst_var_list: std.ArrayList([]const u8) = .empty;
        var walk_class = class_val;
        while (walk_class.isObject()) {
            const walk_obj = walk_class.asObject();
            const walk_num_fields = walk_obj.header.size;
            const inst_vars = walk_obj.getField(Heap.CLASS_FIELD_INST_VARS, walk_num_fields);
            if (inst_vars.isObject()) {
                const vars_array = inst_vars.asObject();
                const vars_size = vars_array.header.size;
                var i: usize = vars_size;
                while (i > 0) {
                    i -= 1;
                    const var_sym = vars_array.getField(i, vars_size);
                    if (var_sym.isObject()) {
                        const sym_obj = var_sym.asObject();
                        if (sym_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            const var_name = sym_obj.bytes(sym_obj.header.size);
                            inst_var_list.insert(temp_alloc, 0, var_name) catch {};
                        }
                    }
                }
            }
            walk_class = walk_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, walk_num_fields);
        }
        gen.instance_variables = inst_var_list.items;

        // Set source code for storage in method
        gen.source_code = source;

        // Compile the method
        const method = gen.compileMethod(ast) catch return error.CompilationFailed;

        // Install the method in the class
        try filein.installMethodInClass(self.heap, target_class, selector, method, class_side);
    }

    fn setStatusMessage(self: *App, msg: []const u8) void {
        self.statusbar.setMessage(msg);
    }

    fn showDebugKeyBytes(self: *App) void {
        const len = input_mod.InputReader.debug_last_len;
        if (len == 0) {
            self.statusbar.setMessage("Key: (no bytes captured)");
            return;
        }

        var buf: [128]u8 = undefined;
        var pos: usize = 0;

        // Write prefix
        const prefix = "Bytes[";
        @memcpy(buf[pos..][0..prefix.len], prefix);
        pos += prefix.len;

        // Write byte count
        const count_str = std.fmt.bufPrint(buf[pos..], "{d}]: ", .{len}) catch "";
        pos += count_str.len;

        // Write each byte as hex
        for (input_mod.InputReader.debug_last_bytes[0..len]) |byte| {
            if (pos + 5 >= buf.len) break;
            const hex = std.fmt.bufPrint(buf[pos..], "{X:0>2} ", .{byte}) catch break;
            pos += hex.len;
        }

        // Also show as chars if printable
        if (pos + len + 4 < buf.len) {
            @memcpy(buf[pos..][0..2], " '");
            pos += 2;
            for (input_mod.InputReader.debug_last_bytes[0..len]) |byte| {
                if (pos >= buf.len - 2) break;
                if (byte >= 32 and byte < 127) {
                    buf[pos] = byte;
                } else {
                    buf[pos] = '.';
                }
                pos += 1;
            }
            buf[pos] = '\'';
            pos += 1;
        }

        self.statusbar.setMessage(buf[0..pos]);
    }

    fn loadFFIFunctionSource(self: *App, lib_name: []const u8, func_name: []const u8) void {
        if (!build_options.ffi_enabled) {
            self.browser.setSource("\"FFI is not available in this build\"") catch {};
            return;
        }

        // Extract base function name from selector (everything before first colon)
        // e.g., "InitWindow:with:and:" -> "InitWindow"
        const base_name = if (std.mem.indexOf(u8, func_name, ":")) |colon_pos|
            func_name[0..colon_pos]
        else
            func_name;

        if (ffi_autogen.getLibraryFunctions(lib_name)) |functions| {
            for (functions) |func| {
                if (std.mem.eql(u8, func.name, base_name)) {
                    // Generate actual method source code
                    var buf: [2048]u8 = undefined;
                    var fbs = std.io.fixedBufferStream(&buf);
                    const writer = fbs.writer();

                    // Build method selector
                    if (func.arg_count == 0) {
                        writer.print("{s}\n", .{func.name}) catch {};
                    } else if (func.arg_count == 1) {
                        writer.print("{s}: arg1\n", .{func.name}) catch {};
                    } else {
                        writer.print("{s}: arg1", .{func.name}) catch {};
                        var i: usize = 1;
                        while (i < func.arg_count) : (i += 1) {
                            const keyword = switch (i) {
                                1 => " with:",
                                2 => " and:",
                                3 => " also:",
                                else => " arg:",
                            };
                            writer.print("{s} arg{d}", .{ keyword, i + 1 }) catch {};
                        }
                        writer.writeAll("\n") catch {};
                    }

                    // Add comment with type info (using Smalltalk-friendly names)
                    writer.writeAll("    \"FFI: ") catch {};
                    writer.print("{s}(", .{func.name}) catch {};
                    var j: usize = 0;
                    while (j < func.arg_count) : (j += 1) {
                        if (j > 0) writer.writeAll(", ") catch {};
                        writer.print("{s}", .{simplifyTypeName(func.arg_types[j])}) catch {};
                    }
                    writer.print(") -> {s}\"\n", .{simplifyTypeName(func.return_type)}) catch {};

                    // Add actual implementation
                    writer.print("    ^'{s}' ffiCall: #{s} with: {{", .{ lib_name, func.name }) catch {};
                    j = 0;
                    while (j < func.arg_count) : (j += 1) {
                        if (j > 0) writer.writeAll(". ") catch {};
                        writer.print(" arg{d}", .{j + 1}) catch {};
                    }
                    writer.writeAll(" }" ) catch {};

                    self.browser.setSource(fbs.getWritten()) catch {};
                    return;
                }
            }
        }

        self.browser.setSource("\"FFI function not found\"") catch {};
    }

    fn generateArgPlaceholders(_: *App, count: usize) []const u8 {
        return switch (count) {
            0 => "",
            1 => "arg1",
            2 => "arg1. arg2",
            3 => "arg1. arg2. arg3",
            4 => "arg1. arg2. arg3. arg4",
            else => "...",
        };
    }

    fn generateExampleArgs(_: *App, count: usize) []const u8 {
        return switch (count) {
            0 => "",
            1 => "1.0",
            2 => "1.0. 2.0",
            3 => "1.0. 2.0. 3.0",
            4 => "1.0. 2.0. 3.0. 4.0",
            else => "...",
        };
    }

    /// Build a Smalltalk selector from a C function name and arg count
    /// e.g., "sin" with 1 arg -> "sin:"
    ///       "pow" with 2 args -> "pow:with:"
    ///       "InitWindow" with 3 args -> "InitWindow:with:and:"
    fn buildFFISelector(func_name: []const u8, arg_count: usize) []const u8 {
        const Static = struct {
            var buf: [256]u8 = undefined;
        };

        var pos: usize = 0;

        // Copy function name
        for (func_name) |c| {
            if (pos < Static.buf.len - 1) {
                Static.buf[pos] = c;
                pos += 1;
            }
        }

        if (arg_count == 0) {
            return Static.buf[0..pos];
        }

        // Add first colon
        if (pos < Static.buf.len - 1) {
            Static.buf[pos] = ':';
            pos += 1;
        }

        // Add additional keyword args
        var i: usize = 1;
        while (i < arg_count) : (i += 1) {
            const keyword = switch (i) {
                1 => "with:",
                2 => "and:",
                3 => "also:",
                4 => "plus:",
                else => "arg:",
            };
            for (keyword) |c| {
                if (pos < Static.buf.len - 1) {
                    Static.buf[pos] = c;
                    pos += 1;
                }
            }
        }

        return Static.buf[0..pos];
    }

    /// Convert C type names to Smalltalk-friendly type names
    fn simplifyTypeName(full_name: []const u8) []const u8 {
        // Handle common patterns
        if (std.mem.eql(u8, full_name, "f64")) return "Float";
        if (std.mem.eql(u8, full_name, "f32")) return "Float";
        if (std.mem.eql(u8, full_name, "i32")) return "Integer";
        if (std.mem.eql(u8, full_name, "i64")) return "Integer";
        if (std.mem.eql(u8, full_name, "u32")) return "Integer";
        if (std.mem.eql(u8, full_name, "u64")) return "Integer";
        if (std.mem.eql(u8, full_name, "u8")) return "Integer";
        if (std.mem.eql(u8, full_name, "i8")) return "Integer";
        if (std.mem.eql(u8, full_name, "c_int")) return "Integer";
        if (std.mem.eql(u8, full_name, "c_uint")) return "Integer";
        if (std.mem.eql(u8, full_name, "c_long")) return "Integer";
        if (std.mem.eql(u8, full_name, "c_ulong")) return "Integer";
        if (std.mem.eql(u8, full_name, "c_float")) return "Float";
        if (std.mem.eql(u8, full_name, "c_double")) return "Float";
        if (std.mem.eql(u8, full_name, "void")) return "nil";
        if (std.mem.eql(u8, full_name, "bool")) return "Boolean";

        // Handle pointers - look for "[*c]" or "*"
        const is_pointer = std.mem.indexOf(u8, full_name, "*") != null or
            std.mem.indexOf(u8, full_name, "[*c]") != null;

        if (is_pointer) {
            // Check for string types (char* or u8*)
            if (std.mem.indexOf(u8, full_name, "u8") != null) return "String";
            if (std.mem.indexOf(u8, full_name, "char") != null) return "String";
            // Other pointers - could be structs or raw pointers
            return "Pointer/ByteArray";
        }

        // Check for struct types (often start with uppercase in C)
        if (full_name.len > 0 and full_name[0] >= 'A' and full_name[0] <= 'Z') {
            return "Struct (ByteArray)";
        }

        // Return as-is for unknown types
        return full_name;
    }

    fn findClass(self: *App, name: []const u8) ?Value {
        // Look in globals dictionary
        return self.heap.globals.get(name);
    }

    fn lookupMethod(self: *App, dict_val: Value, selector: []const u8) ?Value {
        _ = self;
        if (dict_val.isNil() or !dict_val.isObject()) return null;
        if (selector.len == 0) return null;

        const dict_obj = dict_val.asObject();
        const dict_size = dict_obj.header.size;

        // Safety check for reasonable size
        if (dict_size > 10000) return null;

        // Method dictionary is a flat array: [selector1, method1, selector2, method2, ...]
        // Iterate in pairs
        var i: usize = 0;
        while (i + 1 < dict_size) : (i += 2) {
            const key_val = dict_obj.getField(i, dict_size);
            if (key_val.isNil()) continue;
            if (!key_val.isObject()) continue;

            const key_obj = key_val.asObject();
            if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const key_size = key_obj.header.size;
                if (key_size > 0 and key_size < 1000) {
                    const key_str = key_obj.bytes(key_size);
                    if (std.mem.eql(u8, key_str, selector)) {
                        // Method is at the next index
                        return dict_obj.getField(i + 1, dict_size);
                    }
                }
            }
        }

        return null;
    }
};

// Callback for browser - create a new class
fn browserCreateClass(browser: *BrowserTab, class_name: []const u8, superclass_name: []const u8, inst_vars: []const u8) void {
    const app = g_app orelse return;

    // Find superclass
    const superclass_value = if (superclass_name.len == 0 or std.mem.eql(u8, superclass_name, "nil"))
        Value.nil
    else
        app.heap.getGlobal(superclass_name) orelse {
            browser.setStatus("Error: Superclass not found", true);
            return;
        };

    const Object = @import("../vm/object.zig").Object;
    const superclass_obj: ?*Object = if (superclass_value.isObject()) superclass_value.asObject() else null;

    // Determine the package/category for the new class
    const pkg_name = if (browser.selected_package_name.len > 0 and !std.mem.eql(u8, browser.selected_package_name, "All"))
        browser.selected_package_name
    else
        "Unpackaged";

    // Create the class using filein's createDynamicClass
    const new_class = filein.createDynamicClass(
        app.heap,
        class_name,
        superclass_obj,
        inst_vars,
        "", // class_var_names
        "", // pool_dict_names
        "", // class_inst_var_names
        .normal, // class_format
        null, // existing_class
        pkg_name, // category (package name)
    ) catch |err| {
        var buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error creating class: {s}", .{@errorName(err)}) catch "Error creating class";
        browser.setStatus(msg, true);
        return;
    };

    // Register the class as a global
    app.heap.setGlobal(class_name, Value.fromObject(new_class)) catch |err| {
        var buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error registering class: {s}", .{@errorName(err)}) catch "Error registering class";
        browser.setStatus(msg, true);
        return;
    };

    // Update browser class tree
    app.browser.addClass(class_name, if (superclass_name.len > 0) superclass_name else null) catch {};

    // Add to TUI package registry (for UI display)
    app.package_registry.addClassToPackage(pkg_name, class_name) catch {};

    // Refresh the class filter to show the new class
    if (app.package_registry.getPackage(pkg_name)) |package| {
        browser.filterClassesByNames(package.classes.items);
    }

    var buf: [128]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "Created class: {s} in {s}", .{ class_name, pkg_name }) catch "Class created";
    browser.setStatus(msg, false);

    // Add to transcript
    app.transcript.addSuccess(msg) catch {};
}

// Callback for browser - save class definition changes (instance variables)
fn browserSaveClassDefinition(browser: *BrowserTab, class_name: []const u8, definition: []const u8) void {
    const app = g_app orelse return;

    // Parse the definition to extract instance variable names
    // Look for: instanceVariableNames: 'x y z'
    var inst_vars: []const u8 = "";
    if (std.mem.indexOf(u8, definition, "instanceVariableNames: '")) |start_pos| {
        const after_quote = start_pos + "instanceVariableNames: '".len;
        if (std.mem.indexOfPos(u8, definition, after_quote, "'")) |end_pos| {
            inst_vars = definition[after_quote..end_pos];
        }
    }

    // Find the class
    const class_val = app.findClass(class_name) orelse {
        browser.setStatus("Error: Class not found", true);
        return;
    };

    if (!class_val.isObject()) {
        browser.setStatus("Error: Invalid class", true);
        return;
    }

    const class_obj = class_val.asObject();
    const num_fields = class_obj.header.size;

    // Update instance variable names
    const inst_vars_val = app.heap.allocateString(inst_vars) catch {
        browser.setStatus("Error: Could not create inst vars string", true);
        return;
    };
    class_obj.setField(Heap.CLASS_FIELD_INST_VARS, inst_vars_val, num_fields);

    // Note: We don't update the format field here as it encodes both instance size
    // and class format. Changing instance variables at runtime is complex and
    // requires rebuilding instances. This just updates the displayed names.

    var buf: [128]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "Saved class definition: {s}", .{class_name}) catch "Class saved";
    browser.setStatus(msg, false);

    // Add to transcript
    app.transcript.addSuccess(msg) catch {};
}

// Callback for browser - prepare for new method
// Note: save/restore pending source and template setup are now handled in browser.enterMethodPane
fn browserNewMethod(browser: *BrowserTab, class_side: bool) void {
    _ = g_app orelse return;
    _ = class_side; // Used by browser.show_class_side which is already set

    // Check if a class is selected
    if (browser.selected_class_name.len == 0) {
        browser.setStatus("Select a class first", true);
        return;
    }

    // Template and source management now handled in browser.enterMethodPane
    // This callback can be used for additional app-level setup if needed
}

// Callback for browser - package selected
fn browserSelectPackage(browser: *BrowserTab, package_name: []const u8) void {
    const app = g_app orelse return;

    // "All" shows all classes without filtering
    if (std.mem.eql(u8, package_name, "All")) {
        browser.showAllClasses();
        var msg_buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "All classes ({d} total)", .{browser.all_class_roots.items.len}) catch "All classes";
        browser.setStatus(msg, false);
        return;
    }

    // Get the package and filter classes
    if (app.package_registry.getPackage(package_name)) |package| {
        // Filter class tree to show only classes in this package
        browser.filterClassesByNames(package.classes.items);

        // Update status
        var msg_buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "Package: {s} ({d} classes)", .{ package_name, package.classes.items.len }) catch "Package selected";
        browser.setStatus(msg, false);
    } else {
        // Package not found
        var msg_buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "Package not found: {s}", .{package_name}) catch "Package not found";
        browser.setStatus(msg, true);
    }
}

// Callback for browser - save package to file
fn browserCreatePackage(browser: *BrowserTab, package_name: []const u8) void {
    const app = g_app orelse return;

    // Create the package (user package, not system)
    const pkg = app.package_registry.createPackage(package_name) catch {
        browser.setStatus("Out of memory creating package", true);
        return;
    };
    pkg.is_system = false; // User packages are not system packages

    // Success message
    var msg_buf: [64]u8 = undefined;
    const msg = std.fmt.bufPrint(&msg_buf, "Created package: {s}", .{package_name}) catch "Package created";
    browser.setStatus(msg, false);
    app.transcript.addSuccess(msg) catch {};
}

fn browserSavePackage(browser: *BrowserTab, package_name: []const u8) void {
    const app = g_app orelse return;

    // Find the package
    const package = app.package_registry.getPackage(package_name) orelse {
        browser.setStatus("Package not found", true);
        return;
    };

    // File out the package
    app.package_registry.fileOutPackage(package, app.heap) catch |err| {
        const err_msg = switch (err) {
            error.IoError => "I/O error saving package",
            error.NoFilePath => "Package has no file path",
            error.PathTooLong => "Package path too long",
            else => "Error saving package",
        };
        browser.setStatus(err_msg, true);
        return;
    };

    // Success message
    var msg_buf: [64]u8 = undefined;
    const msg = std.fmt.bufPrint(&msg_buf, "Saved package: {s}", .{package_name}) catch "Package saved";
    browser.setStatus(msg, false);

    // Also add to transcript
    app.transcript.addSuccess(msg) catch {};
}

pub fn runTUI(allocator: std.mem.Allocator, heap: *Heap, interp: *Interpreter) !void {
    var app = try App.init(allocator, heap, interp);
    defer {
        g_app = null;
        app.deinit();
    }

    // Store global reference for primitives
    g_app = app;

    // Run the TUI with integrated process scheduling
    try app.runWithScheduler();
}
