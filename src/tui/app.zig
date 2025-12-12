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
const ffi_autogen = @import("../vm/ffi_autogen.zig");

const Heap = memory.Heap;
const Interpreter = interpreter_mod.Interpreter;
const Parser = parser_mod.Parser;
const CodeGenerator = codegen_mod.CodeGenerator;
const Value = @import("../vm/object.zig").Value;

// Global transcript buffer for TUI mode - transcript primitives write here
var g_transcript_buffer: [8192]u8 = undefined;
var g_transcript_len: usize = 0;
var g_transcript_mutex: std.Thread.Mutex = .{};

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

    active_tab: usize = 0,
    running: bool = true,

    // Status items for different tabs
    workspace_items: []const StatusItem,
    browser_items: []const StatusItem,
    default_items: []const StatusItem,

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
        const browser = try BrowserTab.init(allocator, content_rect);
        const ffi_config = try FFIConfigTab.init(allocator, content_rect);

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
            .workspace_items = &[_]StatusItem{
                .{ .text = "Execute", .key = "Ctrl+D" },
                .{ .text = "Print", .key = "Ctrl+P" },
                .{ .text = "Inspect", .key = "Ctrl+I" },
                .{ .text = "Quit", .key = "Ctrl+Q" },
            },
            .browser_items = &[_]StatusItem{
                .{ .text = "Navigate", .key = "Arrows" },
                .{ .text = "Switch Pane", .key = "Tab/L/R" },
                .{ .text = "Quit", .key = "Ctrl+Q" },
            },
            .default_items = &[_]StatusItem{
                .{ .text = "Quit", .key = "Ctrl+Q" },
            },
        };

        // Add welcome message to transcript
        try app.transcript.addInfo("Zig Smalltalk TUI v0.1");
        try app.transcript.addLine("Type code in Workspace and use Ctrl+D to execute", .normal);

        // Set global transcript for primitive output redirection
        transcript_mod.setGlobalTranscript(&app.transcript);

        // Set interpreter transcript callback for primitive output
        interp.setTranscriptCallback(&transcriptCallback);

        // Load classes into browser
        try app.loadClasses();

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
        self.screen.deinit();
        self.terminal.deinit();
        self.allocator.destroy(self);
    }

    pub fn run(self: *App) !void {
        // Initial render
        self.render();
        self.screen.flush();

        while (self.running) {
            // Check for window resize
            self.checkResize();

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

    fn checkResize(self: *App) void {
        const new_size = self.terminal.getSize() catch return;
        if (new_size.width != self.screen.width or new_size.height != self.screen.height) {
            // Resize screen buffer
            self.screen.resize(new_size.width, new_size.height) catch return;
            self.terminal.width = new_size.width;
            self.terminal.height = new_size.height;

            // Force full redraw
            self.screen.forceFullRedraw();
            terminal_mod.ansi.clear();
        }
    }

    fn handleKey(self: *App, key: Key) void {
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

        // Handle tab bar input (for switching between main tabs)
        const tab_result = self.tabbar.handleKey(key);
        if (tab_result == .consumed) {
            self.active_tab = self.tabbar.active_tab;
            self.updateFocus();
            return;
        }

        // Pass to active tab
        const result = switch (self.active_tab) {
            0 => self.transcript.handleKey(key),
            1 => self.workspace.handleKey(key),
            3 => self.ffi_config.handleKey(key),
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

        // Clear screen
        self.screen.clear();

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
    }

    fn loadClasses(self: *App) !void {
        // Iterate through class table and add to browser
        const class_table = self.heap.class_table.items;

        for (class_table) |class_val| {
            if (class_val.isNil() or !class_val.isObject()) continue;

            const class_obj = class_val.asObject();

            // Verify class object is valid
            if (class_obj.header.class_index == 0) continue;

            // Get class name
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isNil() or !name_val.isObject()) continue;

            const name_obj = name_val.asObject();
            if (name_obj.header.class_index != Heap.CLASS_SYMBOL) continue;

            const name_size = name_obj.header.size;
            if (name_size == 0 or name_size > 1000) continue;

            const name = name_obj.bytes(name_size);
            if (name.len == 0) continue;

            // Get superclass to determine hierarchy
            const super_val = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
            var parent_name: ?[]const u8 = null;

            if (!super_val.isNil() and super_val.isObject()) {
                const super_obj = super_val.asObject();
                if (super_obj.header.class_index != 0) {
                    const super_name_val = super_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
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
        }

        // Add FFI libraries to the browser
        self.loadFFILibraries();
    }

    fn loadFFILibraries(self: *App) void {
        // Add a root node for FFI libraries
        self.browser.addClass("FFI Libraries", null) catch return;

        // Add each FFI library as a child
        for (ffi_autogen.available_libraries) |lib_name| {
            self.browser.addClass(lib_name, "FFI Libraries") catch {};
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
        for (ffi_autogen.available_libraries) |lib_name| {
            if (std.mem.eql(u8, name, lib_name)) return true;
        }
        return false;
    }

    pub fn executeCode(self: *App, code: []const u8, print_result: bool) void {
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
                    const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
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

        // Load instance methods
        var instance_methods: std.ArrayList([]const u8) = .empty;
        defer instance_methods.deinit(self.allocator);
        self.collectMethodsFromDict(class_obj, &instance_methods);
        self.browser.setInstanceMethods(instance_methods.items) catch {};

        // Load class methods from metaclass
        var class_methods: std.ArrayList([]const u8) = .empty;
        defer class_methods.deinit(self.allocator);

        const metaclass_val = class_obj.getField(Heap.CLASS_FIELD_METACLASS, Heap.CLASS_NUM_FIELDS);
        if (!metaclass_val.isNil() and metaclass_val.isObject()) {
            const metaclass_obj = metaclass_val.asObject();
            self.collectMethodsFromDict(metaclass_obj, &class_methods);
        }
        self.browser.setClassMethods(class_methods.items) catch {};
    }

    fn loadFFIFunctions(self: *App, lib_name: []const u8) void {
        // Get functions for this library
        if (ffi_autogen.getLibraryFunctions(lib_name)) |functions| {
            // Populate instance methods with FFI function names
            var func_names: std.ArrayList([]const u8) = .empty;
            defer func_names.deinit(self.allocator);

            for (functions) |func| {
                func_names.append(self.allocator, func.name) catch {};
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
        const method_dict_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);
        if (method_dict_val.isNil() or !method_dict_val.isObject()) return;

        const dict_obj = method_dict_val.asObject();
        const dict_size = dict_obj.header.size;

        // Safety check for reasonable dictionary size
        if (dict_size > 10000) return;

        // Method dictionary is a flat array: [selector1, method1, selector2, method2, ...]
        var i: usize = 0;
        while (i + 1 < dict_size) : (i += 2) {
            const selector_val = dict_obj.getField(i, dict_size);

            // Skip nil entries
            if (selector_val.isNil()) continue;
            if (!selector_val.isObject()) continue;

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

        // Check if it's an FFI library
        if (isFFILibrary(class_name)) {
            self.loadFFIFunctionSource(class_name, method_name);
            return;
        }

        // Find class
        const class_val = self.findClass(class_name) orelse return;
        if (!class_val.isObject()) return;

        var class_obj = class_val.asObject();

        // If looking for class-side method, use metaclass
        if (class_side) {
            const metaclass_val = class_obj.getField(Heap.CLASS_FIELD_METACLASS, Heap.CLASS_NUM_FIELDS);
            if (metaclass_val.isNil() or !metaclass_val.isObject()) {
                self.browser.setSource("\"No metaclass\"") catch {};
                return;
            }
            class_obj = metaclass_val.asObject();
        }

        const method_dict_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);
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

    fn loadFFIFunctionSource(self: *App, lib_name: []const u8, func_name: []const u8) void {
        if (ffi_autogen.getLibraryFunctions(lib_name)) |functions| {
            for (functions) |func| {
                if (std.mem.eql(u8, func.name, func_name)) {
                    var buf: [1024]u8 = undefined;
                    const source = std.fmt.bufPrint(&buf,
                        \\"{s} - C FFI Function"
                        \\""
                        \\"Signature:"
                        \\"  {s}({d} args) -> {s}"
                        \\""
                        \\"Usage from Smalltalk:"
                        \\"  '{s}' ffiCall: #{s} with: {{ {s} }}"
                        \\""
                        \\"Example:"
                        \\"  result := '{s}' ffiCall: #{s} with: {{ {s} }}."
                        \\"  Transcript show: result; cr."
                    , .{
                        func.name,
                        func.name, func.arg_count, func.return_type,
                        lib_name, func.name, self.generateArgPlaceholders(func.arg_count),
                        lib_name, func.name, self.generateExampleArgs(func.arg_count),
                    }) catch func_name;

                    self.browser.setSource(source) catch {};
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

pub fn runTUI(allocator: std.mem.Allocator, heap: *Heap, interp: *Interpreter) !void {
    var app = try App.init(allocator, heap, interp);
    defer app.deinit();

    try app.run();
}
