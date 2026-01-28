const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");
const bytecodes = @import("bytecodes.zig");
const interpreter = @import("interpreter.zig");

const Value = object.Value;
const Object = object.Object;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;
const Opcode = bytecodes.Opcode;
const Interpreter = interpreter.Interpreter;

/// Debugger mode
pub const DebugMode = enum {
    run, // Normal execution
    step, // Step one bytecode
    step_over, // Step over message sends (execute full method call)
    step_out, // Step out of current method
    paused, // Paused, waiting for command
};

/// Breakpoint type
pub const Breakpoint = struct {
    method_name: []const u8, // Method name pattern (or "" for any)
    class_name: []const u8, // Class name pattern (or "" for any)
    selector: []const u8, // Selector being sent (for send breakpoints)
    ip: ?usize, // Specific IP (or null for any)
    enabled: bool,
    hit_count: usize,
};

/// Stack frame information for TUI display
pub const StackFrameInfo = struct {
    context_index: usize,
    method: *CompiledMethod,
    method_class: Value,
    selector: []const u8,
    class_name: []const u8,
    receiver: Value,
    ip: usize,
    temp_base: usize,
    num_args: usize,
    num_temps: usize,
};

/// Local variable information
pub const LocalVariable = struct {
    name: []const u8,
    value: Value,
    is_argument: bool,
    index: usize,
};

/// TUI halt callback type
pub const OnHaltCallback = *const fn () void;

/// Interactive debugger for the Smalltalk VM
pub const Debugger = struct {
    interp: *Interpreter,
    mode: DebugMode,
    breakpoints: [64]Breakpoint,
    num_breakpoints: usize,
    step_out_context: usize, // Context level to step out to
    halt_context_index: usize, // Context where halt occurred
    last_command: []const u8,
    stdin: std.fs.File,
    stdout: std.fs.File,

    // TUI integration
    on_halt: ?OnHaltCallback = null,
    tui_mode: bool = false, // When true, don't use stdin/stdout prompt

    pub fn init(interp: *Interpreter) Debugger {
        return .{
            .interp = interp,
            .mode = .run,
            .breakpoints = undefined,
            .num_breakpoints = 0,
            .step_out_context = 0,
            .halt_context_index = 0,
            .last_command = "",
            .stdin = std.fs.File.stdin(),
            .stdout = std.fs.File.stdout(),
            .on_halt = null,
            .tui_mode = false,
        };
    }

    /// Set the TUI callback for when debugger halts
    pub fn setOnHalt(self: *Debugger, callback: ?OnHaltCallback) void {
        self.on_halt = callback;
        self.tui_mode = callback != null;
    }

    /// Trigger a halt (called by primHalt)
    pub fn triggerHalt(self: *Debugger) void {
        self.halt_context_index = self.interp.context_ptr;
        self.mode = .paused;
        debugEnabled = true;
        if (self.on_halt) |cb| {
            cb();
        }
    }

    /// Step into - execute one bytecode then pause
    pub fn stepInto(self: *Debugger) void {
        self.mode = .step;
    }

    /// Step over - execute until back at same context level
    pub fn stepOver(self: *Debugger) void {
        self.step_out_context = self.interp.context_ptr;
        self.mode = .step_over;
    }

    /// Step out - execute until lower context level
    pub fn stepOut(self: *Debugger) void {
        self.step_out_context = self.interp.context_ptr;
        self.mode = .step_out;
    }

    /// Continue normal execution
    pub fn continueExecution(self: *Debugger) void {
        self.mode = .run;
    }

    /// Get stack frames for TUI display
    pub fn getStackFrames(self: *Debugger, allocator: std.mem.Allocator) ![]StackFrameInfo {
        var frames: std.ArrayList(StackFrameInfo) = .empty;

        // Current context (frame 0)
        try frames.append(allocator, self.makeFrameInfo(
            self.interp.method,
            self.interp.method_class,
            self.interp.receiver,
            self.interp.ip,
            self.interp.temp_base,
            self.interp.context_ptr,
        ));

        // Saved contexts
        var i: usize = self.interp.context_ptr;
        while (i > 0) {
            i -= 1;
            const ctx = self.interp.contexts[i];
            try frames.append(allocator, self.makeFrameInfo(
                ctx.method,
                ctx.method_class,
                ctx.receiver,
                ctx.ip,
                ctx.temp_base,
                i,
            ));
            if (frames.items.len >= 50) break; // Limit stack depth
        }

        return frames.toOwnedSlice(allocator);
    }

    fn makeFrameInfo(_: *Debugger, method: *CompiledMethod, method_class: Value, recv: Value, ip: usize, temp_base: usize, ctx_index: usize) StackFrameInfo {
        const lits = method.getLiterals();

        // Get selector from last literal
        var selector: []const u8 = "<unknown>";
        if (lits.len > 0) {
            const last = lits[lits.len - 1];
            if (last.isObject()) {
                const obj = last.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING or obj.header.class_index == Heap.CLASS_SYMBOL) {
                    selector = obj.bytes(obj.header.size);
                    // Truncate at first newline
                    if (std.mem.indexOf(u8, selector, "\n")) |nl| {
                        selector = selector[0..nl];
                    }
                }
            }
        }

        // Get class name
        var class_name: []const u8 = "<unknown>";
        if (method_class.isObject()) {
            const class_obj = method_class.asObject();
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    class_name = name_obj.bytes(name_obj.header.size);
                }
            }
        }

        return StackFrameInfo{
            .context_index = ctx_index,
            .method = method,
            .method_class = method_class,
            .selector = selector,
            .class_name = class_name,
            .receiver = recv,
            .ip = ip,
            .temp_base = temp_base,
            .num_args = method.header.num_args,
            .num_temps = method.header.num_temps,
        };
    }

    /// Get method source code
    pub fn getMethodSource(self: *Debugger, method: *CompiledMethod) []const u8 {
        _ = self;
        const lits = method.getLiterals();
        if (lits.len > 0) {
            const last = lits[lits.len - 1];
            if (last.isObject()) {
                const obj = last.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING) {
                    return obj.bytes(obj.header.size);
                }
            }
        }
        return "";
    }

    /// Get locals for a given frame
    pub fn getLocals(self: *Debugger, frame: StackFrameInfo, allocator: std.mem.Allocator) ![]LocalVariable {
        var locals: std.ArrayList(LocalVariable) = .empty;

        // Get argument and temp names from method source if available
        const num_args = frame.num_args;
        const num_temps = frame.num_temps;

        // Arguments
        var i: usize = 0;
        while (i < num_args) : (i += 1) {
            const idx = frame.temp_base + i;
            if (idx < self.interp.sp) {
                try locals.append(allocator, .{
                    .name = "arg",
                    .value = self.interp.stack[idx],
                    .is_argument = true,
                    .index = i,
                });
            }
        }

        // Temps (after args)
        i = num_args;
        while (i < num_temps) : (i += 1) {
            const idx = frame.temp_base + i;
            if (idx < self.interp.sp) {
                try locals.append(allocator, .{
                    .name = "temp",
                    .value = self.interp.stack[idx],
                    .is_argument = false,
                    .index = i - num_args,
                });
            }
        }

        return locals.toOwnedSlice(allocator);
    }

    /// Get instance variables of receiver
    pub fn getInstanceVars(_: *Debugger, recv: Value, allocator: std.mem.Allocator) ![]LocalVariable {
        var vars: std.ArrayList(LocalVariable) = .empty;

        if (recv.isObject()) {
            const obj = recv.asObject();
            const size = obj.header.size;
            // Don't show bytes for strings/symbols
            if (obj.header.class_index != Heap.CLASS_STRING and obj.header.class_index != Heap.CLASS_SYMBOL) {
                var i: usize = 0;
                while (i < size and i < 20) : (i += 1) {
                    try vars.append(allocator, .{
                        .name = "instVar",
                        .value = obj.getField(i, size),
                        .is_argument = false,
                        .index = i,
                    });
                }
            }
        }

        return vars.toOwnedSlice(allocator);
    }

    /// Restart execution at a specific context
    pub fn restartAtContext(self: *Debugger, ctx_index: usize) void {
        if (ctx_index >= self.interp.context_ptr) {
            // Restart current method
            self.interp.ip = 0;
            self.interp.sp = self.interp.temp_base + self.interp.method.header.num_temps;
        } else {
            // Rewind to earlier context
            self.interp.context_ptr = ctx_index + 1;
            const ctx = self.interp.contexts[ctx_index];
            self.interp.method = ctx.method;
            self.interp.method_class = ctx.method_class;
            self.interp.receiver = ctx.receiver;
            self.interp.temp_base = ctx.temp_base;
            self.interp.ip = 0;
            self.interp.sp = ctx.temp_base + ctx.method.header.num_temps;
        }
        self.mode = .paused;
    }

    /// Add a breakpoint
    pub fn addBreakpoint(self: *Debugger, bp: Breakpoint) !usize {
        if (self.num_breakpoints >= self.breakpoints.len) {
            return error.TooManyBreakpoints;
        }
        self.breakpoints[self.num_breakpoints] = bp;
        self.num_breakpoints += 1;
        return self.num_breakpoints - 1;
    }

    /// Remove a breakpoint by index
    pub fn removeBreakpoint(self: *Debugger, index: usize) void {
        if (index < self.num_breakpoints) {
            // Shift remaining breakpoints down
            var i = index;
            while (i < self.num_breakpoints - 1) : (i += 1) {
                self.breakpoints[i] = self.breakpoints[i + 1];
            }
            self.num_breakpoints -= 1;
        }
    }

    /// Check if any breakpoint matches current state
    pub fn checkBreakpoints(self: *Debugger) bool {
        const method = self.interp.method;
        const lits = method.getLiterals();

        // Get method name from last literal (source string or selector)
        var method_name: []const u8 = "<unknown>";
        if (lits.len > 0) {
            const last = lits[lits.len - 1];
            if (last.isObject()) {
                const obj = last.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING or obj.header.class_index == Heap.CLASS_SYMBOL) {
                    method_name = obj.bytes(obj.header.size);
                }
            }
        }

        // Get class name from receiver
        var class_name: []const u8 = "<unknown>";
        const recv_class = self.interp.heap.classOf(self.interp.receiver);
        if (recv_class.isObject()) {
            const class_obj = recv_class.asObject();
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    class_name = name_obj.bytes(name_obj.header.size);
                }
            }
        }

        for (self.breakpoints[0..self.num_breakpoints]) |*bp| {
            if (!bp.enabled) continue;

            // Check method name
            if (bp.method_name.len > 0 and !std.mem.containsAtLeast(u8, method_name, 1, bp.method_name)) {
                continue;
            }

            // Check class name
            if (bp.class_name.len > 0 and !std.mem.eql(u8, class_name, bp.class_name)) {
                continue;
            }

            // Check IP
            if (bp.ip) |ip| {
                if (self.interp.ip != ip) continue;
            }

            bp.hit_count += 1;
            return true;
        }
        return false;
    }

    /// Called before each bytecode execution
    pub fn beforeBytecode(self: *Debugger) bool {
        // Check mode
        switch (self.mode) {
            .run => {
                // Only pause if breakpoint hit
                if (self.checkBreakpoints()) {
                    self.mode = .paused;
                    self.notifyHalt();
                    return true;
                }
                return false;
            },
            .step => {
                self.mode = .paused;
                self.notifyHalt();
                return true;
            },
            .step_over => {
                // Continue until we're back at the same or lower context level
                if (self.interp.context_ptr <= self.step_out_context) {
                    self.mode = .paused;
                    self.notifyHalt();
                    return true;
                }
                return false;
            },
            .step_out => {
                // Continue until we're at a lower context level
                if (self.interp.context_ptr < self.step_out_context) {
                    self.mode = .paused;
                    self.notifyHalt();
                    return true;
                }
                return false;
            },
            .paused => return true,
        }
    }

    /// Notify TUI when halted (if in TUI mode)
    fn notifyHalt(self: *Debugger) void {
        self.halt_context_index = self.interp.context_ptr;
        if (self.on_halt) |cb| {
            cb();
        }
    }

    /// Print current execution state
    pub fn printState(self: *Debugger) void {
        const method = self.interp.method;
        const lits = method.getLiterals();
        const bcs = method.getBytecodes();

        // Print method info
        var method_name: []const u8 = "<unknown>";
        if (lits.len > 0) {
            const last = lits[lits.len - 1];
            if (last.isObject()) {
                const obj = last.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING or obj.header.class_index == Heap.CLASS_SYMBOL) {
                    method_name = obj.bytes(obj.header.size);
                    // Truncate at first newline
                    if (std.mem.indexOf(u8, method_name, "\n")) |nl| {
                        method_name = method_name[0..nl];
                    }
                }
            }
        }

        // Get class name
        var class_name: []const u8 = "<unknown>";
        const recv_class = self.interp.heap.classOf(self.interp.receiver);
        if (recv_class.isObject()) {
            const class_obj = recv_class.asObject();
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    class_name = name_obj.bytes(name_obj.header.size);
                }
            }
        }

        std.debug.print("\n=== Debugger ===\n", .{});
        std.debug.print("Method: {s}>>{s}\n", .{ class_name, method_name[0..@min(method_name.len, 50)] });
        std.debug.print("IP: {d}/{d}  Context: {d}  SP: {d}\n", .{ self.interp.ip, bcs.len, self.interp.context_ptr, self.interp.sp });

        self.printMethodSourceSnippet();

        // Print current bytecode
        if (self.interp.ip < bcs.len) {
            const byte = bcs[self.interp.ip];
            std.debug.print("Next bytecode: 0x{x:0>2}", .{byte});
            self.printBytecodeDescription(byte);
            std.debug.print("\n", .{});
        }

        // Print surrounding bytecodes
        std.debug.print("\nBytecodes (-> = current):\n", .{});
        const start = if (self.interp.ip > 3) self.interp.ip - 3 else 0;
        const end = @min(self.interp.ip + 5, bcs.len);
        var i: usize = start;
        while (i < end) : (i += 1) {
            const marker: []const u8 = if (i == self.interp.ip) " -> " else "    ";
            std.debug.print("{s}{d:4}: 0x{x:0>2}", .{ marker, i, bcs[i] });
            self.printBytecodeDescription(bcs[i]);
            std.debug.print("\n", .{});
        }
    }

    fn printMethodSourceSnippet(self: *Debugger) void {
        const src = self.interp.currentMethodSource();
        if (src.len == 0 or std.mem.eql(u8, src, "<?>")) return;
        var buf: [256]u8 = undefined;
        var dst_len: usize = 0;
        const max = buf.len - 1;
        var i: usize = 0;
        while (i < src.len and dst_len < max) : (i += 1) {
            const ch = src[i];
            buf[dst_len] = if (ch == '\n' or ch == '\r') ' ' else ch;
            dst_len += 1;
        }
        const snippet = buf[0..dst_len];
        std.debug.print("Source: {s}{s}\n", .{
            snippet,
            if (dst_len < src.len) "..." else "",
        });
    }

    /// Print full method source code
    pub fn printMethodSource(self: *Debugger) void {
        const src = self.interp.currentMethodSource();
        if (src.len == 0 or std.mem.eql(u8, src, "<?>")) {
            std.debug.print("\nNo source available for this method.\n", .{});
            return;
        }

        std.debug.print("\n=== Method Source ===\n", .{});

        // Print source with line numbers
        var line_num: usize = 1;
        var line_start: usize = 0;
        var i: usize = 0;

        while (i < src.len) : (i += 1) {
            if (src[i] == '\n' or i == src.len - 1) {
                const line_end = if (src[i] == '\n') i else i + 1;
                const line = src[line_start..line_end];
                std.debug.print("{d:4}: {s}\n", .{ line_num, line });
                line_num += 1;
                line_start = i + 1;
            }
        }

        std.debug.print("=== End Source ({d} bytes) ===\n", .{src.len});
    }

    fn printBytecodeDescription(self: *Debugger, byte: u8) void {
        _ = self;
        // Check short-form opcodes
        if (Opcode.isPushReceiverVariable(byte)) {
            std.debug.print(" (push_recv_var {d})", .{Opcode.getEmbeddedIndex(byte)});
            return;
        }
        if (Opcode.isPushTemporary(byte)) {
            std.debug.print(" (push_temp {d})", .{Opcode.getEmbeddedIndex(byte)});
            return;
        }
        if (Opcode.isStoreReceiverVariable(byte)) {
            std.debug.print(" (store_recv_var {d})", .{byte - 0x40});
            return;
        }
        if (Opcode.isStoreTemporary(byte)) {
            std.debug.print(" (store_temp {d})", .{byte - 0x50});
            return;
        }
        if (Opcode.isPopStoreReceiverVariable(byte)) {
            std.debug.print(" (pop_store_recv_var {d})", .{byte - 0x60});
            return;
        }
        if (Opcode.isPopStoreTemporary(byte)) {
            std.debug.print(" (pop_store_temp {d})", .{byte - 0x70});
            return;
        }
        if (Opcode.isShortJump(byte)) {
            std.debug.print(" (short_jump +{d})", .{Opcode.getShortJumpOffset(byte)});
            return;
        }

        // Named opcodes
        const opcode: Opcode = @enumFromInt(byte);
        switch (opcode) {
            .push_literal => std.debug.print(" (push_literal)", .{}),
            .push_literal_variable => std.debug.print(" (push_literal_var)", .{}),
            .push_receiver => std.debug.print(" (push_self)", .{}),
            .push_nil => std.debug.print(" (push_nil)", .{}),
            .push_true => std.debug.print(" (push_true)", .{}),
            .push_false => std.debug.print(" (push_false)", .{}),
            .push_integer => std.debug.print(" (push_integer)", .{}),
            .push_integer_16 => std.debug.print(" (push_integer_16)", .{}),
            .push_temporary => std.debug.print(" (push_temp_ext)", .{}),
            .push_temporary_0 => std.debug.print(" (push_temp 0)", .{}),
            .push_temporary_1 => std.debug.print(" (push_temp 1)", .{}),
            .push_temporary_2 => std.debug.print(" (push_temp 2)", .{}),
            .push_temporary_3 => std.debug.print(" (push_temp 3)", .{}),
            .push_temporary_4 => std.debug.print(" (push_temp 4)", .{}),
            .push_temporary_5 => std.debug.print(" (push_temp 5)", .{}),
            .push_temporary_6 => std.debug.print(" (push_temp 6)", .{}),
            .push_temporary_7 => std.debug.print(" (push_temp 7)", .{}),
            .push_temporary_8 => std.debug.print(" (push_temp 8)", .{}),
            .push_temporary_9 => std.debug.print(" (push_temp 9)", .{}),
            .push_temporary_10 => std.debug.print(" (push_temp 10)", .{}),
            .push_temporary_11 => std.debug.print(" (push_temp 11)", .{}),
            .push_temporary_12 => std.debug.print(" (push_temp 12)", .{}),
            .push_temporary_13 => std.debug.print(" (push_temp 13)", .{}),
            .push_temporary_14 => std.debug.print(" (push_temp 14)", .{}),
            .push_temporary_15 => std.debug.print(" (push_temp 15)", .{}),
            .push_receiver_variable => std.debug.print(" (push_recv_var_ext)", .{}),
            .push_receiver_variable_0 => std.debug.print(" (push_recv_var 0)", .{}),
            .push_receiver_variable_1 => std.debug.print(" (push_recv_var 1)", .{}),
            .push_receiver_variable_2 => std.debug.print(" (push_recv_var 2)", .{}),
            .push_receiver_variable_3 => std.debug.print(" (push_recv_var 3)", .{}),
            .push_receiver_variable_4 => std.debug.print(" (push_recv_var 4)", .{}),
            .push_receiver_variable_5 => std.debug.print(" (push_recv_var 5)", .{}),
            .push_receiver_variable_6 => std.debug.print(" (push_recv_var 6)", .{}),
            .push_receiver_variable_7 => std.debug.print(" (push_recv_var 7)", .{}),
            .push_receiver_variable_8 => std.debug.print(" (push_recv_var 8)", .{}),
            .push_receiver_variable_9 => std.debug.print(" (push_recv_var 9)", .{}),
            .push_receiver_variable_10 => std.debug.print(" (push_recv_var 10)", .{}),
            .push_receiver_variable_11 => std.debug.print(" (push_recv_var 11)", .{}),
            .push_receiver_variable_12 => std.debug.print(" (push_recv_var 12)", .{}),
            .push_receiver_variable_13 => std.debug.print(" (push_recv_var 13)", .{}),
            .push_receiver_variable_14 => std.debug.print(" (push_recv_var 14)", .{}),
            .push_receiver_variable_15 => std.debug.print(" (push_recv_var 15)", .{}),
            .push_outer_temp => std.debug.print(" (push_outer_temp)", .{}),
            .store_outer_temp => std.debug.print(" (store_outer_temp)", .{}),
            .send => std.debug.print(" (send)", .{}),
            .super_send => std.debug.print(" (super_send)", .{}),
            .send_plus => std.debug.print(" (send +)", .{}),
            .send_minus => std.debug.print(" (send -)", .{}),
            .send_less_than => std.debug.print(" (send <)", .{}),
            .send_greater_than => std.debug.print(" (send >)", .{}),
            .send_less_or_equal => std.debug.print(" (send <=)", .{}),
            .send_greater_or_equal => std.debug.print(" (send >=)", .{}),
            .send_equal => std.debug.print(" (send =)", .{}),
            .send_not_equal => std.debug.print(" (send ~=)", .{}),
            .send_times => std.debug.print(" (send *)", .{}),
            .send_divide => std.debug.print(" (send /)", .{}),
            .send_at => std.debug.print(" (send at:)", .{}),
            .send_at_put => std.debug.print(" (send at:put:)", .{}),
            .send_size => std.debug.print(" (send size)", .{}),
            .send_class => std.debug.print(" (send class)", .{}),
            .send_value => std.debug.print(" (send value)", .{}),
            .send_new => std.debug.print(" (send new)", .{}),
            .send_mod => std.debug.print(" (send \\)", .{}),
            .send_identical => std.debug.print(" (send ==)", .{}),
            .send_not_identical => std.debug.print(" (send ~~)", .{}),
            .send_value_1 => std.debug.print(" (send value:)", .{}),
            .send_new_size => std.debug.print(" (send new:)", .{}),
            .return_top => std.debug.print(" (return_top)", .{}),
            .return_receiver => std.debug.print(" (return_self)", .{}),
            .return_nil => std.debug.print(" (return_nil)", .{}),
            .return_true => std.debug.print(" (return_true)", .{}),
            .return_false => std.debug.print(" (return_false)", .{}),
            .block_return => std.debug.print(" (block_return)", .{}),
            .pop => std.debug.print(" (pop)", .{}),
            .dup => std.debug.print(" (dup)", .{}),
            .jump => std.debug.print(" (jump)", .{}),
            .jump_if_true => std.debug.print(" (jump_true)", .{}),
            .jump_if_false => std.debug.print(" (jump_false)", .{}),
            .jump_if_nil => std.debug.print(" (jump_nil)", .{}),
            .jump_if_not_nil => std.debug.print(" (jump_not_nil)", .{}),
            .short_jump_0 => std.debug.print(" (short_jump +0)", .{}),
            .short_jump_1 => std.debug.print(" (short_jump +1)", .{}),
            .short_jump_2 => std.debug.print(" (short_jump +2)", .{}),
            .short_jump_3 => std.debug.print(" (short_jump +3)", .{}),
            .short_jump_4 => std.debug.print(" (short_jump +4)", .{}),
            .short_jump_5 => std.debug.print(" (short_jump +5)", .{}),
            .short_jump_6 => std.debug.print(" (short_jump +6)", .{}),
            .short_jump_7 => std.debug.print(" (short_jump +7)", .{}),
            .extended_push => std.debug.print(" (extended_push)", .{}),
            .extended_send => std.debug.print(" (extended_send)", .{}),
            .push_closure => std.debug.print(" (push_closure)", .{}),
            .primitive => std.debug.print(" (primitive)", .{}),
            .make_array => std.debug.print(" (make_array)", .{}),
            .extended_store => std.debug.print(" (extended_store)", .{}),
            .store_temporary_0 => std.debug.print(" (store_temp 0)", .{}),
            .store_temporary_1 => std.debug.print(" (store_temp 1)", .{}),
            .store_temporary_2 => std.debug.print(" (store_temp 2)", .{}),
            .store_temporary_3 => std.debug.print(" (store_temp 3)", .{}),
            .store_temporary_4 => std.debug.print(" (store_temp 4)", .{}),
            .store_temporary_5 => std.debug.print(" (store_temp 5)", .{}),
            .store_temporary_6 => std.debug.print(" (store_temp 6)", .{}),
            .store_temporary_7 => std.debug.print(" (store_temp 7)", .{}),
            .store_temporary_8 => std.debug.print(" (store_temp 8)", .{}),
            .store_temporary_9 => std.debug.print(" (store_temp 9)", .{}),
            .store_temporary_10 => std.debug.print(" (store_temp 10)", .{}),
            .store_temporary_11 => std.debug.print(" (store_temp 11)", .{}),
            .store_temporary_12 => std.debug.print(" (store_temp 12)", .{}),
            .store_temporary_13 => std.debug.print(" (store_temp 13)", .{}),
            .store_temporary_14 => std.debug.print(" (store_temp 14)", .{}),
            .store_temporary_15 => std.debug.print(" (store_temp 15)", .{}),
            .store_receiver_variable_0 => std.debug.print(" (store_recv_var 0)", .{}),
            .store_receiver_variable_1 => std.debug.print(" (store_recv_var 1)", .{}),
            .store_receiver_variable_2 => std.debug.print(" (store_recv_var 2)", .{}),
            .store_receiver_variable_3 => std.debug.print(" (store_recv_var 3)", .{}),
            .store_receiver_variable_4 => std.debug.print(" (store_recv_var 4)", .{}),
            .store_receiver_variable_5 => std.debug.print(" (store_recv_var 5)", .{}),
            .store_receiver_variable_6 => std.debug.print(" (store_recv_var 6)", .{}),
            .store_receiver_variable_7 => std.debug.print(" (store_recv_var 7)", .{}),
            .store_receiver_variable_8 => std.debug.print(" (store_recv_var 8)", .{}),
            .store_receiver_variable_9 => std.debug.print(" (store_recv_var 9)", .{}),
            .store_receiver_variable_10 => std.debug.print(" (store_recv_var 10)", .{}),
            .store_receiver_variable_11 => std.debug.print(" (store_recv_var 11)", .{}),
            .store_receiver_variable_12 => std.debug.print(" (store_recv_var 12)", .{}),
            .store_receiver_variable_13 => std.debug.print(" (store_recv_var 13)", .{}),
            .store_receiver_variable_14 => std.debug.print(" (store_recv_var 14)", .{}),
            .store_receiver_variable_15 => std.debug.print(" (store_recv_var 15)", .{}),
            .pop_store_receiver_variable_0 => std.debug.print(" (pop_store_recv_var 0)", .{}),
            .pop_store_receiver_variable_1 => std.debug.print(" (pop_store_recv_var 1)", .{}),
            .pop_store_receiver_variable_2 => std.debug.print(" (pop_store_recv_var 2)", .{}),
            .pop_store_receiver_variable_3 => std.debug.print(" (pop_store_recv_var 3)", .{}),
            .pop_store_receiver_variable_4 => std.debug.print(" (pop_store_recv_var 4)", .{}),
            .pop_store_receiver_variable_5 => std.debug.print(" (pop_store_recv_var 5)", .{}),
            .pop_store_receiver_variable_6 => std.debug.print(" (pop_store_recv_var 6)", .{}),
            .pop_store_receiver_variable_7 => std.debug.print(" (pop_store_recv_var 7)", .{}),
            .pop_store_temporary_0 => std.debug.print(" (pop_store_temp 0)", .{}),
            .pop_store_temporary_1 => std.debug.print(" (pop_store_temp 1)", .{}),
            .pop_store_temporary_2 => std.debug.print(" (pop_store_temp 2)", .{}),
            .pop_store_temporary_3 => std.debug.print(" (pop_store_temp 3)", .{}),
            .pop_store_temporary_4 => std.debug.print(" (pop_store_temp 4)", .{}),
            .pop_store_temporary_5 => std.debug.print(" (pop_store_temp 5)", .{}),
            .pop_store_temporary_6 => std.debug.print(" (pop_store_temp 6)", .{}),
            .pop_store_temporary_7 => std.debug.print(" (pop_store_temp 7)", .{}),
            .thread => std.debug.print(" (thread)", .{}),
            .nop => std.debug.print(" (nop)", .{}),
            else => {},
        }
    }

    /// Print stack contents
    pub fn printStack(self: *Debugger) void {
        std.debug.print("\nStack (top first):\n", .{});

        if (self.interp.sp == 0) {
            std.debug.print("  <empty>\n", .{});
            return;
        }

        const start = if (self.interp.sp > 10) self.interp.sp - 10 else 0;
        var i: usize = self.interp.sp;
        while (i > start) {
            i -= 1;
            const val = self.interp.stack[i];
            std.debug.print("  [{d}]: ", .{i});
            self.printValue(val);
            std.debug.print("\n", .{});
        }
        if (start > 0) {
            std.debug.print("  ... ({d} more)\n", .{start});
        }
    }

    /// Print temporaries
    pub fn printTemps(self: *Debugger) void {
        const method = self.interp.method;
        const num_temps = method.header.num_temps;

        std.debug.print("\nTemporaries (temp_base={d}, outer_temp_base={d}):\n", .{ self.interp.temp_base, self.interp.outer_temp_base });

        if (num_temps == 0) {
            std.debug.print("  <none>\n", .{});
            return;
        }

        var i: usize = 0;
        while (i < num_temps) : (i += 1) {
            const idx = self.interp.temp_base + i;
            if (idx < self.interp.sp) {
                const val = self.interp.stack[idx];
                std.debug.print("  temp[{d}] @{d}: ", .{ i, idx });
                self.printValue(val);
                std.debug.print("\n", .{});
            }
        }
    }

    /// Print receiver
    pub fn printReceiver(self: *Debugger) void {
        std.debug.print("\nReceiver: ", .{});
        self.printValue(self.interp.receiver);
        std.debug.print("\n", .{});

        // Print receiver's instance variables if it's an object
        if (self.interp.receiver.isObject()) {
            const obj = self.interp.receiver.asObject();
            const size = obj.header.size;
            if (size > 0 and obj.header.class_index != Heap.CLASS_STRING and obj.header.class_index != Heap.CLASS_SYMBOL) {
                std.debug.print("  Instance variables:\n", .{});
                var i: usize = 0;
                while (i < @min(size, 10)) : (i += 1) {
                    const val = obj.getField(i, size);
                    std.debug.print("    [{d}]: ", .{i});
                    self.printValue(val);
                    std.debug.print("\n", .{});
                }
                if (size > 10) {
                    std.debug.print("    ... ({d} more)\n", .{size - 10});
                }
            }
        }
    }

    /// Print call stack
    pub fn printCallStack(self: *Debugger) void {
        std.debug.print("\nCall stack:\n", .{});

        // Print current context
        std.debug.print("  0: ", .{});
        self.printContextInfo(self.interp.method, self.interp.receiver, self.interp.ip);
        std.debug.print("\n", .{});

        // Print saved contexts
        var i: usize = self.interp.context_ptr;
        var frame: usize = 1;
        while (i > 0) : (frame += 1) {
            i -= 1;
            const ctx = self.interp.contexts[i];
            std.debug.print("  {d}: ", .{frame});
            self.printContextInfo(ctx.method, ctx.receiver, ctx.ip);
            std.debug.print("\n", .{});
            if (frame >= 15) {
                std.debug.print("  ... ({d} more frames)\n", .{i});
                break;
            }
        }
    }

    fn printContextInfo(self: *Debugger, method: *CompiledMethod, recv: Value, ip: usize) void {
        const lits = method.getLiterals();

        // Get method name
        var method_name: []const u8 = "<unknown>";
        if (lits.len > 0) {
            const last = lits[lits.len - 1];
            if (last.isObject()) {
                const obj = last.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING or obj.header.class_index == Heap.CLASS_SYMBOL) {
                    method_name = obj.bytes(obj.header.size);
                    if (std.mem.indexOf(u8, method_name, "\n")) |nl| {
                        method_name = method_name[0..nl];
                    }
                }
            }
        }

        // Get class name
        var class_name: []const u8 = "<unknown>";
        const recv_class = self.interp.heap.classOf(recv);
        if (recv_class.isObject()) {
            const class_obj = recv_class.asObject();
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    class_name = name_obj.bytes(name_obj.header.size);
                }
            }
        }

        std.debug.print("{s}>>{s} @{d}", .{ class_name, method_name[0..@min(method_name.len, 40)], ip });
    }

    /// Print a value
    pub fn printValue(self: *Debugger, value: Value) void {
        if (value.isNil()) {
            std.debug.print("nil", .{});
        } else if (value.isTrue()) {
            std.debug.print("true", .{});
        } else if (value.isFalse()) {
            std.debug.print("false", .{});
        } else if (value.isSmallInt()) {
            std.debug.print("{d}", .{value.asSmallInt()});
        } else if (value.isCharacter()) {
            const cp = value.asCharacter();
            if (cp >= 32 and cp < 127) {
                std.debug.print("${c}", .{@as(u8, @intCast(cp))});
            } else {
                std.debug.print("$\\u{{{x}}}", .{cp});
            }
        } else if (value.isObject()) {
            const obj = value.asObject();
            const class_index = obj.header.class_index;

            if (class_index == Heap.CLASS_STRING) {
                const bytes = obj.bytes(obj.header.size);
                const max_len = @min(bytes.len, 40);
                std.debug.print("'{s}'", .{bytes[0..max_len]});
                if (bytes.len > 40) {
                    std.debug.print("...", .{});
                }
            } else if (class_index == Heap.CLASS_SYMBOL) {
                const bytes = obj.bytes(obj.header.size);
                std.debug.print("#{s}", .{bytes});
            } else if (class_index == Heap.CLASS_ARRAY) {
                std.debug.print("#(size={d})", .{obj.header.size});
            } else if (class_index == Heap.CLASS_BLOCK_CLOSURE) {
                std.debug.print("BlockClosure", .{});
            } else {
                // Get class name
                const class = self.interp.heap.getClass(class_index);
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
                std.debug.print("<{s} @{x}>", .{ class_name, @intFromPtr(obj) });
            }
        } else {
            std.debug.print("<unknown:{x}>", .{value.bits});
        }
    }

    /// Print literals
    pub fn printLiterals(self: *Debugger) void {
        const lits = self.interp.method.getLiterals();

        std.debug.print("\nLiterals ({d}):\n", .{lits.len});
        for (lits, 0..) |lit, i| {
            std.debug.print("  [{d}]: ", .{i});
            self.printValue(lit);
            std.debug.print("\n", .{});
        }
    }

    /// Interactive debugger prompt
    pub fn prompt(self: *Debugger) void {
        var buf: [256]u8 = undefined;

        self.printState();

        while (true) {
            std.debug.print("\ndbg> ", .{});

            // Simple line reading
            var line_len: usize = 0;
            while (line_len < buf.len) {
                var byte: [1]u8 = undefined;
                const n = self.stdin.read(&byte) catch {
                    self.mode = .run;
                    return;
                };
                if (n == 0) {
                    self.mode = .run;
                    return;
                }
                if (byte[0] == '\n') break;
                buf[line_len] = byte[0];
                line_len += 1;
            }
            const cmd = std.mem.trim(u8, buf[0..line_len], " \t\r\n");

            if (cmd.len == 0) {
                // Repeat last command
                if (self.last_command.len > 0) {
                    self.executeCommand(self.last_command);
                    if (self.mode != .paused) return;
                }
                continue;
            }

            self.last_command = cmd;
            self.executeCommand(cmd);
            if (self.mode != .paused) return;
        }
    }

    fn executeCommand(self: *Debugger, cmd: []const u8) void {
        if (std.mem.eql(u8, cmd, "s") or std.mem.eql(u8, cmd, "step")) {
            self.mode = .step;
        } else if (std.mem.eql(u8, cmd, "n") or std.mem.eql(u8, cmd, "next")) {
            self.step_out_context = self.interp.context_ptr;
            self.mode = .step_over;
        } else if (std.mem.eql(u8, cmd, "o") or std.mem.eql(u8, cmd, "out")) {
            self.step_out_context = self.interp.context_ptr;
            self.mode = .step_out;
        } else if (std.mem.eql(u8, cmd, "c") or std.mem.eql(u8, cmd, "continue")) {
            self.mode = .run;
        } else if (std.mem.eql(u8, cmd, "q") or std.mem.eql(u8, cmd, "quit")) {
            std.process.exit(0);
        } else if (std.mem.eql(u8, cmd, "stack") or std.mem.eql(u8, cmd, "st")) {
            self.printStack();
        } else if (std.mem.eql(u8, cmd, "temps") or std.mem.eql(u8, cmd, "t")) {
            self.printTemps();
        } else if (std.mem.eql(u8, cmd, "recv") or std.mem.eql(u8, cmd, "r")) {
            self.printReceiver();
        } else if (std.mem.eql(u8, cmd, "bt") or std.mem.eql(u8, cmd, "backtrace")) {
            self.printCallStack();
        } else if (std.mem.eql(u8, cmd, "lits") or std.mem.eql(u8, cmd, "l")) {
            self.printLiterals();
        } else if (std.mem.eql(u8, cmd, "src") or std.mem.eql(u8, cmd, "source")) {
            self.printMethodSource();
        } else if (std.mem.eql(u8, cmd, "state") or std.mem.eql(u8, cmd, "info")) {
            self.printState();
        } else if (std.mem.eql(u8, cmd, "all") or std.mem.eql(u8, cmd, "a")) {
            self.printState();
            self.printMethodSource();
            self.printStack();
            self.printTemps();
            self.printReceiver();
            self.printCallStack();
        } else if (std.mem.eql(u8, cmd, "help") or std.mem.eql(u8, cmd, "h") or std.mem.eql(u8, cmd, "?")) {
            std.debug.print(
                \\Debugger commands:
                \\  s, step      - Step one bytecode
                \\  n, next      - Step over (execute full sends)
                \\  o, out       - Step out of current method
                \\  c, continue  - Continue execution
                \\  q, quit      - Quit debugger
                \\
                \\  stack, st    - Print stack
                \\  temps, t     - Print temporaries
                \\  recv, r      - Print receiver
                \\  bt, backtrace- Print call stack
                \\  lits, l      - Print literals
                \\  src, source  - Print full method source
                \\  state, info  - Print execution state
                \\  all, a       - Print everything
                \\
                \\  bp <name>    - Set breakpoint on method name
                \\  bpc <class>  - Set breakpoint on class
                \\  bpl          - List breakpoints
                \\  bpd <n>      - Delete breakpoint
                \\
                \\  help, h, ?   - Show this help
                \\
                \\Press Enter to repeat last command.
                \\
            , .{});
        } else if (std.mem.startsWith(u8, cmd, "bp ")) {
            const name = std.mem.trim(u8, cmd[3..], " ");
            _ = self.addBreakpoint(.{
                .method_name = name,
                .class_name = "",
                .selector = "",
                .ip = null,
                .enabled = true,
                .hit_count = 0,
            }) catch {
                std.debug.print("Error: too many breakpoints\n", .{});
                return;
            };
            std.debug.print("Breakpoint set on method '{s}'\n", .{name});
        } else if (std.mem.startsWith(u8, cmd, "bpc ")) {
            const name = std.mem.trim(u8, cmd[4..], " ");
            _ = self.addBreakpoint(.{
                .method_name = "",
                .class_name = name,
                .selector = "",
                .ip = null,
                .enabled = true,
                .hit_count = 0,
            }) catch {
                std.debug.print("Error: too many breakpoints\n", .{});
                return;
            };
            std.debug.print("Breakpoint set on class '{s}'\n", .{name});
        } else if (std.mem.eql(u8, cmd, "bpl")) {
            std.debug.print("Breakpoints:\n", .{});
            for (self.breakpoints[0..self.num_breakpoints], 0..) |bp, i| {
                std.debug.print("  {d}: ", .{i});
                if (bp.class_name.len > 0) {
                    std.debug.print("class={s} ", .{bp.class_name});
                }
                if (bp.method_name.len > 0) {
                    std.debug.print("method={s} ", .{bp.method_name});
                }
                if (bp.ip) |ip| {
                    std.debug.print("ip={d} ", .{ip});
                }
                std.debug.print("[{s}] hits={d}\n", .{ if (bp.enabled) "enabled" else "disabled", bp.hit_count });
            }
        } else if (std.mem.startsWith(u8, cmd, "bpd ")) {
            const idx_str = std.mem.trim(u8, cmd[4..], " ");
            const idx = std.fmt.parseInt(usize, idx_str, 10) catch {
                std.debug.print("Invalid breakpoint index\n", .{});
                return;
            };
            if (idx < self.num_breakpoints) {
                self.removeBreakpoint(idx);
                std.debug.print("Breakpoint {d} deleted\n", .{idx});
            } else {
                std.debug.print("Invalid breakpoint index\n", .{});
            }
        } else {
            std.debug.print("Unknown command: {s}\nType 'help' for commands.\n", .{cmd});
        }
    }
};

// Global debugger instance (for easy access from interpreter)
pub var globalDebugger: ?*Debugger = null;
pub var debugEnabled: bool = false;

/// Enable debugging mode
pub fn enableDebugger(interp: *Interpreter) *Debugger {
    const dbg = std.heap.page_allocator.create(Debugger) catch return undefined;
    dbg.* = Debugger.init(interp);
    dbg.mode = .step; // Start in step mode
    globalDebugger = dbg;
    debugEnabled = true;
    return dbg;
}

/// Disable debugging mode
pub fn disableDebugger() void {
    if (globalDebugger) |dbg| {
        std.heap.page_allocator.destroy(dbg);
        globalDebugger = null;
    }
    debugEnabled = false;
}

/// Check if we should break (called from interpreter)
pub fn shouldBreak() bool {
    if (globalDebugger) |dbg| {
        return dbg.beforeBytecode();
    }
    return false;
}

/// Enter debugger prompt
pub fn enterDebugger() void {
    if (globalDebugger) |dbg| {
        if (dbg.tui_mode) {
            // In TUI mode, just stay paused - TUI handles interaction
            // The halt callback already notified the TUI
            return;
        }
        dbg.prompt();
    }
}

/// Get the global debugger instance
pub fn getDebugger() ?*Debugger {
    return globalDebugger;
}

/// Set the TUI halt callback on the global debugger
pub fn setTuiCallback(callback: ?OnHaltCallback) void {
    if (globalDebugger) |dbg| {
        dbg.setOnHalt(callback);
    }
}

/// Initialize debugger in TUI mode (no prompt, uses callbacks)
pub fn initTuiDebugger(interp: *Interpreter, callback: OnHaltCallback) *Debugger {
    const dbg = std.heap.page_allocator.create(Debugger) catch return undefined;
    dbg.* = Debugger.init(interp);
    dbg.setOnHalt(callback);
    globalDebugger = dbg;
    // Don't enable by default - wait for halt
    debugEnabled = false;
    return dbg;
}
