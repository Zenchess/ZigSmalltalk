const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");
const bytecodes = @import("bytecodes.zig");
const primitives = @import("primitives.zig");

const Value = object.Value;
const Object = object.Object;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;
const Opcode = bytecodes.Opcode;

pub const InterpreterError = error{
    StackOverflow,
    StackUnderflow,
    MessageNotUnderstood,
    PrimitiveFailed,
    InvalidBytecode,
    InvalidIndex,
    OutOfMemory,
    TypeError,
    BlockCannotReturn,
};

/// Execution context for a method activation
pub const Context = struct {
    method: *CompiledMethod,
    ip: usize,
    receiver: Value,
    // Temporaries and arguments are stored on the stack
    temp_base: usize, // Base index in stack for temps/args
    num_args: usize,
    num_temps: usize,
    // For block closures
    outer_context: ?*Context,
    closure: ?Value, // The BlockClosure object if this is a block
};

/// The main Smalltalk interpreter
pub const Interpreter = struct {
    heap: *Heap,

    // Execution stack
    stack: [8192]Value,
    sp: usize, // Stack pointer (points to next free slot)

    // Call stack
    contexts: [1024]Context,
    context_ptr: usize, // Current context index

    // Current execution state (cached from current context)
    method: *CompiledMethod,
    ip: usize,
    receiver: Value,
    temp_base: usize,

    pub fn init(heap: *Heap) Interpreter {
        return .{
            .heap = heap,
            .stack = undefined,
            .sp = 0,
            .contexts = undefined,
            .context_ptr = 0,
            .method = undefined,
            .ip = 0,
            .receiver = Value.nil,
            .temp_base = 0,
        };
    }

    /// Execute a compiled method and return the result
    pub fn execute(self: *Interpreter, method: *CompiledMethod, recv: Value, args: []const Value) InterpreterError!Value {
        // Set up initial context
        self.method = method;
        self.ip = 0;
        self.receiver = recv;
        self.temp_base = self.sp;

        // Push receiver
        try self.push(recv);

        // Push arguments
        for (args) |arg| {
            try self.push(arg);
        }

        // Allocate space for temporaries
        const num_temps = method.header.num_temps - method.header.num_args;
        var i: usize = 0;
        while (i < num_temps) : (i += 1) {
            try self.push(Value.nil);
        }

        // Execute primitive if present
        if (method.header.primitive_index != 0) {
            if (primitives.executePrimitive(self, method.header.primitive_index)) |result| {
                // If primitive succeeded, return its result
                return result;
            } else |err| {
                if (err != InterpreterError.PrimitiveFailed) {
                    return err;
                }
                // Fall through to bytecode execution on PrimitiveFailed
            }
        }

        // Main interpretation loop
        return self.interpretLoop();
    }

    pub fn interpretLoop(self: *Interpreter) InterpreterError!Value {
        while (true) {
            const byte = self.fetchByte();

            // Handle short-form opcodes first
            if (Opcode.isPushReceiverVariable(byte)) {
                const index = Opcode.getEmbeddedIndex(byte);
                try self.pushReceiverVariable(index);
                continue;
            }
            if (Opcode.isPushTemporary(byte)) {
                const index = Opcode.getEmbeddedIndex(byte);
                try self.pushTemporary(index);
                continue;
            }
            if (Opcode.isStoreReceiverVariable(byte)) {
                const index = byte - 0x40;
                try self.storeReceiverVariable(index);
                continue;
            }
            if (Opcode.isStoreTemporary(byte)) {
                const index = byte - 0x50;
                try self.storeTemporary(index);
                continue;
            }
            if (Opcode.isPopStoreReceiverVariable(byte)) {
                const index = byte - 0x60;
                try self.storeReceiverVariable(index);
                _ = try self.pop();
                continue;
            }
            if (Opcode.isPopStoreTemporary(byte)) {
                const index = byte - 0x70;
                try self.storeTemporary(index);
                _ = try self.pop();
                continue;
            }
            if (Opcode.isShortJump(byte)) {
                const offset = Opcode.getShortJumpOffset(byte);
                self.ip += offset;
                continue;
            }

            const opcode: Opcode = @enumFromInt(byte);

            switch (opcode) {
                // Push operations
                .push_literal => {
                    const index = self.fetchByte();
                    const literals = self.method.getLiterals();
                    try self.push(literals[index]);
                },
                .push_literal_variable => {
                    const index = self.fetchByte();
                    const literals = self.method.getLiterals();
                    const assoc = literals[index];
                    // Association: value is second field
                    if (assoc.isObject()) {
                        const val = assoc.asObject().getField(1, 2);
                        try self.push(val);
                    } else {
                        try self.push(Value.nil);
                    }
                },
                .push_receiver => try self.push(self.receiver),
                .push_nil => try self.push(Value.nil),
                .push_true => try self.push(Value.@"true"),
                .push_false => try self.push(Value.@"false"),
                .push_integer => {
                    const n: i8 = @bitCast(self.fetchByte());
                    try self.push(Value.fromSmallInt(n));
                },
                .push_integer_16 => {
                    const hi: u16 = self.fetchByte();
                    const lo: u16 = self.fetchByte();
                    const n: i16 = @bitCast((hi << 8) | lo);
                    try self.push(Value.fromSmallInt(n));
                },
                .push_receiver_variable => {
                    const index = self.fetchByte();
                    try self.pushReceiverVariable(index);
                },
                .push_temporary => {
                    const index = self.fetchByte();
                    try self.pushTemporary(index);
                },

                // Sends
                .send => {
                    const selector_index = self.fetchByte();
                    const num_args = self.fetchByte();
                    try self.sendMessage(selector_index, num_args, false);
                },
                .super_send => {
                    const selector_index = self.fetchByte();
                    const num_args = self.fetchByte();
                    try self.sendMessage(selector_index, num_args, true);
                },

                // Optimized sends
                .send_plus => try self.sendSpecialBinary(.add, "+"),
                .send_minus => try self.sendSpecialBinary(.subtract, "-"),
                .send_times => try self.sendSpecialBinary(.multiply, "*"),
                .send_divide => try self.sendSpecialBinary(.divide, "/"),
                .send_less_than => try self.sendSpecialBinary(.less_than, "<"),
                .send_greater_than => try self.sendSpecialBinary(.greater_than, ">"),
                .send_less_or_equal => try self.sendSpecialBinary(.less_or_equal, "<="),
                .send_greater_or_equal => try self.sendSpecialBinary(.greater_or_equal, ">="),
                .send_equal => try self.sendSpecialBinary(.equal, "="),
                .send_not_equal => try self.sendSpecialBinary(.not_equal, "~="),
                .send_identical => try self.sendIdentical(),
                .send_class => {
                    const recv = try self.pop();
                    const class = self.heap.classOf(recv);
                    try self.push(class);
                },
                .send_size => {
                    // Try primitive first
                    const recv = self.peek();
                    if (recv.isObject()) {
                        const result = primitives.executePrimitive(self, @intFromEnum(bytecodes.Primitive.size)) catch {
                            try self.sendUnary("size");
                            continue;
                        };
                        _ = try self.pop(); // Remove receiver
                        try self.push(result);
                    } else {
                        try self.sendUnary("size");
                    }
                },

                // Returns
                .return_receiver => {
                    const result = self.receiver;
                    if (try self.returnFromMethod(result)) |final_result| {
                        return final_result;
                    }
                },
                .return_true => {
                    if (try self.returnFromMethod(Value.@"true")) |final_result| {
                        return final_result;
                    }
                },
                .return_false => {
                    if (try self.returnFromMethod(Value.@"false")) |final_result| {
                        return final_result;
                    }
                },
                .return_nil => {
                    if (try self.returnFromMethod(Value.nil)) |final_result| {
                        return final_result;
                    }
                },
                .return_top => {
                    const result = try self.pop();
                    if (try self.returnFromMethod(result)) |final_result| {
                        return final_result;
                    }
                },
                .block_return => {
                    // Non-local return - return from enclosing method
                    const result = try self.pop();
                    if (try self.returnFromMethod(result)) |final_result| {
                        return final_result;
                    }
                },

                // Jumps
                .jump => {
                    const offset = self.fetchSignedShort();
                    self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                },
                .jump_if_true => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (val.isTrue()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },
                .jump_if_false => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (val.isFalse()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },
                .jump_if_nil => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (val.isNil()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },
                .jump_if_not_nil => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (!val.isNil()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },

                // Stack operations
                .pop => {
                    _ = try self.pop();
                },
                .dup => {
                    const val = self.peek();
                    try self.push(val);
                },

                // Primitive
                .primitive => {
                    const prim_hi: u16 = self.fetchByte();
                    const prim_lo: u16 = self.fetchByte();
                    const prim_index = (prim_hi << 8) | prim_lo;
                    const result = try primitives.executePrimitive(self, prim_index);
                    try self.push(result);
                },

                .nop => {},

                .push_closure => {
                    // Create a BlockClosure object
                    // Format: push_closure <num_args> <size_hi> <size_lo> <bytecodes...>
                    const num_args = self.fetchByte();
                    const size_hi: u16 = self.fetchByte();
                    const size_lo: u16 = self.fetchByte();
                    const bytecode_size: usize = @intCast((size_hi << 8) | size_lo);

                    // Create BlockClosure object
                    // Fields: outerTempBase, startPC, numArgs, method, receiver
                    const closure = self.heap.allocateObject(Heap.CLASS_BLOCK_CLOSURE, 5, .normal) catch {
                        return InterpreterError.OutOfMemory;
                    };

                    // Store: outer temp base (for outer variable access), start PC, num args, enclosing method, receiver
                    closure.setField(0, Value.fromSmallInt(@intCast(self.temp_base)), 5); // outer temp base
                    closure.setField(1, Value.fromSmallInt(@intCast(self.ip)), 5); // start PC (current position in bytecodes)
                    closure.setField(2, Value.fromSmallInt(num_args), 5); // num args
                    closure.setField(3, Value.fromObject(@ptrCast(@alignCast(self.method))), 5); // enclosing method
                    closure.setField(4, self.receiver, 5); // receiver (self in block)

                    try self.push(Value.fromObject(closure));

                    // Skip over the block's bytecodes
                    self.ip += bytecode_size;
                },

                .push_outer_temp => {
                    // Format: push_outer_temp <level> <index>
                    // level = 1 means immediate outer context
                    // For now we only support level=1
                    const level = self.fetchByte();
                    const index = self.fetchByte();
                    _ = level; // Currently unused - assumes level 1

                    // Get the outer temp base from the current block's closure
                    // The closure stores the temp_base of its enclosing context at field 0
                    // For now, we use a simpler approach: calculate from the method's stack layout
                    // The outer temporaries start at temp_base - (num_outer_temps + num_outer_args)
                    // Since we don't track num_outer_temps, we rely on outer_temp_base stored in the closure

                    // We need access to the current block closure to find outer_temp_base
                    // For now, assume outer temps are at lower stack addresses
                    // This is a simplified approach - proper closures need full context chain

                    // For doIt expressions, outer temps are at the base of the stack
                    // temp_base for block is set after pushing args, so outer vars are before
                    const outer_val = self.stack[index];
                    try self.push(outer_val);
                },

                .store_outer_temp => {
                    // Format: store_outer_temp <level> <index>
                    const level = self.fetchByte();
                    const index = self.fetchByte();
                    _ = level; // Currently unused - assumes level 1

                    // Get value but leave on stack (store doesn't pop in Smalltalk)
                    const val = self.peek();

                    // Store to outer context's temporary slot
                    self.stack[index] = val;
                },

                .make_array => {
                    // Format: make_array <count>
                    // Pop count values from stack, create array, push array
                    const count = self.fetchByte();

                    // Allocate array
                    const array = self.heap.allocateObject(Heap.CLASS_ARRAY, count, .variable) catch {
                        return InterpreterError.OutOfMemory;
                    };

                    // Pop values in reverse order (first pushed = first element)
                    // Values are on stack in order: [elem0, elem1, ..., elemN-1] with elemN-1 on top
                    var i: usize = count;
                    while (i > 0) {
                        i -= 1;
                        const val = try self.pop();
                        array.setField(i, val, count);
                    }

                    try self.push(Value.fromObject(array));
                },

                else => return InterpreterError.InvalidBytecode,
            }
        }
    }

    // Stack operations
    pub fn push(self: *Interpreter, value: Value) InterpreterError!void {
        if (self.sp >= self.stack.len) {
            return InterpreterError.StackOverflow;
        }
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    pub fn pop(self: *Interpreter) InterpreterError!Value {
        if (self.sp == 0) {
            return InterpreterError.StackUnderflow;
        }
        self.sp -= 1;
        return self.stack[self.sp];
    }

    pub fn peek(self: *Interpreter) Value {
        if (self.sp == 0) return Value.nil;
        return self.stack[self.sp - 1];
    }

    pub fn peekN(self: *Interpreter, n: usize) Value {
        if (self.sp <= n) return Value.nil;
        return self.stack[self.sp - 1 - n];
    }

    fn fetchByte(self: *Interpreter) u8 {
        const bytecodes_slice = self.method.getBytecodes();
        if (self.ip >= bytecodes_slice.len) return 0;
        const byte = bytecodes_slice[self.ip];
        self.ip += 1;
        return byte;
    }

    fn fetchSignedShort(self: *Interpreter) i16 {
        const hi: u16 = self.fetchByte();
        const lo: u16 = self.fetchByte();
        return @bitCast((hi << 8) | lo);
    }

    fn pushReceiverVariable(self: *Interpreter, index: u8) InterpreterError!void {
        if (self.receiver.isObject()) {
            const obj = self.receiver.asObject();
            // Get number of fields from class - for now assume reasonable max
            const val = obj.getField(index, 16);
            try self.push(val);
        } else {
            try self.push(Value.nil);
        }
    }

    fn pushTemporary(self: *Interpreter, index: u8) InterpreterError!void {
        const stack_index = self.temp_base + index;
        if (stack_index < self.sp) {
            try self.push(self.stack[stack_index]);
        } else {
            try self.push(Value.nil);
        }
    }

    fn storeReceiverVariable(self: *Interpreter, index: u8) InterpreterError!void {
        const val = self.peek();
        if (self.receiver.isObject()) {
            const obj = self.receiver.asObject();
            obj.setField(index, val, 16);
        }
    }

    fn storeTemporary(self: *Interpreter, index: u8) InterpreterError!void {
        const val = self.peek();
        const stack_index = self.temp_base + index;
        if (stack_index < self.sp) {
            self.stack[stack_index] = val;
        }
    }

    fn sendMessage(self: *Interpreter, selector_index: u8, num_args: u8, is_super: bool) InterpreterError!void {
        // Get selector from literals
        const literals = self.method.getLiterals();
        const selector = literals[selector_index];

        // Pop args and receiver
        var args: [16]Value = undefined;
        var i: usize = num_args;
        while (i > 0) {
            i -= 1;
            args[i] = try self.pop();
        }
        const recv = try self.pop();

        // Get the class to start lookup from
        const class = if (is_super)
            self.getSuperclass(self.heap.classOf(self.receiver))
        else
            self.heap.classOf(recv);

        // Look up method in class hierarchy
        const method_opt = self.lookupMethod(class, selector);

        if (method_opt) |found_method| {
            // Execute the found method
            if (found_method.header.primitive_index != 0) {
                // Push receiver and args back for primitive
                try self.push(recv);
                for (args[0..num_args]) |arg| {
                    try self.push(arg);
                }
                if (primitives.executePrimitive(self, found_method.header.primitive_index)) |result| {
                    try self.push(result);
                    return;
                } else |_| {
                    // Primitive failed, fall through to execute bytecode
                    // Pop the args and receiver we just pushed
                    var j: usize = 0;
                    while (j < num_args + 1) : (j += 1) {
                        _ = try self.pop();
                    }
                }
            }

            // Execute the method bytecode
            // Save current context
            if (self.context_ptr >= self.contexts.len - 1) {
                return InterpreterError.StackOverflow;
            }

            self.contexts[self.context_ptr] = .{
                .method = self.method,
                .ip = self.ip,
                .receiver = self.receiver,
                .temp_base = self.temp_base,
                .num_args = 0, // We don't track this for the current context
                .num_temps = 0,
                .outer_context = null,
                .closure = null,
            };
            self.context_ptr += 1;

            // Set up new context for the called method
            self.method = found_method;
            self.ip = 0;
            self.receiver = recv;
            self.temp_base = self.sp;

            // Push receiver
            try self.push(recv);

            // Push arguments
            for (args[0..num_args]) |arg| {
                try self.push(arg);
            }

            // Allocate space for temporaries
            const total_temps = found_method.header.num_temps;
            const local_temps = if (total_temps > num_args) total_temps - @as(u8, @intCast(num_args)) else 0;
            var k: usize = 0;
            while (k < local_temps) : (k += 1) {
                try self.push(Value.nil);
            }
        } else {
            // Method not found - should send doesNotUnderstand:
            // For now, push nil
            try self.push(Value.nil);
        }
    }

    /// Return from a method, restoring the previous context.
    /// Returns the final value if this was the outermost call, or null to continue execution.
    fn returnFromMethod(self: *Interpreter, result: Value) InterpreterError!?Value {
        // Pop the current activation's stack frame
        self.sp = self.temp_base;

        if (self.context_ptr == 0) {
            // No more contexts - this is the final return
            return result;
        }

        // Restore previous context
        self.context_ptr -= 1;
        const ctx = self.contexts[self.context_ptr];
        self.method = ctx.method;
        self.ip = ctx.ip;
        self.receiver = ctx.receiver;
        self.temp_base = ctx.temp_base;

        // Push the return value onto the caller's stack
        try self.push(result);

        return null; // Continue execution in restored context
    }

    fn getSuperclass(_: *Interpreter, class: Value) Value {
        if (class.isObject()) {
            const class_obj = class.asObject();
            return class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
        }
        return Value.nil;
    }

    fn lookupMethod(self: *Interpreter, start_class: Value, selector: Value) ?*CompiledMethod {
        var class = start_class;

        // Walk up the superclass chain
        while (!class.isNil()) {
            if (class.isObject()) {
                const class_obj = class.asObject();

                // Get method dictionary
                const method_dict = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);

                if (method_dict.isObject()) {
                    // Look up selector in method dictionary
                    if (self.lookupInMethodDict(method_dict, selector)) |method| {
                        return method;
                    }
                }

                // Move to superclass
                class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
            } else {
                break;
            }
        }

        return null;
    }

    fn lookupInMethodDict(self: *Interpreter, dict: Value, selector: Value) ?*CompiledMethod {
        _ = self;
        // For now, method dictionaries are stored as Arrays of associations
        // Each association is [selector, method]
        // This is inefficient but simple for bootstrapping

        if (!dict.isObject()) return null;

        const dict_obj = dict.asObject();
        const dict_size = dict_obj.header.size;

        if (dict_size == 0) return null;

        var i: usize = 0;
        while (i + 1 < dict_size) : (i += 2) {
            const key = dict_obj.getField(i, dict_size);
            if (key.isNil()) break; // End of entries

            // Compare selectors
            if (key.eql(selector)) {
                const method_val = dict_obj.getField(i + 1, dict_size);
                if (method_val.isObject()) {
                    // The method value points to a CompiledMethod
                    return @ptrCast(@alignCast(method_val.asObject()));
                }
            }
        }

        return null;
    }

    fn sendSpecialBinary(self: *Interpreter, prim: bytecodes.Primitive, selector: []const u8) InterpreterError!void {
        _ = selector;
        const arg = try self.pop();
        const recv = try self.pop();

        // Try primitive
        try self.push(recv);
        try self.push(arg);

        const result = primitives.executePrimitive(self, @intFromEnum(prim)) catch {
            // Fall back to message send
            _ = try self.pop();
            _ = try self.pop();
            try self.push(recv);
            try self.push(arg);
            // TODO: actually send message
            return InterpreterError.MessageNotUnderstood;
        };

        try self.push(result);
    }

    fn sendIdentical(self: *Interpreter) InterpreterError!void {
        const arg = try self.pop();
        const recv = try self.pop();
        try self.push(Value.fromBool(recv.eql(arg)));
    }

    fn sendUnary(_: *Interpreter, _: []const u8) InterpreterError!void {
        // TODO: actual message lookup
        return InterpreterError.MessageNotUnderstood;
    }
};

test "Interpreter - push and pop" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var interp = Interpreter.init(heap);

    try interp.push(Value.fromSmallInt(42));
    try interp.push(Value.fromSmallInt(100));

    try std.testing.expectEqual(@as(i61, 100), (try interp.pop()).asSmallInt());
    try std.testing.expectEqual(@as(i61, 42), (try interp.pop()).asSmallInt());
}
