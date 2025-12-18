/// Baseline JIT Compiler for Zig Smalltalk
/// Translates bytecode to x86-64 machine code for improved performance
///
/// Strategy:
/// - Direct bytecode-to-native translation (no optimization)
/// - Stack kept in memory (not register-mapped)
/// - Complex operations (sends) call back to runtime
/// - Inline cache stubs for monomorphic sends

const std = @import("std");
const builtin = @import("builtin");
const object = @import("object.zig");
const bytecodes = @import("bytecodes.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const InterpreterError = @import("interpreter.zig").InterpreterError;
const Heap = @import("memory.zig").Heap;

const Value = object.Value;
const CompiledMethod = object.CompiledMethod;
const Opcode = bytecodes.Opcode;

// ============================================================================
// Monomorphic Inline Cache (MIC) Structure
// ============================================================================
// Per-callsite cache for fast method dispatch. Each send site caches
// the last seen receiver class and corresponding method.

/// Call site cache entry for monomorphic inline caching
pub const CallSiteCache = struct {
    /// Expected receiver class (for quick comparison)
    expected_class: Value,
    /// Cached method to call when class matches
    cached_method: ?*CompiledMethod,
    /// Class that defines the cached method (for super sends)
    cached_holder: Value,
    /// Cache version for invalidation
    version: u32,
    /// Cached JIT code for the resolved method (if compiled)
    cached_jit_code: ?*CompiledCode,
    /// Owning method pointer for this call site (used for runtime patching)
    method_ptr: usize,
    /// Bytecode offset of the send opcode in owning method
    bytecode_offset: u16,

    pub const EMPTY: CallSiteCache = .{
        .expected_class = Value.nil,
        .cached_method = null,
        .cached_holder = Value.nil,
        .version = 0,
        .cached_jit_code = null,
        .method_ptr = 0,
        .bytecode_offset = 0,
    };
};

// ============================================================================
// Runtime Send Helper
// ============================================================================
// Called from JIT code to perform message sends. This function:
// 1. Sets up the send via interpreter's sendMessage
// 2. Runs the interpret loop until the send completes
// 3. Returns the result value
//
// Calling convention: System V AMD64
// - rdi: Interpreter pointer
// - rsi: selector_index (u8)
// - rdx: num_args (u8)
// - rcx: bytecode_offset (for inline cache keying)
// Returns: Value (result in rax)

fn jitRuntimeSendCommon(
    interp: *Interpreter,
    selector_index: u8,
    num_args: u8,
    bytecode_offset: usize,
    is_super: bool,
) Value {
    // Save state for error recovery
    const saved_context_ptr = interp.context_ptr;
    const saved_sp = interp.sp;
    const saved_receiver = interp.receiver; // IMPORTANT: save receiver for restore after call

    // Call interpreter's sendMessage to set up the call
    interp.sendMessage(selector_index, num_args, is_super, bytecode_offset) catch {
        interp.receiver = saved_receiver;
        return Value.nil;
    };

    // If sendMessage returned without pushing a context (fast path hit),
    // the result is already on the stack
    if (interp.context_ptr == saved_context_ptr) {
        // Fast path completed - pop and return result
        // For 0-arg sends, result replaces receiver so sp == saved_sp (not greater)
        // We need sp > saved_sp - num_args - 1 = recv_pos to have a result
        const recv_pos = saved_sp - @as(usize, num_args) - 1;
        if (interp.sp > recv_pos) {
            interp.sp -= 1;
            const result = interp.stack[interp.sp];
            interp.receiver = saved_receiver; // Restore caller's receiver
            return result;
        }
        interp.receiver = saved_receiver;
        return Value.nil;
    }

    // Method was called - use primitive_block mechanism to track return
    interp.primitive_block_bases[interp.primitive_block_depth] = saved_context_ptr;
    interp.primitive_block_depth += 1;

    // Run interpreter until the send completes
    const result = interp.interpretLoop() catch {
        // On error, restore state
        interp.primitive_block_depth -= 1;
        interp.context_ptr = saved_context_ptr;
        interp.receiver = saved_receiver;
        return Value.nil;
    };

    interp.primitive_block_depth -= 1;
    interp.receiver = saved_receiver; // Restore caller's receiver

    return result;
}

pub fn runCachedJitMethod(
    interp: *Interpreter,
    cache: *CallSiteCache,
    recv: Value,
    recv_pos: usize,
    num_args: u8,
) Value {
    if (cache.cached_jit_code == null or cache.cached_method == null) {
        return Value.nil;
    }

    const saved_method = interp.method;
    const saved_method_class = interp.method_class;
    const saved_receiver = interp.receiver;
    const saved_ip = interp.ip;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;
    const saved_sp = interp.sp;

    // Initialize callee frame metadata so compiled code sees correct state
    const method = cache.cached_method.?;
    const code = cache.cached_jit_code.?;
    interp.method = method;
    interp.method_class = cache.cached_holder;
    interp.receiver = recv;
    interp.ip = 0;
    interp.temp_base = recv_pos;
    interp.outer_temp_base = recv_pos;
    interp.home_temp_base = recv_pos;
    interp.heap_context = Value.nil;
    interp.home_heap_context = Value.nil;

    // Stack layout: [ ... recv args... ]. Set sp to end of args then add temps.
    const base_sp = recv_pos + 1 + @as(usize, num_args);
    if (base_sp > interp.stack.len) {
        // Restore and bail if corrupted
        interp.method = saved_method;
        interp.method_class = saved_method_class;
        interp.receiver = saved_receiver;
        interp.ip = saved_ip;
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        interp.sp = saved_sp;
        return Value.nil;
    }
    interp.sp = base_sp;

    const total_temps: usize = method.header.num_temps;
    const local_temps = if (total_temps > num_args) total_temps - @as(usize, num_args) else 0;
    var k: usize = 0;
    while (k < local_temps and interp.sp < interp.stack.len) : (k += 1) {
        interp.stack[interp.sp] = Value.nil;
        interp.sp += 1;
    }

    const result = code.entry(interp);

    // Restore caller state
    interp.method = saved_method;
    interp.method_class = saved_method_class;
    interp.receiver = saved_receiver;
    interp.ip = saved_ip;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;
    interp.sp = saved_sp;

    return result;
}

/// Runtime helper called from JIT code to perform a message send
/// This executes the full send and returns the result.
/// Uses primitive_block_depth mechanism to detect when send completes.
pub export fn jit_runtime_send(
    interp: *Interpreter,
    selector_index: u8,
    num_args: u8,
    bytecode_offset: usize,
) callconv(.c) Value {
    return jitRuntimeSendCommon(interp, selector_index, num_args, bytecode_offset, false);
}

/// Runtime helper for super sends
pub export fn jit_runtime_super_send(
    interp: *Interpreter,
    selector_index: u8,
    num_args: u8,
    bytecode_offset: usize,
) callconv(.c) Value {
    return jitRuntimeSendCommon(interp, selector_index, num_args, bytecode_offset, true);
}

/// Runtime helper to create a BlockClosure from JIT code.
/// Parameters come from the push_closure bytecode.
/// Returns the created closure Value.
pub export fn jit_runtime_push_closure(
    interp: *Interpreter,
    block_num_args: u8,
    block_num_temps: u8,
    block_start_ip: usize, // IP of the block's first bytecode
) callconv(.c) Value {
    // Lazily create heap context for closure variable capture
    if (interp.heap_context.isNil()) {
        const method_num_temps = interp.method.header.num_temps;
        const method_num_args = interp.method.header.num_args;
        const num_to_capture = method_num_args + method_num_temps;

        const heap_ctx = interp.createHeapContext(num_to_capture) catch {
            return Value.nil;
        };
        interp.heap_context = Value.fromObject(heap_ctx);

        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = interp.heap_context;
        }
    }

    // Create BlockClosure object
    const closure = interp.heap.allocateObject(Heap.CLASS_BLOCK_CLOSURE, Heap.BLOCK_NUM_FIELDS, .normal) catch {
        return Value.nil;
    };

    // Set closure fields
    closure.setField(Heap.BLOCK_FIELD_OUTER_CONTEXT, interp.heap_context, Heap.BLOCK_NUM_FIELDS);
    closure.setField(Heap.BLOCK_FIELD_START_PC, Value.fromSmallInt(@intCast(block_start_ip)), Heap.BLOCK_NUM_FIELDS);
    closure.setField(Heap.BLOCK_FIELD_NUM_ARGS, Value.fromSmallInt(block_num_args), Heap.BLOCK_NUM_FIELDS);
    closure.setField(Heap.BLOCK_FIELD_METHOD, Value.fromObject(@ptrCast(@alignCast(interp.method))), Heap.BLOCK_NUM_FIELDS);
    closure.setField(Heap.BLOCK_FIELD_RECEIVER, interp.receiver, Heap.BLOCK_NUM_FIELDS);
    closure.setField(Heap.BLOCK_FIELD_HOME_CONTEXT, interp.home_heap_context, Heap.BLOCK_NUM_FIELDS);
    closure.setField(Heap.BLOCK_FIELD_NUM_TEMPS, Value.fromSmallInt(block_num_temps), Heap.BLOCK_NUM_FIELDS);

    return Value.fromObject(closure);
}

/// Special binary operation type for JIT runtime helper
pub const SpecialBinaryOp = enum(u8) {
    add = 0,
    subtract = 1,
    multiply = 2,
    divide = 3,
    less_than = 4,
    greater_than = 5,
    less_or_equal = 6,
    greater_or_equal = 7,
    equal = 8,
    not_equal = 9,
};

/// Runtime helper for specialized binary sends (like +, -, <, <=, etc.)
/// These try the primitive fast path and fall back to method lookup.
pub export fn jit_runtime_special_binary(
    interp: *Interpreter,
    op: SpecialBinaryOp,
) callconv(.c) Value {
    const primitives = @import("primitives.zig");

    // Debug logging disabled for performance
    // std.debug.print("jit_runtime_special_binary: op={d}, sp={d}\n", .{ @intFromEnum(op), interp.sp });

    // Get arg and receiver from stack
    if (interp.sp < 2) {
        // std.debug.print("  ERROR: sp < 2!\n", .{});
        return Value.nil;
    }

    const arg = interp.stack[interp.sp - 1];
    const recv = interp.stack[interp.sp - 2];

    // Map to primitive number
    const prim_num: u16 = switch (op) {
        .add => @intFromEnum(bytecodes.Primitive.add),
        .subtract => @intFromEnum(bytecodes.Primitive.subtract),
        .multiply => @intFromEnum(bytecodes.Primitive.multiply),
        .divide => @intFromEnum(bytecodes.Primitive.divide),
        .less_than => @intFromEnum(bytecodes.Primitive.less_than),
        .greater_than => @intFromEnum(bytecodes.Primitive.greater_than),
        .less_or_equal => @intFromEnum(bytecodes.Primitive.less_or_equal),
        .greater_or_equal => @intFromEnum(bytecodes.Primitive.greater_or_equal),
        .equal => @intFromEnum(bytecodes.Primitive.equal),
        .not_equal => @intFromEnum(bytecodes.Primitive.not_equal),
    };

    // Try primitive fast path
    // Note: executePrimitive pops both args from stack
    if (primitives.executePrimitive(interp, prim_num)) |result| {
        // Primitive succeeded - args already popped by primitive, return result
        // JIT code will push the result onto stack
        return result;
    } else |_| {
        // Primitive failed - need to fall back to method lookup
        // Get the selector string for this operation
        const selector: []const u8 = switch (op) {
            .add => "+",
            .subtract => "-",
            .multiply => "*",
            .divide => "/",
            .less_than => "<",
            .greater_than => ">",
            .less_or_equal => "<=",
            .greater_or_equal => ">=",
            .equal => "=",
            .not_equal => "~=",
        };

        // Pop args from stack for method lookup
        interp.sp -= 2;

        // Lookup and call method
        const class = interp.heap.classOf(recv);
        const selector_sym = interp.heap.internSymbol(selector) catch {
            return Value.nil;
        };

        if (interp.lookupMethod(class, selector_sym)) |method| {
            // Push receiver and arg back for method call
            interp.stack[interp.sp] = recv;
            interp.sp += 1;
            interp.stack[interp.sp] = arg;
            interp.sp += 1;

            // Try primitive if method has one
            if (method.header.primitive_index != 0) {
                if (primitives.executePrimitive(interp, method.header.primitive_index)) |result| {
                    interp.sp -= 2;
                    return result;
                } else |_| {}
            }

            // Execute method bytecode using primitive_block mechanism
            const saved_context_ptr = interp.context_ptr;

            // Set up method call
            if (interp.context_ptr >= interp.contexts.len - 1) {
                interp.sp -= 2;
                return Value.nil;
            }

            interp.contexts[interp.context_ptr] = .{
                .method = interp.method,
                .method_class = interp.method_class,
                .ip = interp.ip,
                .receiver = interp.receiver,
                .temp_base = interp.temp_base,
                .outer_temp_base = interp.outer_temp_base,
                .home_temp_base = interp.home_temp_base,
                .heap_context = interp.heap_context,
                .home_heap_context = interp.home_heap_context,
            };
            interp.context_ptr += 1;

            const recv_pos = interp.sp - 2;
            interp.method = method;
            interp.method_class = class;
            interp.ip = 0;
            interp.receiver = recv;
            interp.temp_base = recv_pos;
            interp.outer_temp_base = recv_pos;
            interp.home_temp_base = recv_pos;
            interp.heap_context = Value.nil;
            interp.home_heap_context = Value.nil;

            // Initialize temps
            const num_temps = method.header.num_temps;
            const num_args: usize = 1; // Binary message has 1 arg
            if (num_temps > num_args) {
                var k: usize = 0;
                while (k < num_temps - num_args and interp.sp < interp.stack.len) : (k += 1) {
                    interp.stack[interp.sp] = Value.nil;
                    interp.sp += 1;
                }
            }

            // Use primitive_block mechanism to detect when send completes
            interp.primitive_block_bases[interp.primitive_block_depth] = saved_context_ptr;
            interp.primitive_block_depth += 1;

            const result = interp.interpretLoop() catch {
                interp.primitive_block_depth -= 1;
                return Value.nil;
            };

            interp.primitive_block_depth -= 1;
            return result;
        }

        return Value.nil;
    }
}

/// Global storage for SmallInteger class bits (for inline cache checks)
/// This is set once during JIT initialization and used by inline cache code.
var smallint_class_bits: u64 = 0;

/// Initialize the SmallInteger class bits for inline cache checks.
/// Must be called after heap is initialized with class table.
pub fn initSmallIntClass(heap: *@import("memory.zig").Heap) void {
    smallint_class_bits = heap.getClass(@import("memory.zig").Heap.CLASS_SMALL_INTEGER).bits;
}

/// Get the address of the SmallInteger class bits (for embedding in JIT code)
pub fn getSmallIntClassBitsAddr() usize {
    return @intFromPtr(&smallint_class_bits);
}

/// Runtime helper for inline cache hit - calls cached JIT code with proper setup.
/// This is a simplified fast path that skips method lookup.
pub export fn jit_runtime_cached_send(
    interp: *Interpreter,
    cache: *CallSiteCache,
    num_args: u8,
) callconv(.c) Value {
    // Get receiver position and value
    const recv_pos = interp.sp - @as(usize, num_args) - 1;
    const recv = interp.stack[recv_pos];

    // Call the cached JIT code with proper setup
    const result = runCachedJitMethod(interp, cache, recv, recv_pos, num_args);

    // Adjust stack: pop receiver/args, caller will push result
    interp.sp = recv_pos;
    return result;
}

/// Runtime helper for at: primitive - slow path for non-Array or bounds errors.
/// Stack: [receiver, index] with sp pointing past index.
/// Returns element or nil on error.
pub export fn jit_runtime_at(interp: *Interpreter) callconv(.c) Value {
    if (interp.sp < 2) return Value.nil;

    const index = interp.stack[interp.sp - 1];
    const recv = interp.stack[interp.sp - 2];

    // Pop receiver and index
    interp.sp -= 2;

    if (!recv.isObject()) return Value.nil;
    if (!index.isSmallInt()) return Value.nil;

    const obj = recv.asObject();
    const idx_raw = index.asSmallInt();
    if (idx_raw < 1) return Value.nil;

    const idx: usize = @intCast(idx_raw - 1); // Convert to 0-based
    const obj_size = obj.header.size;

    if (idx >= obj_size) return Value.nil;

    return obj.fields(obj_size)[idx];
}

/// Runtime helper for at:put: primitive - slow path.
/// Stack: [receiver, index, value] with sp pointing past value.
/// Returns receiver.
pub export fn jit_runtime_at_put(interp: *Interpreter) callconv(.c) Value {
    if (interp.sp < 3) return Value.nil;

    const val = interp.stack[interp.sp - 1];
    const index = interp.stack[interp.sp - 2];
    const recv = interp.stack[interp.sp - 3];

    // Pop receiver, index, and value
    interp.sp -= 3;

    if (!recv.isObject()) return Value.nil;
    if (!index.isSmallInt()) return Value.nil;

    const obj = recv.asObject();
    const idx_raw = index.asSmallInt();
    if (idx_raw < 1) return Value.nil;

    const idx: usize = @intCast(idx_raw - 1); // Convert to 0-based
    const obj_size = obj.header.size;

    if (idx >= obj_size) return Value.nil;

    obj.fields(obj_size)[idx] = val;
    return recv; // at:put: returns receiver
}

/// Runtime helper that uses per-callsite cache to dispatch sends.
/// This is the fast path for compiled JIT code.
pub export fn jit_runtime_send_with_cache(
    interp: *Interpreter,
    cache: *CallSiteCache,
    selector_index: u8,
    num_args: u8,
    bytecode_offset: usize,
    is_super: bool,
) callconv(.c) Value {
    const saved_sp = interp.sp;
    if (saved_sp == 0 or saved_sp <= num_args) {
        return Value.nil;
    }

    const recv_pos = saved_sp - @as(usize, num_args) - 1;
    const recv = interp.stack[recv_pos];
    const recv_class = interp.heap.classOf(recv);

    // Cache hit: class + version match and we have compiled code
    if (cache.expected_class.bits == recv_class.bits and
        cache.version == interp.cache_version and
        cache.cached_jit_code != null)
    {
        const result = runCachedJitMethod(interp, cache, recv, recv_pos, num_args);
        // Pop receiver/args, caller will push the result
        interp.sp = recv_pos;
        return result;
    }

    // Cache miss - fall back to full send path
    const result = jitRuntimeSendCommon(interp, selector_index, num_args, bytecode_offset, is_super);

    // Attempt to patch cache from interpreter inline cache entry (avoids hashing for hot sites)
    if (cache.method_ptr != 0) {
        const ic_hash = cache.method_ptr ^ @as(usize, cache.bytecode_offset);
        const ic_index = ic_hash & (interp.inline_cache.len - 1);
        const ic_entry = &interp.inline_cache[ic_index];
        if (ic_entry.method_ptr == cache.method_ptr and
            ic_entry.bytecode_offset == cache.bytecode_offset and
            ic_entry.cached_method != null and
            ic_entry.cached_class.bits == recv_class.bits)
        {
            var jit_code = ic_entry.cached_jit_code;
            // Try to lazily compile if the interpreter hasn't cached JIT code yet
            if (jit_code == null and interp.jit_enabled) {
                if (interp.jit_compiler) |jit_ptr| {
                    var compiled = jit_ptr.getCompiled(ic_entry.cached_method.?);
                    if (compiled == null and JIT.isJitEligible(ic_entry.cached_method.?)) {
                        _ = jit_ptr.compile(ic_entry.cached_method.?) catch null;
                        compiled = jit_ptr.getCompiled(ic_entry.cached_method.?);
                    }
                    if (compiled) |code| {
                        ic_entry.cached_jit_code = code;
                        jit_code = code;
                    }
                }
            }

            if (jit_code) |code| {
                cache.expected_class = recv_class;
                cache.version = interp.cache_version;
                cache.cached_method = ic_entry.cached_method;
                cache.cached_holder = ic_entry.cached_holder;
                cache.cached_jit_code = code;
            }
        }
    }

    // Align Smalltalk stack for caller push
    interp.sp = recv_pos;

    return result;
}

/// x86-64 register encoding
const Reg = enum(u4) {
    rax = 0,
    rcx = 1,
    rdx = 2,
    rbx = 3,
    rsp = 4,
    rbp = 5,
    rsi = 6,
    rdi = 7,
    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
};

/// JIT calling convention (System V AMD64):
/// - rdi: Interpreter pointer (first arg)
/// - rsi: Method pointer (second arg)
/// - rbx: Interpreter pointer (callee-saved, cached)
/// - r12: Stack pointer (sp) cached
/// - r13: Temp base cached
/// - rbp: Frame pointer
/// - rsp: Native stack pointer

/// Code buffer for generated machine code
pub const CodeBuffer = struct {
    code: []u8,
    pos: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, size: usize) !CodeBuffer {
        const code = try allocator.alloc(u8, size);
        return CodeBuffer{
            .code = code,
            .pos = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CodeBuffer) void {
        self.allocator.free(self.code);
    }

    pub fn emit8(self: *CodeBuffer, byte: u8) void {
        if (self.pos < self.code.len) {
            self.code[self.pos] = byte;
            self.pos += 1;
        }
    }

    pub fn emit16(self: *CodeBuffer, value: u16) void {
        self.emit8(@truncate(value));
        self.emit8(@truncate(value >> 8));
    }

    pub fn emit32(self: *CodeBuffer, value: u32) void {
        self.emit8(@truncate(value));
        self.emit8(@truncate(value >> 8));
        self.emit8(@truncate(value >> 16));
        self.emit8(@truncate(value >> 24));
    }

    pub fn emit64(self: *CodeBuffer, value: u64) void {
        self.emit32(@truncate(value));
        self.emit32(@truncate(value >> 32));
    }

    pub fn currentPos(self: *CodeBuffer) usize {
        return self.pos;
    }

    pub fn getCode(self: *CodeBuffer) []const u8 {
        return self.code[0..self.pos];
    }

    /// Patch a rel32 value at a given position (for backpatching forward jumps)
    pub fn patchRel32(self: *CodeBuffer, pos: usize, value: i32) void {
        if (pos + 4 <= self.code.len) {
            const bytes: [4]u8 = @bitCast(value);
            self.code[pos] = bytes[0];
            self.code[pos + 1] = bytes[1];
            self.code[pos + 2] = bytes[2];
            self.code[pos + 3] = bytes[3];
        }
    }

    // ========================================================================
    // x86-64 Instruction Encoding
    // ========================================================================

    /// REX prefix for 64-bit operations
    fn rex(self: *CodeBuffer, w: bool, r: bool, x: bool, b: bool) void {
        var byte: u8 = 0x40;
        if (w) byte |= 0x08; // 64-bit operand
        if (r) byte |= 0x04; // REX.R (extend ModRM.reg)
        if (x) byte |= 0x02; // REX.X (extend SIB.index)
        if (b) byte |= 0x01; // REX.B (extend ModRM.rm or SIB.base)
        self.emit8(byte);
    }

    /// ModRM byte
    fn modRM(self: *CodeBuffer, mod: u2, reg: u3, rm: u3) void {
        self.emit8((@as(u8, mod) << 6) | (@as(u8, reg) << 3) | @as(u8, rm));
    }

    /// push reg64
    pub fn pushReg(self: *CodeBuffer, reg: Reg) void {
        const r: u8 = @intFromEnum(reg);
        if (r >= 8) {
            self.rex(false, false, false, true);
        }
        self.emit8(0x50 + (r & 7));
    }

    /// pop reg64
    pub fn popReg(self: *CodeBuffer, reg: Reg) void {
        const r: u8 = @intFromEnum(reg);
        if (r >= 8) {
            self.rex(false, false, false, true);
        }
        self.emit8(0x58 + (r & 7));
    }

    /// mov reg64, imm64
    pub fn movRegImm64(self: *CodeBuffer, reg: Reg, imm: u64) void {
        const r: u8 = @intFromEnum(reg);
        self.rex(true, false, false, r >= 8);
        self.emit8(0xB8 + (r & 7));
        self.emit64(imm);
    }

    /// mov reg64, reg64
    pub fn movRegReg(self: *CodeBuffer, dst: Reg, src: Reg) void {
        const d = @intFromEnum(dst);
        const s = @intFromEnum(src);
        self.rex(true, s >= 8, false, d >= 8);
        self.emit8(0x89);
        self.modRM(3, @truncate(s & 7), @truncate(d & 7));
    }

    /// mov reg64, [reg64 + disp32]
    pub fn movRegMem(self: *CodeBuffer, dst: Reg, base: Reg, disp: i32) void {
        const d = @intFromEnum(dst);
        const b = @intFromEnum(base);
        self.rex(true, d >= 8, false, b >= 8);
        self.emit8(0x8B);
        if (disp == 0 and (b & 7) != 5) {
            self.modRM(0, @truncate(d & 7), @truncate(b & 7));
            if ((b & 7) == 4) self.emit8(0x24); // SIB for rsp
        } else if (disp >= -128 and disp <= 127) {
            self.modRM(1, @truncate(d & 7), @truncate(b & 7));
            if ((b & 7) == 4) self.emit8(0x24);
            self.emit8(@bitCast(@as(i8, @truncate(disp))));
        } else {
            self.modRM(2, @truncate(d & 7), @truncate(b & 7));
            if ((b & 7) == 4) self.emit8(0x24);
            self.emit32(@bitCast(disp));
        }
    }

    /// mov [reg64 + disp32], reg64
    pub fn movMemReg(self: *CodeBuffer, base: Reg, disp: i32, src: Reg) void {
        const s = @intFromEnum(src);
        const b = @intFromEnum(base);
        self.rex(true, s >= 8, false, b >= 8);
        self.emit8(0x89);
        if (disp == 0 and (b & 7) != 5) {
            self.modRM(0, @truncate(s & 7), @truncate(b & 7));
            if ((b & 7) == 4) self.emit8(0x24);
        } else if (disp >= -128 and disp <= 127) {
            self.modRM(1, @truncate(s & 7), @truncate(b & 7));
            if ((b & 7) == 4) self.emit8(0x24);
            self.emit8(@bitCast(@as(i8, @truncate(disp))));
        } else {
            self.modRM(2, @truncate(s & 7), @truncate(b & 7));
            if ((b & 7) == 4) self.emit8(0x24);
            self.emit32(@bitCast(disp));
        }
    }

    /// add reg64, imm32
    pub fn addRegImm32(self: *CodeBuffer, reg: Reg, imm: i32) void {
        const r = @intFromEnum(reg);
        self.rex(true, false, false, r >= 8);
        if (imm >= -128 and imm <= 127) {
            self.emit8(0x83);
            self.modRM(3, 0, @truncate(r & 7));
            self.emit8(@bitCast(@as(i8, @truncate(imm))));
        } else {
            self.emit8(0x81);
            self.modRM(3, 0, @truncate(r & 7));
            self.emit32(@bitCast(imm));
        }
    }

    /// sub reg64, imm32
    pub fn subRegImm32(self: *CodeBuffer, reg: Reg, imm: i32) void {
        const r = @intFromEnum(reg);
        self.rex(true, false, false, r >= 8);
        if (imm >= -128 and imm <= 127) {
            self.emit8(0x83);
            self.modRM(3, 5, @truncate(r & 7));
            self.emit8(@bitCast(@as(i8, @truncate(imm))));
        } else {
            self.emit8(0x81);
            self.modRM(3, 5, @truncate(r & 7));
            self.emit32(@bitCast(imm));
        }
    }

    /// add reg64, reg64
    pub fn addRegReg(self: *CodeBuffer, dst: Reg, src: Reg) void {
        const d = @intFromEnum(dst);
        const s = @intFromEnum(src);
        self.rex(true, s >= 8, false, d >= 8);
        self.emit8(0x01);
        self.modRM(3, @truncate(s & 7), @truncate(d & 7));
    }

    /// sub reg64, reg64
    pub fn subRegReg(self: *CodeBuffer, dst: Reg, src: Reg) void {
        const d = @intFromEnum(dst);
        const s = @intFromEnum(src);
        self.rex(true, s >= 8, false, d >= 8);
        self.emit8(0x29);
        self.modRM(3, @truncate(s & 7), @truncate(d & 7));
    }

    /// cmp reg64, reg64
    pub fn cmpRegReg(self: *CodeBuffer, r1: Reg, r2: Reg) void {
        const a = @intFromEnum(r1);
        const b = @intFromEnum(r2);
        self.rex(true, b >= 8, false, a >= 8);
        self.emit8(0x39);
        self.modRM(3, @truncate(b & 7), @truncate(a & 7));
    }

    /// cmp reg64, imm32
    pub fn cmpRegImm32(self: *CodeBuffer, reg: Reg, imm: i32) void {
        const r = @intFromEnum(reg);
        self.rex(true, false, false, r >= 8);
        if (imm >= -128 and imm <= 127) {
            self.emit8(0x83);
            self.modRM(3, 7, @truncate(r & 7));
            self.emit8(@bitCast(@as(i8, @truncate(imm))));
        } else {
            self.emit8(0x81);
            self.modRM(3, 7, @truncate(r & 7));
            self.emit32(@bitCast(imm));
        }
    }

    /// cmp reg64, imm64 (uses r11 as scratch)
    pub fn cmpRegImm64(self: *CodeBuffer, reg: Reg, imm: u64) void {
        // x86-64 doesn't have cmp with 64-bit immediate
        // Use r11 as scratch: mov r11, imm64; cmp reg, r11
        self.movRegImm64(.r11, imm);
        self.cmpRegReg(reg, .r11);
    }

    /// ret
    pub fn ret(self: *CodeBuffer) void {
        self.emit8(0xC3);
    }

    /// call reg64
    pub fn callReg(self: *CodeBuffer, reg: Reg) void {
        const r = @intFromEnum(reg);
        if (r >= 8) {
            self.rex(false, false, false, true);
        }
        self.emit8(0xFF);
        self.modRM(3, 2, @truncate(r & 7));
    }

    /// call [rip + disp32] (for calling function pointers)
    pub fn callRipRelative(self: *CodeBuffer, disp: i32) void {
        self.emit8(0xFF);
        self.emit8(0x15);
        self.emit32(@bitCast(disp));
    }

    /// jmp rel32
    pub fn jmpRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0xE9);
        self.emit32(@bitCast(offset));
    }

    /// jmp rel8
    pub fn jmpRel8(self: *CodeBuffer, offset: i8) void {
        self.emit8(0xEB);
        self.emit8(@bitCast(offset));
    }

    /// je rel32 (jump if equal/zero)
    pub fn jeRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x84);
        self.emit32(@bitCast(offset));
    }

    /// jne rel32 (jump if not equal/not zero)
    pub fn jneRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x85);
        self.emit32(@bitCast(offset));
    }

    /// jl rel32 (jump if less, signed)
    pub fn jlRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x8C);
        self.emit32(@bitCast(offset));
    }

    /// jge rel32 (jump if greater or equal, signed)
    pub fn jgeRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x8D);
        self.emit32(@bitCast(offset));
    }

    /// jle rel32 (jump if less or equal, signed)
    pub fn jleRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x8E);
        self.emit32(@bitCast(offset));
    }

    /// jg rel32 (jump if greater, signed)
    pub fn jgRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x8F);
        self.emit32(@bitCast(offset));
    }

    /// cmp reg, imm8 (compare register with 8-bit immediate)
    pub fn cmpRegImm8(self: *CodeBuffer, reg: Reg, imm: u8) void {
        const reg_idx: u8 = @intFromEnum(reg);
        if (reg_idx >= 8) {
            self.rex(true, false, false, true); // REX.W + REX.B for extended reg
        } else {
            self.rex(true, false, false, false); // REX.W for 64-bit
        }
        self.emit8(0x83); // CMP r/m64, imm8
        self.emit8(0xF8 | (reg_idx & 0x7)); // ModRM: mod=11, reg=111 (/7), rm=reg
        self.emit8(imm);
    }

    /// and reg, imm8 (AND register with 8-bit immediate)
    pub fn andRegImm8(self: *CodeBuffer, reg: Reg, imm: u8) void {
        const reg_idx: u8 = @intFromEnum(reg);
        if (reg_idx >= 8) {
            self.rex(true, false, false, true);
        } else {
            self.rex(true, false, false, false);
        }
        self.emit8(0x83); // AND r/m64, imm8
        self.emit8(0xE0 | (reg_idx & 0x7)); // ModRM: mod=11, reg=100 (/4), rm=reg
        self.emit8(imm);
    }

    /// nop
    pub fn nop(self: *CodeBuffer) void {
        self.emit8(0x90);
    }

    /// int3 (breakpoint)
    pub fn int3(self: *CodeBuffer) void {
        self.emit8(0xCC);
    }
};

/// Compiled native code for a method
// Page size for memory allocation (4KB on most systems)
const PAGE_SIZE: usize = 4096;

// ============================================================================
// Cross-Platform Executable Memory Allocation
// ============================================================================

/// Allocate executable memory (works on Windows and POSIX)
fn allocateExecutableMemory(size: usize) ![]align(PAGE_SIZE) u8 {
    if (builtin.os.tag == .windows) {
        const windows = std.os.windows;
        const ptr = try windows.VirtualAlloc(
            null,
            size,
            windows.MEM_COMMIT | windows.MEM_RESERVE,
            windows.PAGE_EXECUTE_READWRITE,
        );
        const aligned_ptr: [*]align(PAGE_SIZE) u8 = @alignCast(@ptrCast(ptr));
        return aligned_ptr[0..size];
    } else {
        return std.posix.mmap(
            null,
            size,
            std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
    }
}

/// Free executable memory (works on Windows and POSIX)
fn freeExecutableMemory(mem: []align(PAGE_SIZE) u8) void {
    if (builtin.os.tag == .windows) {
        const windows = std.os.windows;
        windows.VirtualFree(@ptrCast(mem.ptr), 0, windows.MEM_RELEASE);
    } else {
        std.posix.munmap(mem);
    }
}

pub const CompiledCode = struct {
    code: []u8,
    entry: *const fn (*Interpreter) callconv(.c) Value,
    method: *CompiledMethod,
    allocator: std.mem.Allocator,
    call_site_caches: []CallSiteCache,

    pub fn deinit(self: *CompiledCode) void {
        if (self.call_site_caches.len > 0) {
            self.allocator.free(self.call_site_caches);
        }
        // Free executable memory
        const aligned_ptr: [*]align(PAGE_SIZE) u8 = @alignCast(self.code.ptr);
        freeExecutableMemory(aligned_ptr[0..self.code.len]);
    }
};

/// The JIT Compiler
pub const JIT = struct {
    allocator: std.mem.Allocator,
    compiled_methods: std.AutoHashMap(*CompiledMethod, *CompiledCode),

    // Offsets into Interpreter struct (must match interpreter.zig)
    const INTERP_STACK_OFFSET = @offsetOf(Interpreter, "stack");
    const INTERP_SP_OFFSET = @offsetOf(Interpreter, "sp");
    const INTERP_TEMP_BASE_OFFSET = @offsetOf(Interpreter, "temp_base");
    const INTERP_RECEIVER_OFFSET = @offsetOf(Interpreter, "receiver");
    const INTERP_CACHE_VERSION_OFFSET = @offsetOf(Interpreter, "cache_version");
    const INTERP_HEAP_OFFSET = @offsetOf(Interpreter, "heap");

    // Offsets into CallSiteCache struct
    const CACHE_EXPECTED_CLASS_OFFSET = @offsetOf(CallSiteCache, "expected_class");
    const CACHE_VERSION_OFFSET = @offsetOf(CallSiteCache, "version");
    const CACHE_JIT_CODE_OFFSET = @offsetOf(CallSiteCache, "cached_jit_code");

    pub fn init(allocator: std.mem.Allocator) JIT {
        return JIT{
            .allocator = allocator,
            .compiled_methods = std.AutoHashMap(*CompiledMethod, *CompiledCode).init(allocator),
        };
    }

    pub fn deinit(self: *JIT) void {
        var it = self.compiled_methods.valueIterator();
        while (it.next()) |compiled| {
            compiled.*.deinit();
            self.allocator.destroy(compiled.*);
        }
        self.compiled_methods.deinit();
    }

    /// Drop all compiled methods (used on GC to avoid stale pointers)
    pub fn reset(self: *JIT) void {
        var it = self.compiled_methods.valueIterator();
        while (it.next()) |compiled| {
            compiled.*.deinit();
            self.allocator.destroy(compiled.*);
        }
        self.compiled_methods.clearRetainingCapacity();
    }

    /// Invalidate all call-site caches after a GC so stale heap pointers aren't reused.
    /// Keeps compiled code alive but forces repatching on next send.
    pub fn invalidateCachesForGc(self: *JIT) void {
        var it = self.compiled_methods.valueIterator();
        while (it.next()) |compiled| {
            for (compiled.*.call_site_caches) |*cache| {
                cache.expected_class = Value.nil;
                cache.cached_method = null;
                cache.cached_holder = Value.nil;
                cache.version = 0;
                cache.cached_jit_code = null; // force repatch
            }
        }
    }

    /// Get or compile a method
    pub fn getCompiled(self: *JIT, method: *CompiledMethod) ?*CompiledCode {
        if (self.compiled_methods.get(method)) |compiled| {
            return compiled;
        }

        // Check if method is JIT-eligible
        if (!isJitEligible(method)) {
            return null;
        }

        // Try to compile the method
        const compiled = self.compile(method) catch return null;
        self.compiled_methods.put(method, compiled) catch {
            compiled.deinit();
            self.allocator.destroy(compiled);
            return null;
        };
        return compiled;
    }

    /// Check if a method can be JIT compiled
    /// Returns false for methods with blocks, jumps, or other complex ops
    pub fn isJitEligible(method: *CompiledMethod) bool {
        // Primitive methods should not be JIT compiled - the primitive must be tried first
        // and the bytecodes are only fallback code for when the primitive fails
        if (method.header.primitive_index != 0) {
            return false;
        }

        // Temporaries beyond arguments currently require more frame setup; skip for safety
        if (method.header.num_temps > 0) {
            // std.debug.print("JIT ineligible: has {} temps\n", .{method.header.num_temps});
            return false;
        }

        const bc = method.getBytecodes();
        var ip: usize = 0;

        while (ip < bc.len) {
            const opcode = bc[ip];
            ip += 1;

            // Short-form opcodes are generally OK
            if (Opcode.isPushReceiverVariable(opcode)) continue;
            if (Opcode.isPushTemporary(opcode)) continue;
            if (Opcode.isStoreTemporary(opcode)) continue;
            if (Opcode.isPopStoreTemporary(opcode)) continue;

            // Check for disqualifying opcodes
            const op: Opcode = @enumFromInt(opcode);
            switch (op) {
                // Simple ops - OK
                .push_receiver, .push_nil, .push_true, .push_false,
                .pop, .dup, .return_top, .return_receiver, .return_nil => {},

                // Push with index - OK
                .push_literal => ip += 1,
                .push_temporary => ip += 1,
                .push_integer => ip += 1, // signed 8-bit integer follows

                // Sends - OK (we have runtime send support)
                .send => ip += 2,
                // super_send is temporarily disabled due to stack overflow bug
                .super_send => return false,

                // Specialized binary sends - OK (no operands)
                .send_plus, .send_minus, .send_times, .send_divide,
                .send_less_than, .send_greater_than,
                .send_less_or_equal, .send_greater_or_equal,
                .send_equal, .send_not_equal => {},

                // Jumps - now supported with backpatching
                .jump, .jump_if_true, .jump_if_false, .jump_if_nil, .jump_if_not_nil => {
                    ip += 2; // skip 16-bit offset
                },

                // Short jumps (offset in opcode, forward only)
                .short_jump_0, .short_jump_1, .short_jump_2, .short_jump_3,
                .short_jump_4, .short_jump_5, .short_jump_6, .short_jump_7 => {},

                // Block creation - now supported with runtime helper
                .push_closure => {
                    // Skip: num_args, num_temps, size_hi, size_lo
                    if (ip + 4 > bc.len) return false;
                    const size_hi: u16 = bc[ip + 2];
                    const size_lo: u16 = bc[ip + 3];
                    const block_size: usize = @intCast((size_hi << 8) | size_lo);
                    ip += 4 + block_size; // Skip header + block bytecodes
                },

                // Non-local return - NOT eligible (complex)
                .block_return => return false,

                // Extended opcodes - NOT eligible for now
                .extended_push, .extended_store, .extended_send => return false,

                // Everything else - NOT eligible
                else => {
                    // std.debug.print("JIT ineligible: unsupported opcode 0x{x}\n", .{opcode});
                    return false;
                },
            }
        }

        return true;
    }

    /// Compile a method to native code
    pub fn compile(self: *JIT, method: *CompiledMethod) !*CompiledCode {
        const bc = method.getBytecodes();

        // Pre-scan to count send sites so we can allocate per-callsite caches
        var send_count: usize = 0;
        var scan_ip: usize = 0;
        while (scan_ip < bc.len) {
            const opcode = bc[scan_ip];
            scan_ip += 1;
            if (Opcode.isPushReceiverVariable(opcode) or Opcode.isPushTemporary(opcode) or Opcode.isStoreTemporary(opcode) or Opcode.isPopStoreTemporary(opcode)) {
                continue;
            }
            const op: Opcode = @enumFromInt(opcode);
            switch (op) {
                .push_literal, .push_temporary => scan_ip += 1,
                .send, .super_send => {
                    // selector + arg count bytes
                    scan_ip += 2;
                    send_count += 1;
                },
                else => {},
            }
        }

        var call_site_caches = try self.allocator.alloc(CallSiteCache, send_count);
        errdefer self.allocator.free(call_site_caches);
        for (call_site_caches) |*cache| {
            cache.* = CallSiteCache.EMPTY;
            cache.method_ptr = @intFromPtr(method);
        }

        var buffer = try CodeBuffer.init(self.allocator, 4096);
        defer buffer.deinit();

        // Track bytecode IP → native code position for jump resolution
        // We allocate one entry per bytecode byte (not all will be used)
        var bc_to_native = try self.allocator.alloc(usize, bc.len + 1);
        defer self.allocator.free(bc_to_native);
        for (bc_to_native) |*entry| {
            entry.* = 0; // 0 means "not yet emitted"
        }

        // Forward jump patches: (patch_position, target_bytecode_ip)
        const ForwardPatch = struct {
            patch_pos: usize, // position of rel32 in code buffer
            target_bc_ip: usize, // bytecode IP to jump to
        };
        var forward_patches = std.ArrayListUnmanaged(ForwardPatch){};
        defer forward_patches.deinit(self.allocator);

        // Generate prologue
        self.emitPrologue(&buffer);

        // Translate bytecodes
        var ip: usize = 0;
        var call_site_index: usize = 0;

        while (ip < bc.len) {
            // Record native position for this bytecode IP (before consuming opcode)
            bc_to_native[ip] = buffer.currentPos();

            const opcode = bc[ip];
            ip += 1;

            // Handle short-form opcodes
            if (Opcode.isPushReceiverVariable(opcode)) {
                const index = Opcode.getEmbeddedIndex(opcode);
                self.emitPushReceiverVariable(&buffer, index);
                continue;
            }
            if (Opcode.isPushTemporary(opcode)) {
                const index = Opcode.getEmbeddedIndex(opcode);
                self.emitPushTemporary(&buffer, index);
                continue;
            }
            if (Opcode.isStoreTemporary(opcode)) {
                const index = opcode - 0x50;
                self.emitStoreTemporary(&buffer, index);
                continue;
            }
            if (Opcode.isPopStoreTemporary(opcode)) {
                const index = opcode - 0x70;
                self.emitPopStoreTemporary(&buffer, index);
                continue;
            }

            // Handle regular opcodes
            const op: Opcode = @enumFromInt(opcode);
            switch (op) {
                .push_receiver => self.emitPushSelf(&buffer),
                .push_nil => self.emitPushNil(&buffer),
                .push_true => self.emitPushTrue(&buffer),
                .push_false => self.emitPushFalse(&buffer),
                .push_literal => {
                    const lit_idx = bc[ip];
                    ip += 1;
                    self.emitPushLiteral(&buffer, method, lit_idx);
                },
                .push_temporary => {
                    const idx = bc[ip];
                    ip += 1;
                    self.emitPushTemporary(&buffer, idx);
                },
                .push_integer => {
                    const signed_val: i8 = @bitCast(bc[ip]);
                    ip += 1;
                    // Convert to SmallInteger Value and push
                    const value = Value.fromSmallInt(@intCast(signed_val));
                    self.emitPushImmediate(&buffer, value);
                },
                .pop => self.emitPop(&buffer),
                .dup => self.emitDup(&buffer),
                .return_top => {
                    self.emitReturnTop(&buffer);
                },
                .return_receiver => {
                    self.emitReturnSelf(&buffer);
                },
                .return_nil => {
                    self.emitReturnNil(&buffer);
                },
                .send => {
                    const selector_idx = bc[ip];
                    ip += 1;
                    const n_args = bc[ip];
                    ip += 1;
                    // bytecode_offset is the position of the send opcode (ip - 3)
                    const send_offset = ip - 3;
                    if (call_site_index < call_site_caches.len) {
                        call_site_caches[call_site_index].bytecode_offset = @intCast(send_offset);
                        call_site_index += 1;
                        self.emitSend(&buffer, selector_idx, n_args, send_offset, &call_site_caches[call_site_index - 1], false);
                    } else {
                        self.emitReturnNil(&buffer);
                        break;
                    }
                },
                .super_send => {
                    const selector_idx = bc[ip];
                    ip += 1;
                    const n_args = bc[ip];
                    ip += 1;
                    const send_offset = ip - 3;
                    if (call_site_index < call_site_caches.len) {
                        call_site_caches[call_site_index].bytecode_offset = @intCast(send_offset);
                        call_site_index += 1;
                        self.emitSuperSend(&buffer, selector_idx, n_args, send_offset, &call_site_caches[call_site_index - 1]);
                    } else {
                        self.emitReturnNil(&buffer);
                        break;
                    }
                },
                // Specialized binary sends
                .send_plus => self.emitSpecialBinaryOp(&buffer, .add),
                .send_minus => self.emitSpecialBinaryOp(&buffer, .subtract),
                .send_times => self.emitSpecialBinaryOp(&buffer, .multiply),
                .send_divide => self.emitSpecialBinaryOp(&buffer, .divide),
                .send_less_than => self.emitSpecialBinaryOp(&buffer, .less_than),
                .send_greater_than => self.emitSpecialBinaryOp(&buffer, .greater_than),
                .send_less_or_equal => self.emitSpecialBinaryOp(&buffer, .less_or_equal),
                .send_greater_or_equal => self.emitSpecialBinaryOp(&buffer, .greater_or_equal),
                .send_equal => self.emitSpecialBinaryOp(&buffer, .equal),
                .send_not_equal => self.emitSpecialBinaryOp(&buffer, .not_equal),
                // Jump bytecodes with 16-bit signed offset (big-endian: hi byte first)
                .jump => {
                    const offset_hi = bc[ip];
                    const offset_lo = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast((@as(u16, offset_hi) << 8) | @as(u16, offset_lo));
                    const target_bc_ip: usize = @intCast(@as(isize, @intCast(ip)) + @as(isize, offset));

                    // Check if target is backward (already emitted) or forward (needs patching)
                    if (target_bc_ip < ip and bc_to_native[target_bc_ip] != 0) {
                        // Backward jump - target known
                        const target_native = bc_to_native[target_bc_ip];
                        const jump_end = buffer.currentPos() + 5; // jmp rel32 is 5 bytes
                        const rel_offset: i32 = @intCast(@as(isize, @intCast(target_native)) - @as(isize, @intCast(jump_end)));
                        buffer.jmpRel32(rel_offset);
                    } else {
                        // Forward jump - emit placeholder and record for patching
                        buffer.emit8(0xE9); // jmp rel32
                        try forward_patches.append(self.allocator, .{ .patch_pos = buffer.currentPos(), .target_bc_ip = target_bc_ip });
                        buffer.emit32(0); // placeholder
                    }
                },
                .jump_if_true => {
                    const offset_hi = bc[ip];
                    const offset_lo = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast((@as(u16, offset_hi) << 8) | @as(u16, offset_lo));
                    const target_bc_ip: usize = @intCast(@as(isize, @intCast(ip)) + @as(isize, offset));

                    // Pop TOS and compare with true
                    self.emitPopToRax(&buffer);
                    buffer.cmpRegImm64(.rax, Value.@"true".bits);

                    if (target_bc_ip < ip and bc_to_native[target_bc_ip] != 0) {
                        const target_native = bc_to_native[target_bc_ip];
                        const jump_end = buffer.currentPos() + 6; // je rel32 is 6 bytes
                        const rel_offset: i32 = @intCast(@as(isize, @intCast(target_native)) - @as(isize, @intCast(jump_end)));
                        buffer.jeRel32(rel_offset);
                    } else {
                        buffer.emit8(0x0F);
                        buffer.emit8(0x84); // je rel32
                        try forward_patches.append(self.allocator, .{ .patch_pos = buffer.currentPos(), .target_bc_ip = target_bc_ip });
                        buffer.emit32(0);
                    }
                },
                .jump_if_false => {
                    const offset_hi = bc[ip];
                    const offset_lo = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast((@as(u16, offset_hi) << 8) | @as(u16, offset_lo));
                    const target_bc_ip: usize = @intCast(@as(isize, @intCast(ip)) + @as(isize, offset));

                    // Pop TOS and compare with false
                    self.emitPopToRax(&buffer);
                    buffer.cmpRegImm64(.rax, Value.@"false".bits);

                    if (target_bc_ip < ip and bc_to_native[target_bc_ip] != 0) {
                        const target_native = bc_to_native[target_bc_ip];
                        const jump_end = buffer.currentPos() + 6;
                        const rel_offset: i32 = @intCast(@as(isize, @intCast(target_native)) - @as(isize, @intCast(jump_end)));
                        buffer.jeRel32(rel_offset);
                    } else {
                        buffer.emit8(0x0F);
                        buffer.emit8(0x84);
                        try forward_patches.append(self.allocator, .{ .patch_pos = buffer.currentPos(), .target_bc_ip = target_bc_ip });
                        buffer.emit32(0);
                    }
                },
                .jump_if_nil => {
                    const offset_hi = bc[ip];
                    const offset_lo = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast((@as(u16, offset_hi) << 8) | @as(u16, offset_lo));
                    const target_bc_ip: usize = @intCast(@as(isize, @intCast(ip)) + @as(isize, offset));

                    // Pop TOS and compare with nil
                    self.emitPopToRax(&buffer);
                    buffer.cmpRegImm64(.rax, Value.nil.bits);

                    if (target_bc_ip < ip and bc_to_native[target_bc_ip] != 0) {
                        const target_native = bc_to_native[target_bc_ip];
                        const jump_end = buffer.currentPos() + 6;
                        const rel_offset: i32 = @intCast(@as(isize, @intCast(target_native)) - @as(isize, @intCast(jump_end)));
                        buffer.jeRel32(rel_offset);
                    } else {
                        buffer.emit8(0x0F);
                        buffer.emit8(0x84);
                        try forward_patches.append(self.allocator, .{ .patch_pos = buffer.currentPos(), .target_bc_ip = target_bc_ip });
                        buffer.emit32(0);
                    }
                },
                .jump_if_not_nil => {
                    const offset_hi = bc[ip];
                    const offset_lo = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast((@as(u16, offset_hi) << 8) | @as(u16, offset_lo));
                    const target_bc_ip: usize = @intCast(@as(isize, @intCast(ip)) + @as(isize, offset));

                    // Pop TOS and compare with nil (jump if NOT equal)
                    self.emitPopToRax(&buffer);
                    buffer.cmpRegImm64(.rax, Value.nil.bits);

                    if (target_bc_ip < ip and bc_to_native[target_bc_ip] != 0) {
                        const target_native = bc_to_native[target_bc_ip];
                        const jump_end = buffer.currentPos() + 6;
                        const rel_offset: i32 = @intCast(@as(isize, @intCast(target_native)) - @as(isize, @intCast(jump_end)));
                        buffer.jneRel32(rel_offset);
                    } else {
                        buffer.emit8(0x0F);
                        buffer.emit8(0x85); // jne rel32
                        try forward_patches.append(self.allocator, .{ .patch_pos = buffer.currentPos(), .target_bc_ip = target_bc_ip });
                        buffer.emit32(0);
                    }
                },
                // Short jumps (0xB8-0xBF) - forward only, offset in opcode
                .short_jump_0, .short_jump_1, .short_jump_2, .short_jump_3,
                .short_jump_4, .short_jump_5, .short_jump_6, .short_jump_7 => {
                    const short_offset = Opcode.getShortJumpOffset(opcode);
                    const target_bc_ip = ip + short_offset;

                    // Short jumps are always forward
                    buffer.emit8(0xE9); // jmp rel32
                    try forward_patches.append(self.allocator, .{ .patch_pos = buffer.currentPos(), .target_bc_ip = target_bc_ip });
                    buffer.emit32(0);
                },
                // Block creation - call runtime helper
                .push_closure => {
                    if (ip + 4 <= bc.len) {
                        const block_num_args = bc[ip];
                        const block_num_temps = bc[ip + 1];
                        const size_hi: u16 = bc[ip + 2];
                        const size_lo: u16 = bc[ip + 3];
                        const block_size: usize = @intCast((size_hi << 8) | size_lo);
                        ip += 4;

                        // Block's bytecodes start at current ip
                        const block_start_ip = ip;
                        ip += block_size; // Skip block bytecodes

                        // Emit call to jit_runtime_push_closure
                        self.emitPushClosure(&buffer, block_num_args, block_num_temps, block_start_ip);
                    } else {
                        self.emitReturnNil(&buffer);
                        break;
                    }
                },
                else => {
                    // For unsupported opcodes, bail out to interpreter
                    // For now, just return nil
                    self.emitReturnNil(&buffer);
                    break;
                },
            }
        }

        // Record final bytecode position (for forward jumps to end)
        bc_to_native[ip] = buffer.currentPos();

        // Generate epilogue if we haven't returned yet
        self.emitEpilogue(&buffer);

        // Backpatch all forward jumps
        for (forward_patches.items) |patch| {
            const target_native = bc_to_native[patch.target_bc_ip];
            if (target_native != 0) {
                // Calculate relative offset from end of jump instruction
                const jump_end = patch.patch_pos + 4; // rel32 is 4 bytes
                const rel_offset: i32 = @intCast(@as(isize, @intCast(target_native)) - @as(isize, @intCast(jump_end)));
                buffer.patchRel32(patch.patch_pos, rel_offset);
            }
        }

        // Allocate executable memory and copy code
        const code_size = buffer.pos;
        const aligned_size = std.mem.alignForward(usize, code_size, PAGE_SIZE);

        const executable_mem = try allocateExecutableMemory(aligned_size);

        @memcpy(executable_mem[0..code_size], buffer.code[0..code_size]);

        // Create compiled code struct
        const compiled = try self.allocator.create(CompiledCode);
        compiled.* = CompiledCode{
            .code = executable_mem[0..aligned_size],
            .entry = @ptrCast(@alignCast(executable_mem.ptr)),
            .method = method,
            .allocator = self.allocator,
            .call_site_caches = call_site_caches,
        };

        return compiled;
    }

    // ========================================================================
    // Code Generation Helpers
    // ========================================================================

    /// Prologue: Set up frame, cache interpreter pointer and stack state
    /// Register allocation:
    ///   rbx = interpreter pointer (callee-saved)
    ///   r12 = Smalltalk stack pointer (sp) - cached, write back at safepoints
    ///   r13 = stack base address (interpreter.stack.ptr)
    fn emitPrologue(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // push rbp
        buf.pushReg(.rbp);
        // mov rbp, rsp
        buf.movRegReg(.rbp, .rsp);
        // push rbx (callee-saved, will hold interpreter ptr)
        buf.pushReg(.rbx);
        // push r12 (callee-saved, will hold sp)
        buf.pushReg(.r12);
        // push r13 (callee-saved, will hold stack base)
        buf.pushReg(.r13);
        // Cache interpreter pointer in rbx
        // Windows x64: first arg in RCX, System V AMD64: first arg in RDI
        if (builtin.os.tag == .windows) {
            buf.movRegReg(.rbx, .rcx);
        } else {
            buf.movRegReg(.rbx, .rdi);
        }
        // Cache sp in r12: mov r12, [rbx + SP_OFFSET]
        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
        // Cache stack base in r13: lea r13, [rbx + STACK_OFFSET]
        buf.movRegReg(.r13, .rbx);
        buf.addRegImm32(.r13, INTERP_STACK_OFFSET);
    }

    /// Epilogue: Write back sp and restore registers
    fn emitEpilogue(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Write back sp: mov [rbx + SP_OFFSET], r12
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);
        // Return nil if we fall through
        buf.movRegImm64(.rax, Value.nil.bits);
        // pop r13
        buf.popReg(.r13);
        // pop r12
        buf.popReg(.r12);
        // pop rbx
        buf.popReg(.rbx);
        // pop rbp
        buf.popReg(.rbp);
        // ret
        buf.ret();
    }

    /// Push nil onto Smalltalk stack (uses cached r12=sp, r13=stack base)
    fn emitPushNil(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load nil value
        buf.movRegImm64(.rax, Value.nil.bits);
        // Store at stack[r12]: mov [r13 + r12*8], rax
        // NOTE: r13 base with mod=00 requires disp32, so use mod=01 with disp8=0
        buf.rex(true, false, true, true); // REX.W + REX.X (r12) + REX.B (r13)
        buf.emit8(0x89); // mov [r13 + r12*8 + 0], rax
        buf.emit8(0x44); // ModRM: mod=01, [SIB + disp8]
        buf.emit8(0xE5); // SIB: scale=8, index=r12, base=r13
        buf.emit8(0x00); // disp8 = 0
        // Increment sp: r12 += 1
        buf.addRegImm32(.r12, 1);
    }

    /// Push an immediate Value onto Smalltalk stack (uses cached r12=sp, r13=stack base)
    fn emitPushImmediate(self: *JIT, buf: *CodeBuffer, value: Value) void {
        _ = self;
        // Load immediate value
        buf.movRegImm64(.rax, value.bits);
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true); // REX.W + REX.X (r12) + REX.B (r13)
        buf.emit8(0x89);
        buf.emit8(0x44); // ModRM: mod=01, [SIB + disp8]
        buf.emit8(0xE5); // SIB: scale=8, index=r12, base=r13
        buf.emit8(0x00); // disp8 = 0
        // Increment sp: r12 += 1
        buf.addRegImm32(.r12, 1);
    }

    /// Push true onto Smalltalk stack (uses cached r12=sp, r13=stack base)
    fn emitPushTrue(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        buf.movRegImm64(.rax, Value.@"true".bits);
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        buf.addRegImm32(.r12, 1);
    }

    /// Push false onto Smalltalk stack (uses cached r12=sp, r13=stack base)
    fn emitPushFalse(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        buf.movRegImm64(.rax, Value.@"false".bits);
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        buf.addRegImm32(.r12, 1);
    }

    /// Push self (receiver) onto stack (uses cached r12=sp, r13=stack base)
    fn emitPushSelf(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load receiver from interpreter
        buf.movRegMem(.rax, .rbx, INTERP_RECEIVER_OFFSET);
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        buf.addRegImm32(.r12, 1);
    }

    /// Push a literal value (uses cached r12=sp, r13=stack base)
    fn emitPushLiteral(self: *JIT, buf: *CodeBuffer, method: *CompiledMethod, index: u8) void {
        _ = self;
        const literals = method.getLiterals();
        if (index < literals.len) {
            const lit = literals[index];
            buf.movRegImm64(.rax, lit.bits);
            // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
            buf.rex(true, false, true, true);
            buf.emit8(0x89);
            buf.emit8(0x44); // mod=01, [SIB + disp8]
            buf.emit8(0xE5);
            buf.emit8(0x00); // disp8 = 0
            buf.addRegImm32(.r12, 1);
        }
    }

    /// Push a temporary variable (uses cached r12=sp, r13=stack base)
    fn emitPushTemporary(self: *JIT, buf: *CodeBuffer, index: u8) void {
        _ = self;
        // temp_base + 1 + index is the stack position
        // Load temp_base into rax
        buf.movRegMem(.rax, .rbx, INTERP_TEMP_BASE_OFFSET);
        // Add 1 + index
        buf.addRegImm32(.rax, 1 + @as(i32, index));
        // Load value from stack[temp_base + 1 + index]: mov rcx, [r13 + rax*8 + 0]
        buf.rex(true, false, false, true); // REX.W + REX.B (r13)
        buf.emit8(0x8B); // mov rcx, [r13 + rax*8 + 0]
        buf.emit8(0x4C); // mod=01, [SIB + disp8]
        buf.emit8(0xC5); // SIB: scale=8, index=rax, base=r13
        buf.emit8(0x00); // disp8 = 0
        // Push this value at stack[r12]: mov [r13 + r12*8 + 0], rcx
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x4C); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        buf.addRegImm32(.r12, 1);
    }

    /// Push a receiver instance variable (simplified - just pushes nil for now)
    fn emitPushReceiverVariable(self: *JIT, buf: *CodeBuffer, index: u8) void {
        _ = index;
        self.emitPushNil(buf);
    }

    /// Store top of stack to temporary (don't pop) (uses cached r12=sp, r13=stack base)
    fn emitStoreTemporary(self: *JIT, buf: *CodeBuffer, index: u8) void {
        _ = self;
        // Load TOS (at r12-1): mov rax, [r13 + (r12-1)*8]
        // First compute r12-1 in rcx
        buf.movRegReg(.rcx, .r12);
        buf.subRegImm32(.rcx, 1);
        // Load TOS value: mov rdx, [r13 + rcx*8 + 0]
        buf.rex(true, false, false, true);
        buf.emit8(0x8B);
        buf.emit8(0x54); // mod=01, [SIB + disp8]
        buf.emit8(0xCD); // SIB: scale=8, index=rcx, base=r13
        buf.emit8(0x00); // disp8 = 0
        // Calculate temp position: temp_base + 1 + index
        buf.movRegMem(.rax, .rbx, INTERP_TEMP_BASE_OFFSET);
        buf.addRegImm32(.rax, 1 + @as(i32, index));
        // Store to temp: mov [r13 + rax*8 + 0], rdx
        buf.rex(true, false, false, true);
        buf.emit8(0x89);
        buf.emit8(0x54); // mod=01, [SIB + disp8]
        buf.emit8(0xC5);
        buf.emit8(0x00); // disp8 = 0
    }

    /// Pop and store to temporary
    fn emitPopStoreTemporary(self: *JIT, buf: *CodeBuffer, index: u8) void {
        self.emitStoreTemporary(buf, index);
        self.emitPop(buf);
    }

    /// Pop top of stack (uses cached r12)
    fn emitPop(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Decrement sp: r12 -= 1
        buf.subRegImm32(.r12, 1);
    }

    /// Pop top of stack into rax (for comparisons) (uses cached r12=sp, r13=stack base)
    fn emitPopToRax(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Decrement sp
        buf.subRegImm32(.r12, 1);
        // Load stack[r12] into rax: mov rax, [r13 + r12*8 + 0]
        buf.rex(true, false, true, true);
        buf.emit8(0x8B);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
    }

    /// Duplicate top of stack (uses cached r12=sp, r13=stack base)
    fn emitDup(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load TOS (at r12-1) into rax
        buf.movRegReg(.rax, .r12);
        buf.subRegImm32(.rax, 1);
        // mov rcx, [r13 + rax*8 + 0]
        buf.rex(true, false, false, true);
        buf.emit8(0x8B);
        buf.emit8(0x4C); // mod=01, [SIB + disp8]
        buf.emit8(0xC5);
        buf.emit8(0x00); // disp8 = 0
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rcx
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x4C); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        buf.addRegImm32(.r12, 1);
    }

    /// Return top of stack (uses cached r12=sp, r13=stack base)
    fn emitReturnTop(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load TOS (at r12-1) into rax: mov rax, [r13 + (r12-1)*8 + 0]
        buf.movRegReg(.rax, .r12);
        buf.subRegImm32(.rax, 1);
        // mov rax, [r13 + rax*8 + 0]
        buf.rex(true, false, false, true);
        buf.emit8(0x8B);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xC5);
        buf.emit8(0x00); // disp8 = 0
        // Write back sp: mov [rbx + SP_OFFSET], r12
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);
        // Epilogue
        buf.popReg(.r13);
        buf.popReg(.r12);
        buf.popReg(.rbx);
        buf.popReg(.rbp);
        buf.ret();
    }

    /// Return self (uses cached r12 for writeback)
    fn emitReturnSelf(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load receiver into rax
        buf.movRegMem(.rax, .rbx, INTERP_RECEIVER_OFFSET);
        // Write back sp: mov [rbx + SP_OFFSET], r12
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);
        // Epilogue
        buf.popReg(.r13);
        buf.popReg(.r12);
        buf.popReg(.rbx);
        buf.popReg(.rbp);
        buf.ret();
    }

    /// Return nil (uses cached r12 for writeback)
    fn emitReturnNil(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        buf.movRegImm64(.rax, Value.nil.bits);
        // Write back sp: mov [rbx + SP_OFFSET], r12
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);
        buf.popReg(.r13);
        buf.popReg(.r12);
        buf.popReg(.rbx);
        buf.popReg(.rbp);
        buf.ret();
    }

    /// Emit a message send (uses cached r12=sp, r13=stack base)
    /// This generates code to call jit_runtime_send_with_cache and push the result
    fn emitSend(
        self: *JIT,
        buf: *CodeBuffer,
        selector_idx: u8,
        num_args: u8,
        bytecode_offset: usize,
        cache: *CallSiteCache,
        is_super: bool,
    ) void {
        _ = self;
        const send_fn_ptr = @intFromPtr(&jit_runtime_send_with_cache);
        const super_flag: u64 = if (is_super) 1 else 0;

        // SAFEPOINT: Write back r12 to interpreter.sp before call
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        if (builtin.os.tag == .windows) {
            // Windows x64 calling convention:
            // rcx = arg1, rdx = arg2, r8 = arg3, r9 = arg4
            // args 5+ go on stack, plus 32 bytes shadow space before call
            // Note: 56 = 32 shadow + 16 args + 8 alignment (prologue leaves RSP 8-aligned)

            // Allocate stack space: 32 bytes shadow + 16 bytes for 2 stack args + 8 alignment
            buf.subRegImm32(.rsp, 56);

            // Stack args start at [rsp+32] after shadow space
            // Push arg6 (is_super) to stack at [rsp+40]
            buf.movRegImm64(.rax, super_flag);
            buf.rex(true, false, false, false);
            buf.emit8(0x89);
            buf.emit8(0x44);
            buf.emit8(0x24);
            buf.emit8(40);

            // Push arg5 (bytecode_offset) to stack at [rsp+32]
            buf.movRegImm64(.rax, @as(u64, bytecode_offset));
            buf.rex(true, false, false, false);
            buf.emit8(0x89);
            buf.emit8(0x44);
            buf.emit8(0x24);
            buf.emit8(32);

            // Move arg4 (num_args) to r9
            buf.movRegImm64(.r9, @as(u64, num_args));

            // Move arg3 (selector_index) to r8
            buf.movRegImm64(.r8, @as(u64, selector_idx));

            // Move arg2 (callsite cache pointer) to rdx
            buf.movRegImm64(.rdx, @intFromPtr(cache));

            // Move arg1 (interpreter pointer) to rcx
            buf.movRegReg(.rcx, .rbx);

            // Load function address and call
            buf.movRegImm64(.r10, send_fn_ptr);
            buf.callReg(.r10);

            // Restore stack
            buf.addRegImm32(.rsp, 56);
        } else {
            // System V AMD64 calling convention:
            // rdi = arg1, rsi = arg2, rdx = arg3, rcx = arg4, r8 = arg5, r9 = arg6

            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.rsi, @intFromPtr(cache));
            buf.movRegImm64(.rdx, @as(u64, selector_idx));
            buf.movRegImm64(.rcx, @as(u64, num_args));
            buf.movRegImm64(.r8, @as(u64, bytecode_offset));
            buf.movRegImm64(.r9, super_flag);
            buf.movRegImm64(.r10, send_fn_ptr);
            buf.callReg(.r10);
        }

        // Reload r12 from interpreter.sp (runtime may have modified it)
        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);

        // Result is in rax - push it onto Smalltalk stack using cached regs
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        // Increment sp: r12 += 1
        buf.addRegImm32(.r12, 1);
    }

    /// Emit a super send
    fn emitSuperSend(
        self: *JIT,
        buf: *CodeBuffer,
        selector_idx: u8,
        num_args: u8,
        bytecode_offset: usize,
        cache: *CallSiteCache,
    ) void {
        self.emitSend(buf, selector_idx, num_args, bytecode_offset, cache, true);
    }

    /// Emit an inline send with fast-path cache check for SmallIntegers
    /// This checks the receiver tag inline and only calls runtime on miss
    fn emitSendInline(
        self: *JIT,
        buf: *CodeBuffer,
        selector_idx: u8,
        num_args: u8,
        bytecode_offset: usize,
        cache: *CallSiteCache,
        is_super: bool,
    ) void {
        _ = self;
        const send_fn_ptr = @intFromPtr(&jit_runtime_send_with_cache);
        const cached_send_fn_ptr = @intFromPtr(&jit_runtime_cached_send);
        const super_flag: u64 = if (is_super) 1 else 0;
        const cache_ptr = @intFromPtr(cache);
        const smallint_class_addr = getSmallIntClassBitsAddr();

        // SAFEPOINT: Write back r12 to interpreter.sp before any calls
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        // === FAST PATH: Inline SmallInt cache check ===

        // Load receiver from stack: rax = stack[sp - num_args - 1]
        const recv_disp: i8 = -@as(i8, @intCast((@as(u16, num_args) + 1) * 8));
        buf.rex(true, false, true, true);
        buf.emit8(0x8B);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(@bitCast(recv_disp));

        // Check if receiver is SmallInt: (rax & 7) == 1
        buf.movRegReg(.rcx, .rax);
        buf.andRegImm8(.rcx, 7);
        buf.cmpRegImm8(.rcx, 1);

        // jne slow_path (not a SmallInt)
        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne_not_smallint = buf.currentPos();
        buf.emit32(0);

        // Receiver is SmallInt - compare cache.expected_class with SmallInteger class
        // Load SmallInteger class bits from global
        buf.movRegImm64(.rax, smallint_class_addr);
        buf.rex(true, false, false, false);
        buf.emit8(0x8B);
        buf.emit8(0x00); // mov rax, [rax]

        // Load cache.expected_class into rcx
        buf.movRegImm64(.rcx, cache_ptr + CACHE_EXPECTED_CLASS_OFFSET);
        buf.rex(true, false, false, false);
        buf.emit8(0x8B);
        buf.emit8(0x09);

        // cmp rax, rcx
        buf.cmpRegReg(.rax, .rcx);

        // jne slow_path
        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne_class = buf.currentPos();
        buf.emit32(0);

        // Check cache.version with interp.cache_version
        buf.movRegImm64(.rcx, cache_ptr + CACHE_VERSION_OFFSET);
        buf.emit8(0x8B);
        buf.emit8(0x11); // mov edx, [rcx]

        buf.rex(false, false, false, false);
        buf.emit8(0x8B);
        buf.emit8(0x83);
        buf.emit32(@as(u32, INTERP_CACHE_VERSION_OFFSET)); // mov eax, [rbx + offset]

        buf.emit8(0x39);
        buf.emit8(0xC2); // cmp edx, eax

        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne_version = buf.currentPos();
        buf.emit32(0);

        // Check cache.cached_jit_code != null
        buf.movRegImm64(.rcx, cache_ptr + CACHE_JIT_CODE_OFFSET);
        buf.rex(true, false, false, false);
        buf.emit8(0x8B);
        buf.emit8(0x09);

        buf.rex(true, false, false, false);
        buf.emit8(0x85);
        buf.emit8(0xC9); // test rcx, rcx

        buf.emit8(0x0F);
        buf.emit8(0x84);
        const patch_je_null = buf.currentPos();
        buf.emit32(0);

        // === CACHE HIT: Call cached_send helper ===
        if (builtin.os.tag == .windows) {
            buf.subRegImm32(.rsp, 40);
            buf.movRegImm64(.r8, @as(u64, num_args));
            buf.movRegImm64(.rdx, cache_ptr);
            buf.movRegReg(.rcx, .rbx);
            buf.movRegImm64(.r10, cached_send_fn_ptr);
            buf.callReg(.r10);
            buf.addRegImm32(.rsp, 40);
        } else {
            buf.movRegImm64(.rdx, @as(u64, num_args));
            buf.movRegImm64(.rsi, cache_ptr);
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.r10, cached_send_fn_ptr);
            buf.callReg(.r10);
        }

        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(0x00);
        buf.addRegImm32(.r12, 1);

        // Jump to end
        buf.emit8(0xE9);
        const patch_jmp_end = buf.currentPos();
        buf.emit32(0);

        // === SLOW PATH ===
        const slow_path_pos = buf.currentPos();
        buf.patchRel32(patch_jne_not_smallint, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne_not_smallint)) - 4));
        buf.patchRel32(patch_jne_class, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne_class)) - 4));
        buf.patchRel32(patch_jne_version, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne_version)) - 4));
        buf.patchRel32(patch_je_null, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_je_null)) - 4));

        // Call full runtime send
        if (builtin.os.tag == .windows) {
            buf.subRegImm32(.rsp, 56);
            buf.movRegImm64(.rax, super_flag);
            buf.rex(true, false, false, false);
            buf.emit8(0x89);
            buf.emit8(0x44);
            buf.emit8(0x24);
            buf.emit8(40);
            buf.movRegImm64(.rax, @as(u64, bytecode_offset));
            buf.rex(true, false, false, false);
            buf.emit8(0x89);
            buf.emit8(0x44);
            buf.emit8(0x24);
            buf.emit8(32);
            buf.movRegImm64(.r9, @as(u64, num_args));
            buf.movRegImm64(.r8, @as(u64, selector_idx));
            buf.movRegImm64(.rdx, cache_ptr);
            buf.movRegReg(.rcx, .rbx);
            buf.movRegImm64(.r10, send_fn_ptr);
            buf.callReg(.r10);
            buf.addRegImm32(.rsp, 56);
        } else {
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.rsi, cache_ptr);
            buf.movRegImm64(.rdx, @as(u64, selector_idx));
            buf.movRegImm64(.rcx, @as(u64, num_args));
            buf.movRegImm64(.r8, @as(u64, bytecode_offset));
            buf.movRegImm64(.r9, super_flag);
            buf.movRegImm64(.r10, send_fn_ptr);
            buf.callReg(.r10);
        }

        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(0x00);
        buf.addRegImm32(.r12, 1);

        // === END ===
        const end_pos = buf.currentPos();
        buf.patchRel32(patch_jmp_end, @intCast(@as(isize, @intCast(end_pos)) - @as(isize, @intCast(patch_jmp_end)) - 4));
    }

    /// Emit code to create a BlockClosure (uses cached r12=sp, r13=stack base)
    fn emitPushClosure(
        self: *JIT,
        buf: *CodeBuffer,
        block_num_args: u8,
        block_num_temps: u8,
        block_start_ip: usize,
    ) void {
        _ = self;
        const push_closure_fn_ptr = @intFromPtr(&jit_runtime_push_closure);

        // SAFEPOINT: Write back r12 to interpreter.sp before call
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        if (builtin.os.tag == .windows) {
            // Windows x64: rcx, rdx, r8, r9
            // Allocate 40 bytes: 32 shadow + 8 alignment (prologue leaves RSP 8-aligned)
            buf.subRegImm32(.rsp, 40);

            // arg4 (block_start_ip) to r9
            buf.movRegImm64(.r9, @as(u64, block_start_ip));

            // arg3 (block_num_temps) to r8
            buf.movRegImm64(.r8, @as(u64, block_num_temps));

            // arg2 (block_num_args) to rdx
            buf.movRegImm64(.rdx, @as(u64, block_num_args));

            // arg1 (interpreter pointer) to rcx
            buf.movRegReg(.rcx, .rbx);

            // Load function address and call
            buf.movRegImm64(.r10, push_closure_fn_ptr);
            buf.callReg(.r10);

            // Restore stack
            buf.addRegImm32(.rsp, 40);
        } else {
            // System V AMD64: rdi, rsi, rdx, rcx
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.rsi, @as(u64, block_num_args));
            buf.movRegImm64(.rdx, @as(u64, block_num_temps));
            buf.movRegImm64(.rcx, @as(u64, block_start_ip));
            buf.movRegImm64(.r10, push_closure_fn_ptr);
            buf.callReg(.r10);
        }

        // Reload r12 from interpreter.sp (runtime may have modified it)
        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);

        // Result (closure Value) is in rax - push onto stack
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        // Increment sp: r12 += 1
        buf.addRegImm32(.r12, 1);
    }

    /// Emit code for specialized binary sends (uses cached r12=sp, r13=stack base)
    fn emitSpecialBinary(
        self: *JIT,
        buf: *CodeBuffer,
        op: SpecialBinaryOp,
    ) void {
        _ = self;
        const special_binary_fn_ptr = @intFromPtr(&jit_runtime_special_binary);

        // SAFEPOINT: Write back r12 to interpreter.sp before call
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        if (builtin.os.tag == .windows) {
            // Windows x64: rcx, rdx
            // Allocate 40 bytes: 32 shadow + 8 alignment (prologue leaves RSP 8-aligned)
            buf.subRegImm32(.rsp, 40);

            // arg2 (op) to rdx
            buf.movRegImm64(.rdx, @as(u64, @intFromEnum(op)));

            // arg1 (interpreter pointer) to rcx
            buf.movRegReg(.rcx, .rbx);

            // Load function address and call
            buf.movRegImm64(.r10, special_binary_fn_ptr);
            buf.callReg(.r10);

            // Restore stack
            buf.addRegImm32(.rsp, 40);
        } else {
            // System V AMD64: rdi, rsi
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.rsi, @as(u64, @intFromEnum(op)));
            buf.movRegImm64(.r10, special_binary_fn_ptr);
            buf.callReg(.r10);
        }

        // Reload r12 from interpreter.sp (runtime may have modified it)
        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);

        // Result is in rax - push onto stack
        // Store at stack[r12]: mov [r13 + r12*8 + 0], rax
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44); // mod=01, [SIB + disp8]
        buf.emit8(0xE5);
        buf.emit8(0x00); // disp8 = 0
        // Increment sp: r12 += 1
        buf.addRegImm32(.r12, 1);
    }

    /// Emit inline code for SmallInteger comparisons with fallback to runtime
    /// This is the fast path for Phase 2.2
    fn emitComparisonInline(
        self: *JIT,
        buf: *CodeBuffer,
        op: SpecialBinaryOp,
    ) void {
        _ = self;
        const special_binary_fn_ptr = @intFromPtr(&jit_runtime_special_binary);

        // SmallInteger tag constants
        const SMALLINT_TAG: u8 = 1;
        const TAG_MASK: u8 = 7;
        const TRUE_BITS: u64 = Value.@"true".bits;
        const FALSE_BITS: u64 = Value.@"false".bits;

        // Load arg (TOS) into rax: mov rax, [r13 + r12*8 - 8]
        buf.rex(true, false, true, true); // REX.W + REX.X (r12) + REX.B (r13)
        buf.emit8(0x8B); // mov
        buf.emit8(0x44); // ModRM: mod=01, reg=rax, rm=SIB
        buf.emit8(0xE5); // SIB: scale=8, index=r12, base=r13
        buf.emit8(0xF8); // disp8 = -8

        // Load receiver (TOS-1) into rcx: mov rcx, [r13 + r12*8 - 16]
        buf.rex(true, false, true, true);
        buf.emit8(0x8B);
        buf.emit8(0x4C); // ModRM: mod=01, reg=rcx, rm=SIB
        buf.emit8(0xE5); // SIB
        buf.emit8(0xF0); // disp8 = -16

        // Check arg is SmallInt: mov rdx, rax; and rdx, 7; cmp rdx, 1; jne slow_path
        buf.movRegReg(.rdx, .rax);
        buf.andRegImm8(.rdx, TAG_MASK);
        buf.cmpRegImm8(.rdx, SMALLINT_TAG);
        // jne slow_path (will patch later)
        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne1 = buf.currentPos();
        buf.emit32(0); // placeholder

        // Check receiver is SmallInt: mov rdx, rcx; and rdx, 7; cmp rdx, 1; jne slow_path
        buf.movRegReg(.rdx, .rcx);
        buf.andRegImm8(.rdx, TAG_MASK);
        buf.cmpRegImm8(.rdx, SMALLINT_TAG);
        // jne slow_path
        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne2 = buf.currentPos();
        buf.emit32(0); // placeholder

        // Both are SmallInts - compare directly (signed comparison works on tagged values)
        // cmp rcx, rax (receiver vs arg)
        buf.cmpRegReg(.rcx, .rax);

        // Set result based on comparison type
        // Strategy: load false, then conditionally jump over the "load true" based on condition
        buf.movRegImm64(.rax, FALSE_BITS);

        // Emit conditional jump to skip loading true (jump if condition NOT met)
        // For <= : if rcx <= rax, result is true, so jg (jump if greater) skips true
        // For <  : if rcx < rax, result is true, so jge skips true
        // For >= : if rcx >= rax, result is true, so jl skips true
        // For >  : if rcx > rax, result is true, so jle skips true
        // For =  : if rcx == rax, result is true, so jne skips true
        // For ~= : if rcx != rax, result is true, so je skips true
        const skip_true_offset: i32 = 10; // movabs rax, imm64 = 10 bytes
        switch (op) {
            .less_or_equal => buf.jgRel32(skip_true_offset),
            .less_than => buf.jgeRel32(skip_true_offset),
            .greater_or_equal => buf.jlRel32(skip_true_offset),
            .greater_than => buf.jleRel32(skip_true_offset),
            .equal => buf.jneRel32(skip_true_offset),
            .not_equal => buf.jeRel32(skip_true_offset),
            else => unreachable,
        }

        // Load true (condition was met)
        buf.movRegImm64(.rax, TRUE_BITS);

        // Store result and adjust sp: result goes at stack[sp-2], sp becomes sp-1
        // mov [r13 + r12*8 - 16], rax (store at receiver position)
        buf.rex(true, false, true, true);
        buf.emit8(0x89); // mov r/m64, r64
        buf.emit8(0x44); // ModRM: mod=01, reg=rax, rm=SIB
        buf.emit8(0xE5); // SIB
        buf.emit8(0xF0); // disp8 = -16

        // sub r12, 1 (sp--)
        buf.subRegImm32(.r12, 1);

        // jmp to end
        buf.emit8(0xE9);
        const patch_jmp_end = buf.currentPos();
        buf.emit32(0); // placeholder

        // slow_path: patch the jne jumps
        const slow_path_pos = buf.currentPos();
        const rel_to_slow1: i32 = @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne1)) - 4);
        const rel_to_slow2: i32 = @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne2)) - 4);
        buf.patchRel32(patch_jne1, rel_to_slow1);
        buf.patchRel32(patch_jne2, rel_to_slow2);

        // Slow path: call jit_runtime_special_binary
        // SAFEPOINT: Write back r12 to interpreter.sp before call
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        if (builtin.os.tag == .windows) {
            buf.subRegImm32(.rsp, 40);
            buf.movRegImm64(.rdx, @as(u64, @intFromEnum(op)));
            buf.movRegReg(.rcx, .rbx);
            buf.movRegImm64(.r10, special_binary_fn_ptr);
            buf.callReg(.r10);
            buf.addRegImm32(.rsp, 40);
        } else {
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.rsi, @as(u64, @intFromEnum(op)));
            buf.movRegImm64(.r10, special_binary_fn_ptr);
            buf.callReg(.r10);
        }

        // Reload r12 and push result
        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(0x00);
        buf.addRegImm32(.r12, 1);

        // end: patch the jmp
        const end_pos = buf.currentPos();
        const rel_to_end: i32 = @intCast(@as(isize, @intCast(end_pos)) - @as(isize, @intCast(patch_jmp_end)) - 4);
        buf.patchRel32(patch_jmp_end, rel_to_end);
    }

    /// Emit inline code for SmallInteger arithmetic with overflow check
    fn emitArithmeticInline(
        self: *JIT,
        buf: *CodeBuffer,
        op: SpecialBinaryOp,
    ) void {
        _ = self;
        const special_binary_fn_ptr = @intFromPtr(&jit_runtime_special_binary);

        const SMALLINT_TAG: u8 = 1;
        const TAG_MASK: u8 = 7;

        // Load arg (TOS) into rax
        buf.rex(true, false, true, true);
        buf.emit8(0x8B);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(0xF8); // disp8 = -8

        // Load receiver (TOS-1) into rcx
        buf.rex(true, false, true, true);
        buf.emit8(0x8B);
        buf.emit8(0x4C);
        buf.emit8(0xE5);
        buf.emit8(0xF0); // disp8 = -16

        // Check arg is SmallInt
        buf.movRegReg(.rdx, .rax);
        buf.andRegImm8(.rdx, TAG_MASK);
        buf.cmpRegImm8(.rdx, SMALLINT_TAG);
        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne1 = buf.currentPos();
        buf.emit32(0);

        // Check receiver is SmallInt
        buf.movRegReg(.rdx, .rcx);
        buf.andRegImm8(.rdx, TAG_MASK);
        buf.cmpRegImm8(.rdx, SMALLINT_TAG);
        buf.emit8(0x0F);
        buf.emit8(0x85);
        const patch_jne2 = buf.currentPos();
        buf.emit32(0);

        // Both are SmallInts - perform arithmetic
        // For tagged SmallInts: (a << 3 | 1) + (b << 3 | 1) = (a+b) << 3 + 2
        // We need to subtract 1 to get proper tag: result - 1 = (a+b) << 3 + 1
        // For subtraction: (a << 3 | 1) - (b << 3 | 1) = (a-b) << 3, need to add 1

        switch (op) {
            .add => {
                // add rcx, rax (rcx = receiver + arg)
                buf.rex(true, false, false, false);
                buf.emit8(0x01); // ADD r/m64, r64
                buf.emit8(0xC1); // ModRM: mod=11, reg=rax, rm=rcx
                // jo slow_path (jump on overflow)
                buf.emit8(0x0F);
                buf.emit8(0x80);
                const patch_jo = buf.currentPos();
                buf.emit32(0);
                // sub rcx, 1 (fix tag: result has tag 2, need tag 1)
                buf.subRegImm32(.rcx, 1);
                buf.movRegReg(.rax, .rcx);

                // Store result
                buf.rex(true, false, true, true);
                buf.emit8(0x89);
                buf.emit8(0x44);
                buf.emit8(0xE5);
                buf.emit8(0xF0);
                buf.subRegImm32(.r12, 1);

                // jmp end
                buf.emit8(0xE9);
                const patch_jmp_end = buf.currentPos();
                buf.emit32(0);

                // slow_path
                const slow_path_pos = buf.currentPos();
                buf.patchRel32(patch_jne1, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne1)) - 4));
                buf.patchRel32(patch_jne2, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne2)) - 4));
                buf.patchRel32(patch_jo, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jo)) - 4));

                // Call runtime
                buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);
                if (builtin.os.tag == .windows) {
                    buf.subRegImm32(.rsp, 40);
                    buf.movRegImm64(.rdx, @as(u64, @intFromEnum(op)));
                    buf.movRegReg(.rcx, .rbx);
                    buf.movRegImm64(.r10, special_binary_fn_ptr);
                    buf.callReg(.r10);
                    buf.addRegImm32(.rsp, 40);
                } else {
                    buf.movRegReg(.rdi, .rbx);
                    buf.movRegImm64(.rsi, @as(u64, @intFromEnum(op)));
                    buf.movRegImm64(.r10, special_binary_fn_ptr);
                    buf.callReg(.r10);
                }
                buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
                buf.rex(true, false, true, true);
                buf.emit8(0x89);
                buf.emit8(0x44);
                buf.emit8(0xE5);
                buf.emit8(0x00);
                buf.addRegImm32(.r12, 1);

                // end
                const end_pos = buf.currentPos();
                buf.patchRel32(patch_jmp_end, @intCast(@as(isize, @intCast(end_pos)) - @as(isize, @intCast(patch_jmp_end)) - 4));
            },
            .subtract => {
                // sub rcx, rax (rcx = receiver - arg)
                buf.rex(true, false, false, false);
                buf.emit8(0x29); // SUB r/m64, r64
                buf.emit8(0xC1); // ModRM: mod=11, reg=rax, rm=rcx
                // jo slow_path
                buf.emit8(0x0F);
                buf.emit8(0x80);
                const patch_jo = buf.currentPos();
                buf.emit32(0);
                // add rcx, 1 (fix tag: result has tag 0, need tag 1)
                buf.addRegImm32(.rcx, 1);
                buf.movRegReg(.rax, .rcx);

                // Store result
                buf.rex(true, false, true, true);
                buf.emit8(0x89);
                buf.emit8(0x44);
                buf.emit8(0xE5);
                buf.emit8(0xF0);
                buf.subRegImm32(.r12, 1);

                // jmp end
                buf.emit8(0xE9);
                const patch_jmp_end = buf.currentPos();
                buf.emit32(0);

                // slow_path
                const slow_path_pos = buf.currentPos();
                buf.patchRel32(patch_jne1, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne1)) - 4));
                buf.patchRel32(patch_jne2, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jne2)) - 4));
                buf.patchRel32(patch_jo, @intCast(@as(isize, @intCast(slow_path_pos)) - @as(isize, @intCast(patch_jo)) - 4));

                // Call runtime
                buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);
                if (builtin.os.tag == .windows) {
                    buf.subRegImm32(.rsp, 40);
                    buf.movRegImm64(.rdx, @as(u64, @intFromEnum(op)));
                    buf.movRegReg(.rcx, .rbx);
                    buf.movRegImm64(.r10, special_binary_fn_ptr);
                    buf.callReg(.r10);
                    buf.addRegImm32(.rsp, 40);
                } else {
                    buf.movRegReg(.rdi, .rbx);
                    buf.movRegImm64(.rsi, @as(u64, @intFromEnum(op)));
                    buf.movRegImm64(.r10, special_binary_fn_ptr);
                    buf.callReg(.r10);
                }
                buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
                buf.rex(true, false, true, true);
                buf.emit8(0x89);
                buf.emit8(0x44);
                buf.emit8(0xE5);
                buf.emit8(0x00);
                buf.addRegImm32(.r12, 1);

                // end
                const end_pos = buf.currentPos();
                buf.patchRel32(patch_jmp_end, @intCast(@as(isize, @intCast(end_pos)) - @as(isize, @intCast(patch_jmp_end)) - 4));
            },
            else => unreachable,
        }
    }

    /// Emit inline code for at: with fast path for Arrays
    fn emitAtInline(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        const runtime_fn_ptr = @intFromPtr(&jit_runtime_at);

        // For now, always use runtime helper until inline code is debugged
        // SAFEPOINT: Write back r12 to interpreter.sp before call
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        // Call runtime helper: jit_runtime_at(interp)
        if (builtin.os.tag == .windows) {
            buf.subRegImm32(.rsp, 40); // shadow space + alignment
            buf.movRegReg(.rcx, .rbx); // interp pointer
            buf.movRegImm64(.r10, runtime_fn_ptr);
            buf.callReg(.r10);
            buf.addRegImm32(.rsp, 40);
        } else {
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.r10, runtime_fn_ptr);
            buf.callReg(.r10);
        }

        // Reload sp and push result
        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(0x00);
        buf.addRegImm32(.r12, 1);
    }

    /// Emit inline code for at:put: with fast path for Arrays
    fn emitAtPutInline(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        const runtime_fn_ptr = @intFromPtr(&jit_runtime_at_put);

        // For now, always use runtime helper until inline code is debugged
        // SAFEPOINT: Write back r12 to interpreter.sp before call
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .r12);

        if (builtin.os.tag == .windows) {
            buf.subRegImm32(.rsp, 40);
            buf.movRegReg(.rcx, .rbx);
            buf.movRegImm64(.r10, runtime_fn_ptr);
            buf.callReg(.r10);
            buf.addRegImm32(.rsp, 40);
        } else {
            buf.movRegReg(.rdi, .rbx);
            buf.movRegImm64(.r10, runtime_fn_ptr);
            buf.callReg(.r10);
        }

        buf.movRegMem(.r12, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, true, true);
        buf.emit8(0x89);
        buf.emit8(0x44);
        buf.emit8(0xE5);
        buf.emit8(0x00);
        buf.addRegImm32(.r12, 1);
    }

    /// Emit specialized binary operation - dispatches to inline or runtime version
    fn emitSpecialBinaryOp(
        self: *JIT,
        buf: *CodeBuffer,
        op: SpecialBinaryOp,
    ) void {
        switch (op) {
            // Comparisons - use inline fast path
            .less_than, .greater_than, .less_or_equal, .greater_or_equal, .equal, .not_equal => {
                self.emitComparisonInline(buf, op);
            },
            // Arithmetic - use inline fast path with overflow check
            .add, .subtract => {
                self.emitArithmeticInline(buf, op);
            },
            // Multiply/divide - use runtime helper (more complex overflow handling)
            .multiply, .divide => {
                self.emitSpecialBinary(buf, op);
            },
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "CodeBuffer basic operations" {
    var buf = try CodeBuffer.init(std.testing.allocator, 1024);
    defer buf.deinit();

    buf.emit8(0x90); // nop
    buf.emit8(0xC3); // ret

    try std.testing.expectEqual(@as(usize, 2), buf.pos);
    try std.testing.expectEqual(@as(u8, 0x90), buf.code[0]);
    try std.testing.expectEqual(@as(u8, 0xC3), buf.code[1]);
}

test "CodeBuffer register instructions" {
    var buf = try CodeBuffer.init(std.testing.allocator, 1024);
    defer buf.deinit();

    buf.pushReg(.rbp);
    buf.movRegReg(.rbp, .rsp);
    buf.popReg(.rbp);
    buf.ret();

    // push rbp = 0x55
    // mov rbp, rsp = 48 89 e5
    // pop rbp = 0x5d
    // ret = 0xc3
    try std.testing.expectEqual(@as(u8, 0x55), buf.code[0]);
}
