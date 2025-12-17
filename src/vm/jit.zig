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

    // Call interpreter's sendMessage to set up the call
    interp.sendMessage(selector_index, num_args, is_super, bytecode_offset) catch {
        return Value.nil;
    };

    // If sendMessage returned without pushing a context (fast path hit),
    // the result is already on the stack
    if (interp.context_ptr == saved_context_ptr) {
        // Fast path completed - pop and return result
        if (interp.sp > saved_sp) {
            interp.sp -= 1;
            return interp.stack[interp.sp];
        }
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
        return Value.nil;
    };

    interp.primitive_block_depth -= 1;
    return result;
}

fn runCachedJitMethod(
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
    // Temporary debug logging to trace early JIT sends
    const debug_max: usize = 32;
    {
        var static_count: usize = 0;
        if (static_count < debug_max) {
            static_count += 1;
            const recv_val = if (interp.sp > 0 and interp.sp > num_args)
                interp.stack[interp.sp - @as(usize, num_args) - 1]
            else
                Value.nil;
            std.debug.print("jit_send[{d}] sel_idx={d} args={d} bc_ofs={d} is_super={any} recv_tag=0x{x} cache_ver={d} expected=0x{x}\n",
                .{ static_count, selector_index, num_args, bytecode_offset, is_super, recv_val.bits & Value.TAG_MASK, cache.version, cache.expected_class.bits });
        }
    }

    const saved_sp = interp.sp;
    if (saved_sp == 0 or saved_sp <= num_args) return Value.nil;

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
        // Temporaries beyond arguments currently require more frame setup; skip for safety
        if (method.header.num_temps > 0) return false;

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

                // Sends - OK (we have runtime send support)
                .send => ip += 2,
                .super_send => ip += 2,

                // Jumps - now supported with backpatching
                .jump, .jump_if_true, .jump_if_false, .jump_if_nil, .jump_if_not_nil => {
                    ip += 2; // skip 16-bit offset
                },

                // Short jumps (offset in opcode, forward only)
                .short_jump_0, .short_jump_1, .short_jump_2, .short_jump_3,
                .short_jump_4, .short_jump_5, .short_jump_6, .short_jump_7 => {},

                // Block creation - NOT eligible
                .push_closure => return false,

                // Non-local return - NOT eligible (complex)
                .block_return => return false,

                // Extended opcodes - NOT eligible for now
                .extended_push, .extended_store, .extended_send => return false,

                // Everything else - NOT eligible
                else => return false,
            }
        }

        return true;
    }

    /// Compile a method to native code
    pub fn compile(self: *JIT, method: *CompiledMethod) !*CompiledCode {
        // Debug trace to see which methods are compiled
        std.debug.print("JIT compile method@0x{x} num_temps={d} num_literals={d} bytecode_size={d}\n",
            .{ @intFromPtr(method), method.header.num_temps, method.header.num_literals, method.header.bytecode_size });

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
                // Jump bytecodes with 16-bit signed offset
                .jump => {
                    const offset_lo = bc[ip];
                    const offset_hi = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast(@as(u16, offset_lo) | (@as(u16, offset_hi) << 8));
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
                    const offset_lo = bc[ip];
                    const offset_hi = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast(@as(u16, offset_lo) | (@as(u16, offset_hi) << 8));
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
                    const offset_lo = bc[ip];
                    const offset_hi = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast(@as(u16, offset_lo) | (@as(u16, offset_hi) << 8));
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
                    const offset_lo = bc[ip];
                    const offset_hi = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast(@as(u16, offset_lo) | (@as(u16, offset_hi) << 8));
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
                    const offset_lo = bc[ip];
                    const offset_hi = bc[ip + 1];
                    ip += 2;
                    const offset: i16 = @bitCast(@as(u16, offset_lo) | (@as(u16, offset_hi) << 8));
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

    /// Prologue: Set up frame, cache interpreter pointer
    fn emitPrologue(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // push rbp
        buf.pushReg(.rbp);
        // mov rbp, rsp
        buf.movRegReg(.rbp, .rsp);
        // push rbx (callee-saved, will hold interpreter ptr)
        buf.pushReg(.rbx);
        // push r12 (callee-saved)
        buf.pushReg(.r12);
        // push r13 (callee-saved)
        buf.pushReg(.r13);
        // mov rbx, rdi (cache interpreter pointer in rbx)
        buf.movRegReg(.rbx, .rdi);
    }

    /// Epilogue: Restore and return
    fn emitEpilogue(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
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

    /// Push nil onto Smalltalk stack
    fn emitPushNil(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load sp from interpreter: mov rax, [rbx + SP_OFFSET]
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        // Load stack base address: lea rcx, [rbx + STACK_OFFSET]
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // Store nil at stack[sp]: mov [rcx + rax*8], nil
        buf.movRegImm64(.rdx, Value.nil.bits);
        // mov [rcx + rax*8], rdx - need SIB encoding
        // Simplified: calculate address manually
        // rax = sp (index), need stack[sp] = stack_base + sp * 8
        buf.rex(true, false, false, false);
        buf.emit8(0x89); // mov [rcx + rax*8], rdx
        buf.emit8(0x14); // ModRM: [SIB]
        buf.emit8(0xC1); // SIB: scale=8, index=rax, base=rcx
        // Increment sp: inc qword [rbx + SP_OFFSET]
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Push true onto Smalltalk stack
    fn emitPushTrue(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        buf.movRegImm64(.rdx, Value.@"true".bits);
        buf.rex(true, false, false, false);
        buf.emit8(0x89);
        buf.emit8(0x14);
        buf.emit8(0xC1);
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Push false onto Smalltalk stack
    fn emitPushFalse(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        buf.movRegImm64(.rdx, Value.@"false".bits);
        buf.rex(true, false, false, false);
        buf.emit8(0x89);
        buf.emit8(0x14);
        buf.emit8(0xC1);
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Push self (receiver) onto stack
    fn emitPushSelf(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load receiver from interpreter
        buf.movRegMem(.rdx, .rbx, INTERP_RECEIVER_OFFSET);
        // Load sp
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        // Stack base
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // Store receiver at stack[sp]
        buf.rex(true, false, false, false);
        buf.emit8(0x89);
        buf.emit8(0x14);
        buf.emit8(0xC1);
        // Increment sp
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Push a literal value
    fn emitPushLiteral(self: *JIT, buf: *CodeBuffer, method: *CompiledMethod, index: u8) void {
        _ = self;
        const literals = method.getLiterals();
        if (index < literals.len) {
            const lit = literals[index];
            // Load sp
            buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
            // Stack base
            buf.movRegReg(.rcx, .rbx);
            buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
            // Load literal value
            buf.movRegImm64(.rdx, lit.bits);
            // Store at stack[sp]
            buf.rex(true, false, false, false);
            buf.emit8(0x89);
            buf.emit8(0x14);
            buf.emit8(0xC1);
            // Increment sp
            buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
            buf.addRegImm32(.rax, 1);
            buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
        }
    }

    /// Push a temporary variable
    fn emitPushTemporary(self: *JIT, buf: *CodeBuffer, index: u8) void {
        _ = self;
        // temp_base + 1 + index is the stack position
        // Load temp_base
        buf.movRegMem(.rax, .rbx, INTERP_TEMP_BASE_OFFSET);
        // Add 1 + index
        buf.addRegImm32(.rax, 1 + @as(i32, index));
        // Stack base
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // Load value from stack[temp_base + 1 + index]
        buf.rex(true, false, false, false);
        buf.emit8(0x8B); // mov rdx, [rcx + rax*8]
        buf.emit8(0x14);
        buf.emit8(0xC1);
        // Now push this value
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, false, false);
        buf.emit8(0x89); // mov [rcx + rax*8], rdx
        buf.emit8(0x14);
        buf.emit8(0xC1);
        // Increment sp
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Push a receiver instance variable (simplified)
    fn emitPushReceiverVariable(self: *JIT, buf: *CodeBuffer, index: u8) void {
        // This is complex - for now just push nil
        _ = index;
        self.emitPushNil(buf);
    }

    /// Store top of stack to temporary (don't pop)
    fn emitStoreTemporary(self: *JIT, buf: *CodeBuffer, index: u8) void {
        _ = self;
        // Load sp - 1 (top of stack)
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.subRegImm32(.rax, 1);
        // Stack base
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // Load TOS value
        buf.rex(true, false, false, false);
        buf.emit8(0x8B); // mov rdx, [rcx + rax*8]
        buf.emit8(0x14);
        buf.emit8(0xC1);
        // Calculate temp position: temp_base + 1 + index
        buf.movRegMem(.rax, .rbx, INTERP_TEMP_BASE_OFFSET);
        buf.addRegImm32(.rax, 1 + @as(i32, index));
        // Store to temp
        buf.rex(true, false, false, false);
        buf.emit8(0x89); // mov [rcx + rax*8], rdx
        buf.emit8(0x14);
        buf.emit8(0xC1);
    }

    /// Pop and store to temporary
    fn emitPopStoreTemporary(self: *JIT, buf: *CodeBuffer, index: u8) void {
        self.emitStoreTemporary(buf, index);
        self.emitPop(buf);
    }

    /// Pop top of stack
    fn emitPop(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Decrement sp
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.subRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Pop top of stack into rax (for comparisons)
    fn emitPopToRax(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Decrement sp and load value
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.subRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
        // Load stack[sp] into rax
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // mov rax, [rcx + rax*8]
        buf.rex(true, false, false, false);
        buf.emit8(0x8B); // mov
        buf.emit8(0x04); // ModRM: [rcx + rax*8]
        buf.emit8(0xC1); // SIB: scale=8 (3<<6), index=rax (0<<3), base=rcx (1)
    }

    /// Duplicate top of stack
    fn emitDup(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load sp - 1
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.subRegImm32(.rax, 1);
        // Stack base
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // Load TOS
        buf.rex(true, false, false, false);
        buf.emit8(0x8B);
        buf.emit8(0x14);
        buf.emit8(0xC1);
        // Store at sp
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.rex(true, false, false, false);
        buf.emit8(0x89);
        buf.emit8(0x14);
        buf.emit8(0xC1);
        // Increment sp
        buf.addRegImm32(.rax, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rax);
    }

    /// Return top of stack
    fn emitReturnTop(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load sp - 1
        buf.movRegMem(.rax, .rbx, INTERP_SP_OFFSET);
        buf.subRegImm32(.rax, 1);
        // Stack base
        buf.movRegReg(.rcx, .rbx);
        buf.addRegImm32(.rcx, INTERP_STACK_OFFSET);
        // Load TOS into rax (return value)
        buf.rex(true, false, false, false);
        buf.emit8(0x8B); // mov rax, [rcx + rax*8]
        buf.emit8(0x04);
        buf.emit8(0xC1);
        // Epilogue
        buf.popReg(.r13);
        buf.popReg(.r12);
        buf.popReg(.rbx);
        buf.popReg(.rbp);
        buf.ret();
    }

    /// Return self
    fn emitReturnSelf(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        // Load receiver into rax
        buf.movRegMem(.rax, .rbx, INTERP_RECEIVER_OFFSET);
        // Epilogue
        buf.popReg(.r13);
        buf.popReg(.r12);
        buf.popReg(.rbx);
        buf.popReg(.rbp);
        buf.ret();
    }

    /// Return nil
    fn emitReturnNil(self: *JIT, buf: *CodeBuffer) void {
        _ = self;
        buf.movRegImm64(.rax, Value.nil.bits);
        buf.popReg(.r13);
        buf.popReg(.r12);
        buf.popReg(.rbx);
        buf.popReg(.rbp);
        buf.ret();
    }

    /// Emit a message send
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
        // System V AMD64 calling convention:
        // rdi = arg1 (interpreter pointer, already in rbx)
        // rsi = arg2 (callsite cache pointer)
        // rdx = arg3 (selector_index)
        // rcx = arg4 (num_args)
        // r8  = arg5 (bytecode_offset)
        // r9  = arg6 (is_super flag)

        // Move interpreter pointer to rdi (first arg)
        buf.movRegReg(.rdi, .rbx);

        // Move callsite cache pointer to rsi (second arg)
        buf.movRegImm64(.rsi, @intFromPtr(cache));

        // Move selector_index to rdx (third arg)
        buf.movRegImm64(.rdx, @as(u64, selector_idx));

        // Move num_args to rcx (fourth arg)
        buf.movRegImm64(.rcx, @as(u64, num_args));

        // Move bytecode_offset to r8 (fifth arg)
        buf.movRegImm64(.r8, @as(u64, bytecode_offset));

        // Move is_super flag to r9 (sixth arg)
        const super_flag: u64 = if (is_super) 1 else 0;
        buf.movRegImm64(.r9, super_flag);

        // Load address of jit_runtime_send into r10 (scratch register)
        const send_fn_ptr = @intFromPtr(&jit_runtime_send_with_cache);
        buf.movRegImm64(.r10, send_fn_ptr);

        // Call r10
        buf.callReg(.r10);

        // Result is in rax - push it onto Smalltalk stack
        // Load sp
        buf.movRegMem(.rcx, .rbx, INTERP_SP_OFFSET);
        // Stack base
        buf.movRegReg(.rdx, .rbx);
        buf.addRegImm32(.rdx, INTERP_STACK_OFFSET);
        // Store result at stack[sp]
        // mov [rdx + rcx*8], rax
        buf.rex(true, false, false, false);
        buf.emit8(0x89); // mov [rdx + rcx*8], rax
        buf.emit8(0x04);
        buf.emit8(0xCA); // SIB: scale=8, index=rcx, base=rdx
        // Increment sp
        buf.movRegMem(.rcx, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rcx, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rcx);
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
