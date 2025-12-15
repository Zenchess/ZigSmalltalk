/// Baseline JIT Compiler for Zig Smalltalk
/// Translates bytecode to x86-64 machine code for improved performance
///
/// Strategy:
/// - Direct bytecode-to-native translation (no optimization)
/// - Stack kept in memory (not register-mapped)
/// - Complex operations (sends) call back to runtime
/// - Inline cache stubs for monomorphic sends

const std = @import("std");
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

    pub const EMPTY: CallSiteCache = .{
        .expected_class = Value.nil,
        .cached_method = null,
        .cached_holder = Value.nil,
        .version = 0,
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

/// Runtime helper called from JIT code to perform a message send
/// This executes the full send and returns the result.
/// Uses primitive_block_depth mechanism to detect when send completes.
pub export fn jit_runtime_send(
    interp: *Interpreter,
    selector_index: u8,
    num_args: u8,
    bytecode_offset: usize,
) callconv(.c) Value {
    // Save state for error recovery
    const saved_context_ptr = interp.context_ptr;
    const saved_sp = interp.sp;

    // Call interpreter's sendMessage to set up the call
    interp.sendMessage(selector_index, num_args, false, bytecode_offset) catch {
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

/// Runtime helper for super sends
pub export fn jit_runtime_super_send(
    interp: *Interpreter,
    selector_index: u8,
    num_args: u8,
    bytecode_offset: usize,
) callconv(.c) Value {
    const saved_context_ptr = interp.context_ptr;
    const saved_sp = interp.sp;

    interp.sendMessage(selector_index, num_args, true, bytecode_offset) catch {
        return Value.nil;
    };

    if (interp.context_ptr == saved_context_ptr) {
        if (interp.sp > saved_sp) {
            interp.sp -= 1;
            return interp.stack[interp.sp];
        }
        return Value.nil;
    }

    interp.primitive_block_bases[interp.primitive_block_depth] = saved_context_ptr;
    interp.primitive_block_depth += 1;

    const result = interp.interpretLoop() catch {
        interp.primitive_block_depth -= 1;
        interp.context_ptr = saved_context_ptr;
        return Value.nil;
    };

    interp.primitive_block_depth -= 1;
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

pub const CompiledCode = struct {
    code: []u8,
    entry: *const fn (*Interpreter) callconv(.c) Value,
    method: *CompiledMethod,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CompiledCode) void {
        // Free executable memory
        const aligned_ptr: [*]align(PAGE_SIZE) u8 = @alignCast(self.code.ptr);
        std.posix.munmap(aligned_ptr[0..self.code.len]);
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

                // Jumps - NOT eligible (would need backpatching)
                .jump, .jump_if_true, .jump_if_false, .jump_if_nil, .jump_if_not_nil => {
                    return false;
                },

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
        var buffer = try CodeBuffer.init(self.allocator, 4096);
        defer buffer.deinit();

        // Generate prologue
        self.emitPrologue(&buffer);

        // Translate bytecodes
        const bc = method.getBytecodes();
        var ip: usize = 0;

        while (ip < bc.len) {
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
                    self.emitSend(&buffer, selector_idx, n_args, send_offset);
                },
                .super_send => {
                    const selector_idx = bc[ip];
                    ip += 1;
                    const n_args = bc[ip];
                    ip += 1;
                    const send_offset = ip - 3;
                    self.emitSuperSend(&buffer, selector_idx, n_args, send_offset);
                },
                // Jump bytecodes - bail for now, will implement later
                .jump, .jump_if_true, .jump_if_false, .jump_if_nil, .jump_if_not_nil => {
                    // These require backpatching - bail to interpreter for now
                    self.emitReturnNil(&buffer);
                    break;
                },
                // Short jumps (0xB8-0xBF) are handled by isPushTemporary check
                // but if we get here, bail
                else => {
                    // For unsupported opcodes, bail out to interpreter
                    // For now, just return nil
                    self.emitReturnNil(&buffer);
                    break;
                },
            }
        }

        // Generate epilogue if we haven't returned yet
        self.emitEpilogue(&buffer);

        // Allocate executable memory and copy code
        const code_size = buffer.pos;
        const aligned_size = std.mem.alignForward(usize, code_size, PAGE_SIZE);

        const executable_mem = try std.posix.mmap(
            null,
            aligned_size,
            std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );

        @memcpy(executable_mem[0..code_size], buffer.code[0..code_size]);

        // Create compiled code struct
        const compiled = try self.allocator.create(CompiledCode);
        compiled.* = CompiledCode{
            .code = executable_mem[0..aligned_size],
            .entry = @ptrCast(@alignCast(executable_mem.ptr)),
            .method = method,
            .allocator = self.allocator,
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
    /// This generates code to call jit_runtime_send and push the result
    fn emitSend(self: *JIT, buf: *CodeBuffer, selector_idx: u8, num_args: u8, bytecode_offset: usize) void {
        _ = self;
        // System V AMD64 calling convention:
        // rdi = arg1 (interpreter pointer, already in rbx)
        // rsi = arg2 (selector_index)
        // rdx = arg3 (num_args)
        // rcx = arg4 (bytecode_offset)

        // Move interpreter pointer to rdi (first arg)
        buf.movRegReg(.rdi, .rbx);

        // Move selector_index to rsi (second arg)
        buf.movRegImm64(.rsi, @as(u64, selector_idx));

        // Move num_args to rdx (third arg)
        buf.movRegImm64(.rdx, @as(u64, num_args));

        // Move bytecode_offset to rcx (fourth arg)
        buf.movRegImm64(.rcx, @as(u64, bytecode_offset));

        // Load address of jit_runtime_send into r10 (scratch register)
        const send_fn_ptr = @intFromPtr(&jit_runtime_send);
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
    fn emitSuperSend(self: *JIT, buf: *CodeBuffer, selector_idx: u8, num_args: u8, bytecode_offset: usize) void {
        _ = self;
        // Same as emitSend but calls jit_runtime_super_send

        buf.movRegReg(.rdi, .rbx);
        buf.movRegImm64(.rsi, @as(u64, selector_idx));
        buf.movRegImm64(.rdx, @as(u64, num_args));
        buf.movRegImm64(.rcx, @as(u64, bytecode_offset));

        const send_fn_ptr = @intFromPtr(&jit_runtime_super_send);
        buf.movRegImm64(.r10, send_fn_ptr);
        buf.callReg(.r10);

        // Push result onto Smalltalk stack
        buf.movRegMem(.rcx, .rbx, INTERP_SP_OFFSET);
        buf.movRegReg(.rdx, .rbx);
        buf.addRegImm32(.rdx, INTERP_STACK_OFFSET);
        buf.rex(true, false, false, false);
        buf.emit8(0x89);
        buf.emit8(0x04);
        buf.emit8(0xCA);
        buf.movRegMem(.rcx, .rbx, INTERP_SP_OFFSET);
        buf.addRegImm32(.rcx, 1);
        buf.movMemReg(.rbx, INTERP_SP_OFFSET, .rcx);
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
