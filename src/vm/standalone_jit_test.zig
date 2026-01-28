/// Standalone JIT test - no dependencies on the VM
const std = @import("std");

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

/// Simple code buffer
const CodeBuffer = struct {
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

    fn rex(self: *CodeBuffer, w: bool, r: bool, x: bool, b: bool) void {
        var byte: u8 = 0x40;
        if (w) byte |= 0x08;
        if (r) byte |= 0x04;
        if (x) byte |= 0x02;
        if (b) byte |= 0x01;
        self.emit8(byte);
    }

    fn modRM(self: *CodeBuffer, mod: u2, reg: u3, rm: u3) void {
        self.emit8((@as(u8, mod) << 6) | (@as(u8, reg) << 3) | @as(u8, rm));
    }

    pub fn pushReg(self: *CodeBuffer, reg: Reg) void {
        const r: u8 = @intFromEnum(reg);
        if (r >= 8) self.rex(false, false, false, true);
        self.emit8(0x50 + (r & 7));
    }

    pub fn popReg(self: *CodeBuffer, reg: Reg) void {
        const r: u8 = @intFromEnum(reg);
        if (r >= 8) self.rex(false, false, false, true);
        self.emit8(0x58 + (r & 7));
    }

    pub fn movRegImm64(self: *CodeBuffer, reg: Reg, imm: u64) void {
        const r: u8 = @intFromEnum(reg);
        self.rex(true, false, false, r >= 8);
        self.emit8(0xB8 + (r & 7));
        self.emit64(imm);
    }

    pub fn movRegReg(self: *CodeBuffer, dst: Reg, src: Reg) void {
        const d: u8 = @intFromEnum(dst);
        const s: u8 = @intFromEnum(src);
        self.rex(true, s >= 8, false, d >= 8);
        self.emit8(0x89);
        self.modRM(3, @truncate(s & 7), @truncate(d & 7));
    }

    pub fn addRegImm32(self: *CodeBuffer, reg: Reg, imm: i32) void {
        const r: u8 = @intFromEnum(reg);
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

    pub fn addRegReg(self: *CodeBuffer, dst: Reg, src: Reg) void {
        const d: u8 = @intFromEnum(dst);
        const s: u8 = @intFromEnum(src);
        self.rex(true, s >= 8, false, d >= 8);
        self.emit8(0x01);
        self.modRM(3, @truncate(s & 7), @truncate(d & 7));
    }

    pub fn cmpRegReg(self: *CodeBuffer, r1: Reg, r2: Reg) void {
        const a: u8 = @intFromEnum(r1);
        const b: u8 = @intFromEnum(r2);
        self.rex(true, b >= 8, false, a >= 8);
        self.emit8(0x39);
        self.modRM(3, @truncate(b & 7), @truncate(a & 7));
    }

    pub fn ret(self: *CodeBuffer) void {
        self.emit8(0xC3);
    }

    pub fn jmpRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0xE9);
        self.emit32(@bitCast(offset));
    }

    pub fn jeRel32(self: *CodeBuffer, offset: i32) void {
        self.emit8(0x0F);
        self.emit8(0x84);
        self.emit32(@bitCast(offset));
    }
};

// Smalltalk tagged value constants
const NIL_BITS: u64 = 0x6;
const TRUE_BITS: u64 = 0xE;
const FALSE_BITS: u64 = 0x16;

fn makeSmallInt(value: i64) u64 {
    const unsigned: u64 = @bitCast(value);
    return (unsigned << 3) | 0x1;
}

fn extractSmallInt(bits: u64) i64 {
    return @bitCast(bits >> 3);
}

const PAGE_SIZE: usize = 4096;

fn makeExecutable(allocator: std.mem.Allocator, code: []const u8) ![]u8 {
    _ = allocator;
    const aligned_size = std.mem.alignForward(usize, code.len, PAGE_SIZE);

    const mem = try std.posix.mmap(
        null,
        aligned_size,
        std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );

    @memcpy(mem[0..code.len], code);
    return mem;
}

fn freeExecutable(mem: []u8) void {
    const aligned_ptr: [*]align(PAGE_SIZE) u8 = @ptrCast(@alignCast(mem.ptr));
    std.posix.munmap(@alignCast(aligned_ptr[0..mem.len]));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("=== Standalone JIT Code Generation Test ===\n\n", .{});

    // Test 1: Generate simple code that returns a constant
    std.debug.print("Test 1: Generate code that returns nil\n", .{});
    {
        var buf = try CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);
        buf.movRegImm64(.rax, NIL_BITS);
        buf.popReg(.rbp);
        buf.ret();

        std.debug.print("  Generated {} bytes of code\n", .{buf.pos});

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();

        std.debug.print("  Called JIT code, result: 0x{x}\n", .{result});
        std.debug.print("  Expected nil: 0x{x}\n", .{NIL_BITS});

        if (result == NIL_BITS) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 2: Generate code that returns true
    std.debug.print("Test 2: Generate code that returns true\n", .{});
    {
        var buf = try CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);
        buf.movRegImm64(.rax, TRUE_BITS);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();

        std.debug.print("  Result: 0x{x}, Expected: 0x{x}\n", .{ result, TRUE_BITS });
        if (result == TRUE_BITS) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 3: Generate code that returns a SmallInteger
    std.debug.print("Test 3: Generate code that returns SmallInteger 42\n", .{});
    {
        var buf = try CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);
        const val42 = makeSmallInt(42);
        buf.movRegImm64(.rax, val42);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();

        std.debug.print("  Result bits: 0x{x}\n", .{result});
        const decoded = extractSmallInt(result);
        std.debug.print("  Decoded SmallInt: {}\n", .{decoded});
        if (decoded == 42) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 4: Arithmetic - add two numbers
    std.debug.print("Test 4: Add 10 + 32 using registers\n", .{});
    {
        var buf = try CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);
        buf.movRegImm64(.rcx, 10);
        buf.movRegImm64(.rdx, 32);
        buf.addRegReg(.rcx, .rdx);
        buf.movRegReg(.rax, .rcx);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();

        std.debug.print("  Result: {}, Expected: 42\n", .{result});
        if (result == 42) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 5: Simple loop - count to 1000000
    std.debug.print("Test 5: Loop counting to 1,000,000\n", .{});
    {
        var buf = try CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);

        buf.movRegImm64(.rcx, 0); // counter = 0
        buf.movRegImm64(.rdx, 1000000); // limit = 1000000

        const loop_start = buf.currentPos();

        buf.cmpRegReg(.rcx, .rdx);
        const je_pos = buf.currentPos();
        buf.jeRel32(0); // Placeholder

        buf.addRegImm32(.rcx, 1);

        const loop_end = buf.currentPos();
        const back_offset = @as(i32, @intCast(loop_start)) - @as(i32, @intCast(loop_end)) - 5;
        buf.jmpRel32(back_offset);

        const after_loop = buf.currentPos();
        const forward_offset = @as(i32, @intCast(after_loop)) - @as(i32, @intCast(je_pos)) - 6;
        buf.code[je_pos + 2] = @truncate(@as(u32, @bitCast(forward_offset)));
        buf.code[je_pos + 3] = @truncate(@as(u32, @bitCast(forward_offset)) >> 8);
        buf.code[je_pos + 4] = @truncate(@as(u32, @bitCast(forward_offset)) >> 16);
        buf.code[je_pos + 5] = @truncate(@as(u32, @bitCast(forward_offset)) >> 24);

        buf.movRegReg(.rax, .rcx);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);

        const start = std.time.milliTimestamp();
        const result = func();
        const end = std.time.milliTimestamp();

        std.debug.print("  Result: {}, Time: {}ms\n", .{ result, end - start });
        if (result == 1000000) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 6: Benchmark - tight loop with 100M iterations
    std.debug.print("Test 6: Benchmark - 100M loop iterations\n", .{});
    {
        var buf = try CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);

        buf.movRegImm64(.rcx, 0); // counter = 0
        buf.movRegImm64(.rdx, 100000000); // limit = 100M

        const loop_start = buf.currentPos();
        buf.cmpRegReg(.rcx, .rdx);
        const je_pos = buf.currentPos();
        buf.jeRel32(0);
        buf.addRegImm32(.rcx, 1);
        const loop_end = buf.currentPos();
        const back_offset = @as(i32, @intCast(loop_start)) - @as(i32, @intCast(loop_end)) - 5;
        buf.jmpRel32(back_offset);

        const after_loop = buf.currentPos();
        const forward_offset = @as(i32, @intCast(after_loop)) - @as(i32, @intCast(je_pos)) - 6;
        buf.code[je_pos + 2] = @truncate(@as(u32, @bitCast(forward_offset)));
        buf.code[je_pos + 3] = @truncate(@as(u32, @bitCast(forward_offset)) >> 8);
        buf.code[je_pos + 4] = @truncate(@as(u32, @bitCast(forward_offset)) >> 16);
        buf.code[je_pos + 5] = @truncate(@as(u32, @bitCast(forward_offset)) >> 24);

        buf.movRegReg(.rax, .rcx);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);

        const start = std.time.milliTimestamp();
        const result = func();
        const end = std.time.milliTimestamp();

        const elapsed_ms = end - start;
        const iters_per_sec = if (elapsed_ms > 0) @divTrunc(@as(u64, 100000000) * 1000, @as(u64, @intCast(elapsed_ms))) else 0;

        std.debug.print("  100M iterations in {}ms\n", .{elapsed_ms});
        std.debug.print("  ~{} iterations/sec\n", .{iters_per_sec});
        std.debug.print("  Result: {}\n", .{result});
        if (result == 100000000) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    std.debug.print("=== All JIT tests completed ===\n", .{});
}
