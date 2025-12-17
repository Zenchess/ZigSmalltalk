/// Simple JIT test - standalone executable to verify JIT code generation
const std = @import("std");
const jit = @import("jit.zig");
const object = @import("object.zig");

const Value = object.Value;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("=== JIT Code Generation Test ===\n\n", .{});

    // Test 1: Generate simple code that returns a constant
    std.debug.print("Test 1: Generate code that returns nil\n", .{});
    {
        var buf = try jit.CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        // Prologue
        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);

        // Return nil (0x6)
        buf.movRegImm64(.rax, Value.nil.bits);

        // Epilogue
        buf.popReg(.rbp);
        buf.ret();

        std.debug.print("  Generated {} bytes of code\n", .{buf.pos});

        // Make executable and call
        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();

        std.debug.print("  Called JIT code, result: 0x{x}\n", .{result});
        std.debug.print("  Expected nil.bits: 0x{x}\n", .{Value.nil.bits});

        if (result == Value.nil.bits) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 2: Generate code that returns true
    std.debug.print("Test 2: Generate code that returns true\n", .{});
    {
        var buf = try jit.CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);
        buf.movRegImm64(.rax, Value.@"true".bits);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();

        std.debug.print("  Result: 0x{x}, Expected: 0x{x}\n", .{result, Value.@"true".bits});
        if (result == Value.@"true".bits) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 3: Generate code that returns a SmallInteger
    std.debug.print("Test 3: Generate code that returns SmallInteger 42\n", .{});
    {
        var buf = try jit.CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);
        const val42 = Value.fromSmallInt(42);
        buf.movRegImm64(.rax, val42.bits);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);
        const result = func();
        const result_val = Value{ .bits = result };

        std.debug.print("  Result bits: 0x{x}\n", .{result});
        if (result_val.isSmallInt()) {
            std.debug.print("  Decoded SmallInt: {}\n", .{result_val.asSmallInt()});
            if (result_val.asSmallInt() == 42) {
                std.debug.print("  PASS!\n\n", .{});
            } else {
                std.debug.print("  FAIL!\n\n", .{});
            }
        } else {
            std.debug.print("  FAIL - not a SmallInt!\n\n", .{});
        }
    }

    // Test 4: Arithmetic - add two numbers
    std.debug.print("Test 4: Add 10 + 32 using registers\n", .{});
    {
        var buf = try jit.CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);

        // Load 10 into rcx, 32 into rdx
        buf.movRegImm64(.rcx, 10);
        buf.movRegImm64(.rdx, 32);
        // Add them
        buf.addRegReg(.rcx, .rdx);
        // Move result to rax
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
        var buf = try jit.CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);

        // counter in rcx, limit in rdx
        buf.movRegImm64(.rcx, 0);         // counter = 0
        buf.movRegImm64(.rdx, 1000000);   // limit = 1000000

        const loop_start = buf.currentPos();

        // Compare counter to limit
        buf.cmpRegReg(.rcx, .rdx);
        // Jump to end if equal
        const je_pos = buf.currentPos();
        buf.jeRel32(0); // Placeholder, will patch

        // Increment counter
        buf.addRegImm32(.rcx, 1);

        // Jump back to loop start
        const loop_end = buf.currentPos();
        const back_offset = @as(i32, @intCast(loop_start)) - @as(i32, @intCast(loop_end)) - 5;
        buf.jmpRel32(back_offset);

        // Patch the je instruction
        const after_loop = buf.currentPos();
        const forward_offset = @as(i32, @intCast(after_loop)) - @as(i32, @intCast(je_pos)) - 6;
        buf.code[je_pos + 2] = @truncate(@as(u32, @bitCast(forward_offset)));
        buf.code[je_pos + 3] = @truncate(@as(u32, @bitCast(forward_offset)) >> 8);
        buf.code[je_pos + 4] = @truncate(@as(u32, @bitCast(forward_offset)) >> 16);
        buf.code[je_pos + 5] = @truncate(@as(u32, @bitCast(forward_offset)) >> 24);

        // Return counter
        buf.movRegReg(.rax, .rcx);
        buf.popReg(.rbp);
        buf.ret();

        const code = try makeExecutable(allocator, buf.getCode());
        defer freeExecutable(code);

        const func: *const fn () callconv(.c) u64 = @ptrCast(code.ptr);

        // Time it
        const start = std.time.milliTimestamp();
        const result = func();
        const end = std.time.milliTimestamp();

        std.debug.print("  Result: {}, Time: {}ms\n", .{result, end - start});
        if (result == 1000000) {
            std.debug.print("  PASS!\n\n", .{});
        } else {
            std.debug.print("  FAIL!\n\n", .{});
        }
    }

    // Test 6: Benchmark - tight loop with 100M iterations
    std.debug.print("Test 6: Benchmark - 100M loop iterations\n", .{});
    {
        var buf = try jit.CodeBuffer.init(allocator, 4096);
        defer buf.deinit();

        buf.pushReg(.rbp);
        buf.movRegReg(.rbp, .rsp);

        buf.movRegImm64(.rcx, 0);           // counter = 0
        buf.movRegImm64(.rdx, 100000000);   // limit = 100M

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

fn makeExecutable(allocator: std.mem.Allocator, code: []const u8) ![]align(std.mem.page_size) u8 {
    _ = allocator;
    const aligned_size = std.mem.alignForward(usize, code.len, std.mem.page_size);

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

fn freeExecutable(mem: []align(std.mem.page_size) u8) void {
    std.posix.munmap(mem);
}
