const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    if (args.len < 4) {
        std.debug.print("Usage: gen_ffi_headers <libffi_src_dir> <output_dir> <target>\n", .{});
        std.debug.print("  target: x86_64-windows, x86_64-linux, aarch64-linux\n", .{});
        std.process.exit(1);
    }

    const libffi_src = args[1];
    const output_dir = args[2];
    const target_str = args[3];

    // Parse target
    const is_windows = std.mem.indexOf(u8, target_str, "windows") != null;
    const is_x86_64 = std.mem.indexOf(u8, target_str, "x86_64") != null;
    const is_aarch64 = std.mem.indexOf(u8, target_str, "aarch64") != null;

    // Determine target macro
    const target_macro = if (is_windows and is_x86_64)
        "X86_WIN64"
    else if (is_x86_64)
        "X86_64"
    else if (is_aarch64)
        "AARCH64"
    else
        "X86_64"; // fallback

    // Generate ffi.h
    try generateFfiH(allocator, libffi_src, output_dir, target_macro);

    // Generate fficonfig.h
    try generateFfiConfigH(allocator, output_dir, target_macro, is_windows, is_x86_64, is_aarch64);

    // Copy ffitarget.h from the appropriate architecture directory
    try copyFfitargetH(allocator, libffi_src, output_dir, is_x86_64, is_aarch64);

    std.debug.print("Generated FFI headers for {s}\n", .{target_str});
}

fn generateFfiH(
    allocator: std.mem.Allocator,
    libffi_src: []const u8,
    output_dir: []const u8,
    target_macro: []const u8,
) !void {
    // Read ffi.h.in template
    const template_path = try std.fs.path.join(allocator, &.{ libffi_src, "include", "ffi.h.in" });
    const template = try std.fs.cwd().readFileAlloc(allocator, template_path, 1024 * 1024);

    // Perform substitutions
    var result: []const u8 = template;

    // @TARGET@ -> target macro
    result = try replace(allocator, result, "@TARGET@", target_macro);

    // @HAVE_LONG_DOUBLE@ -> 1 (most platforms have it)
    result = try replace(allocator, result, "@HAVE_LONG_DOUBLE@", "1");

    // @FFI_EXEC_TRAMPOLINE_TABLE@ -> 0 (not used on x86/Windows)
    result = try replace(allocator, result, "@FFI_EXEC_TRAMPOLINE_TABLE@", "0");

    // @VERSION@ -> 3.4.6
    result = try replace(allocator, result, "@VERSION@", "3.4.6");

    // Write output
    const output_path = try std.fs.path.join(allocator, &.{ output_dir, "ffi.h" });
    try std.fs.cwd().makePath(output_dir);
    try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = result });
}

fn generateFfiConfigH(
    allocator: std.mem.Allocator,
    output_dir: []const u8,
    target_macro: []const u8,
    is_windows: bool,
    is_x86_64: bool,
    is_aarch64: bool,
) !void {
    var buf = std.ArrayListUnmanaged(u8){};
    const writer = buf.writer(allocator);

    try writer.writeAll("/* Auto-generated fficonfig.h for libffi */\n");
    try writer.writeAll("#ifndef LIBFFI_CONFIG_H\n");
    try writer.writeAll("#define LIBFFI_CONFIG_H\n\n");

    // Target macro
    try writer.print("#define {s} 1\n\n", .{target_macro});

    // Standard headers
    try writer.writeAll("#define HAVE_INTTYPES_H 1\n");
    try writer.writeAll("#define HAVE_MEMORY_H 1\n");
    try writer.writeAll("#define HAVE_STDINT_H 1\n");
    try writer.writeAll("#define HAVE_STDLIB_H 1\n");
    try writer.writeAll("#define HAVE_STRING_H 1\n");
    try writer.writeAll("#define STDC_HEADERS 1\n\n");

    if (is_windows) {
        try writer.writeAll("#define HAVE_ALLOCA 1\n");
        try writer.writeAll("#define FFI_HIDDEN\n");
    } else {
        try writer.writeAll("#define HAVE_ALLOCA 1\n");
        try writer.writeAll("#define HAVE_ALLOCA_H 1\n");
        try writer.writeAll("#define HAVE_SYS_STAT_H 1\n");
        try writer.writeAll("#define HAVE_SYS_TYPES_H 1\n");
        try writer.writeAll("#define HAVE_SYS_MMAN_H 1\n");
        try writer.writeAll("#define HAVE_MMAP 1\n");
        try writer.writeAll("#define HAVE_MKOSTEMP 1\n");
        try writer.writeAll("#define FFI_HIDDEN __attribute__((visibility(\"hidden\")))\n");
        try writer.writeAll("#define FFI_EXEC_STATIC_TRAMP 1\n");
        try writer.writeAll("#define FFI_MMAP_EXEC_WRIT 1\n");
    }

    try writer.writeAll("\n/* Version */\n");
    try writer.writeAll("#define VERSION \"3.4.6\"\n\n");

    // Size definitions
    try writer.writeAll("/* Type sizes */\n");
    try writer.writeAll("#define SIZEOF_DOUBLE 8\n");
    if (is_windows) {
        try writer.writeAll("#define SIZEOF_LONG_DOUBLE 8\n");
    } else {
        try writer.writeAll("#define SIZEOF_LONG_DOUBLE 16\n");
        // Note: HAVE_LONG_DOUBLE_VARIANT is only needed for powerpc where ABI affects long double size
        // On x86_64, long double is always 80-bit IEEE extended precision, no variant needed
    }

    if (is_x86_64 or is_aarch64) {
        try writer.writeAll("#define SIZEOF_SIZE_T 8\n");
    } else {
        try writer.writeAll("#define SIZEOF_SIZE_T 4\n");
    }

    try writer.writeAll("#define HAVE_LONG_DOUBLE 1\n\n");

    // Closures and trampolines
    try writer.writeAll("/* Closures */\n");
    try writer.writeAll("#define FFI_CLOSURES 1\n");
    try writer.writeAll("#define FFI_GO_CLOSURES 1\n");
    try writer.writeAll("#define FFI_NATIVE_RAW_API 0\n");
    try writer.writeAll("#define FFI_EXEC_TRAMPOLINE_TABLE 0\n\n");

    // Assembly compatibility
    if (!is_windows and is_x86_64) {
        // HAVE_AS_X86_PCREL enables portable PC-relative addressing (X - .)
        // Without this, libffi uses X@rel which requires GNU assembler extensions
        try writer.writeAll("/* Assembly compatibility */\n");
        try writer.writeAll("#define HAVE_AS_X86_PCREL 1\n\n");
    }

    // FFI_HIDDEN macro - needed for both C and assembly
    // On Windows, we don't have hidden visibility, so it's empty
    // On Linux/ELF, we use .hidden for assembly and __attribute__((visibility("hidden"))) for C
    if (is_windows) {
        try writer.writeAll("/* FFI_HIDDEN - Windows doesn't support hidden visibility */\n");
        try writer.writeAll("#ifdef LIBFFI_ASM\n");
        try writer.writeAll("#define FFI_HIDDEN(name)\n");
        try writer.writeAll("#else\n");
        try writer.writeAll("#define FFI_HIDDEN\n");
        try writer.writeAll("#endif\n");
    } else {
        try writer.writeAll("/* FFI_HIDDEN - use hidden visibility on ELF platforms */\n");
        try writer.writeAll("#define HAVE_HIDDEN_VISIBILITY_ATTRIBUTE 1\n");
        try writer.writeAll("#ifdef LIBFFI_ASM\n");
        try writer.writeAll("#define FFI_HIDDEN(name) .hidden name\n");
        try writer.writeAll("#else\n");
        try writer.writeAll("#define FFI_HIDDEN __attribute__((visibility(\"hidden\")))\n");
        try writer.writeAll("#endif\n");
    }

    try writer.writeAll("\n#endif /* LIBFFI_CONFIG_H */\n");

    // Write output
    const output_path = try std.fs.path.join(allocator, &.{ output_dir, "fficonfig.h" });
    try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = buf.items });
}

fn replace(allocator: std.mem.Allocator, input: []const u8, pattern: []const u8, replacement: []const u8) ![]const u8 {
    var result = std.ArrayListUnmanaged(u8){};
    var i: usize = 0;

    while (i < input.len) {
        if (i + pattern.len <= input.len and std.mem.eql(u8, input[i .. i + pattern.len], pattern)) {
            try result.appendSlice(allocator, replacement);
            i += pattern.len;
        } else {
            try result.append(allocator, input[i]);
            i += 1;
        }
    }

    return result.items;
}

fn copyFfitargetH(
    allocator: std.mem.Allocator,
    libffi_src: []const u8,
    output_dir: []const u8,
    is_x86_64: bool,
    is_aarch64: bool,
) !void {
    // Determine the architecture-specific directory
    const arch_dir = if (is_x86_64)
        "x86"
    else if (is_aarch64)
        "aarch64"
    else
        "x86"; // fallback

    // Read ffitarget.h from the architecture directory
    const src_path = try std.fs.path.join(allocator, &.{ libffi_src, "src", arch_dir, "ffitarget.h" });
    const content = try std.fs.cwd().readFileAlloc(allocator, src_path, 1024 * 1024);

    // Write to output directory
    const output_path = try std.fs.path.join(allocator, &.{ output_dir, "ffitarget.h" });
    try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = content });
}
