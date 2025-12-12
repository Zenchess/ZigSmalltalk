//! FFI Binding Generator
//!
//! Reads ffi-config.json and generates src/vm/ffi_generated.zig
//! with the appropriate @cImport statements and function lists.
//!
//! Run with: zig build gen-ffi

const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read config file
    const config_content = std.fs.cwd().readFileAlloc(allocator, "ffi-config.json", 1024 * 1024) catch |err| {
        std.debug.print("Warning: Could not read ffi-config.json: {}\n", .{err});
        std.debug.print("Using default FFI configuration.\n", .{});
        try generateDefault(allocator);
        return;
    };
    defer allocator.free(config_content);

    // Parse JSON
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, config_content, .{}) catch |err| {
        std.debug.print("Error parsing ffi-config.json: {}\n", .{err});
        return err;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) {
        std.debug.print("Error: ffi-config.json root must be an object\n", .{});
        return error.InvalidFormat;
    }

    const libs_val = root.object.get("libraries") orelse {
        std.debug.print("Error: ffi-config.json must have 'libraries' array\n", .{});
        return error.InvalidFormat;
    };
    if (libs_val != .array) {
        std.debug.print("Error: 'libraries' must be an array\n", .{});
        return error.InvalidFormat;
    }

    // Collect enabled libraries
    var libraries: std.ArrayList(Library) = .empty;
    defer {
        for (libraries.items) |lib| {
            allocator.free(lib.name);
            for (lib.headers) |h| allocator.free(h);
            allocator.free(lib.headers);
            for (lib.functions) |f| allocator.free(f);
            allocator.free(lib.functions);
        }
        libraries.deinit(allocator);
    }

    for (libs_val.array.items) |lib_val| {
        if (lib_val != .object) continue;

        const enabled_val = lib_val.object.get("enabled");
        if (enabled_val != null and enabled_val.? == .bool and !enabled_val.?.bool) continue;

        const name_val = lib_val.object.get("name") orelse continue;
        const headers_val = lib_val.object.get("headers") orelse continue;

        if (name_val != .string or headers_val != .array) continue;

        var headers: std.ArrayList([]u8) = .empty;
        for (headers_val.array.items) |h| {
            if (h == .string) {
                try headers.append(allocator, try allocator.dupe(u8, h.string));
            }
        }

        // Parse functions array (if empty or missing, use auto-discovery)
        var functions: std.ArrayList([]u8) = .empty;
        var auto_discover = true; // Default to auto-discovery
        if (lib_val.object.get("functions")) |funcs_val| {
            if (funcs_val == .array and funcs_val.array.items.len > 0) {
                auto_discover = false; // Explicit function list provided
                for (funcs_val.array.items) |f| {
                    if (f == .string) {
                        try functions.append(allocator, try allocator.dupe(u8, f.string));
                    }
                }
            }
        }

        try libraries.append(allocator, .{
            .name = try allocator.dupe(u8, name_val.string),
            .headers = try headers.toOwnedSlice(allocator),
            .functions = try functions.toOwnedSlice(allocator),
            .auto_discover = auto_discover,
        });
    }

    // Generate the output file
    try generateFile(allocator, libraries.items);

    var total_funcs: usize = 0;
    for (libraries.items) |lib| {
        total_funcs += lib.functions.len;
    }
    std.debug.print("Generated src/vm/ffi_generated.zig with {d} libraries, {d} functions\n", .{ libraries.items.len, total_funcs });
}

const Library = struct {
    name: []u8,
    headers: [][]u8,
    functions: [][]u8,
    auto_discover: bool, // If true, auto-discover all functions at compile time
};

fn generateDefault(allocator: std.mem.Allocator) !void {
    _ = allocator;

    var file = try std.fs.cwd().createFile("src/vm/ffi_generated.zig", .{});
    defer file.close();

    try file.writeAll(
        \\//! Auto-generated FFI bindings
        \\//! Generated from ffi-config.json by tools/gen_ffi.zig
        \\//! DO NOT EDIT - regenerate with: zig build gen-ffi
        \\
        \\const std = @import("std");
        \\
        \\/// LibC library imports
        \\pub const LibC = @cImport({
        \\    @cInclude("stdio.h");
        \\    @cInclude("stdlib.h");
        \\    @cInclude("string.h");
        \\    @cInclude("time.h");
        \\});
        \\
        \\/// LibMath library imports
        \\pub const LibMath = @cImport({
        \\    @cInclude("math.h");
        \\});
        \\
        \\/// Combined C imports (for backwards compatibility)
        \\pub const c = @cImport({
        \\    @cInclude("stdio.h");
        \\    @cInclude("stdlib.h");
        \\    @cInclude("string.h");
        \\    @cInclude("time.h");
        \\    @cInclude("math.h");
        \\});
        \\
        \\/// List of configured libraries
        \\pub const library_names = [_][]const u8{
        \\    "LibC",
        \\    "LibMath",
        \\};
        \\
        \\/// Function lists for each library
        \\pub const library_functions = struct {
        \\    pub const LibC = [_][]const u8{
        \\        "puts", "putchar", "getchar", "strlen", "strcmp", "strncmp",
        \\        "memset", "memcpy", "memmove", "memcmp",
        \\        "malloc", "calloc", "realloc", "free",
        \\        "atoi", "atol", "atof", "abs", "labs",
        \\        "rand", "srand", "time", "clock", "system", "getenv", "exit",
        \\    };
        \\    pub const LibMath = [_][]const u8{
        \\        "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
        \\        "sinh", "cosh", "tanh", "exp", "log", "log10", "log2",
        \\        "pow", "sqrt", "cbrt", "ceil", "floor", "round", "trunc",
        \\        "fabs", "fmod", "fmin", "fmax", "hypot",
        \\    };
        \\};
        \\
    );
}

fn generateFile(allocator: std.mem.Allocator, libraries: []const Library) !void {
    _ = allocator;

    var file = try std.fs.cwd().createFile("src/vm/ffi_generated.zig", .{});
    defer file.close();

    // Write header
    try file.writeAll(
        \\//! Auto-generated FFI bindings
        \\//! Generated from ffi-config.json by tools/gen_ffi.zig
        \\//! DO NOT EDIT - regenerate with: zig build gen-ffi
        \\
        \\const std = @import("std");
        \\
        \\
    );

    // Generate @cImport for each library
    for (libraries) |lib| {
        var buf: [4096]u8 = undefined;
        var len = std.fmt.bufPrint(&buf, "/// {s} library imports\npub const {s} = @cImport({{\n", .{ lib.name, lib.name }) catch continue;
        try file.writeAll(len);

        for (lib.headers) |header| {
            len = std.fmt.bufPrint(&buf, "    @cInclude(\"{s}\");\n", .{header}) catch continue;
            try file.writeAll(len);
        }
        try file.writeAll("});\n\n");
    }

    // Generate combined 'c' import for backwards compatibility
    try file.writeAll("/// Combined C imports (for backwards compatibility)\npub const c = @cImport({\n");
    for (libraries) |lib| {
        for (lib.headers) |header| {
            var buf: [256]u8 = undefined;
            const len = std.fmt.bufPrint(&buf, "    @cInclude(\"{s}\");\n", .{header}) catch continue;
            try file.writeAll(len);
        }
    }
    try file.writeAll("});\n\n");

    // Generate list of available libraries
    try file.writeAll("/// List of configured libraries\npub const library_names = [_][]const u8{\n");
    for (libraries) |lib| {
        var buf: [256]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "    \"{s}\",\n", .{lib.name}) catch continue;
        try file.writeAll(len);
    }
    try file.writeAll("};\n\n");

    // Generate function lists for each library (only for non-auto libraries)
    try file.writeAll("/// Function lists for each library (empty = auto-discover)\npub const library_functions = struct {\n");
    for (libraries) |lib| {
        var buf: [256]u8 = undefined;
        var len = std.fmt.bufPrint(&buf, "    pub const {s} = [_][]const u8{{\n", .{lib.name}) catch continue;
        try file.writeAll(len);

        for (lib.functions) |func| {
            len = std.fmt.bufPrint(&buf, "        \"{s}\",\n", .{func}) catch continue;
            try file.writeAll(len);
        }
        try file.writeAll("    };\n");
    }
    try file.writeAll("};\n\n");

    // Generate auto-discovery flags
    try file.writeAll("/// Auto-discovery flags (true = discover all functions at compile time)\npub const library_auto = struct {\n");
    for (libraries) |lib| {
        var buf: [256]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "    pub const {s} = {};\n", .{ lib.name, lib.auto_discover }) catch continue;
        try file.writeAll(len);
    }
    try file.writeAll("};\n\n");

    // Generate library binding instantiations
    try file.writeAll("// ============================================================================\n");
    try file.writeAll("// Auto-generated library bindings\n");
    try file.writeAll("// ============================================================================\n\n");
    try file.writeAll("const ffi_autogen = @import(\"ffi_autogen.zig\");\n\n");

    for (libraries) |lib| {
        var buf: [512]u8 = undefined;
        if (lib.auto_discover) {
            // Use auto-discovery
            const len = std.fmt.bufPrint(&buf, "pub const {s}_binding = ffi_autogen.FFILibraryAuto({s}, \"{s}\");\n", .{ lib.name, lib.name, lib.name }) catch continue;
            try file.writeAll(len);
        } else {
            // Use explicit function list
            const len = std.fmt.bufPrint(&buf, "pub const {s}_binding = ffi_autogen.FFILibrary({s}, \"{s}\", &library_functions.{s});\n", .{ lib.name, lib.name, lib.name, lib.name }) catch continue;
            try file.writeAll(len);
        }
    }

    // Generate callFFI dispatch function
    try file.writeAll("\n/// Call an FFI function by library and function name\n");
    try file.writeAll("pub fn callFFI(library: []const u8, func_name: []const u8, heap: *ffi_autogen.Heap, args: []const ffi_autogen.Value, alloc: std.mem.Allocator) ffi_autogen.FFIError!ffi_autogen.Value {\n");
    for (libraries) |lib| {
        var buf: [256]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "    if (std.mem.eql(u8, library, \"{s}\")) return {s}_binding.call(func_name, heap, args, alloc);\n", .{ lib.name, lib.name }) catch continue;
        try file.writeAll(len);
    }
    try file.writeAll("    return ffi_autogen.FFIError.UnknownFunction;\n}\n\n");

    // Generate getLibraryFunctions dispatch
    try file.writeAll("/// Get all functions for a library\n");
    try file.writeAll("pub fn getLibraryFunctions(library: []const u8) ?[]const ffi_autogen.FFIFunction {\n");
    for (libraries) |lib| {
        var buf: [256]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "    if (std.mem.eql(u8, library, \"{s}\")) return &{s}_binding.functions;\n", .{ lib.name, lib.name }) catch continue;
        try file.writeAll(len);
    }
    try file.writeAll("    return null;\n}\n\n");

    // Generate getLibraryFunctionNames dispatch
    try file.writeAll("/// Get function names for a library\n");
    try file.writeAll("pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {\n");
    for (libraries) |lib| {
        var buf: [256]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "    if (std.mem.eql(u8, library, \"{s}\")) return {s}_binding.getFunctionNames();\n", .{ lib.name, lib.name }) catch continue;
        try file.writeAll(len);
    }
    try file.writeAll("    return null;\n}\n");
}
