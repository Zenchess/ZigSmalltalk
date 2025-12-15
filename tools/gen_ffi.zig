//! FFI Binding Generator
//!
//! Reads ffi-config.json and generates ffi_generated.zig
//! with the appropriate @cImport statements and function lists.
//!
//! This generator checks if headers exist before including libraries,
//! allowing graceful handling of missing optional dependencies.
//!
//! Usage:
//!   zig build gen-ffi                    # Manual generation to src/vm/ffi_generated.zig
//!   gen-ffi <target> <output_dir>        # Build-time generation with target awareness

const std = @import("std");
const builtin = @import("builtin");

// Global state for target platform (set from command line or defaults to build host)
var g_target_is_windows: bool = builtin.os.tag == .windows;

/// Escape backslashes for Zig string literals (Windows paths need \\ instead of \)
fn escapeBackslashes(path: []const u8, buf: *[1024]u8) []const u8 {
    var pos: usize = 0;
    for (path) |c| {
        if (pos >= buf.len - 2) break;
        if (c == '\\') {
            buf[pos] = '\\';
            pos += 1;
            buf[pos] = '\\';
            pos += 1;
        } else {
            buf[pos] = c;
            pos += 1;
        }
    }
    return buf[0..pos];
}

// Global output path (set from command line or defaults to src/vm)
var g_output_path: []const u8 = "src/vm/ffi_generated.zig";

pub fn main() !void {
    // Use arena allocator to avoid tracking individual allocations
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);

    if (args.len >= 3) {
        // Build-time invocation: gen-ffi <target> <output_dir>
        const target_str = args[1];
        const output_dir = args[2];

        // Set target platform from argument
        g_target_is_windows = std.mem.indexOf(u8, target_str, "windows") != null;

        // Build output path
        g_output_path = try std.fs.path.join(allocator, &.{ output_dir, "ffi_generated.zig" });

        std.debug.print("gen-ffi: Target={s}, Output={s}\n", .{ target_str, g_output_path });
    }

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

    // Collect enabled libraries (with header availability check)
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

        // Collect headers and check if they exist
        var headers: std.ArrayList([]u8) = .empty;
        var all_headers_found = true;
        for (headers_val.array.items) |h| {
            if (h == .string) {
                try headers.append(allocator, try allocator.dupe(u8, h.string));
                // Check if header exists on this system
                if (!headerExists(h.string)) {
                    all_headers_found = false;
                }
            }
        }

        // Skip libraries whose headers are not available
        // Exception: standard C library headers (stdio.h, stdlib.h, etc.) are always available
        const is_standard_lib = std.mem.eql(u8, name_val.string, "LibC") or std.mem.eql(u8, name_val.string, "LibMath");
        if (!is_standard_lib and !all_headers_found) {
            std.debug.print("gen-ffi: Skipping {s} (headers not found)\n", .{name_val.string});
            for (headers.items) |h| allocator.free(h);
            headers.deinit(allocator);
            continue;
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

        // Check for glew flag
        var glew = false;
        if (lib_val.object.get("glew")) |glew_val| {
            if (glew_val == .bool) {
                glew = glew_val.bool;
            }
        }

        std.debug.print("gen-ffi: Including {s}\n", .{name_val.string});
        try libraries.append(allocator, .{
            .name = try allocator.dupe(u8, name_val.string),
            .headers = try headers.toOwnedSlice(allocator),
            .functions = try functions.toOwnedSlice(allocator),
            .auto_discover = auto_discover,
            .glew = glew,
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
    glew: bool, // If true, add GLEW helper functions
};

/// Check if a header file exists and is compatible with target platform
fn headerExists(header: []const u8) bool {
    // Check if header path is compatible with target platform
    const is_windows_path = isWindowsPath(header);
    const is_unix_path = header.len > 0 and header[0] == '/';

    // Skip paths that are incompatible with target
    if (is_windows_path and !g_target_is_windows) {
        std.debug.print("gen-ffi: Skipping Windows path '{s}' for non-Windows target\n", .{header});
        return false;
    }
    if (is_unix_path and g_target_is_windows) {
        std.debug.print("gen-ffi: Skipping Unix path '{s}' for Windows target\n", .{header});
        return false;
    }

    // For build-time generation, we trust that the config is correct
    // since we can't actually check paths on a different platform
    if (isFullPath(header)) {
        // If running on the same platform as target, check if file exists
        const host_is_windows = builtin.os.tag == .windows;
        if (host_is_windows == g_target_is_windows) {
            if (std.fs.cwd().access(header, .{})) |_| {
                return true;
            } else |_| {
                return false;
            }
        }
        // Cross-platform: trust that full paths in config are valid
        return true;
    }

    // Otherwise search in standard include paths (only on matching platform)
    const host_is_windows = builtin.os.tag == .windows;
    if (host_is_windows != g_target_is_windows) {
        // Can't check include paths on different platform, assume standard headers exist
        return true;
    }

    if (g_target_is_windows) {
        // Windows include paths (MSYS2/MinGW, vcpkg, etc.)
        const windows_paths = [_][]const u8{
            "C:\\msys64\\mingw64\\include\\",
            "C:\\msys64\\ucrt64\\include\\",
            "C:\\vcpkg\\installed\\x64-windows\\include\\",
            "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\",
        };
        for (windows_paths) |prefix| {
            var path_buf: [512]u8 = undefined;
            const path = std.fmt.bufPrint(&path_buf, "{s}{s}", .{ prefix, header }) catch continue;
            if (std.fs.cwd().access(path, .{})) |_| {
                return true;
            } else |_| {}
        }
        return false;
    } else {
        // Unix/Linux include paths
        const unix_paths = [_][]const u8{
            "/usr/include/",
            "/usr/local/include/",
            "/opt/homebrew/include/",
        };
        for (unix_paths) |prefix| {
            var path_buf: [512]u8 = undefined;
            const path = std.fmt.bufPrint(&path_buf, "{s}{s}", .{ prefix, header }) catch continue;
            if (std.fs.cwd().access(path, .{})) |_| {
                return true;
            } else |_| {}
        }
        return false;
    }
}

/// Check if a path is a Windows path (has drive letter)
fn isWindowsPath(path: []const u8) bool {
    if (path.len >= 3 and path[1] == ':' and (path[2] == '\\' or path[2] == '/')) return true;
    return false;
}

/// Check if a string looks like a full file path
fn isFullPath(path: []const u8) bool {
    if (path.len == 0) return false;
    // Unix absolute path
    if (path[0] == '/') return true;
    // Windows absolute path (e.g., C:\...)
    if (path.len >= 3 and path[1] == ':' and (path[2] == '\\' or path[2] == '/')) return true;
    return false;
}

fn generateDefault(allocator: std.mem.Allocator) !void {
    _ = allocator;

    // Ensure output directory exists
    if (std.fs.path.dirname(g_output_path)) |dir| {
        std.fs.cwd().makePath(dir) catch {};
    }
    var file = try std.fs.cwd().createFile(g_output_path, .{});
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

    // Ensure output directory exists
    if (std.fs.path.dirname(g_output_path)) |dir| {
        std.fs.cwd().makePath(dir) catch {};
    }
    var file = try std.fs.cwd().createFile(g_output_path, .{});
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

    // Check if any library uses GLEW
    var has_glew = false;
    for (libraries) |lib| {
        if (lib.glew) {
            has_glew = true;
            break;
        }
    }

    // Generate @cImport for each library
    for (libraries) |lib| {
        var buf: [4096]u8 = undefined;
        var len = std.fmt.bufPrint(&buf, "/// {s} library imports\npub const {s} = @cImport({{\n", .{ lib.name, lib.name }) catch continue;
        try file.writeAll(len);

        for (lib.headers) |header| {
            var escape_buf: [1024]u8 = undefined;
            const escaped_header = escapeBackslashes(header, &escape_buf);
            len = std.fmt.bufPrint(&buf, "    @cInclude(\"{s}\");\n", .{escaped_header}) catch continue;
            try file.writeAll(len);
        }
        try file.writeAll("});\n\n");
    }

    // Add GLX import if we have GLEW
    if (has_glew) {
        try file.writeAll("/// GLX for glXGetProcAddress\npub const GLX = @cImport({\n    @cInclude(\"GL/glx.h\");\n});\n\n");
    }

    // Generate combined 'c' import for backwards compatibility (excluding GLEW to avoid conflicts)
    // Always include standard C headers for malloc, free, math functions, etc.
    try file.writeAll(
        \\/// Combined C imports (for backwards compatibility)
        \\pub const c = @cImport({
        \\    @cInclude("stdio.h");
        \\    @cInclude("stdlib.h");
        \\    @cInclude("string.h");
        \\    @cInclude("math.h");
        \\
    );
    for (libraries) |lib| {
        // Skip GLEW libraries since they conflict with other GL includes
        if (lib.glew) continue;
        for (lib.headers) |header| {
            var buf: [256]u8 = undefined;
            var escape_buf: [1024]u8 = undefined;
            const escaped_header = escapeBackslashes(header, &escape_buf);
            const len = std.fmt.bufPrint(&buf, "    @cInclude(\"{s}\");\n", .{escaped_header}) catch continue;
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
            if (lib.glew) {
                // GLEW has untranslatable macros, use FFILibraryAuto without struct support
                const len = std.fmt.bufPrint(&buf, "pub const {s}_binding = ffi_autogen.FFILibraryAuto({s}, \"{s}\");\n", .{ lib.name, lib.name, lib.name }) catch continue;
                try file.writeAll(len);
            } else {
                // Use auto-discovery with struct support
                const len = std.fmt.bufPrint(&buf, "pub const {s}_binding = ffi_autogen.FFILibraryWithStructs({s}, \"{s}\");\n", .{ lib.name, lib.name, lib.name }) catch continue;
                try file.writeAll(len);
            }
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
    try file.writeAll("    return null;\n}\n\n");

    // Check if any library supports struct introspection
    var has_struct_support = false;
    for (libraries) |lib| {
        if (lib.auto_discover and !lib.glew) {
            has_struct_support = true;
            break;
        }
    }

    // Generate getLibraryStructNames dispatch (only for auto-discover libraries without glew)
    try file.writeAll("/// Get struct names for a library\n");
    if (has_struct_support) {
        try file.writeAll("pub fn getLibraryStructNames(library: []const u8) ?[]const []const u8 {\n");
    } else {
        try file.writeAll("pub fn getLibraryStructNames(_: []const u8) ?[]const []const u8 {\n");
    }
    for (libraries) |lib| {
        if (lib.auto_discover and !lib.glew) {
            var buf: [256]u8 = undefined;
            const len = std.fmt.bufPrint(&buf, "    if (std.mem.eql(u8, library, \"{s}\")) return {s}_binding.getStructNames();\n", .{ lib.name, lib.name }) catch continue;
            try file.writeAll(len);
        }
    }
    try file.writeAll("    return null;\n}\n\n");

    // Generate getStructInfo dispatch
    try file.writeAll("/// Get struct info by name\n");
    if (has_struct_support) {
        try file.writeAll("pub fn getStructInfo(library: []const u8, struct_name: []const u8) ?ffi_autogen.StructInfo {\n");
    } else {
        try file.writeAll("pub fn getStructInfo(_: []const u8, _: []const u8) ?ffi_autogen.StructInfo {\n");
    }
    for (libraries) |lib| {
        if (lib.auto_discover and !lib.glew) {
            var buf: [256]u8 = undefined;
            const len = std.fmt.bufPrint(&buf, "    if (std.mem.eql(u8, library, \"{s}\")) return {s}_binding.getStruct(struct_name);\n", .{ lib.name, lib.name }) catch continue;
            try file.writeAll(len);
        }
    }
    try file.writeAll("    return null;\n}\n\n");

    // Generate generateStructCode dispatch
    try file.writeAll("/// Generate Smalltalk code for all structs in a library\n");
    if (has_struct_support) {
        try file.writeAll("pub fn generateStructCode(library: []const u8, writer: anytype) !bool {\n");
    } else {
        try file.writeAll("pub fn generateStructCode(_: []const u8, _: anytype) !bool {\n");
    }
    for (libraries) |lib| {
        if (lib.auto_discover and !lib.glew) {
            var buf: [256]u8 = undefined;
            const len = std.fmt.bufPrint(&buf, "    if (std.mem.eql(u8, library, \"{s}\")) {{\n        try {s}_binding.generateStructCode(writer);\n        return true;\n    }}\n", .{ lib.name, lib.name }) catch continue;
            try file.writeAll(len);
        }
    }
    try file.writeAll("    return false;\n}\n");

    // Add GLEW helper functions if any library uses GLEW
    if (has_glew) {
        try file.writeAll(
            \\
            \\/// Set glewExperimental to GL_TRUE (required for core profile contexts)
            \\pub fn setGlewExperimental(value: bool) void {
            \\    GL.glewExperimental = if (value) GL.GL_TRUE else GL.GL_FALSE;
            \\}
            \\
            \\/// Get an OpenGL function pointer by name using glXGetProcAddress
            \\/// Returns the function pointer address for use with runtime FFI
            \\/// NOTE: Requires a valid OpenGL context to be current
            \\pub fn getGLEWFunctionPointer(name: []const u8) ?usize {
            \\    // Create null-terminated string for glXGetProcAddress
            \\    var name_buf: [256]u8 = undefined;
            \\    if (name.len >= name_buf.len) return null;
            \\    @memcpy(name_buf[0..name.len], name);
            \\    name_buf[name.len] = 0;
            \\
            \\    // Use glXGetProcAddress to dynamically get the function pointer
            \\    const proc = GLX.glXGetProcAddress(@ptrCast(&name_buf));
            \\    if (proc) |p| {
            \\        return @intFromPtr(p);
            \\    }
            \\    return null;
            \\}
            \\
        );
    }
}
