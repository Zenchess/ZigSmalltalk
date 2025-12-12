//! Auto-generated FFI bindings
//! Generated from ffi-config.json by tools/gen_ffi.zig
//! DO NOT EDIT - regenerate with: zig build gen-ffi

const std = @import("std");

/// LibC library imports
pub const LibC = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("time.h");
});

/// LibMath library imports
pub const LibMath = @cImport({
    @cInclude("math.h");
});

/// Raylib library imports
pub const Raylib = @cImport({
    @cInclude("raylib.h");
});

/// Combined C imports (for backwards compatibility)
pub const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("time.h");
    @cInclude("math.h");
    @cInclude("raylib.h");
});

/// List of configured libraries
pub const library_names = [_][]const u8{
    "LibC",
    "LibMath",
    "Raylib",
};

/// Function lists for each library (empty = auto-discover)
pub const library_functions = struct {
    pub const LibC = [_][]const u8{
        "puts",
        "putchar",
        "getchar",
        "strlen",
        "strcmp",
        "strncmp",
        "memset",
        "memcpy",
        "memmove",
        "memcmp",
        "malloc",
        "calloc",
        "realloc",
        "free",
        "atoi",
        "atol",
        "atof",
        "abs",
        "labs",
        "rand",
        "srand",
        "time",
        "clock",
        "system",
        "getenv",
        "exit",
    };
    pub const LibMath = [_][]const u8{
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "atan2",
        "sinh",
        "cosh",
        "tanh",
        "exp",
        "log",
        "log10",
        "log2",
        "pow",
        "sqrt",
        "cbrt",
        "ceil",
        "floor",
        "round",
        "trunc",
        "fabs",
        "fmod",
        "fmin",
        "fmax",
        "hypot",
    };
    pub const Raylib = [_][]const u8{
    };
};

/// Auto-discovery flags (true = discover all functions at compile time)
pub const library_auto = struct {
    pub const LibC = false;
    pub const LibMath = false;
    pub const Raylib = true;
};

// ============================================================================
// Auto-generated library bindings
// ============================================================================

const ffi_autogen = @import("ffi_autogen.zig");

pub const LibC_binding = ffi_autogen.FFILibrary(LibC, "LibC", &library_functions.LibC);
pub const LibMath_binding = ffi_autogen.FFILibrary(LibMath, "LibMath", &library_functions.LibMath);
pub const Raylib_binding = ffi_autogen.FFILibraryWithStructs(Raylib, "Raylib");

/// Call an FFI function by library and function name
pub fn callFFI(library: []const u8, func_name: []const u8, heap: *ffi_autogen.Heap, args: []const ffi_autogen.Value, alloc: std.mem.Allocator) ffi_autogen.FFIError!ffi_autogen.Value {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.call(func_name, heap, args, alloc);
    return ffi_autogen.FFIError.UnknownFunction;
}

/// Get all functions for a library
pub fn getLibraryFunctions(library: []const u8) ?[]const ffi_autogen.FFIFunction {
    if (std.mem.eql(u8, library, "LibC")) return &LibC_binding.functions;
    if (std.mem.eql(u8, library, "LibMath")) return &LibMath_binding.functions;
    if (std.mem.eql(u8, library, "Raylib")) return &Raylib_binding.functions;
    return null;
}

/// Get function names for a library
pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getFunctionNames();
    return null;
}

/// Get struct names for a library
pub fn getLibraryStructNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getStructNames();
    // Other libraries don't have struct bindings yet
    return null;
}

/// Get struct info by name
pub fn getStructInfo(library: []const u8, struct_name: []const u8) ?ffi_autogen.StructInfo {
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getStruct(struct_name);
    return null;
}

/// Generate Smalltalk code for all structs in a library
pub fn generateStructCode(library: []const u8, writer: anytype) !bool {
    if (std.mem.eql(u8, library, "Raylib")) {
        try Raylib_binding.generateStructCode(writer);
        return true;
    }
    return false;
}
