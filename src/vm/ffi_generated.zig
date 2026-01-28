//! Auto-generated FFI bindings
//! Generated from ffi-config.json by tools/gen_ffi.zig
//! DO NOT EDIT - regenerate with: zig build gen-ffi

const std = @import("std");

/// LibC library imports
pub const LibC = @cImport({
});

/// LibMath library imports
pub const LibMath = @cImport({
});

/// OpenGL library imports
pub const OpenGL = @cImport({
});

/// FacilIO library imports
pub const FacilIO = @cImport({
    @cInclude("/home/jacob/programming/ZigSmalltalk/vendor/facil.io/lib/facil/fiobj/fiobj.h");
    @cInclude("/home/jacob/programming/ZigSmalltalk/vendor/facil.io/lib/facil/http/http.h");
    @cInclude("/home/jacob/programming/ZigSmalltalk/vendor/facil.io/lib/facil/http/websockets.h");
});

/// Combined C imports (for backwards compatibility)
pub const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("math.h");
});
/// List of configured libraries
pub const library_names = [_][]const u8{
    "LibC",
    "LibMath",
    "OpenGL",
    "FacilIO",
};

/// Function lists for each library (empty = auto-discover)
pub const library_functions = struct {
    pub const LibC = [_][]const u8{
    };
    pub const LibMath = [_][]const u8{
    };
    pub const OpenGL = [_][]const u8{
    };
    pub const FacilIO = [_][]const u8{
        "fio_start",
        "fio_stop",
        "http_listen",
        "http_send_body",
        "http_set_header",
        "http_finish",
        "http_send_error",
        "websocket_write",
        "websocket_close",
        "websocket_attach",
    };
};

/// Auto-discovery flags (true = discover all functions at compile time)
pub const library_auto = struct {
    pub const LibC = true;
    pub const LibMath = true;
    pub const OpenGL = true;
    pub const FacilIO = false;
};

/// Struct lists for each library (explicit list of structs to export)
pub const library_structs = struct {
    pub const LibC = [_][]const u8{
    };
    pub const LibMath = [_][]const u8{
    };
    pub const OpenGL = [_][]const u8{
    };
    pub const FacilIO = [_][]const u8{
    };
};

// ============================================================================
// Auto-generated library bindings
// ============================================================================

const ffi_autogen = @import("ffi_autogen.zig");

pub const LibC_binding = ffi_autogen.EmptyLibrary("LibC");
pub const LibMath_binding = ffi_autogen.EmptyLibrary("LibMath");
pub const OpenGL_binding = ffi_autogen.EmptyLibrary("OpenGL");
pub const FacilIO_binding = ffi_autogen.FFILibrary(FacilIO, "FacilIO", &library_functions.FacilIO);

/// Call an FFI function by library and function name
pub fn callFFI(library: []const u8, func_name: []const u8, heap: *ffi_autogen.Heap, args: []const ffi_autogen.Value, alloc: std.mem.Allocator) ffi_autogen.FFIError!ffi_autogen.Value {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "OpenGL")) return OpenGL_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "FacilIO")) return FacilIO_binding.call(func_name, heap, args, alloc);
    return ffi_autogen.FFIError.UnknownFunction;
}

/// Get all functions for a library
pub fn getLibraryFunctions(library: []const u8) ?[]const ffi_autogen.FFIFunction {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.functions;
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.functions;
    if (std.mem.eql(u8, library, "OpenGL")) return OpenGL_binding.functions;
    if (std.mem.eql(u8, library, "FacilIO")) return FacilIO_binding.functions;
    return null;
}

/// Get function names for a library
pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "OpenGL")) return OpenGL_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "FacilIO")) return FacilIO_binding.getFunctionNames();
    return null;
}

/// Get struct names for a library
pub fn getLibraryStructNames(_: []const u8) ?[]const []const u8 {
    return null;
}

/// Get struct info by name
pub fn getStructInfo(_: []const u8, _: []const u8) ?ffi_autogen.StructInfo {
    return null;
}

/// Generate Smalltalk code for all structs in a library
pub fn generateStructCode(_: []const u8, _: anytype) !bool {
    return false;
}
