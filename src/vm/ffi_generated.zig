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

/// Raylib library imports
pub const Raylib = @cImport({
    @cInclude("C:\\raylib\\include\\raylib.h");
});

/// GLFW library imports
pub const GLFW = @cImport({
    @cInclude("C:\\programming\\ZigSmalltalk\\vendor\\glfw\\glfw-3.4.bin.WIN64\\include\\GLFW\\glfw3.h");
});

/// OpenGL library imports
pub const OpenGL = @cImport({
});

/// OpenXR library imports
pub const OpenXR = @cImport({
    @cInclude("C:\\programming\\ZigSmalltalk\\vendor\\openxr-loader\\include\\openxr\\openxr.h");
});

/// Combined C imports (for backwards compatibility)
pub const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("math.h");
    @cInclude("C:\\raylib\\include\\raylib.h");
    @cInclude("C:\\programming\\ZigSmalltalk\\vendor\\glfw\\glfw-3.4.bin.WIN64\\include\\GLFW\\glfw3.h");
    @cInclude("C:\\programming\\ZigSmalltalk\\vendor\\openxr-loader\\include\\openxr\\openxr.h");
});

/// List of configured libraries
pub const library_names = [_][]const u8{
    "LibC",
    "LibMath",
    "Raylib",
    "GLFW",
    "OpenGL",
    "OpenXR",
};

/// Function lists for each library (empty = auto-discover)
pub const library_functions = struct {
    pub const LibC = [_][]const u8{
    };
    pub const LibMath = [_][]const u8{
    };
    pub const Raylib = [_][]const u8{
    };
    pub const GLFW = [_][]const u8{
    };
    pub const OpenGL = [_][]const u8{
    };
    pub const OpenXR = [_][]const u8{
    };
};

/// Auto-discovery flags (true = discover all functions at compile time)
pub const library_auto = struct {
    pub const LibC = true;
    pub const LibMath = true;
    pub const Raylib = true;
    pub const GLFW = true;
    pub const OpenGL = true;
    pub const OpenXR = true;
};

/// Struct lists for each library (explicit list of structs to export)
pub const library_structs = struct {
    pub const LibC = [_][]const u8{
    };
    pub const LibMath = [_][]const u8{
    };
    pub const Raylib = [_][]const u8{
    };
    pub const GLFW = [_][]const u8{
    };
    pub const OpenGL = [_][]const u8{
    };
    pub const OpenXR = [_][]const u8{
    };
};

// ============================================================================
// Auto-generated library bindings
// ============================================================================

const ffi_autogen = @import("ffi_autogen.zig");

pub const LibC_binding = ffi_autogen.EmptyLibrary("LibC");
pub const LibMath_binding = ffi_autogen.EmptyLibrary("LibMath");
pub const Raylib_binding = ffi_autogen.FFILibraryWithStructs(Raylib, "Raylib");
pub const GLFW_binding = ffi_autogen.FFILibraryWithStructs(GLFW, "GLFW");
pub const OpenGL_binding = ffi_autogen.EmptyLibrary("OpenGL");
pub const OpenXR_binding = ffi_autogen.FFILibraryWithStructs(OpenXR, "OpenXR");

/// Call an FFI function by library and function name
pub fn callFFI(library: []const u8, func_name: []const u8, heap: *ffi_autogen.Heap, args: []const ffi_autogen.Value, alloc: std.mem.Allocator) ffi_autogen.FFIError!ffi_autogen.Value {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "OpenGL")) return OpenGL_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "OpenXR")) return OpenXR_binding.call(func_name, heap, args, alloc);
    return ffi_autogen.FFIError.UnknownFunction;
}

/// Get all functions for a library
pub fn getLibraryFunctions(library: []const u8) ?[]const ffi_autogen.FFIFunction {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.functions;
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.functions;
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.functions;
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.functions;
    if (std.mem.eql(u8, library, "OpenGL")) return OpenGL_binding.functions;
    if (std.mem.eql(u8, library, "OpenXR")) return OpenXR_binding.functions;
    return null;
}

/// Get function names for a library
pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "OpenGL")) return OpenGL_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "OpenXR")) return OpenXR_binding.getFunctionNames();
    return null;
}

/// Get struct names for a library
pub fn getLibraryStructNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getStructNames();
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.getStructNames();
    if (std.mem.eql(u8, library, "OpenXR")) return OpenXR_binding.getStructNames();
    return null;
}

/// Get struct info by name
pub fn getStructInfo(library: []const u8, struct_name: []const u8) ?ffi_autogen.StructInfo {
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getStruct(struct_name);
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.getStruct(struct_name);
    if (std.mem.eql(u8, library, "OpenXR")) return OpenXR_binding.getStruct(struct_name);
    return null;
}

/// Generate Smalltalk code for all structs in a library
pub fn generateStructCode(library: []const u8, writer: anytype) !bool {
    if (std.mem.eql(u8, library, "Raylib")) {
        try Raylib_binding.generateStructCode(writer);
        return true;
    }
    if (std.mem.eql(u8, library, "GLFW")) {
        try GLFW_binding.generateStructCode(writer);
        return true;
    }
    if (std.mem.eql(u8, library, "OpenXR")) {
        try OpenXR_binding.generateStructCode(writer);
        return true;
    }
    return false;
}
