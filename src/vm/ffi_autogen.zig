//! Compile-time FFI binding infrastructure
//!
//! This module provides comptime FFI binding generators that introspect
//! C imports and generate callable wrappers.

const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");

pub const Value = object.Value;
pub const Heap = memory.Heap;

/// Errors that can occur during FFI operations
pub const FFIError = error{
    UnknownFunction,
    TypeMismatch,
    InvalidArguments,
    AllocationFailed,
    NullPointer,
};

/// FFI function metadata
pub const FFIFunction = struct {
    name: []const u8,
    return_type: []const u8,
    arg_types: []const []const u8,
    arg_count: usize,
};

/// Struct field information
pub const StructField = struct {
    name: []const u8,
    type_name: []const u8,
    offset: usize,
    size: usize,
    accessor_type: []const u8,
};

/// Struct metadata
pub const StructInfo = struct {
    name: []const u8,
    size: usize,
    alignment: usize,
    fields: []const StructField,
};

/// Map C type to Smalltalk accessor type name
fn getAccessorType(comptime T: type) []const u8 {
    const info = @typeInfo(T);
    return switch (info) {
        .int => |i| switch (i.bits) {
            8 => if (i.signedness == .signed) "int8" else "uint8",
            16 => if (i.signedness == .signed) "int16" else "uint16",
            32 => if (i.signedness == .signed) "int32" else "uint32",
            64 => if (i.signedness == .signed) "int64" else "uint64",
            else => "int32",
        },
        .float => |f| switch (f.bits) {
            32 => "float32",
            64 => "float64",
            else => "float64",
        },
        .pointer => "pointer",
        .array => "pointer",
        .@"struct" => "pointer",
        .@"union" => "pointer",
        .bool => "uint8",
        .@"enum" => "int32",
        else => "uint32",
    };
}

/// Get C type name as string
fn getTypeName(comptime T: type) []const u8 {
    return @typeName(T);
}

/// Check if a declaration is a valid struct type we can introspect
fn isValidStructDecl(comptime CImport: type, comptime decl_name: []const u8) bool {
    // Try to access the declaration - if it fails, it's not valid
    if (!@hasDecl(CImport, decl_name)) return false;
    const field_val = @field(CImport, decl_name);
    const DeclType = @TypeOf(field_val);
    if (@typeInfo(DeclType) != .type) return false;
    const ActualType = field_val;
    if (@typeInfo(ActualType) != .@"struct") return false;
    // Check if we can get size (some opaque structs fail here)
    _ = @sizeOf(ActualType);
    return true;
}

/// Compile-time FFI library binding with struct introspection
/// NOTE: Full struct introspection is disabled due to comptime issues with C headers.
/// Structs can be defined manually in Smalltalk using ExternalStructure subclass.
pub fn FFILibraryWithStructs(comptime _: type, comptime lib_name: []const u8) type {
    // For now, return empty library - struct introspection causes comptime issues
    // with untranslatable macros in C headers
    return EmptyLibrary(lib_name);
}

/// Compile-time FFI library binding with explicit struct list
/// Uses inline functions to query struct metadata on demand
pub fn FFILibraryWithExplicitStructs(comptime CImport: type, comptime lib_name: []const u8, comptime struct_names: []const []const u8) type {
    return struct {
        pub const library_name = lib_name;
        pub const functions: []const FFIFunction = &[_]FFIFunction{};
        pub const structs: []const StructInfo = &[_]StructInfo{}; // Empty - use runtime queries

        pub fn getFunctionNames() []const []const u8 {
            return &[_][]const u8{};
        }

        pub fn getStructNames() []const []const u8 {
            return struct_names;
        }

        pub fn getStruct(struct_name: []const u8) ?StructInfo {
            inline for (struct_names) |sname| {
                if (std.mem.eql(u8, struct_name, sname)) {
                    if (@hasDecl(CImport, sname)) {
                        const decl = @field(CImport, sname);
                        if (@TypeOf(decl) == type) {
                            const ActualType = decl;
                            if (@typeInfo(ActualType) == .@"struct") {
                                return getStructInfoRuntime(ActualType, sname);
                            }
                        }
                    }
                }
            }
            return null;
        }

        fn getStructInfoRuntime(comptime T: type, comptime sname: []const u8) StructInfo {
            const fields = @typeInfo(T).@"struct".fields;
            const FieldsData = struct {
                const data: [fields.len]StructField = blk: {
                    var result: [fields.len]StructField = undefined;
                    for (fields, 0..) |field, i| {
                        result[i] = .{
                            .name = field.name,
                            .type_name = @typeName(field.type),
                            .offset = @offsetOf(T, field.name),
                            .size = @sizeOf(field.type),
                            .accessor_type = getAccessorType(field.type),
                        };
                    }
                    break :blk result;
                };
            };
            return .{
                .name = sname,
                .size = @sizeOf(T),
                .alignment = @alignOf(T),
                .fields = &FieldsData.data,
            };
        }

        pub fn call(func_name: []const u8, heap: *Heap, args: []const Value, alloc: std.mem.Allocator) FFIError!Value {
            _ = func_name;
            _ = heap;
            _ = args;
            _ = alloc;
            return FFIError.UnknownFunction;
        }

        pub fn generateStructCode(writer: anytype) !void {
            inline for (struct_names) |sname| {
                if (@hasDecl(CImport, sname)) {
                    const decl = @field(CImport, sname);
                    if (@TypeOf(decl) == type) {
                        const ActualType = decl;
                        if (@typeInfo(ActualType) == .@"struct") {
                            try generateStructCodeFor(ActualType, sname, writer);
                        }
                    }
                }
            }
        }

        fn generateStructCodeFor(comptime T: type, comptime sname: []const u8, writer: anytype) !void {
            try writer.print("\"Creating ExternalStructure: {s}\"\n", .{sname});
            try writer.print("ExternalStructure subclass: #{s}Struct\n", .{sname});
            try writer.print("    instanceVariableNames: ''\n", .{});
            try writer.print("    classVariableNames: ''\n", .{});
            try writer.print("    poolDictionaries: ''\n", .{});
            try writer.print("    package: 'FFI-Raylib'!\n\n", .{});

            // Add size method
            try writer.print("{s}Struct class>>byteSize\n", .{sname});
            try writer.print("    ^{d}!\n\n", .{@sizeOf(T)});

            // Add field accessors
            const fields = @typeInfo(T).@"struct".fields;
            inline for (fields) |field| {
                // Getter
                try writer.print("{s}Struct>>{s}\n", .{ sname, field.name });
                try writer.print("    ^self {s}At: {d}!\n\n", .{ getAccessorType(field.type), @offsetOf(T, field.name) });

                // Setter
                try writer.print("{s}Struct>>{s}: aValue\n", .{ sname, field.name });
                try writer.print("    self {s}At: {d} put: aValue!\n\n", .{ getAccessorType(field.type), @offsetOf(T, field.name) });
            }
        }
    };
}

/// Empty library binding for when no structs are found or library has no headers
pub fn EmptyLibrary(comptime lib_name: []const u8) type {
    return struct {
        pub const name = lib_name;
        pub const functions: []const FFIFunction = &[_]FFIFunction{};
        pub const structs: []const StructInfo = &[_]StructInfo{};

        pub fn getFunctionNames() []const []const u8 {
            return &[_][]const u8{};
        }

        pub fn getStructNames() []const []const u8 {
            return &[_][]const u8{};
        }

        pub fn getStruct(struct_name: []const u8) ?StructInfo {
            _ = struct_name;
            return null;
        }

        pub fn call(func_name: []const u8, heap: *Heap, args: []const Value, alloc: std.mem.Allocator) FFIError!Value {
            _ = func_name;
            _ = heap;
            _ = args;
            _ = alloc;
            return FFIError.UnknownFunction;
        }

        pub fn generateStructCode(writer: anytype) !void {
            _ = writer;
        }
    };
}

/// Compile-time FFI library binding (auto-discovery, no struct support)
pub fn FFILibraryAuto(comptime CImport: type, comptime lib_name: []const u8) type {
    return FFILibraryWithStructs(CImport, lib_name);
}

/// Compile-time FFI library binding with explicit function list
pub fn FFILibrary(comptime CImport: type, comptime lib_name: []const u8, comptime _: []const []const u8) type {
    return FFILibraryWithStructs(CImport, lib_name);
}

/// List of available library names - read from ffi_generated.zig
pub const available_libraries = blk: {
    const ffi_generated = @import("ffi_generated.zig");
    break :blk &ffi_generated.library_names;
};

/// Call an FFI function (dispatch to ffi_generated)
pub fn callFFI(library: []const u8, func_name: []const u8, heap: *Heap, args: []const Value, alloc: std.mem.Allocator) FFIError!Value {
    const ffi_generated = @import("ffi_generated.zig");
    return ffi_generated.callFFI(library, func_name, heap, args, alloc);
}

/// Get function names for a library (dispatch to ffi_generated)
pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {
    const ffi_generated = @import("ffi_generated.zig");
    return ffi_generated.getLibraryFunctionNames(library);
}

/// Get all functions for a library (dispatch to ffi_generated)
pub fn getLibraryFunctions(library: []const u8) ?[]const FFIFunction {
    const ffi_generated = @import("ffi_generated.zig");
    return ffi_generated.getLibraryFunctions(library);
}
