//! Runtime FFI using libffi
//!
//! This module provides dynamic function calling capability using libffi.
//! Unlike the compile-time FFI, this can call any function pointer at runtime,
//! including GLEW's dynamically-loaded OpenGL functions.

const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");

pub const Value = object.Value;
pub const Heap = memory.Heap;

/// libffi C imports
const ffi = @cImport({
    @cInclude("ffi.h");
});

/// FFI type descriptors
pub const FFIType = enum {
    void,
    uint8,
    sint8,
    uint16,
    sint16,
    uint32,
    sint32,
    uint64,
    sint64,
    float32,
    float64,
    pointer,
    string,
    string_array, // char** - array of strings
};

/// Convert FFIType to libffi type
fn toFFIType(t: FFIType) *ffi.ffi_type {
    return switch (t) {
        .void => &ffi.ffi_type_void,
        .uint8 => &ffi.ffi_type_uint8,
        .sint8 => &ffi.ffi_type_sint8,
        .uint16 => &ffi.ffi_type_uint16,
        .sint16 => &ffi.ffi_type_sint16,
        .uint32 => &ffi.ffi_type_uint32,
        .sint32 => &ffi.ffi_type_sint32,
        .uint64 => &ffi.ffi_type_uint64,
        .sint64 => &ffi.ffi_type_sint64,
        .float32 => &ffi.ffi_type_float,
        .float64 => &ffi.ffi_type_double,
        .pointer, .string, .string_array => &ffi.ffi_type_pointer,
    };
}

/// Errors from runtime FFI
pub const RuntimeFFIError = error{
    PrepFailed,
    CallFailed,
    TypeMismatch,
    TooManyArguments,
    AllocationFailed,
    InvalidFunctionPointer,
};

/// Maximum arguments for runtime FFI calls
const MAX_ARGS = 32;

/// Runtime FFI call context
pub const CallContext = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,

    // Temporary storage for argument values
    arg_values: [MAX_ARGS]u64 = undefined,
    arg_pointers: [MAX_ARGS]?*anyopaque = undefined,
    // Fixed-size array for temporary string allocations
    string_temps: [MAX_ARGS]?[]u8 = [_]?[]u8{null} ** MAX_ARGS,
    string_count: usize = 0,
    // Fixed-size array for temporary pointer array allocations (char**)
    ptr_array_temps: [MAX_ARGS]?[][*c]u8 = [_]?[][*c]u8{null} ** MAX_ARGS,
    ptr_array_count: usize = 0,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) CallContext {
        return .{
            .allocator = allocator,
            .heap = heap,
        };
    }

    pub fn deinit(self: *CallContext) void {
        // Free temporary strings
        for (self.string_temps[0..self.string_count]) |maybe_s| {
            if (maybe_s) |s| {
                self.allocator.free(s);
            }
        }
        // Free temporary pointer arrays
        for (self.ptr_array_temps[0..self.ptr_array_count]) |maybe_arr| {
            if (maybe_arr) |arr| {
                self.allocator.free(arr);
            }
        }
    }

    pub fn addStringTemp(self: *CallContext, s: []u8) RuntimeFFIError!void {
        if (self.string_count >= MAX_ARGS) {
            // Can't track this allocation - must free it now to avoid leak
            self.allocator.free(s);
            return RuntimeFFIError.TooManyArguments;
        }
        self.string_temps[self.string_count] = s;
        self.string_count += 1;
    }

    pub fn addPtrArrayTemp(self: *CallContext, arr: [][*c]u8) RuntimeFFIError!void {
        if (self.ptr_array_count >= MAX_ARGS) {
            // Can't track this allocation - must free it now to avoid leak
            self.allocator.free(arr);
            return RuntimeFFIError.TooManyArguments;
        }
        self.ptr_array_temps[self.ptr_array_count] = arr;
        self.ptr_array_count += 1;
    }
};

/// Convert Smalltalk Value to C value for libffi
fn valueToCValue(ctx: *CallContext, val: Value, typ: FFIType, out: *u64) RuntimeFFIError!void {
    switch (typ) {
        .void => {},
        .uint8, .sint8, .uint16, .sint16, .uint32, .sint32, .uint64, .sint64 => {
            if (val.isSmallInt()) {
                out.* = @bitCast(@as(i64, val.asSmallInt()));
            } else {
                return RuntimeFFIError.TypeMismatch;
            }
        },
        .float32 => {
            var f: f32 = undefined;
            if (val.isSmallInt()) {
                f = @floatFromInt(val.asSmallInt());
            } else if (val.isObject()) {
                const obj = val.asObject();
                if (obj.header.class_index == Heap.CLASS_FLOAT) {
                    const bytes = obj.bytes(8);
                    const f64_val: f64 = @bitCast(bytes[0..8].*);
                    f = @floatCast(f64_val);
                } else {
                    return RuntimeFFIError.TypeMismatch;
                }
            } else {
                return RuntimeFFIError.TypeMismatch;
            }
            const f_bits: u32 = @bitCast(f);
            out.* = f_bits;
        },
        .float64 => {
            var f: f64 = undefined;
            if (val.isSmallInt()) {
                f = @floatFromInt(val.asSmallInt());
            } else if (val.isObject()) {
                const obj = val.asObject();
                if (obj.header.class_index == Heap.CLASS_FLOAT) {
                    const bytes = obj.bytes(8);
                    f = @bitCast(bytes[0..8].*);
                } else {
                    return RuntimeFFIError.TypeMismatch;
                }
            } else {
                return RuntimeFFIError.TypeMismatch;
            }
            out.* = @bitCast(f);
        },
        .pointer => {
            if (val.isNil()) {
                out.* = 0;
            } else if (val.isSmallInt()) {
                const addr = val.asSmallInt();
                if (addr < 0) {
                    return RuntimeFFIError.TypeMismatch; // Negative addresses invalid
                }
                out.* = @intCast(addr);
            } else if (val.isObject()) {
                const obj = val.asObject();
                // ByteArray - return pointer to data
                if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) {
                    const bytes = obj.bytes(obj.header.size);
                    out.* = @intFromPtr(bytes.ptr);
                } else {
                    return RuntimeFFIError.TypeMismatch;
                }
            } else {
                return RuntimeFFIError.TypeMismatch;
            }
        },
        .string => {
            if (val.isNil()) {
                out.* = 0;
            } else if (val.isObject()) {
                const obj = val.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING or
                    obj.header.class_index == Heap.CLASS_SYMBOL)
                {
                    const bytes = obj.bytes(obj.header.size);
                    // Allocate null-terminated copy
                    const c_str = ctx.allocator.alloc(u8, bytes.len + 1) catch return RuntimeFFIError.AllocationFailed;
                    @memcpy(c_str[0..bytes.len], bytes);
                    c_str[bytes.len] = 0;
                    try ctx.addStringTemp(c_str);
                    out.* = @intFromPtr(c_str.ptr);
                } else {
                    return RuntimeFFIError.TypeMismatch;
                }
            } else {
                return RuntimeFFIError.TypeMismatch;
            }
        },
        .string_array => {
            // Handle char** - array of strings
            if (val.isNil()) {
                out.* = 0;
            } else if (val.isObject()) {
                const obj = val.asObject();
                // Must be an Array
                if (obj.header.class_index == Heap.CLASS_ARRAY) {
                    const size = obj.header.size;
                    // Allocate array of char* pointers
                    const ptr_array = ctx.allocator.alloc([*c]u8, size) catch return RuntimeFFIError.AllocationFailed;
                    try ctx.addPtrArrayTemp(ptr_array);

                    // Convert each element to a null-terminated string
                    const fields = obj.fields(size);
                    for (fields, 0..) |elem, i| {
                        if (elem.isNil()) {
                            ptr_array[i] = null;
                        } else if (elem.isObject()) {
                            const str_obj = elem.asObject();
                            if (str_obj.header.class_index == Heap.CLASS_STRING or
                                str_obj.header.class_index == Heap.CLASS_SYMBOL)
                            {
                                const bytes = str_obj.bytes(str_obj.header.size);
                                // Allocate null-terminated copy
                                const c_str = ctx.allocator.alloc(u8, bytes.len + 1) catch return RuntimeFFIError.AllocationFailed;
                                @memcpy(c_str[0..bytes.len], bytes);
                                c_str[bytes.len] = 0;
                                try ctx.addStringTemp(c_str);
                                ptr_array[i] = @ptrCast(c_str.ptr);
                            } else {
                                return RuntimeFFIError.TypeMismatch;
                            }
                        } else {
                            return RuntimeFFIError.TypeMismatch;
                        }
                    }
                    out.* = @intFromPtr(ptr_array.ptr);
                } else {
                    return RuntimeFFIError.TypeMismatch;
                }
            } else {
                return RuntimeFFIError.TypeMismatch;
            }
        },
    }
}

/// Convert C return value to Smalltalk Value
fn cValueToValue(heap: *Heap, result: u64, typ: FFIType) RuntimeFFIError!Value {
    return switch (typ) {
        .void => Value.nil,
        .uint8, .sint8, .uint16, .sint16, .uint32, .sint32 => blk: {
            const as_i64: i64 = @bitCast(result);
            const as_i61: i61 = @intCast(as_i64);
            break :blk Value.fromSmallInt(as_i61);
        },
        .uint64, .sint64 => blk: {
            const as_i64: i64 = @bitCast(result);
            if (as_i64 >= -(1 << 60) and as_i64 < (1 << 60)) {
                const as_i61: i61 = @intCast(as_i64);
                break :blk Value.fromSmallInt(as_i61);
            } else {
                // TODO: Return as LargeInteger
                break :blk Value.nil;
            }
        },
        .float32 => blk: {
            const f_bits: u32 = @truncate(result);
            const f: f32 = @bitCast(f_bits);
            const f64_val: f64 = @floatCast(f);
            const float_obj = heap.allocateFloat(f64_val) catch return RuntimeFFIError.AllocationFailed;
            break :blk float_obj;
        },
        .float64 => blk: {
            const f: f64 = @bitCast(result);
            const float_obj = heap.allocateFloat(f) catch return RuntimeFFIError.AllocationFailed;
            break :blk float_obj;
        },
        .pointer, .string, .string_array => blk: {
            if (result == 0) {
                break :blk Value.nil;
            }
            // Check if pointer fits in SmallInt (61 bits signed)
            const max_small_int: u64 = (@as(u64, 1) << 60) - 1;
            if (result > max_small_int) {
                // Address too large for SmallInt - return nil
                break :blk Value.nil;
            }
            const as_i61: i61 = @intCast(result);
            break :blk Value.fromSmallInt(as_i61);
        },
    };
}

/// Call a function pointer with the given arguments
///
/// fn_ptr: The function pointer to call
/// ret_type: The return type
/// arg_types: Array of argument types
/// args: Array of Smalltalk Values to pass
pub fn callFunction(
    fn_ptr: *const anyopaque,
    ret_type: FFIType,
    arg_types: []const FFIType,
    args: []const Value,
    heap: *Heap,
    allocator: std.mem.Allocator,
) RuntimeFFIError!Value {
    if (arg_types.len != args.len) {
        return RuntimeFFIError.TypeMismatch;
    }
    if (arg_types.len > MAX_ARGS) {
        return RuntimeFFIError.TooManyArguments;
    }
    if (@intFromPtr(fn_ptr) == 0) {
        return RuntimeFFIError.InvalidFunctionPointer;
    }

    var ctx = CallContext.init(allocator, heap);
    defer ctx.deinit();

    // Prepare argument types for libffi
    var ffi_arg_types: [MAX_ARGS]*ffi.ffi_type = undefined;
    for (arg_types, 0..) |t, i| {
        ffi_arg_types[i] = toFFIType(t);
    }

    // Prepare CIF (Call Interface)
    var cif: ffi.ffi_cif = undefined;
    const ffi_arg_types_ptr: [*c][*c]ffi.ffi_type = if (arg_types.len > 0) @ptrCast(&ffi_arg_types) else null;
    const status = ffi.ffi_prep_cif(
        &cif,
        ffi.FFI_DEFAULT_ABI,
        @intCast(arg_types.len),
        toFFIType(ret_type),
        ffi_arg_types_ptr,
    );
    if (status != ffi.FFI_OK) {
        return RuntimeFFIError.PrepFailed;
    }

    // Convert arguments
    for (args, 0..) |arg, i| {
        try valueToCValue(&ctx, arg, arg_types[i], &ctx.arg_values[i]);
        ctx.arg_pointers[i] = @ptrCast(&ctx.arg_values[i]);
    }

    // Prepare return value storage
    var ret_value: u64 = 0;

    // Call the function
    ffi.ffi_call(
        &cif,
        @ptrCast(@alignCast(fn_ptr)),
        @ptrCast(&ret_value),
        if (arg_types.len > 0) @ptrCast(&ctx.arg_pointers) else null,
    );

    // Convert return value
    return cValueToValue(heap, ret_value, ret_type);
}

/// Parse a type string into FFIType
/// Supported: "void", "int", "uint", "int32", "uint32", "int64", "uint64",
///            "float", "double", "float32", "float64", "pointer", "string"
pub fn parseType(type_str: []const u8) ?FFIType {
    if (std.mem.eql(u8, type_str, "void")) return .void;
    if (std.mem.eql(u8, type_str, "int") or std.mem.eql(u8, type_str, "int32") or std.mem.eql(u8, type_str, "sint32")) return .sint32;
    if (std.mem.eql(u8, type_str, "uint") or std.mem.eql(u8, type_str, "uint32")) return .uint32;
    if (std.mem.eql(u8, type_str, "int8") or std.mem.eql(u8, type_str, "sint8")) return .sint8;
    if (std.mem.eql(u8, type_str, "uint8")) return .uint8;
    if (std.mem.eql(u8, type_str, "int16") or std.mem.eql(u8, type_str, "sint16")) return .sint16;
    if (std.mem.eql(u8, type_str, "uint16")) return .uint16;
    if (std.mem.eql(u8, type_str, "int64") or std.mem.eql(u8, type_str, "sint64")) return .sint64;
    if (std.mem.eql(u8, type_str, "uint64")) return .uint64;
    if (std.mem.eql(u8, type_str, "float") or std.mem.eql(u8, type_str, "float32")) return .float32;
    if (std.mem.eql(u8, type_str, "double") or std.mem.eql(u8, type_str, "float64")) return .float64;
    if (std.mem.eql(u8, type_str, "pointer") or std.mem.eql(u8, type_str, "ptr")) return .pointer;
    if (std.mem.eql(u8, type_str, "string") or std.mem.eql(u8, type_str, "cstring")) return .string;
    if (std.mem.eql(u8, type_str, "char**") or std.mem.eql(u8, type_str, "string**") or
        std.mem.eql(u8, type_str, "string_array") or std.mem.eql(u8, type_str, "cstring*")) return .string_array;
    return null;
}

/// Call a function by parsing a signature string
/// Signature format: "returnType(argType1,argType2,...)"
/// Example: "uint32(uint32,pointer,uint32,pointer)"
pub fn callWithSignature(
    fn_ptr: *const anyopaque,
    signature: []const u8,
    args: []const Value,
    heap: *Heap,
    allocator: std.mem.Allocator,
) RuntimeFFIError!Value {
    // Parse signature
    const paren_start = std.mem.indexOf(u8, signature, "(") orelse return RuntimeFFIError.TypeMismatch;
    const paren_end = std.mem.lastIndexOf(u8, signature, ")") orelse return RuntimeFFIError.TypeMismatch;

    const ret_str = signature[0..paren_start];
    const args_str = signature[paren_start + 1 .. paren_end];

    const ret_type = parseType(ret_str) orelse return RuntimeFFIError.TypeMismatch;

    // Parse argument types
    var arg_types: [MAX_ARGS]FFIType = undefined;
    var arg_count: usize = 0;

    if (args_str.len > 0) {
        var iter = std.mem.splitSequence(u8, args_str, ",");
        while (iter.next()) |type_str| {
            if (arg_count >= MAX_ARGS) return RuntimeFFIError.TooManyArguments;
            arg_types[arg_count] = parseType(type_str) orelse return RuntimeFFIError.TypeMismatch;
            arg_count += 1;
        }
    }

    return callFunction(fn_ptr, ret_type, arg_types[0..arg_count], args, heap, allocator);
}

// Test
test "runtime ffi types" {
    try std.testing.expectEqual(FFIType.sint32, parseType("int").?);
    try std.testing.expectEqual(FFIType.float32, parseType("float").?);
    try std.testing.expectEqual(FFIType.pointer, parseType("pointer").?);
    try std.testing.expectEqual(FFIType.string_array, parseType("char**").?);
    try std.testing.expectEqual(FFIType.string_array, parseType("string_array").?);
}
