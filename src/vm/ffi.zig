const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");

const Value = object.Value;
const Object = object.Object;
const Heap = memory.Heap;

/// Import generated FFI bindings
/// Regenerate with: zig build gen-ffi
const ffi_gen = @import("ffi_generated.zig");
const c = ffi_gen.c;

/// FFI Type enumeration for marshalling
pub const FFIType = enum(u8) {
    void = 0,
    int8 = 1,
    int16 = 2,
    int32 = 3,
    int64 = 4,
    uint8 = 5,
    uint16 = 6,
    uint32 = 7,
    uint64 = 8,
    float32 = 9,
    float64 = 10,
    pointer = 11,
    string = 12, // null-terminated C string
    size_t = 13,
};

/// Errors that can occur during FFI operations
pub const FFIError = error{
    InvalidType,
    NullPointer,
    StringTooLong,
    AllocationFailed,
    TypeMismatch,
    InvalidPointer,
};

/// External pointer wrapper - stores a raw C pointer
pub const ExternalPointer = struct {
    address: usize,

    pub fn isNull(self: ExternalPointer) bool {
        return self.address == 0;
    }

    pub fn asPtr(self: ExternalPointer, comptime T: type) ?*T {
        if (self.address == 0) return null;
        return @ptrFromInt(self.address);
    }
};

// =============================================================================
// Type Marshalling: Smalltalk Value <-> C Types
// =============================================================================

/// Convert a Smalltalk Value to a C integer type
pub fn valueToInt(comptime T: type, val: Value) FFIError!T {
    if (val.isSmallInt()) {
        const int_val = val.asSmallInt();
        return @intCast(int_val);
    }
    return FFIError.TypeMismatch;
}

/// Convert a C integer to a Smalltalk Value
pub fn intToValue(comptime T: type, val: T) Value {
    const as_i61: i61 = @intCast(val);
    return Value.fromSmallInt(as_i61);
}

/// Convert a Smalltalk Value to a C float/double
pub fn valueToFloat(comptime T: type, val: Value, heap: *Heap) FFIError!T {
    if (val.isSmallInt()) {
        const int_val = val.asSmallInt();
        return @floatFromInt(int_val);
    }
    if (val.isObject()) {
        const obj = val.asObject();
        if (obj.header.class_index == Heap.CLASS_FLOAT) {
            const bytes = obj.bytes(8);
            const float_val: f64 = @bitCast(bytes[0..8].*);
            return @floatCast(float_val);
        }
    }
    _ = heap;
    return FFIError.TypeMismatch;
}

/// Convert a C float/double to a Smalltalk Value
pub fn floatToValue(comptime T: type, val: T, heap: *Heap) FFIError!Value {
    const as_f64: f64 = @floatCast(val);
    const float_val = heap.allocateFloat(as_f64) catch return FFIError.AllocationFailed;
    return float_val;
}

/// Convert a Smalltalk String to a C string pointer
/// Returns a pointer to the string data (NOT null-terminated in general)
/// For null-terminated, use valueToStringZ
pub fn valueToString(val: Value) FFIError![]const u8 {
    if (!val.isObject()) return FFIError.TypeMismatch;
    const obj = val.asObject();
    if (obj.header.class_index != Heap.CLASS_STRING) return FFIError.TypeMismatch;
    return obj.bytes(obj.header.size);
}

/// Convert a Smalltalk String to a null-terminated C string
/// Allocates memory using the provided allocator
pub fn valueToStringZ(val: Value, allocator: std.mem.Allocator) FFIError![:0]const u8 {
    const str = try valueToString(val);
    return allocator.dupeZ(u8, str) catch return FFIError.AllocationFailed;
}

/// Convert a C string to a Smalltalk String Value
pub fn stringToValue(str: [*:0]const u8, heap: *Heap) FFIError!Value {
    const len = std.mem.len(str);
    const string_obj = heap.allocateString(str[0..len]) catch return FFIError.AllocationFailed;
    return Value.fromObject(string_obj);
}

/// Convert a Smalltalk Value to a raw pointer (for ExternalPointer objects or ByteArray)
pub fn valueToPointer(val: Value) FFIError!?*anyopaque {
    if (val.isNil()) return null;
    if (val.isSmallInt()) {
        // Treat SmallInteger as a raw address
        const addr = val.asSmallInt();
        if (addr == 0) return null;
        // Validate that address is non-negative before converting to pointer
        if (addr < 0) return FFIError.InvalidPointer;
        return @ptrFromInt(@as(usize, @intCast(addr)));
    }
    if (!val.isObject()) return FFIError.TypeMismatch;
    const obj = val.asObject();

    // ByteArray - return pointer to data
    if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) {
        const bytes = obj.bytes(obj.header.size);
        return @ptrCast(bytes.ptr);
    }

    // String - return pointer to data
    if (obj.header.class_index == Heap.CLASS_STRING) {
        const bytes = obj.bytes(obj.header.size);
        return @ptrCast(bytes.ptr);
    }

    return FFIError.TypeMismatch;
}

/// Convert a raw pointer to a Smalltalk Value (as SmallInteger address)
pub fn pointerToValue(ptr: ?*anyopaque) Value {
    if (ptr == null) return Value.nil;
    const addr: usize = @intFromPtr(ptr);
    // Check if address fits in SmallInt range (i61)
    const max_small_int: usize = (@as(usize, 1) << 60) - 1;
    if (addr > max_small_int) {
        // Address too large for SmallInt - return nil as a fallback
        // In a full implementation, this would return an ExternalAddress object
        return Value.nil;
    }
    return Value.fromSmallInt(@intCast(addr));
}

// =============================================================================
// LibC Wrapper Functions
// These directly call C functions with proper marshalling
// =============================================================================

pub fn libc_strlen(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const str = try valueToString(args[0]);
    return intToValue(usize, str.len);
}

pub fn libc_malloc(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const size = try valueToInt(usize, args[0]);
    const ptr = c.malloc(size);
    return pointerToValue(ptr);
}

pub fn libc_free(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]);
    c.free(ptr);
    return Value.nil;
}

pub fn libc_memset(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 3) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]);
    const val = try valueToInt(c_int, args[1]);
    const size = try valueToInt(usize, args[2]);
    _ = c.memset(ptr, val, size);
    return pointerToValue(ptr);
}

pub fn libc_memcpy(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 3) return FFIError.InvalidType;
    const dest = try valueToPointer(args[0]);
    const src = try valueToPointer(args[1]);
    const size = try valueToInt(usize, args[2]);
    _ = c.memcpy(dest, src, size);
    return pointerToValue(dest);
}

pub fn libc_puts(heap: *Heap, args: []Value, allocator: std.mem.Allocator) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const str = try valueToStringZ(args[0], allocator);
    defer allocator.free(str);
    const result = c.puts(str.ptr);
    return intToValue(c_int, result);
}

// Math functions
pub fn libc_sin(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.sin(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_cos(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.cos(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_sqrt(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.sqrt(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_pow(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 2) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const y = try valueToFloat(f64, args[1], heap);
    const result = c.pow(x, y);
    return floatToValue(f64, result, heap);
}

pub fn libc_exp(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.exp(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_log(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.log(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_floor(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.floor(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_ceil(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.ceil(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_abs_float(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.fabs(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_atan2(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 2) return FFIError.InvalidType;
    const y = try valueToFloat(f64, args[0], heap);
    const x = try valueToFloat(f64, args[1], heap);
    const result = c.atan2(y, x);
    return floatToValue(f64, result, heap);
}

pub fn libc_tan(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.tan(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_asin(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.asin(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_acos(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.acos(x);
    return floatToValue(f64, result, heap);
}

pub fn libc_atan(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const x = try valueToFloat(f64, args[0], heap);
    const result = c.atan(x);
    return floatToValue(f64, result, heap);
}

// =============================================================================
// Memory read/write operations for ExternalPointer
// =============================================================================

pub fn readInt8(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const typed_ptr: *i8 = @ptrCast(@alignCast(ptr));
    return intToValue(i8, typed_ptr.*);
}

pub fn readInt16(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const typed_ptr: *i16 = @ptrCast(@alignCast(ptr));
    return intToValue(i16, typed_ptr.*);
}

pub fn readInt32(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const typed_ptr: *i32 = @ptrCast(@alignCast(ptr));
    return intToValue(i32, typed_ptr.*);
}

pub fn readInt64(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 1) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const typed_ptr: *i64 = @ptrCast(@alignCast(ptr));
    // Note: May overflow if value > 2^60
    return intToValue(i64, typed_ptr.*);
}

pub fn readFloat64(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 1) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const typed_ptr: *f64 = @ptrCast(@alignCast(ptr));
    return floatToValue(f64, typed_ptr.*, heap);
}

pub fn writeInt8(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 2) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const val = try valueToInt(i8, args[1]);
    const typed_ptr: *i8 = @ptrCast(@alignCast(ptr));
    typed_ptr.* = val;
    return Value.nil;
}

pub fn writeInt32(heap: *Heap, args: []Value) FFIError!Value {
    _ = heap;
    if (args.len < 2) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const val = try valueToInt(i32, args[1]);
    const typed_ptr: *i32 = @ptrCast(@alignCast(ptr));
    typed_ptr.* = val;
    return Value.nil;
}

pub fn writeFloat64(heap: *Heap, args: []Value) FFIError!Value {
    if (args.len < 2) return FFIError.InvalidType;
    const ptr = try valueToPointer(args[0]) orelse return FFIError.NullPointer;
    const val = try valueToFloat(f64, args[1], heap);
    const typed_ptr: *f64 = @ptrCast(@alignCast(ptr));
    typed_ptr.* = val;
    return Value.nil;
}

// =============================================================================
// Primitive dispatcher - called from primitives.zig
// =============================================================================

/// FFI sub-primitive numbers (offset from base)
pub const FFIPrimitive = enum(u8) {
    // Memory management
    malloc = 0,
    free = 1,
    memset = 2,
    memcpy = 3,

    // String functions
    strlen = 10,
    puts = 11,

    // Math functions
    sin = 20,
    cos = 21,
    sqrt = 22,
    pow = 23,
    exp = 24,
    log = 25,
    floor = 26,
    ceil = 27,
    fabs = 28,
    atan2 = 29,
    tan = 30,
    asin = 31,
    acos = 32,
    atan = 33,

    // Memory read
    read_int8 = 40,
    read_int16 = 41,
    read_int32 = 42,
    read_int64 = 43,
    read_float64 = 44,

    // Memory write
    write_int8 = 50,
    write_int32 = 52,
    write_float64 = 54,
};

/// Dispatch an FFI call based on sub-primitive number
pub fn dispatchFFI(sub_prim: u8, heap: *Heap, args: []Value, allocator: std.mem.Allocator) FFIError!Value {
    const prim: FFIPrimitive = @enumFromInt(sub_prim);
    return switch (prim) {
        .malloc => libc_malloc(heap, args),
        .free => libc_free(heap, args),
        .memset => libc_memset(heap, args),
        .memcpy => libc_memcpy(heap, args),
        .strlen => libc_strlen(heap, args),
        .puts => libc_puts(heap, args, allocator),
        .sin => libc_sin(heap, args),
        .cos => libc_cos(heap, args),
        .sqrt => libc_sqrt(heap, args),
        .pow => libc_pow(heap, args),
        .exp => libc_exp(heap, args),
        .log => libc_log(heap, args),
        .floor => libc_floor(heap, args),
        .ceil => libc_ceil(heap, args),
        .fabs => libc_abs_float(heap, args),
        .atan2 => libc_atan2(heap, args),
        .tan => libc_tan(heap, args),
        .asin => libc_asin(heap, args),
        .acos => libc_acos(heap, args),
        .atan => libc_atan(heap, args),
        .read_int8 => readInt8(heap, args),
        .read_int16 => readInt16(heap, args),
        .read_int32 => readInt32(heap, args),
        .read_int64 => readInt64(heap, args),
        .read_float64 => readFloat64(heap, args),
        .write_int8 => writeInt8(heap, args),
        .write_int32 => writeInt32(heap, args),
        .write_float64 => writeFloat64(heap, args),
    };
}
