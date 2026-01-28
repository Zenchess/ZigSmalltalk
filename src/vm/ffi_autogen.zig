//! Automatic FFI Binding Generator
//!
//! Uses Zig's comptime to automatically generate Smalltalk wrappers
//! for C functions imported via @cImport.

const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");

pub const Value = object.Value;
pub const Heap = memory.Heap;

/// Errors that can occur during FFI operations
pub const FFIError = error{
    InvalidType,
    NullPointer,
    TypeMismatch,
    AllocationFailed,
    UnknownFunction,
    TooManyArguments,
    NotEnoughArguments,
};

/// Maximum number of arguments for auto-generated FFI calls
const MAX_ARGS = 16;

/// Convert Smalltalk Value to C type
fn valueToCType(comptime T: type, val: Value, heap: *Heap, allocator: std.mem.Allocator) FFIError!T {
    // Handle void
    if (T == void) return {};

    // Handle integers
    if (@typeInfo(T) == .int or @typeInfo(T) == .comptime_int) {
        if (val.isSmallInt()) {
            return @intCast(val.asSmallInt());
        }
        return FFIError.TypeMismatch;
    }

    // Handle floats
    if (@typeInfo(T) == .float or @typeInfo(T) == .comptime_float) {
        if (val.isSmallInt()) {
            return @floatFromInt(val.asSmallInt());
        }
        if (val.isObject()) {
            const obj = val.asObject();
            if (obj.header.class_index == Heap.CLASS_FLOAT) {
                const bytes = obj.bytes(8);
                const float_val: f64 = @bitCast(bytes[0..8].*);
                return @floatCast(float_val);
            }
        }
        return FFIError.TypeMismatch;
    }

    // Handle bool
    if (T == bool) {
        if (val.isTrue()) return true;
        if (val.isFalse()) return false;
        return FFIError.TypeMismatch;
    }

    // Handle pointers
    if (@typeInfo(T) == .pointer) {
        const ptr_info = @typeInfo(T).pointer;
        const child = ptr_info.child;
        const is_c_pointer = (ptr_info.size == .c);

        // Special case: [*c][*c]const u8 or similar (char** - array of strings)
        if (@typeInfo(child) == .pointer) {
            const inner_ptr_info = @typeInfo(child).pointer;
            const inner_child = inner_ptr_info.child;
            // Check if this is char** (pointer to pointer to char)
            if (inner_child == u8 or ((@typeInfo(inner_child) == .int) and @typeInfo(inner_child).int.bits == 8)) {
                // Accept Array of Strings
                if (val.isObject()) {
                    const obj = val.asObject();
                    if (obj.header.class_index == Heap.CLASS_ARRAY) {
                        const arr_fields = obj.fields(obj.header.size);
                        // Allocate array of C string pointers
                        const c_strings = allocator.alloc([*c]const u8, arr_fields.len) catch return FFIError.AllocationFailed;
                        // Track how many strings we've allocated for cleanup on error
                        var strings_allocated: usize = 0;
                        errdefer {
                            // Free any allocated strings on error
                            for (0..strings_allocated) |j| {
                                // Calculate length from null terminator
                                var len: usize = 0;
                                while (c_strings[j][len] != 0) : (len += 1) {}
                                allocator.free(c_strings[j][0 .. len + 1]);
                            }
                            allocator.free(c_strings);
                        }
                        for (arr_fields, 0..) |elem, i| {
                            if (elem.isObject()) {
                                const str_obj = elem.asObject();
                                if (str_obj.header.class_index == Heap.CLASS_STRING or
                                    str_obj.header.class_index == Heap.CLASS_SYMBOL)
                                {
                                    const str_bytes = str_obj.bytes(str_obj.header.size);
                                    const c_str = allocator.allocSentinel(u8, str_bytes.len, 0) catch return FFIError.AllocationFailed;
                                    @memcpy(c_str, str_bytes);
                                    c_strings[i] = c_str.ptr;
                                    strings_allocated += 1;
                                } else {
                                    return FFIError.TypeMismatch;
                                }
                            } else {
                                return FFIError.TypeMismatch;
                            }
                        }
                        return @ptrCast(c_strings.ptr);
                    }
                }
            }
        }

        // Special case: [*c]const u8 or [*:0]const u8 (C strings)
        if (child == u8 or ((@typeInfo(child) == .int) and @typeInfo(child).int.bits == 8)) {
            // For C pointers, nil maps to null (address 0)
            if (is_c_pointer and val.isNil()) {
                const zero: usize = 0;
                return @ptrFromInt(zero);
            }
            if (val.isObject()) {
                const obj = val.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING or
                    obj.header.class_index == Heap.CLASS_SYMBOL)
                {
                    const bytes = obj.bytes(obj.header.size);
                    // Smalltalk strings are NOT null-terminated, but C expects null-terminated strings.
                    // Allocate a copy with null terminator for safe C interop.
                    const c_str = allocator.allocSentinel(u8, bytes.len, 0) catch return FFIError.AllocationFailed;
                    @memcpy(c_str, bytes);
                    return @ptrCast(c_str.ptr);
                }
                // Also accept ByteArray for raw byte buffers
                if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) {
                    const bytes = obj.bytes(obj.header.size);
                    return @ptrCast(bytes.ptr);
                }
            }
            return FFIError.TypeMismatch;
        }

        // Special case: pointer to int/uint types - accept ByteArray as output buffer
        if (@typeInfo(child) == .int) {
            if (val.isObject()) {
                const obj = val.asObject();
                if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) {
                    // Return pointer to ByteArray's data for C to write to
                    const bytes = obj.bytes(obj.header.size);
                    return @ptrCast(@alignCast(bytes.ptr));
                }
                // Also accept Array of integers - allocate and convert
                if (obj.header.class_index == Heap.CLASS_ARRAY) {
                    const arr_fields = obj.fields(obj.header.size);
                    const int_array = allocator.alloc(child, arr_fields.len) catch return FFIError.AllocationFailed;
                    errdefer allocator.free(int_array);
                    for (arr_fields, 0..) |elem, i| {
                        if (elem.isSmallInt()) {
                            int_array[i] = @intCast(elem.asSmallInt());
                        } else {
                            return FFIError.TypeMismatch;
                        }
                    }
                    return @ptrCast(int_array.ptr);
                }
            }
        }

        // Special case: pointer to float types - accept ByteArray or Array
        if (@typeInfo(child) == .float) {
            if (val.isObject()) {
                const obj = val.asObject();
                if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY) {
                    // Return pointer to ByteArray's data for C to write to
                    const bytes = obj.bytes(obj.header.size);
                    return @ptrCast(@alignCast(bytes.ptr));
                }
                // Accept Array of numbers - allocate and convert to float array
                if (obj.header.class_index == Heap.CLASS_ARRAY) {
                    const arr_fields = obj.fields(obj.header.size);
                    const float_array = allocator.alloc(child, arr_fields.len) catch return FFIError.AllocationFailed;
                    errdefer allocator.free(float_array);
                    for (arr_fields, 0..) |elem, i| {
                        if (elem.isSmallInt()) {
                            float_array[i] = @floatFromInt(elem.asSmallInt());
                        } else if (elem.isObject()) {
                            const float_obj = elem.asObject();
                            if (float_obj.header.class_index == Heap.CLASS_FLOAT) {
                                const float_bytes = float_obj.bytes(8);
                                const float_val: f64 = @bitCast(float_bytes[0..8].*);
                                float_array[i] = @floatCast(float_val);
                            } else {
                                return FFIError.TypeMismatch;
                            }
                        } else {
                            return FFIError.TypeMismatch;
                        }
                    }
                    return @ptrCast(float_array.ptr);
                }
            }
        }

        // Generic pointer - from integer address or nil
        if (is_c_pointer and val.isNil()) {
            // C pointers allow null (address 0)
            const zero: usize = 0;
            return @ptrFromInt(zero);
        }
        if (val.isSmallInt()) {
            const addr: usize = @intCast(val.asSmallInt());
            if (addr == 0 and !is_c_pointer) {
                return FFIError.NullPointer;
            }
            return @ptrFromInt(addr);
        }

        return FFIError.TypeMismatch;
    }

    // Handle optional pointers
    if (@typeInfo(T) == .optional) {
        const child = @typeInfo(T).optional.child;
        if (@typeInfo(child) == .pointer) {
            // Accept nil or SmallInt(0) as null for optional pointers
            if (val.isNil()) return null;
            if (val.isSmallInt() and val.asSmallInt() == 0) return null;
            // For non-zero values, recurse but allow the pointer to be created
            if (val.isSmallInt()) {
                const addr: usize = @intCast(val.asSmallInt());
                return @ptrFromInt(addr);
            }
            return valueToCType(child, val, heap, allocator);
        }
    }

    // Handle structs (for pass-by-value)
    // The Smalltalk value should be a ByteArray with the struct data
    if (@typeInfo(T) == .@"struct") {
        const struct_size = @sizeOf(T);
        if (struct_size > 64) {
            // Don't support very large structs by value
            return FFIError.TypeMismatch;
        }

        if (val.isObject()) {
            const obj = val.asObject();
            // Accept ByteArray or any byte-type object
            if (obj.header.class_index == Heap.CLASS_BYTE_ARRAY or
                obj.header.class_index == Heap.CLASS_STRING)
            {
                const bytes = obj.bytes(obj.header.size);
                if (bytes.len >= struct_size) {
                    // Copy bytes into struct
                    var result: T = undefined;
                    const result_bytes: *[struct_size]u8 = @ptrCast(&result);
                    @memcpy(result_bytes, bytes[0..struct_size]);
                    return result;
                }
            }
        }
        return FFIError.TypeMismatch;
    }

    return FFIError.TypeMismatch;
}

/// Convert C type to Smalltalk Value
fn cTypeToValue(comptime T: type, val: T, heap: *Heap) FFIError!Value {
    // Handle void
    if (T == void) return Value.nil;

    // Handle integers
    if (@typeInfo(T) == .int or @typeInfo(T) == .comptime_int) {
        const as_i61: i61 = @intCast(val);
        return Value.fromSmallInt(as_i61);
    }

    // Handle floats
    if (@typeInfo(T) == .float or @typeInfo(T) == .comptime_float) {
        const as_f64: f64 = @floatCast(val);
        const float_obj = heap.allocateFloat(as_f64) catch return FFIError.AllocationFailed;
        return float_obj;
    }

    // Handle bool
    if (T == bool) {
        return if (val) Value.true else Value.false;
    }

    // Handle pointers - return as integer address
    if (@typeInfo(T) == .pointer) {
        if (@intFromPtr(val) == 0) return Value.nil;
        const addr: i61 = @intCast(@intFromPtr(val));
        return Value.fromSmallInt(addr);
    }

    // Handle optional pointers
    if (@typeInfo(T) == .optional) {
        if (val == null) return Value.nil;
        const child = @typeInfo(T).optional.child;
        if (@typeInfo(child) == .pointer) {
            const addr: i61 = @intCast(@intFromPtr(val.?));
            return Value.fromSmallInt(addr);
        }
    }

    // Handle structs - return as ByteArray
    if (@typeInfo(T) == .@"struct") {
        const struct_size = @sizeOf(T);
        if (struct_size > 64) {
            return FFIError.TypeMismatch;
        }

        // Allocate a ByteArray to hold the struct data
        const byte_array = heap.allocateByteArray(struct_size) catch return FFIError.AllocationFailed;
        const obj = byte_array.asObject();
        const bytes = obj.bytes(struct_size);

        // Copy struct data to ByteArray
        const val_bytes: *const [struct_size]u8 = @ptrCast(&val);
        @memcpy(bytes, val_bytes);

        return byte_array;
    }

    return FFIError.TypeMismatch;
}

/// Generate a wrapper function for a C function
fn generateWrapper(comptime func: anytype) fn (*Heap, []const Value, std.mem.Allocator) FFIError!Value {
    const FnType = @TypeOf(func);
    const fn_info = @typeInfo(FnType).@"fn";
    const params = fn_info.params;
    const ReturnType = fn_info.return_type orelse void;

    return struct {
        fn call(heap: *Heap, args: []const Value, allocator: std.mem.Allocator) FFIError!Value {
            // Check argument count
            if (args.len < params.len) return FFIError.NotEnoughArguments;
            if (args.len > params.len) return FFIError.TooManyArguments;

            // Convert arguments
            var c_args: std.meta.ArgsTuple(FnType) = undefined;
            inline for (params, 0..) |param, i| {
                if (param.type) |T| {
                    c_args[i] = try valueToCType(T, args[i], heap, allocator);
                }
            }

            // Call the function
            const result = @call(.auto, func, c_args);

            // Handle return value
            if (@typeInfo(ReturnType) == .error_union) {
                const actual_result = result catch return FFIError.TypeMismatch;
                return cTypeToValue(@typeInfo(ReturnType).error_union.payload, actual_result, heap);
            } else {
                return cTypeToValue(ReturnType, result, heap);
            }
        }
    }.call;
}

/// Maximum number of arguments we track type info for
pub const MAX_FFI_ARGS = 16;

/// Registry entry for an FFI function
pub const FFIFunction = struct {
    name: []const u8,
    wrapper: *const fn (*Heap, []const Value, std.mem.Allocator) FFIError!Value,
    arg_count: usize,
    return_type: []const u8,
    /// Type names for each argument (up to MAX_FFI_ARGS)
    arg_types: [MAX_FFI_ARGS][]const u8,
};

/// Check if a function type can be wrapped (all types resolvable)
fn canWrapFunction(comptime FnType: type) bool {
    const fn_info = @typeInfo(FnType).@"fn";

    // Skip variadic functions (like printf)
    if (fn_info.is_var_args) return false;

    // Check return type
    if (fn_info.return_type) |ret| {
        if (!isWrappableType(ret)) return false;
    }

    // Check parameter types
    for (fn_info.params) |param| {
        if (param.type) |T| {
            if (!isWrappableType(T)) return false;
        } else {
            return false; // Unknown parameter type
        }
    }

    return true;
}

/// Check if a type can be marshalled between Zig and Smalltalk
fn isWrappableType(comptime T: type) bool {
    // void is wrappable
    if (T == void) return true;

    // Integers are wrappable
    if (@typeInfo(T) == .int or @typeInfo(T) == .comptime_int) return true;

    // Floats are wrappable (but not weird ones)
    if (@typeInfo(T) == .float) {
        const bits = @typeInfo(T).float.bits;
        return bits == 16 or bits == 32 or bits == 64;
    }
    if (@typeInfo(T) == .comptime_float) return true;

    // Bool is wrappable
    if (T == bool) return true;

    // Pointers are wrappable
    if (@typeInfo(T) == .pointer) return true;

    // Optional pointers are wrappable
    if (@typeInfo(T) == .optional) {
        const child = @typeInfo(T).optional.child;
        if (@typeInfo(child) == .pointer) return true;
    }

    // Structs with reasonable sizes are wrappable (for pass-by-value)
    // This enables things like Raylib's Color, Vector2, Rectangle, etc.
    if (@typeInfo(T) == .@"struct") {
        const struct_size = @sizeOf(T);
        // Allow structs up to 64 bytes (covers most common cases)
        // Larger structs should be passed by pointer
        return struct_size > 0 and struct_size <= 64;
    }

    // Arrays, enums, unions, etc - skip for now
    return false;
}

/// Check if a declaration name should be skipped (internal C declarations)
fn shouldSkipDecl(name: []const u8) bool {
    // Skip internal/reserved names (start with underscore)
    if (name.len > 0 and name[0] == '_') return true;

    // Skip C struct tag names (prefer typedef names like Vector2 over struct_Vector2)
    if (std.mem.startsWith(u8, name, "struct_")) return true;

    // Skip all-uppercase names (typically C macros)
    var all_upper = true;
    for (name) |c| {
        if (c >= 'a' and c <= 'z') {
            all_upper = false;
            break;
        }
    }
    if (all_upper and name.len > 0) return true;

    // Skip known problematic macros/declarations
    const skip_names = [_][]const u8{
        "stdin",
        "stdout",
        "stderr",
        // Math macros
        "fpclassify",
        "isfinite",
        "isinf",
        "isnan",
        "isnormal",
        "signbit",
        "isgreater",
        "isgreaterequal",
        "isless",
        "islessequal",
        "islessgreater",
        "isunordered",
        // va_list functions/macros
        "vprintf",
        "vfprintf",
        "vsprintf",
        "vsnprintf",
        "vscanf",
        "vfscanf",
        "vsscanf",
        "va_start",
        "va_end",
        "va_arg",
        "va_copy",
        // Common C macros that can't be wrapped
        "offsetof",
        "sizeof",
        "alignof",
        "typeof",
        "NULL",
        "EOF",
        "BUFSIZ",
        "FILENAME_MAX",
        "FOPEN_MAX",
        "TMP_MAX",
        "L_tmpnam",
    };
    for (skip_names) |skip| {
        if (std.mem.eql(u8, name, skip)) return true;
    }

    // Skip names containing certain patterns (common in macros)
    if (std.mem.indexOf(u8, name, "va_") != null) return true;
    if (std.mem.indexOf(u8, name, "__") != null) return true;

    return false;
}

/// Count all wrappable functions in a @cImport (auto-discovery)
fn countAllWrappableFunctions(comptime CImport: type) usize {
    @setEvalBranchQuota(100000000); // Increased from 10M to 100M for large headers like OpenXR
    const decls = @typeInfo(CImport).@"struct".decls;
    comptime var count: usize = 0;

    inline for (decls) |decl| {
        if (!shouldSkipDecl(decl.name)) {
            const field = @field(CImport, decl.name);
            const FieldType = @TypeOf(field);
            if (@typeInfo(FieldType) == .@"fn") {
                if (canWrapFunction(FieldType)) {
                    count += 1;
                }
            }
        }
    }
    return count;
}

/// Auto-discover and generate registry for ALL wrappable functions in a @cImport
pub fn generateRegistryAuto(comptime CImport: type) [countAllWrappableFunctions(CImport)]FFIFunction {
    @setEvalBranchQuota(100000000); // Increased from 10M to 100M for large headers like OpenXR
    const count = countAllWrappableFunctions(CImport);
    const decls = @typeInfo(CImport).@"struct".decls;

    var funcs: [count]FFIFunction = undefined;
    var idx: usize = 0;

    inline for (decls) |decl| {
        if (!shouldSkipDecl(decl.name)) {
            const field = @field(CImport, decl.name);
            const FieldType = @TypeOf(field);

            if (@typeInfo(FieldType) == .@"fn") {
                if (canWrapFunction(FieldType)) {
                    const fn_info = @typeInfo(FieldType).@"fn";

                    // Build arg_types array
                    var arg_types: [MAX_FFI_ARGS][]const u8 = .{""} ** MAX_FFI_ARGS;
                    inline for (fn_info.params, 0..) |param, i| {
                        if (i < MAX_FFI_ARGS) {
                            arg_types[i] = @typeName(param.type orelse void);
                        }
                    }

                    funcs[idx] = .{
                        .name = decl.name,
                        .wrapper = generateWrapper(field),
                        .arg_count = fn_info.params.len,
                        .return_type = @typeName(fn_info.return_type orelse void),
                        .arg_types = arg_types,
                    };
                    idx += 1;
                }
            }
        }
    }

    return funcs;
}

/// FFI Library with auto-discovery - no explicit function list needed
pub fn FFILibraryAuto(comptime CImport: type, comptime lib_name: []const u8) type {
    const funcs = generateRegistryAuto(CImport);
    return struct {
        pub const name = lib_name;
        pub const c = CImport;
        pub const functions: []const FFIFunction = &funcs;

        /// Look up a function by name
        pub fn getFunction(func_name: []const u8) ?FFIFunction {
            for (functions) |func| {
                if (std.mem.eql(u8, func.name, func_name)) {
                    return func;
                }
            }
            return null;
        }

        /// Call a function by name
        pub fn call(func_name: []const u8, heap: *Heap, args: []const Value, allocator: std.mem.Allocator) FFIError!Value {
            if (getFunction(func_name)) |func| {
                return func.wrapper(heap, args, allocator);
            }
            return FFIError.UnknownFunction;
        }

        /// Get list of all function names (precomputed at comptime)
        pub const function_names = blk: {
            var names: [functions.len][]const u8 = undefined;
            for (functions, 0..) |func, i| {
                names[i] = func.name;
            }
            break :blk names;
        };

        pub fn getFunctionNames() []const []const u8 {
            return &function_names;
        }

        pub fn getStructNames() []const []const u8 {
            return &[_][]const u8{};
        }

        pub fn getStruct(_: []const u8) ?StructInfo {
            return null;
        }

        pub fn generateStructCode(_: anytype) !void {}
    };
}

/// Count valid functions for a list of names
fn countValidFunctions(comptime CImport: type, comptime func_names: []const []const u8) usize {
    @setEvalBranchQuota(100000);
    comptime var count: usize = 0;
    inline for (func_names) |name| {
        if (@hasDecl(CImport, name)) {
            const FieldType = @TypeOf(@field(CImport, name));
            if (@typeInfo(FieldType) == .@"fn") {
                if (canWrapFunction(FieldType)) {
                    count += 1;
                }
            }
        }
    }
    return count;
}

/// Generate FFI registry for a list of specific function names
pub fn generateRegistryFor(comptime CImport: type, comptime func_names: []const []const u8) [countValidFunctions(CImport, func_names)]FFIFunction {
    @setEvalBranchQuota(100000);
    const count = countValidFunctions(CImport, func_names);

    var funcs: [count]FFIFunction = undefined;
    var idx: usize = 0;

    inline for (func_names) |name| {
        if (@hasDecl(CImport, name)) {
            const field = @field(CImport, name);
            const FieldType = @TypeOf(field);

            if (@typeInfo(FieldType) == .@"fn") {
                if (canWrapFunction(FieldType)) {
                    const fn_info = @typeInfo(FieldType).@"fn";

                    // Build arg_types array
                    var arg_types: [MAX_FFI_ARGS][]const u8 = .{""} ** MAX_FFI_ARGS;
                    inline for (fn_info.params, 0..) |param, i| {
                        if (i < MAX_FFI_ARGS) {
                            arg_types[i] = @typeName(param.type orelse void);
                        }
                    }

                    funcs[idx] = .{
                        .name = name,
                        .wrapper = generateWrapper(field),
                        .arg_count = fn_info.params.len,
                        .return_type = @typeName(fn_info.return_type orelse void),
                        .arg_types = arg_types,
                    };
                    idx += 1;
                }
            }
        }
    }

    return funcs;
}

/// FFI Library - contains all auto-generated bindings for a C import
pub fn FFILibrary(comptime CImport: type, comptime lib_name: []const u8, comptime func_names: []const []const u8) type {
    return struct {
        pub const name = lib_name;
        pub const c = CImport;
        pub const functions = generateRegistryFor(CImport, func_names);

        /// Look up a function by name
        pub fn getFunction(func_name: []const u8) ?FFIFunction {
            for (functions) |func| {
                if (std.mem.eql(u8, func.name, func_name)) {
                    return func;
                }
            }
            return null;
        }

        /// Call a function by name
        pub fn call(func_name: []const u8, heap: *Heap, args: []const Value, allocator: std.mem.Allocator) FFIError!Value {
            if (getFunction(func_name)) |func| {
                return func.wrapper(heap, args, allocator);
            }
            return FFIError.UnknownFunction;
        }

        /// Get list of all function names (precomputed at comptime)
        pub const function_names = blk: {
            var names: [functions.len][]const u8 = undefined;
            for (functions, 0..) |func, i| {
                names[i] = func.name;
            }
            break :blk names;
        };

        pub fn getFunctionNames() []const []const u8 {
            return &function_names;
        }
    };
}

// ============================================================================
// Test with LibC
// ============================================================================

/// Standard math functions to bind
pub const math_functions = [_][]const u8{
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

/// Standard C library functions to bind
pub const libc_functions = [_][]const u8{
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

// ============================================================================
// Pre-instantiated Library Bindings (from generated config)
// ============================================================================

// Import from generated file - contains library bindings and dispatch functions
const ffi_gen = @import("ffi_generated.zig");

/// List of available libraries (from generated config)
pub const available_libraries = ffi_gen.library_names;

/// Call an FFI function by library and function name
/// Delegates to generated dispatch function
pub fn callFFI(library: []const u8, func_name: []const u8, heap: *Heap, args: []const Value, allocator: std.mem.Allocator) FFIError!Value {
    return ffi_gen.callFFI(library, func_name, heap, args, allocator);
}

/// Get all functions for a library
/// Delegates to generated dispatch function
pub fn getLibraryFunctions(library: []const u8) ?[]const FFIFunction {
    return ffi_gen.getLibraryFunctions(library);
}

/// Get function names for a library
/// Delegates to generated dispatch function
pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {
    return ffi_gen.getLibraryFunctionNames(library);
}

// ============================================================================
// Struct Introspection and Code Generation
// ============================================================================

/// Information about a struct field
pub const StructField = struct {
    name: []const u8,
    offset: usize,
    size: usize,
    type_name: []const u8,
    /// Smalltalk accessor type: "uint8", "int8", "uint16", "int16", "uint32", "int32", "float32", "float64"
    accessor_type: []const u8,
};

/// Information about a struct
pub const StructInfo = struct {
    name: []const u8,
    size: usize,
    fields: []const StructField,
};

/// Map a C type to Smalltalk accessor type
fn getAccessorType(comptime T: type) []const u8 {
    return switch (@typeInfo(T)) {
        .int => |info| {
            if (info.signedness == .unsigned) {
                return switch (info.bits) {
                    8 => "uint8",
                    16 => "uint16",
                    32 => "uint32",
                    64 => "uint64",
                    else => "uint32",
                };
            } else {
                return switch (info.bits) {
                    8 => "int8",
                    16 => "int16",
                    32 => "int32",
                    64 => "int64",
                    else => "int32",
                };
            }
        },
        .float => |info| {
            return switch (info.bits) {
                32 => "float32",
                64 => "float64",
                else => "float64",
            };
        },
        .bool => "uint8",
        .pointer => "uint64", // Pointers as 64-bit addresses
        else => "uint8", // Default fallback
    };
}

/// Count fields in a struct type
fn countStructFields(comptime T: type) usize {
    const info = @typeInfo(T);
    if (info != .@"struct") return 0;
    return info.@"struct".fields.len;
}

/// Generate field info for a struct at comptime
fn generateStructFields(comptime T: type) [countStructFields(T)]StructField {
    const info = @typeInfo(T).@"struct";
    var fields: [info.fields.len]StructField = undefined;

    inline for (info.fields, 0..) |field, i| {
        fields[i] = .{
            .name = field.name,
            .offset = @offsetOf(T, field.name),
            .size = @sizeOf(field.type),
            .type_name = @typeName(field.type),
            .accessor_type = getAccessorType(field.type),
        };
    }

    return fields;
}

/// Generate StructInfo for a single type
pub fn getStructInfo(comptime T: type, comptime name: []const u8) StructInfo {
    const fields = generateStructFields(T);
    return .{
        .name = name,
        .size = @sizeOf(T),
        .fields = &fields,
    };
}

/// Check if a struct should be wrapped (reasonable size, simple fields)
fn isWrappableStruct(comptime T: type) bool {
    const info = @typeInfo(T);
    if (info != .@"struct") return false;

    const struct_info = info.@"struct";

    // Skip empty structs
    if (struct_info.fields.len == 0) return false;

    // Skip very large structs (increased from 256 to 512 bytes for OpenXR)
    if (@sizeOf(T) > 512) return false;

    // Skip packed/extern structs with complex layouts? No, we want those
    // Skip structs with function pointers or other complex types
    for (struct_info.fields) |field| {
        const field_info = @typeInfo(field.type);
        switch (field_info) {
            .@"fn" => return false, // Skip function pointers
            .@"union" => return false, // Skip unions
            .array => |arr| {
                // Allow small arrays of primitives
                if (arr.len > 64) return false;
            },
            else => {},
        }
    }

    return true;
}

/// Count wrappable structs in a @cImport
fn countWrappableStructs(comptime CImport: type) usize {
    @setEvalBranchQuota(100000000); // Increased from 10M to 100M for large headers like OpenXR
    const decls = @typeInfo(CImport).@"struct".decls;
    comptime var count: usize = 0;

    inline for (decls) |decl| {
        if (!shouldSkipDecl(decl.name)) {
            if (@hasDecl(CImport, decl.name)) {
                const DeclType = @TypeOf(@field(CImport, decl.name));
                if (DeclType == type) {
                    const T = @field(CImport, decl.name);
                    if (@typeInfo(T) == .@"struct" and isWrappableStruct(T)) {
                        count += 1;
                    }
                }
            }
        }
    }
    return count;
}

/// Get all struct infos from a @cImport
pub fn getStructInfos(comptime CImport: type) [countWrappableStructs(CImport)]StructInfo {
    @setEvalBranchQuota(100000000); // Increased from 10M to 100M for large headers like OpenXR
    const count = countWrappableStructs(CImport);
    const decls = @typeInfo(CImport).@"struct".decls;

    var infos: [count]StructInfo = undefined;
    var idx: usize = 0;

    inline for (decls) |decl| {
        if (!shouldSkipDecl(decl.name)) {
            if (@hasDecl(CImport, decl.name)) {
                const DeclType = @TypeOf(@field(CImport, decl.name));
                if (DeclType == type) {
                    const T = @field(CImport, decl.name);
                    if (@typeInfo(T) == .@"struct" and isWrappableStruct(T)) {
                        const fields = generateStructFields(T);
                        infos[idx] = .{
                            .name = decl.name,
                            .size = @sizeOf(T),
                            .fields = &fields,
                        };
                        idx += 1;
                    }
                }
            }
        }
    }

    return infos;
}

/// Generate Smalltalk code for a struct as an ExternalStructure subclass
pub fn generateSmalltalkStruct(info: StructInfo, writer: anytype) !void {
    // Class definition
    try writer.print("ExternalStructure subclass: #{s}\n", .{info.name});
    try writer.writeAll("    instanceVariableNames: ''\n");
    try writer.writeAll("    classVariableNames: ''\n");
    try writer.writeAll("    poolDictionaries: ''\n");
    try writer.writeAll("    classInstanceVariableNames: ''!\n\n");

    // Class methods
    try writer.print("!{s} class methodsFor!\n\n", .{info.name});
    try writer.print("byteSize\n    \"Size of this struct in bytes\"\n    ^{d}!\n\n", .{info.size});
    try writer.writeAll("!\n\n");

    // Instance methods (field accessors)
    try writer.print("!{s} methodsFor!\n\n", .{info.name});

    for (info.fields) |field| {
        // Getter
        try writer.print("{s}\n    \"{s} field ({s}, offset {d})\"\n    ^self {s}At: {d}!\n\n", .{
            field.name,
            field.name,
            field.type_name,
            field.offset,
            field.accessor_type,
            field.offset,
        });

        // Setter
        try writer.print("{s}: value\n    self {s}At: {d} put: value!\n\n", .{
            field.name,
            field.accessor_type,
            field.offset,
        });
    }

    try writer.writeAll("!\n\n");
}

/// Generate Smalltalk code for all structs in a library
pub fn generateAllStructCode(comptime CImport: type, writer: anytype) !void {
    const infos = getStructInfos(CImport);
    for (infos) |info| {
        try generateSmalltalkStruct(info, writer);
    }
}

/// FFI Library with struct support
pub fn FFILibraryWithStructs(comptime CImport: type, comptime lib_name: []const u8) type {
    const funcs = generateRegistryAuto(CImport);
    const struct_infos = getStructInfos(CImport);
    return struct {
        pub const name = lib_name;
        pub const c = CImport;
        pub const functions: []const FFIFunction = &funcs;
        pub const structs: []const StructInfo = &struct_infos;

        pub fn getFunction(func_name: []const u8) ?FFIFunction {
            for (functions) |func| {
                if (std.mem.eql(u8, func.name, func_name)) {
                    return func;
                }
            }
            return null;
        }

        pub fn call(func_name: []const u8, heap: *Heap, args: []const Value, allocator: std.mem.Allocator) FFIError!Value {
            if (getFunction(func_name)) |func| {
                return func.wrapper(heap, args, allocator);
            }
            return FFIError.UnknownFunction;
        }

        pub const function_names = blk: {
            var names: [funcs.len][]const u8 = undefined;
            for (funcs, 0..) |func, i| {
                names[i] = func.name;
            }
            break :blk names;
        };

        pub fn getFunctionNames() []const []const u8 {
            return &function_names;
        }

        pub const struct_names = blk: {
            var names: [struct_infos.len][]const u8 = undefined;
            for (struct_infos, 0..) |info, i| {
                names[i] = info.name;
            }
            break :blk names;
        };

        pub fn getStructNames() []const []const u8 {
            return &struct_names;
        }

        pub fn getStruct(struct_name: []const u8) ?StructInfo {
            for (struct_infos) |info| {
                if (std.mem.eql(u8, info.name, struct_name)) {
                    return info;
                }
            }
            return null;
        }

        /// Generate Smalltalk code for all structs
        pub fn generateStructCode(writer: anytype) !void {
            for (struct_infos) |info| {
                try generateSmalltalkStruct(info, writer);
            }
        }
    };
}

/// Empty library for libraries without headers
pub fn EmptyLibrary(comptime lib_name: []const u8) type {
    return struct {
        pub const name = lib_name;
        pub const functions: []const FFIFunction = &[_]FFIFunction{};

        pub fn getFunction(_: []const u8) ?FFIFunction {
            return null;
        }

        pub fn call(_: []const u8, _: *Heap, _: []const Value, _: std.mem.Allocator) FFIError!Value {
            return FFIError.UnknownFunction;
        }

        pub fn getFunctionNames() []const []const u8 {
            return &[_][]const u8{};
        }

        pub fn getStructNames() []const []const u8 {
            return &[_][]const u8{};
        }

        pub fn getStruct(_: []const u8) ?StructInfo {
            return null;
        }

        pub fn generateStructCode(_: anytype) !void {}
    };
}

// ============================================================================
// Tests
// ============================================================================

test "auto-generate math bindings" {
    const libc = @cImport({
        @cInclude("math.h");
    });

    const MathLib = FFILibrary(libc, "LibMath", &math_functions);

    // Check that sin is registered
    if (MathLib.getFunction("sin")) |func| {
        std.debug.print("Found function: {s} with {d} args\n", .{ func.name, func.arg_count });
    }

    // Print all available functions
    std.debug.print("\nAvailable math functions ({d} total):\n", .{MathLib.functions.len});
    for (MathLib.functions) |func| {
        std.debug.print("  {s}({d} args) -> {s}\n", .{ func.name, func.arg_count, func.return_type });
    }
}
