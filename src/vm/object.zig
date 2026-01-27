const std = @import("std");

/// Tagged pointer value type for Smalltalk objects.
/// Uses the lowest 3 bits as a tag:
///   xxx000 = Heap pointer (aligned)
///   xxx001 = SmallInteger (61-bit signed, shifted left by 3)
///   xxx010 = Character (21-bit Unicode codepoint, shifted left by 3)
///   xxx110 = Special constants (nil=0, true=8, false=16)
pub const Value = packed struct {
    bits: u64,

    pub const TAG_MASK: u64 = 0b111;
    pub const TAG_OBJECT: u64 = 0b000;
    pub const TAG_SMALL_INT: u64 = 0b001;
    pub const TAG_CHARACTER: u64 = 0b010;
    pub const TAG_SPECIAL: u64 = 0b110;

    // Special object encodings (using TAG_SPECIAL)
    pub const NIL_BITS: u64 = TAG_SPECIAL; // 0b110 = 6
    pub const TRUE_BITS: u64 = (1 << 3) | TAG_SPECIAL; // 0b1110 = 14
    pub const FALSE_BITS: u64 = (2 << 3) | TAG_SPECIAL; // 0b10110 = 22

    // Pre-defined special values
    pub const nil: Value = .{ .bits = NIL_BITS };
    pub const @"true": Value = .{ .bits = TRUE_BITS };
    pub const @"false": Value = .{ .bits = FALSE_BITS };

    pub fn isSmallInt(self: Value) bool {
        return (self.bits & TAG_MASK) == TAG_SMALL_INT;
    }

    pub fn isCharacter(self: Value) bool {
        return (self.bits & TAG_MASK) == TAG_CHARACTER;
    }

    pub fn isObject(self: Value) bool {
        return (self.bits & TAG_MASK) == TAG_OBJECT and self.bits != 0;
    }

    pub fn isSpecial(self: Value) bool {
        return (self.bits & TAG_MASK) == TAG_SPECIAL;
    }

    pub fn isNil(self: Value) bool {
        return self.bits == NIL_BITS;
    }

    pub fn isTrue(self: Value) bool {
        return self.bits == TRUE_BITS;
    }

    pub fn isFalse(self: Value) bool {
        return self.bits == FALSE_BITS;
    }

    pub fn isBoolean(self: Value) bool {
        return self.isTrue() or self.isFalse();
    }

    /// Convert SmallInteger to Zig i61
    pub fn asSmallInt(self: Value) i61 {
        std.debug.assert(self.isSmallInt());
        // Arithmetic right shift to preserve sign
        const signed: i64 = @bitCast(self.bits);
        return @intCast(signed >> 3);
    }

    /// Create a SmallInteger Value from a Zig integer
    pub fn fromSmallInt(n: i61) Value {
        const shifted: u64 = @bitCast(@as(i64, n) << 3);
        return .{ .bits = shifted | TAG_SMALL_INT };
    }

    /// Convert Character to Unicode codepoint
    pub fn asCharacter(self: Value) u21 {
        std.debug.assert(self.isCharacter());
        return @intCast(self.bits >> 3);
    }

    /// Create a Character Value from a Unicode codepoint
    pub fn fromCharacter(cp: u21) Value {
        return .{ .bits = (@as(u64, cp) << 3) | TAG_CHARACTER };
    }

    /// Get the Object pointer from an object Value
    pub fn asObject(self: Value) *Object {
        std.debug.assert(self.isObject());
        return @ptrFromInt(self.bits);
    }

    /// Create a Value from an Object pointer
    pub fn fromObject(obj: *Object) Value {
        return .{ .bits = @intFromPtr(obj) };
    }

    /// Create a boolean Value from a Zig bool
    pub fn fromBool(b: bool) Value {
        return if (b) Value.@"true" else Value.@"false";
    }

    /// Convert a boolean Value to Zig bool
    pub fn asBool(self: Value) bool {
        std.debug.assert(self.isBoolean());
        return self.isTrue();
    }

    /// Check equality of two Values
    pub fn eql(self: Value, other: Value) bool {
        return self.bits == other.bits;
    }

    /// Format for debugging
    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.isNil()) {
            try writer.writeAll("nil");
        } else if (self.isTrue()) {
            try writer.writeAll("true");
        } else if (self.isFalse()) {
            try writer.writeAll("false");
        } else if (self.isSmallInt()) {
            try writer.print("{d}", .{self.asSmallInt()});
        } else if (self.isCharacter()) {
            try writer.print("${c}", .{@as(u8, @intCast(self.asCharacter()))});
        } else if (self.isObject()) {
            try writer.print("Object@{x}", .{self.bits});
        } else {
            try writer.print("Unknown({x})", .{self.bits});
        }
    }
};

/// Class format - how instances store their data
pub const ClassFormat = enum(u8) {
    normal = 0, // Fixed fields, all pointers (Values)
    variable = 1, // Like Array - indexed + named fields
    bytes = 2, // Like ByteArray - raw bytes
    words = 3, // Like WordArray - raw 32-bit words
    compiled_method = 4, // Special format for methods
    weak = 5, // Weak references
};

/// Object header - appears before every heap object
/// Must be 16 bytes total to maintain 8-byte alignment for Value fields
pub const ObjectHeader = extern struct {
    class_index: u32, // Index into class table
    hash: u32, // Identity hash code
    flags: u8, // Format and GC flags packed into single byte
    _padding: [3]u8 = .{0} ** 3, // Padding
    size: u32 = 0, // Number of fields/bytes (used for variable-size objects)

    // Flag bit positions
    pub const FORMAT_MASK: u8 = 0x07;
    pub const MARKED_BIT: u8 = 0x08;
    pub const PINNED_BIT: u8 = 0x10;
    pub const IMMUTABLE_BIT: u8 = 0x20;
    pub const REMEMBERED_BIT: u8 = 0x40;

    pub fn getFormat(self: *const ObjectHeader) ClassFormat {
        return @enumFromInt(self.flags & FORMAT_MASK);
    }

    pub fn setFormat(self: *ObjectHeader, format: ClassFormat) void {
        self.flags = (self.flags & ~FORMAT_MASK) | @intFromEnum(format);
    }

    pub fn isMarked(self: *const ObjectHeader) bool {
        return (self.flags & MARKED_BIT) != 0;
    }

    pub fn setMarked(self: *ObjectHeader, marked: bool) void {
        if (marked) {
            self.flags |= MARKED_BIT;
        } else {
            self.flags &= ~MARKED_BIT;
        }
    }
};

/// Heap object structure
pub const Object = struct {
    header: ObjectHeader,
    // Fields follow immediately after header in memory
    // For normal objects: array of Value
    // For bytes objects: array of u8
    // Access via helper methods

    /// Get a slice of the object's Value fields
    pub fn fields(self: *Object, count: usize) []Value {
        const ptr: [*]Value = @ptrFromInt(@intFromPtr(self) + @sizeOf(ObjectHeader));
        return ptr[0..count];
    }

    /// Get a single field by index (with bounds checking)
    pub fn getField(self: *Object, index: usize, num_fields: usize) Value {
        if (index >= num_fields) {
            return Value.nil; // Return nil for out-of-bounds access instead of crashing
        }
        return self.fields(num_fields)[index];
    }

    /// Set a single field by index (with bounds checking)
    pub fn setField(self: *Object, index: usize, value: Value, num_fields: usize) void {
        if (index >= num_fields) {
            return; // Silently ignore out-of-bounds writes instead of crashing
        }
        self.fields(num_fields)[index] = value;
    }

    /// Get raw bytes for a bytes-format object
    pub fn bytes(self: *Object, count: usize) []u8 {
        const ptr: [*]u8 = @ptrFromInt(@intFromPtr(self) + @sizeOf(ObjectHeader));
        return ptr[0..count];
    }
};

/// Compiled method structure
pub const CompiledMethod = struct {
    header: MethodHeader,
    // Followed by:
    // - literals: [num_literals]Value
    // - bytecodes: [bytecode_size]u8

    pub const MethodHeader = packed struct {
        num_args: u8,
        num_temps: u8,
        num_literals: u16,
        primitive_index: u16, // 0 = no primitive
        flags: MethodFlags,
        bytecode_size: u16,
    };

    pub const MethodFlags = packed struct {
        has_context: bool = false, // Method creates a context (has blocks with non-local returns)
        has_source: bool = false, // Method has source code stored as last literal
        _padding: u14 = 0,
    };

    pub fn getLiterals(self: *CompiledMethod) []Value {
        const ptr: [*]Value = @ptrFromInt(@intFromPtr(self) + @sizeOf(MethodHeader));
        return ptr[0..self.header.num_literals];
    }

    pub fn getBytecodes(self: *CompiledMethod) []u8 {
        const lit_bytes = @as(usize, self.header.num_literals) * @sizeOf(Value);
        const ptr: [*]u8 = @ptrFromInt(@intFromPtr(self) + @sizeOf(MethodHeader) + lit_bytes);
        return ptr[0..self.header.bytecode_size];
    }

    /// Total byte size of the compiled method, including header, literals and bytecodes
    pub fn byteSize(self: *CompiledMethod) usize {
        const header_size = @sizeOf(MethodHeader);
        const literals_size = @as(usize, self.header.num_literals) * @sizeOf(Value);
        const bytecodes_size = @as(usize, self.header.bytecode_size);
        return header_size + literals_size + bytecodes_size;
    }

    /// Convenience view of the compiled method as a byte slice
    pub fn asBytes(self: *CompiledMethod) []u8 {
        return @as([*]u8, @ptrCast(self))[0..self.byteSize()];
    }

    /// Get the compiled method as an aligned byte slice for proper deallocation
    pub fn asBytesAligned(self: *CompiledMethod) []align(@alignOf(CompiledMethod)) u8 {
        const ptr: [*]align(@alignOf(CompiledMethod)) u8 = @ptrCast(self);
        return ptr[0..self.byteSize()];
    }

    /// Free storage for this compiled method using the allocator it was created with
    pub fn destroy(self: *CompiledMethod, allocator: std.mem.Allocator) void {
        allocator.free(self.asBytesAligned());
    }

    /// Get the source code if stored
    pub fn getSource(self: *CompiledMethod) ?[]const u8 {
        if (!self.header.flags.has_source) return null;
        if (self.header.num_literals == 0) return null;

        // Source is stored as the last literal
        const literals = self.getLiterals();
        const source_val = literals[literals.len - 1];

        if (!source_val.isObject()) return null;

        const obj = source_val.asObject();
        const memory = @import("memory.zig");
        if (obj.header.class_index != memory.Heap.CLASS_STRING) return null;

        return obj.bytes(obj.header.size);
    }
};

// Test the Value type
test "Value - SmallInteger" {
    const v1 = Value.fromSmallInt(42);
    try std.testing.expect(v1.isSmallInt());
    try std.testing.expectEqual(@as(i61, 42), v1.asSmallInt());

    const v2 = Value.fromSmallInt(-100);
    try std.testing.expect(v2.isSmallInt());
    try std.testing.expectEqual(@as(i61, -100), v2.asSmallInt());

    const v3 = Value.fromSmallInt(0);
    try std.testing.expect(v3.isSmallInt());
    try std.testing.expectEqual(@as(i61, 0), v3.asSmallInt());
}

test "Value - Special objects" {
    try std.testing.expect(Value.nil.isNil());
    try std.testing.expect(Value.nil.isSpecial());
    try std.testing.expect(!Value.nil.isSmallInt());

    try std.testing.expect(Value.@"true".isTrue());
    try std.testing.expect(Value.@"true".isBoolean());

    try std.testing.expect(Value.@"false".isFalse());
    try std.testing.expect(Value.@"false".isBoolean());
}

test "Value - Character" {
    const ch = Value.fromCharacter('A');
    try std.testing.expect(ch.isCharacter());
    try std.testing.expectEqual(@as(u21, 'A'), ch.asCharacter());
}

test "Value - Equality" {
    const a = Value.fromSmallInt(42);
    const b = Value.fromSmallInt(42);
    const c = Value.fromSmallInt(43);

    try std.testing.expect(a.eql(b));
    try std.testing.expect(!a.eql(c));
    try std.testing.expect(Value.nil.eql(Value.nil));
}
