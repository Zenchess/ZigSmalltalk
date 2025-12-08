const std = @import("std");
const object = @import("object.zig");
const Value = object.Value;
const Object = object.Object;
const ObjectHeader = object.ObjectHeader;
const ClassFormat = object.ClassFormat;
const CompiledMethod = object.CompiledMethod;

/// Heap - manages all Smalltalk objects
pub const Heap = struct {
    allocator: std.mem.Allocator,

    // Memory spaces (for semi-space GC)
    from_space: []align(8) u8,
    to_space: []align(8) u8,
    alloc_ptr: usize,
    space_size: usize,

    // Class table - maps class index to Class object
    class_table: std.ArrayList(Value),

    // Symbol table - interned strings
    symbol_table: std.StringHashMapUnmanaged(Value),

    // Global dictionary (Smalltalk)
    globals: std.StringHashMapUnmanaged(Value),

    // Hash counter for identity hashes
    next_hash: u32 = 1,

    // Well-known class indices
    pub const CLASS_OBJECT: u32 = 0;
    pub const CLASS_CLASS: u32 = 1;
    pub const CLASS_METACLASS: u32 = 2;
    pub const CLASS_BEHAVIOR: u32 = 3;
    pub const CLASS_CLASS_DESCRIPTION: u32 = 4;
    pub const CLASS_SMALL_INTEGER: u32 = 5;
    pub const CLASS_STRING: u32 = 6;
    pub const CLASS_SYMBOL: u32 = 7;
    pub const CLASS_ARRAY: u32 = 8;
    pub const CLASS_BYTE_ARRAY: u32 = 9;
    pub const CLASS_COMPILED_METHOD: u32 = 10;
    pub const CLASS_BLOCK_CLOSURE: u32 = 11;
    pub const CLASS_UNDEFINED_OBJECT: u32 = 12;
    pub const CLASS_TRUE: u32 = 13;
    pub const CLASS_FALSE: u32 = 14;
    pub const CLASS_CHARACTER: u32 = 15;
    pub const CLASS_INTERVAL: u32 = 16;
    pub const CLASS_FLOAT: u32 = 17;

    // Class object field indices
    pub const CLASS_FIELD_SUPERCLASS: usize = 0;
    pub const CLASS_FIELD_METHOD_DICT: usize = 1;
    pub const CLASS_FIELD_FORMAT: usize = 2;
    pub const CLASS_FIELD_INST_VARS: usize = 3;
    pub const CLASS_FIELD_NAME: usize = 4;
    pub const CLASS_NUM_FIELDS: usize = 5;

    pub fn init(allocator: std.mem.Allocator, heap_size: usize) !*Heap {
        const heap = try allocator.create(Heap);
        errdefer allocator.destroy(heap);

        const space_size = heap_size / 2;

        heap.* = .{
            .allocator = allocator,
            .from_space = try allocator.alignedAlloc(u8, .@"8", space_size),
            .to_space = try allocator.alignedAlloc(u8, .@"8", space_size),
            .alloc_ptr = 0,
            .space_size = space_size,
            .class_table = .{},
            .symbol_table = .{},
            .globals = .{},
        };

        return heap;
    }

    pub fn deinit(self: *Heap) void {
        self.allocator.free(self.from_space);
        self.allocator.free(self.to_space);
        self.class_table.deinit(self.allocator);

        // Free interned symbol strings
        var it = self.symbol_table.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.symbol_table.deinit(self.allocator);

        // Free global keys
        var git = self.globals.iterator();
        while (git.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.globals.deinit(self.allocator);

        self.allocator.destroy(self);
    }

    /// Allocate raw bytes from the heap
    fn allocRaw(self: *Heap, size: usize) ![]u8 {
        // Align to 8 bytes
        const aligned_size = (size + 7) & ~@as(usize, 7);

        if (self.alloc_ptr + aligned_size > self.space_size) {
            try self.collectGarbage();
            if (self.alloc_ptr + aligned_size > self.space_size) {
                return error.OutOfMemory;
            }
        }

        const start = self.alloc_ptr;
        self.alloc_ptr += aligned_size;
        return self.from_space[start .. start + size];
    }

    /// Allocate a new object with the given class and number of fields
    pub fn allocateObject(self: *Heap, class_index: u32, num_fields: usize, format: ClassFormat) !*Object {
        const field_size = switch (format) {
            .normal, .variable, .weak => num_fields * @sizeOf(Value),
            .bytes => num_fields, // num_fields is byte count
            .words => num_fields * 4,
            .compiled_method => num_fields, // Special handling
        };

        const total_size = @sizeOf(ObjectHeader) + field_size;
        const mem = try self.allocRaw(total_size);

        const obj: *Object = @ptrCast(@alignCast(mem.ptr));
        obj.header = .{
            .class_index = class_index,
            .hash = self.nextHash(),
            .flags = @intFromEnum(format),
            .size = @intCast(num_fields),
        };

        // Initialize fields to nil for pointer objects
        if (format == .normal or format == .variable or format == .weak) {
            const fields = obj.fields(num_fields);
            for (fields) |*f| {
                f.* = Value.nil;
            }
        } else if (format == .bytes) {
            const bytes_slice = obj.bytes(num_fields);
            @memset(bytes_slice, 0);
        }

        return obj;
    }

    /// Allocate a string object
    pub fn allocateString(self: *Heap, content: []const u8) !Value {
        const obj = try self.allocateObject(CLASS_STRING, content.len, .bytes);
        const bytes_slice = obj.bytes(content.len);
        @memcpy(bytes_slice, content);
        return Value.fromObject(obj);
    }

    /// Allocate a ByteArray
    pub fn allocateByteArray(self: *Heap, size: usize) !Value {
        const obj = try self.allocateObject(CLASS_BYTE_ARRAY, size, .bytes);
        return Value.fromObject(obj);
    }

    /// Allocate an Array
    pub fn allocateArray(self: *Heap, size: usize) !Value {
        const obj = try self.allocateObject(CLASS_ARRAY, size, .variable);
        return Value.fromObject(obj);
    }

    /// Allocate a Float object containing the given f64 value
    pub fn allocateFloat(self: *Heap, value: f64) !Value {
        // Float is stored as 8 bytes (one 64-bit word)
        const obj = try self.allocateObject(CLASS_FLOAT, 8, .bytes);
        const bytes_slice = obj.bytes(8);
        const value_bytes: [8]u8 = @bitCast(value);
        @memcpy(bytes_slice, &value_bytes);
        return Value.fromObject(obj);
    }

    /// Extract f64 value from a Float object
    pub fn getFloatValue(_: *Heap, value: Value) ?f64 {
        if (!value.isObject()) return null;
        const obj = value.asObject();
        if (obj.header.class_index != CLASS_FLOAT) return null;
        const bytes_slice = obj.bytes(8);
        return @bitCast(bytes_slice[0..8].*);
    }

    /// Intern a symbol (returns existing one if already interned)
    pub fn internSymbol(self: *Heap, name: []const u8) !Value {
        if (self.symbol_table.get(name)) |existing| {
            return existing;
        }

        // Create new symbol
        const obj = try self.allocateObject(CLASS_SYMBOL, name.len, .bytes);
        const bytes_slice = obj.bytes(name.len);
        @memcpy(bytes_slice, name);

        const sym = Value.fromObject(obj);

        // Store with owned key
        const owned_key = try self.allocator.dupe(u8, name);
        try self.symbol_table.put(self.allocator, owned_key, sym);

        return sym;
    }

    /// Register a class in the class table
    pub fn registerClass(self: *Heap, class_obj: Value) !u32 {
        const index: u32 = @intCast(self.class_table.items.len);
        try self.class_table.append(self.allocator, class_obj);
        return index;
    }

    /// Get class by index
    pub fn getClass(self: *Heap, index: u32) Value {
        if (index < self.class_table.items.len) {
            return self.class_table.items[index];
        }
        return Value.nil;
    }

    /// Get the class of a Value
    pub fn classOf(self: *Heap, value: Value) Value {
        if (value.isSmallInt()) {
            return self.getClass(CLASS_SMALL_INTEGER);
        } else if (value.isCharacter()) {
            return self.getClass(CLASS_CHARACTER);
        } else if (value.isNil()) {
            return self.getClass(CLASS_UNDEFINED_OBJECT);
        } else if (value.isTrue()) {
            return self.getClass(CLASS_TRUE);
        } else if (value.isFalse()) {
            return self.getClass(CLASS_FALSE);
        } else if (value.isObject()) {
            const obj = value.asObject();
            return self.getClass(obj.header.class_index);
        }
        return Value.nil;
    }

    /// Set a global variable
    pub fn setGlobal(self: *Heap, name: []const u8, value: Value) !void {
        const result = try self.globals.getOrPut(self.allocator, try self.allocator.dupe(u8, name));
        if (result.found_existing) {
            self.allocator.free(result.key_ptr.*);
            result.key_ptr.* = try self.allocator.dupe(u8, name);
        }
        result.value_ptr.* = value;
    }

    /// Get a global variable
    pub fn getGlobal(self: *Heap, name: []const u8) ?Value {
        return self.globals.get(name);
    }

    fn nextHash(self: *Heap) u32 {
        const h = self.next_hash;
        self.next_hash +%= 1;
        if (self.next_hash == 0) self.next_hash = 1;
        return h;
    }

    /// Garbage collection (simple semi-space copying)
    pub fn collectGarbage(self: *Heap) !void {
        // Swap spaces
        const temp = self.from_space;
        self.from_space = self.to_space;
        self.to_space = temp;

        const old_alloc_ptr = self.alloc_ptr;
        self.alloc_ptr = 0;

        // Copy roots
        // 1. Class table
        for (self.class_table.items) |*item| {
            item.* = try self.copyObject(item.*);
        }

        // 2. Symbol table
        var sym_it = self.symbol_table.iterator();
        while (sym_it.next()) |entry| {
            entry.value_ptr.* = try self.copyObject(entry.value_ptr.*);
        }

        // 3. Globals
        var global_it = self.globals.iterator();
        while (global_it.next()) |entry| {
            entry.value_ptr.* = try self.copyObject(entry.value_ptr.*);
        }

        // Scan copied objects (Cheney's algorithm)
        var scan_ptr: usize = 0;
        while (scan_ptr < self.alloc_ptr) {
            const obj: *Object = @ptrCast(@alignCast(self.from_space.ptr + scan_ptr));
            const format = obj.header.getFormat();

            const field_size = switch (format) {
                .normal, .variable, .weak => blk: {
                    // Need to scan and copy referenced objects
                    const num_fields = self.objectFieldCount(obj);
                    const fields_slice = obj.fields(num_fields);
                    for (fields_slice) |*field| {
                        field.* = try self.copyObject(field.*);
                    }
                    break :blk num_fields * @sizeOf(Value);
                },
                .bytes => self.objectByteCount(obj),
                .words => self.objectByteCount(obj),
                .compiled_method => blk: {
                    // Copy method literals
                    const method: *CompiledMethod = @ptrCast(@alignCast(obj));
                    const literals = method.getLiterals();
                    for (literals) |*lit| {
                        lit.* = try self.copyObject(lit.*);
                    }
                    break :blk @sizeOf(CompiledMethod.MethodHeader) +
                        method.header.num_literals * @sizeOf(Value) +
                        method.header.bytecode_size;
                },
            };

            const obj_size = @sizeOf(ObjectHeader) + field_size;
            scan_ptr += (obj_size + 7) & ~@as(usize, 7);
        }

        _ = old_alloc_ptr;
    }

    fn copyObject(self: *Heap, value: Value) !Value {
        if (!value.isObject()) {
            return value; // Immediate values don't need copying
        }

        const old_obj = value.asObject();

        // Check if already copied (forwarding pointer in old location)
        if (old_obj.header.isMarked()) {
            // The hash field contains the offset in new space
            const new_ptr: *Object = @ptrCast(@alignCast(self.from_space.ptr + old_obj.header.hash));
            return Value.fromObject(new_ptr);
        }

        // Calculate size
        const format = old_obj.header.getFormat();
        const field_size = switch (format) {
            .normal, .variable, .weak => self.objectFieldCount(old_obj) * @sizeOf(Value),
            .bytes, .words => self.objectByteCount(old_obj),
            .compiled_method => blk: {
                const method: *CompiledMethod = @ptrCast(@alignCast(old_obj));
                break :blk @sizeOf(CompiledMethod.MethodHeader) +
                    method.header.num_literals * @sizeOf(Value) +
                    method.header.bytecode_size;
            },
        };
        const total_size = @sizeOf(ObjectHeader) + field_size;
        const aligned_size = (total_size + 7) & ~@as(usize, 7);

        // Allocate in new space
        if (self.alloc_ptr + aligned_size > self.space_size) {
            return error.OutOfMemory;
        }

        const new_ptr = self.alloc_ptr;
        self.alloc_ptr += aligned_size;

        // Copy object
        const src = @as([*]u8, @ptrCast(old_obj));
        const dst = self.from_space.ptr + new_ptr;
        @memcpy(dst[0..total_size], src[0..total_size]);

        const new_obj: *Object = @ptrCast(@alignCast(dst));

        // Leave forwarding pointer in old object
        old_obj.header.setMarked(true);
        old_obj.header.hash = @intCast(new_ptr);

        return Value.fromObject(new_obj);
    }

    fn objectFieldCount(self: *Heap, obj: *Object) usize {
        // Get field count from class format info
        // For now, we'll store it in a simple way
        const class = self.getClass(obj.header.class_index);
        if (class.isObject()) {
            const class_obj = class.asObject();
            const format_val = class_obj.getField(CLASS_FIELD_FORMAT, CLASS_NUM_FIELDS);
            if (format_val.isSmallInt()) {
                return @intCast(format_val.asSmallInt());
            }
        }
        return 0;
    }

    fn objectByteCount(self: *Heap, obj: *Object) usize {
        // Similar to above
        const class = self.getClass(obj.header.class_index);
        if (class.isObject()) {
            const class_obj = class.asObject();
            const format_val = class_obj.getField(CLASS_FIELD_FORMAT, CLASS_NUM_FIELDS);
            if (format_val.isSmallInt()) {
                return @intCast(format_val.asSmallInt());
            }
        }
        return 0;
    }

    /// Get bytes from a string/bytearray object
    pub fn getBytes(self: *Heap, value: Value) ?[]const u8 {
        _ = self;
        if (!value.isObject()) return null;
        const obj = value.asObject();
        if (obj.header.getFormat() != .bytes) return null;

        // Get byte count from somewhere - for now use a fixed approach
        // In a real impl, we'd store size in the object or class
        const ptr: [*]u8 = @ptrFromInt(@intFromPtr(obj) + @sizeOf(ObjectHeader));
        // This is a simplified version - in reality we need to store the size
        return ptr[0..0]; // placeholder
    }

    /// Get the string representation of a symbol (for debugging)
    pub fn symbolName(self: *Heap, sym: Value) ?[]const u8 {
        _ = self;
        if (!sym.isObject()) return null;
        const obj = sym.asObject();
        if (obj.header.class_index != CLASS_SYMBOL) return null;

        // The symbol's bytes are its name
        // Again, we need proper size tracking
        return null; // placeholder
    }
};

test "Heap - basic allocation" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    // Allocate an object
    const obj = try heap.allocateObject(Heap.CLASS_OBJECT, 2, .normal);
    try std.testing.expect(obj.header.class_index == Heap.CLASS_OBJECT);

    // Fields should be nil
    const fields = obj.fields(2);
    try std.testing.expect(fields[0].isNil());
    try std.testing.expect(fields[1].isNil());
}

test "Heap - string allocation" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    const str = try heap.allocateString("Hello");
    try std.testing.expect(str.isObject());

    const obj = str.asObject();
    try std.testing.expect(obj.header.class_index == Heap.CLASS_STRING);
}

test "Heap - symbol interning" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    const sym1 = try heap.internSymbol("test");
    const sym2 = try heap.internSymbol("test");
    const sym3 = try heap.internSymbol("other");

    try std.testing.expect(sym1.eql(sym2)); // Same symbol
    try std.testing.expect(!sym1.eql(sym3)); // Different symbol
}
