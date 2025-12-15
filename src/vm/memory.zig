const std = @import("std");
const object = @import("object.zig");
const Value = object.Value;
const Object = object.Object;
const ObjectHeader = object.ObjectHeader;
const ClassFormat = object.ClassFormat;
const CompiledMethod = object.CompiledMethod;

// Forward declaration for interpreter reference
const Interpreter = @import("interpreter.zig").Interpreter;

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

    // Interpreter reference for GC stack tracing
    interpreter: ?*Interpreter = null,

    // Current image path (for SessionManager >> imagePath)
    image_path: ?[]const u8 = null,

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
    pub const CLASS_EXCEPTION: u32 = 18;
    pub const CLASS_ERROR: u32 = 19;
    pub const CLASS_MESSAGE: u32 = 20; // For doesNotUnderstand:
    pub const CLASS_DICTIONARY: u32 = 21;
    pub const CLASS_SET: u32 = 22;
    pub const CLASS_ORDERED_COLLECTION: u32 = 23;
    pub const CLASS_READ_STREAM: u32 = 24;
    pub const CLASS_WRITE_STREAM: u32 = 25;
    pub const CLASS_FILE_STREAM: u32 = 26;
    pub const CLASS_METHOD_CONTEXT: u32 = 27; // Heap-allocated activation record

    pub const CLASS_DEAF_OBJECT: u32 = 36;
    pub const CLASS_TRANSCRIPT_SHELL: u32 = 37;
    pub const CLASS_ASSOCIATION: u32 = 38;
    pub const CLASS_POOL_DICTIONARY: u32 = 39;
    pub const CLASS_MAGNITUDE: u32 = 40;
    pub const CLASS_NUMBER: u32 = 41;
    pub const CLASS_INTEGER: u32 = 42;
    pub const CLASS_BOOLEAN: u32 = 43;
    pub const CLASS_COLLECTION: u32 = 44;
    pub const CLASS_SEQUENCEABLE_COLLECTION: u32 = 45;
    pub const CLASS_ARRAYED_COLLECTION: u32 = 46;

    // Class object field indices
    pub const CLASS_FIELD_SUPERCLASS: usize = 0;
    pub const CLASS_FIELD_METHOD_DICT: usize = 1;
    pub const CLASS_FIELD_FORMAT: usize = 2; // Holds Dolphin-style instanceSpec for the class
    pub const CLASS_FIELD_INST_VARS: usize = 3;
    pub const CLASS_FIELD_NAME: usize = 4;
    pub const CLASS_FIELD_CLASS_VARS: usize = 5; // Class variables dictionary
    pub const CLASS_FIELD_METACLASS: usize = 6; // Points to metaclass object (for class-side methods)
    pub const CLASS_FIELD_POOL_DICTS: usize = 7; // Array of pool dictionaries
    pub const CLASS_FIELD_CLASS_INST_VARS: usize = 8; // Class instance variables (stored on metaclass)
    pub const CLASS_FIELD_CATEGORY: usize = 9; // Package/category name (Symbol)
    pub const CLASS_NUM_FIELDS: usize = 10;

    // Metaclass has an extra field: thisClass (the class it's a metaclass of)
    pub const METACLASS_FIELD_THIS_CLASS: usize = 10;
    pub const METACLASS_NUM_FIELDS: usize = 11;

    // MethodContext field indices (heap-allocated activation record)
    // MethodContext is variable-sized: fixed fields + stack slots for temps/args
    pub const CONTEXT_FIELD_SENDER: usize = 0; // Calling context or nil
    pub const CONTEXT_FIELD_PC: usize = 1; // Instruction pointer (SmallInteger)
    pub const CONTEXT_FIELD_STACKP: usize = 2; // Stack pointer within context (SmallInteger)
    pub const CONTEXT_FIELD_METHOD: usize = 3; // CompiledMethod being executed
    pub const CONTEXT_FIELD_CLOSURE_OR_RECEIVER: usize = 4; // For blocks: closure, for methods: receiver
    pub const CONTEXT_NUM_FIXED_FIELDS: usize = 5;
    // After fixed fields: stack slots (temps, args, operand stack)

    // BlockClosure field indices (updated to use context references)
    pub const BLOCK_FIELD_OUTER_CONTEXT: usize = 0; // MethodContext where block was created
    pub const BLOCK_FIELD_START_PC: usize = 1; // SmallInteger - bytecode offset
    pub const BLOCK_FIELD_NUM_ARGS: usize = 2; // SmallInteger - number of arguments
    pub const BLOCK_FIELD_METHOD: usize = 3; // CompiledMethod containing block's bytecodes
    pub const BLOCK_FIELD_RECEIVER: usize = 4; // Receiver when block was created
    pub const BLOCK_FIELD_HOME_CONTEXT: usize = 5; // Home context for non-local returns
    pub const BLOCK_FIELD_NUM_TEMPS: usize = 6; // SmallInteger - number of block-local temps
    pub const BLOCK_NUM_FIELDS: usize = 7;

    // Dolphin Behavior instanceSpec masks (kept here so VM can synthesize instanceSpec values)
    pub const INSTSPEC_SIZE_MASK: i61 = 0xFF;
    pub const INSTSPEC_VARIABLE_MASK: i61 = 0x1000;
    pub const INSTSPEC_POINTERS_MASK: i61 = 0x2000;
    pub const INSTSPEC_NULLTERM_MASK: i61 = 0x4000; // Exposed for completeness
    pub const INSTSPEC_INDIRECT_MASK: i61 = 0x800;

    pub const InstanceSpecInfo = struct {
        inst_size: usize,
        format: ClassFormat,
        is_pointers: bool,
        is_variable: bool,
    };

    /// Encode our ClassFormat + fixed inst var count into a Dolphin-compatible instanceSpec.
    /// We only set the bits we understand (pointers/variable/size); other bits remain clear.
    pub fn encodeInstanceSpec(inst_size: usize, format: ClassFormat) i61 {
        const size_part: i61 = @intCast(inst_size & 0xFF);
        const variable = switch (format) {
            .variable, .bytes, .words => INSTSPEC_VARIABLE_MASK,
            else => 0,
        };
        const pointers = switch (format) {
            .bytes, .words => 0,
            else => INSTSPEC_POINTERS_MASK,
        };
        return size_part | variable | pointers;
    }

    /// Decode a Dolphin-style instanceSpec into inst size + ClassFormat (best effort).
    pub fn decodeInstanceSpec(spec: i61) InstanceSpecInfo {
        const inst_size: usize = @intCast(spec & INSTSPEC_SIZE_MASK);
        const is_variable = (spec & INSTSPEC_VARIABLE_MASK) != 0;
        const is_pointers = (spec & INSTSPEC_POINTERS_MASK) != 0;

        const format: ClassFormat = if (is_variable) blk: {
            break :blk if (is_pointers) .variable else .bytes;
        } else blk2: {
            break :blk2 if (is_pointers) .normal else .bytes;
        };

        return .{
            .inst_size = inst_size,
            .format = format,
            .is_pointers = is_pointers,
            .is_variable = is_variable,
        };
    }

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

            // Check if this is a class object - if so, return its metaclass
            if (obj.header.class_index == CLASS_CLASS or obj.header.class_index == CLASS_METACLASS) {
                // This is a class or metaclass object - get its metaclass from the METACLASS field
                const obj_size = obj.header.size;
                const metaclass = obj.getField(CLASS_FIELD_METACLASS, obj_size);
                if (!metaclass.isNil() and metaclass.isObject()) {
                    return metaclass;
                }
                // Lazily create a metaclass if missing
                if (self.createMetaclassIfMissing(obj)) |meta_val| {
                    return meta_val;
                }
                // Debug: metaclass lookup failed
                const class_name = obj.getField(CLASS_FIELD_NAME, obj_size);
                if (class_name.isObject()) {
                    const name_obj = class_name.asObject();
                    if (name_obj.header.class_index == CLASS_SYMBOL) {
                        const name_bytes = name_obj.bytes(name_obj.header.size);
                        std.debug.print("classOf: class '{s}' has nil metaclass, falling back to Class\n", .{name_bytes});
                    }
                }
            }

            return self.getClass(obj.header.class_index);
        }
        return Value.nil;
    }

    fn createMetaclassIfMissing(self: *Heap, class_obj: *Object) ?Value {
        // Attempt to synthesize a metaclass when one is absent.
        const new_meta = self.allocateObject(CLASS_METACLASS, METACLASS_NUM_FIELDS, .normal) catch return null;
        const class_size = class_obj.header.size;

        // Set name if available
        const name_val = class_obj.getField(CLASS_FIELD_NAME, class_size);
        if (name_val.isObject() and name_val.asObject().header.class_index == CLASS_SYMBOL) {
            const name_obj = name_val.asObject();
            const name_bytes = name_obj.bytes(name_obj.header.size);
            var buf: [128]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{s} class", .{name_bytes}) catch null;
            if (formatted) |slice| {
                if (self.internSymbol(slice)) |sym| {
                    new_meta.setField(CLASS_FIELD_NAME, sym, METACLASS_NUM_FIELDS);
                } else |_| {}
            }
        }

        // Link to the class and set format/superclass
        new_meta.setField(METACLASS_FIELD_THIS_CLASS, Value.fromObject(class_obj), METACLASS_NUM_FIELDS);
        const fmt = encodeInstanceSpec(@intCast(CLASS_NUM_FIELDS), .normal);
        new_meta.setField(CLASS_FIELD_FORMAT, Value.fromSmallInt(fmt), METACLASS_NUM_FIELDS);

        // Inherit metaclass chain from superclass if possible
        const super_val = class_obj.getField(CLASS_FIELD_SUPERCLASS, class_size);
        if (super_val.isObject()) {
            const super_obj = super_val.asObject();
            const super_meta = super_obj.getField(CLASS_FIELD_METACLASS, super_obj.header.size);
            if (super_meta.isObject()) {
                new_meta.setField(CLASS_FIELD_SUPERCLASS, super_meta, METACLASS_NUM_FIELDS);
            }
        }
        if (new_meta.getField(CLASS_FIELD_SUPERCLASS, METACLASS_NUM_FIELDS).isNil()) {
            new_meta.setField(CLASS_FIELD_SUPERCLASS, self.getClass(CLASS_CLASS), METACLASS_NUM_FIELDS);
        }

        class_obj.setField(CLASS_FIELD_METACLASS, Value.fromObject(new_meta), class_size);
        return Value.fromObject(new_meta);
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

        // 4. Interpreter stack (critical for GC during execution)
        if (self.interpreter) |interp| {
            // Trace all values on the stack
            for (interp.stack[0..interp.sp]) |*slot| {
                slot.* = try self.copyObject(slot.*);
            }
            // Trace current receiver
            interp.receiver = try self.copyObject(interp.receiver);
            // Trace method class
            interp.method_class = try self.copyObject(interp.method_class);
            // Trace current method - always copy as it's a heap object
            const method_val = Value.fromObject(@ptrCast(@alignCast(interp.method)));
            const new_method_val = try self.copyObject(method_val);
            if (new_method_val.isObject()) {
                interp.method = @ptrCast(@alignCast(new_method_val.asObject()));
            }
            // IMPORTANT: Also trace literals of off-heap methods!
            // Off-heap methods (allocated with page_allocator) aren't scanned by the
            // normal GC scan loop, so we need to explicitly trace their literals.
            try self.traceMethodLiterals(interp.method);
            // Trace context stack
            for (interp.contexts[0..interp.context_ptr]) |*ctx| {
                ctx.receiver = try self.copyObject(ctx.receiver);
                ctx.method_class = try self.copyObject(ctx.method_class);
                if (ctx.closure) |closure| {
                    ctx.closure = try self.copyObject(closure);
                }
                // Trace context method - always copy as it's a heap object
                const ctx_method_val = Value.fromObject(@ptrCast(@alignCast(ctx.method)));
                const new_ctx_method_val = try self.copyObject(ctx_method_val);
                if (new_ctx_method_val.isObject()) {
                    ctx.method = @ptrCast(@alignCast(new_ctx_method_val.asObject()));
                }
                // Also trace literals of off-heap methods in context stack
                try self.traceMethodLiterals(ctx.method);
            }
            // Trace exception handler values
            for (interp.exception_handlers[0..interp.handler_ptr]) |*handler| {
                handler.exception_class = try self.copyObject(handler.exception_class);
                handler.handler_block = try self.copyObject(handler.handler_block);
            }
            // Trace last MNU receiver
            interp.last_mnu_receiver = try self.copyObject(interp.last_mnu_receiver);
            // Trace current exception
            interp.current_exception = try self.copyObject(interp.current_exception);
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
        const obj_addr = @intFromPtr(old_obj);

        // Check if already in new space (from_space after swap) - already copied
        const new_space_start = @intFromPtr(self.from_space.ptr);
        const new_space_end = new_space_start + self.alloc_ptr;
        if (obj_addr >= new_space_start and obj_addr < new_space_end) {
            return value; // Already in new space, no need to copy
        }

        // Validate pointer is within old space (to_space after swap)
        const old_space_start = @intFromPtr(self.to_space.ptr);
        const old_space_end = old_space_start + self.space_size;
        if (obj_addr < old_space_start or obj_addr >= old_space_end) {
            // This is not a valid heap pointer - return unchanged
            // This might be FFI data that looks like a pointer, or corrupted data
            return value;
        }

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

    /// Trace literals in a method that may be off-heap.
    /// Off-heap methods (allocated with page_allocator) aren't scanned by the
    /// normal GC scan loop, so we need to explicitly trace their literals.
    fn traceMethodLiterals(self: *Heap, method: *CompiledMethod) !void {
        const method_addr = @intFromPtr(method);
        const old_space_start = @intFromPtr(self.to_space.ptr);
        const old_space_end = old_space_start + self.space_size;
        const new_space_start = @intFromPtr(self.from_space.ptr);
        const new_space_end = new_space_start + self.alloc_ptr;

        // If method is in GC heap (either old or new space), its literals will be
        // traced by the normal scan loop. Only trace off-heap methods.
        if ((method_addr >= old_space_start and method_addr < old_space_end) or
            (method_addr >= new_space_start and method_addr < new_space_end))
        {
            return; // In-heap method, will be scanned normally
        }

        // Method is off-heap - explicitly trace its literals
        const literals = method.getLiterals();
        for (literals) |*lit| {
            lit.* = try self.copyObject(lit.*);
        }
    }

    fn objectFieldCount(self: *Heap, obj: *Object) usize {
        // Get field count from class format info
        // For now, we'll store it in a simple way
        const class = self.getClass(obj.header.class_index);
        if (class.isObject()) {
            const class_obj = class.asObject();
            const format_val = class_obj.getField(CLASS_FIELD_FORMAT, class_obj.header.size);
            if (format_val.isSmallInt()) {
                const info = decodeInstanceSpec(format_val.asSmallInt());
                return info.inst_size;
            }
        }
        return 0;
    }

    fn objectByteCount(self: *Heap, obj: *Object) usize {
        // Similar to above
        const class = self.getClass(obj.header.class_index);
        if (class.isObject()) {
            const class_obj = class.asObject();
            const format_val = class_obj.getField(CLASS_FIELD_FORMAT, class_obj.header.size);
            if (format_val.isSmallInt()) {
                const info = decodeInstanceSpec(format_val.asSmallInt());
                return info.inst_size;
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
