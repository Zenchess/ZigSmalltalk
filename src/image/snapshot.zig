const std = @import("std");
const object = @import("../vm/object.zig");
const memory = @import("../vm/memory.zig");

const Value = object.Value;
const Object = object.Object;
const ObjectHeader = object.ObjectHeader;
const ClassFormat = object.ClassFormat;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;

/// Very small, portable-ish snapshot format.
/// We encode Values so that heap pointers become offsets and compiled methods
/// are stored separately and referenced by index.

const magic: [4]u8 = .{ 'Z', 'S', 'N', 'P' }; // Zig Snapshot

pub const SnapshotError = error{
    InvalidImage,
    UnsupportedVersion,
    OutOfMemory,
    IoError,
};

const ImageHeader = extern struct {
    magic: [4]u8 = magic,
    version: u32 = 1,
    space_size: u64, // Size of from_space (one semi-space)
    used_size: u64, // Bytes used in from_space
    class_count: u32,
    symbol_count: u32,
    global_count: u32,
    method_count: u32,
    object_count: u32,
};

const ValueTag = enum(u8) {
    immediate = 0,
    heap_object = 1,
    method = 2,
};

const EncodedValue = packed struct {
    tag: ValueTag,
    payload: u64,
};

const MethodEntry = struct {
    ptr: *CompiledMethod,
};

/// Write a snapshot to the given path.
pub fn saveToFile(heap: *Heap, allocator: std.mem.Allocator, path: []const u8) SnapshotError!void {
    var file = std.fs.cwd().createFile(path, .{ .truncate = true }) catch return SnapshotError.IoError;
    defer file.close();

    try save(heap, allocator, file);
}

/// Load a snapshot from the given path, returning a fresh Heap.
pub fn loadFromFile(allocator: std.mem.Allocator, path: []const u8) SnapshotError!*Heap {
    var file = std.fs.cwd().openFile(path, .{}) catch return SnapshotError.IoError;
    defer file.close();

    return load(allocator, file);
}

fn writeAll(writer: anytype, bytes: []const u8) SnapshotError!void {
    writer.writeAll(bytes) catch return SnapshotError.IoError;
}

fn writeStruct(writer: anytype, value: anytype) SnapshotError!void {
    try writeAll(writer, std.mem.asBytes(&value));
}

fn readExact(reader: anytype, buf: []u8) !void {
    var offset: usize = 0;
    while (offset < buf.len) {
        const n = reader.read(buf[offset..]) catch return SnapshotError.IoError;
        if (n == 0) return SnapshotError.InvalidImage;
        offset += n;
    }
}

fn readStruct(reader: anytype, comptime T: type) !T {
    var value: T = undefined;
    try readExact(reader, std.mem.asBytes(&value));
    return value;
}

/// Encode a Value so we can rebase pointers on load.
fn encodeValue(val: Value, base: usize, heap_end: usize, allocator: std.mem.Allocator, methods: *std.AutoHashMap(usize, usize), method_list: *std.ArrayList(MethodEntry)) !EncodedValue {
    if (!val.isObject()) {
        return .{ .tag = .immediate, .payload = val.bits };
    }

    const ptr_usize: usize = @intFromPtr(val.asObject());
    if (ptr_usize >= base and ptr_usize < heap_end) {
        return .{
            .tag = .heap_object,
            .payload = @intCast(ptr_usize - base),
        };
    }

    // Treat any non-heap object reference as a compiled method.
    const gop = try methods.getOrPut(ptr_usize);
    if (!gop.found_existing) {
        gop.value_ptr.* = method_list.items.len;
        try method_list.append(allocator, .{ .ptr = @ptrFromInt(ptr_usize) });
    }

    return .{
        .tag = .method,
        .payload = @intCast(methods.get(ptr_usize).?),
    };
}

fn decodeValue(enc: EncodedValue, base: usize, methods: []?*CompiledMethod) !Value {
    return switch (enc.tag) {
        .immediate => Value{ .bits = enc.payload },
        .heap_object => blk: {
            const obj_ptr = base + @as(usize, @intCast(enc.payload));
            break :blk Value.fromObject(@ptrFromInt(obj_ptr));
        },
        .method => blk: {
            const idx: usize = @intCast(enc.payload);
            if (idx >= methods.len or methods[idx] == null) return SnapshotError.InvalidImage;
            break :blk Value.fromObject(@ptrCast(methods[idx].?));
        },
    };
}

fn objectPayloadSize(obj: *Object) usize {
    const format = obj.header.getFormat();
    return switch (format) {
        .normal, .variable, .weak => obj.header.size * @sizeOf(Value),
        .bytes => obj.header.size,
        .words => obj.header.size * 4,
        .compiled_method => obj.header.size,
    };
}

fn objectTotalSize(obj: *Object) usize {
    const payload = objectPayloadSize(obj);
    const total = @sizeOf(ObjectHeader) + payload;
    return (total + 7) & ~@as(usize, 7);
}

fn scanHeapObjects(heap: *Heap, allocator: std.mem.Allocator, methods: *std.AutoHashMap(usize, usize), method_list: *std.ArrayList(MethodEntry)) !u32 {
    const base = @intFromPtr(heap.from_space.ptr);
    const heap_end = base + heap.alloc_ptr;
    var count: u32 = 0;

    var offset: usize = 0;
    while (offset < heap.alloc_ptr) : (count += 1) {
        const obj: *Object = @ptrCast(@alignCast(heap.from_space.ptr + offset));
        const format = obj.header.getFormat();

        if (format == .normal or format == .variable or format == .weak) {
            const fields = obj.fields(obj.header.size);
            for (fields) |v| {
                _ = try encodeValue(v, base, heap_end, allocator, methods, method_list);
            }
        } else if (format == .compiled_method) {
            // Rare, but handle literals similarly
            const method: *CompiledMethod = @ptrCast(@alignCast(obj));
            const lits = method.getLiterals();
            for (lits) |v| {
                _ = try encodeValue(v, base, heap_end, allocator, methods, method_list);
            }
        }

        offset += objectTotalSize(obj);
    }

    return count;
}

pub fn save(heap: *Heap, allocator: std.mem.Allocator, writer: anytype) SnapshotError!void {
    var methods = std.AutoHashMap(usize, usize).init(allocator);
    defer methods.deinit();
    var method_list = std.ArrayList(MethodEntry){
        .items = &[_]MethodEntry{},
        .capacity = 0,
    };
    defer method_list.deinit(allocator);

    // First pass: scan heap + roots to collect methods
    const base = @intFromPtr(heap.from_space.ptr);
    const heap_end = base + heap.alloc_ptr;

    // Roots: class table
    for (heap.class_table.items) |v| {
        _ = try encodeValue(v, base, heap_end, allocator, &methods, &method_list);
    }

    // Roots: symbol table
    var sym_it = heap.symbol_table.iterator();
    while (sym_it.next()) |entry| {
        _ = try encodeValue(entry.value_ptr.*, base, heap_end, allocator, &methods, &method_list);
    }

    // Roots: globals
    var glob_it = heap.globals.iterator();
    while (glob_it.next()) |entry| {
        _ = try encodeValue(entry.value_ptr.*, base, heap_end, allocator, &methods, &method_list);
    }

    const object_count = try scanHeapObjects(heap, allocator, &methods, &method_list);

    const header = ImageHeader{
        .space_size = heap.space_size,
        .used_size = heap.alloc_ptr,
        .class_count = @intCast(heap.class_table.items.len),
        .symbol_count = @intCast(heap.symbol_table.count()),
        .global_count = @intCast(heap.globals.count()),
        .method_count = @intCast(method_list.items.len),
        .object_count = object_count,
    };

    try writeStruct(writer, header);

    // Write class table
    for (heap.class_table.items) |v| {
        const enc = try encodeValue(v, base, heap_end, allocator, &methods, &method_list);
        try writeStruct(writer, enc);
    }

    // Write symbol table
    sym_it = heap.symbol_table.iterator();
    while (sym_it.next()) |entry| {
        const key = entry.key_ptr.*;
        const len_be: u32 = @intCast(key.len);
        try writeStruct(writer, len_be);
        try writeAll(writer, key);
        const enc = try encodeValue(entry.value_ptr.*, base, heap_end, allocator, &methods, &method_list);
        try writeStruct(writer, enc);
    }

    // Write globals
    glob_it = heap.globals.iterator();
    while (glob_it.next()) |entry| {
        const key = entry.key_ptr.*;
        const len_be: u32 = @intCast(key.len);
        try writeStruct(writer, len_be);
        try writeAll(writer, key);
        const enc = try encodeValue(entry.value_ptr.*, base, heap_end, allocator, &methods, &method_list);
        try writeStruct(writer, enc);
    }

    // Write methods
    for (method_list.items) |m| {
        const method = m.ptr;
        try writeStruct(writer, method.header);

        const lits = method.getLiterals();
        const lit_count: u32 = @intCast(lits.len);
        try writeStruct(writer, lit_count);
        for (lits) |lit| {
            const enc = try encodeValue(lit, base, heap_end, allocator, &methods, &method_list);
            try writeStruct(writer, enc);
        }

        const bc = method.getBytecodes();
        const bc_len: u32 = @intCast(bc.len);
        try writeStruct(writer, bc_len);
        try writeAll(writer, bc);
    }

    // Write heap objects
    var offset: usize = 0;
    while (offset < heap.alloc_ptr) {
        const obj: *Object = @ptrCast(@alignCast(heap.from_space.ptr + offset));
        const format = obj.header.getFormat();
        try writeStruct(writer, @as(u64, @intCast(offset)));
        try writeStruct(writer, obj.header);

        switch (format) {
            .normal, .variable, .weak => {
                const fields = obj.fields(obj.header.size);
                const count: u32 = @intCast(fields.len);
                try writeStruct(writer, count);
                for (fields) |f| {
                    const enc = try encodeValue(f, base, heap_end, allocator, &methods, &method_list);
                    try writeStruct(writer, enc);
                }
            },
            .bytes => {
                const data = obj.bytes(obj.header.size);
                try writeAll(writer, data);
            },
            .words => {
                const bytes_slice: []const u8 = obj.bytes(obj.header.size * 4);
                try writeAll(writer, bytes_slice);
            },
            .compiled_method => {
                const method: *CompiledMethod = @ptrCast(@alignCast(obj));
                // Header already written via obj.header; now literals + bytecodes
                const lits = method.getLiterals();
                const lit_count: u32 = @intCast(lits.len);
                try writeStruct(writer, lit_count);
                for (lits) |lit| {
                    const enc = try encodeValue(lit, base, heap_end, allocator, &methods, &method_list);
                    try writeStruct(writer, enc);
                }
                const bc = method.getBytecodes();
                const bc_len: u32 = @intCast(bc.len);
                try writeStruct(writer, bc_len);
                try writeAll(writer, bc);
            },
        }

        offset += objectTotalSize(obj);
    }
}

pub fn load(allocator: std.mem.Allocator, reader: anytype) SnapshotError!*Heap {
    const header = readStruct(reader, ImageHeader) catch return SnapshotError.IoError;
    if (!std.mem.eql(u8, &header.magic, &magic)) return SnapshotError.InvalidImage;
    if (header.version != 1) return SnapshotError.UnsupportedVersion;

    const heap_size = header.space_size * 2;
    const heap = try Heap.init(allocator, heap_size);

    // Prepare space
    if (header.used_size > heap.space_size) return SnapshotError.InvalidImage;
    heap.alloc_ptr = @intCast(header.used_size);

    const base_ptr: usize = @intFromPtr(heap.from_space.ptr);

    // Read class table
    try heap.class_table.ensureTotalCapacity(allocator, header.class_count);
    var class_items = heap.class_table.items;
    heap.class_table.items = class_items[0..0];

    // Temporarily store encoded values to resolve methods after methods are loaded
    const encoded_classes = try allocator.alloc(EncodedValue, header.class_count);
    defer allocator.free(encoded_classes);
    for (encoded_classes, 0..) |*slot, idx| {
        slot.* = readStruct(reader, EncodedValue) catch return SnapshotError.IoError;
        _ = idx;
    }

    // Symbol table entries
    const SymbolEntry = struct { key: []u8, value: EncodedValue };
    var encoded_symbols = std.ArrayList(SymbolEntry){
        .items = &[_]SymbolEntry{},
        .capacity = 0,
    };
    defer {
        for (encoded_symbols.items) |entry| allocator.free(entry.key);
        encoded_symbols.deinit(allocator);
    }
    try encoded_symbols.ensureTotalCapacity(allocator, header.symbol_count);
    var sym_i: u32 = 0;
    while (sym_i < header.symbol_count) : (sym_i += 1) {
        const len = readStruct(reader, u32) catch return SnapshotError.IoError;
        const key_buf = try allocator.alloc(u8, len);
        readExact(reader, key_buf) catch return SnapshotError.IoError;
        const enc = readStruct(reader, EncodedValue) catch return SnapshotError.IoError;
        encoded_symbols.appendAssumeCapacity(.{ .key = key_buf, .value = enc });
    }

    // Globals
    var encoded_globals = std.ArrayList(SymbolEntry){
        .items = &[_]SymbolEntry{},
        .capacity = 0,
    };
    try encoded_globals.ensureTotalCapacity(allocator, header.global_count);
    defer {
        for (encoded_globals.items) |entry| allocator.free(entry.key);
        encoded_globals.deinit(allocator);
    }
    var g_i: u32 = 0;
    while (g_i < header.global_count) : (g_i += 1) {
        const len = readStruct(reader, u32) catch return SnapshotError.IoError;
        const key_buf = try allocator.alloc(u8, len);
        readExact(reader, key_buf) catch return SnapshotError.IoError;
        const enc = readStruct(reader, EncodedValue) catch return SnapshotError.IoError;
        encoded_globals.appendAssumeCapacity(.{ .key = key_buf, .value = enc });
    }

    // Methods
    var methods = try allocator.alloc(?*CompiledMethod, header.method_count);
    for (methods) |*m| m.* = null;
    defer allocator.free(methods);

    var method_idx: usize = 0;
    while (method_idx < header.method_count) : (method_idx += 1) {
        const mh = readStruct(reader, CompiledMethod.MethodHeader) catch return SnapshotError.IoError;
        const lit_count = readStruct(reader, u32) catch return SnapshotError.IoError;
        const literals = try allocator.alloc(EncodedValue, lit_count);
        defer allocator.free(literals);
        for (literals, 0..) |*lit, i| {
            lit.* = readStruct(reader, EncodedValue) catch return SnapshotError.IoError;
            _ = i;
        }
        const bc_len = readStruct(reader, u32) catch return SnapshotError.IoError;
        const bytecodes = try allocator.alloc(u8, bc_len);
        defer allocator.free(bytecodes);
        readExact(reader, bytecodes) catch return SnapshotError.IoError;

        // Allocate compiled method
        const header_size = @sizeOf(CompiledMethod.MethodHeader);
        const literals_size = @as(usize, mh.num_literals) * @sizeOf(Value);
        const total_size = header_size + literals_size + bc_len;
        const mem = allocator.alignedAlloc(u8, .@"8", total_size) catch return SnapshotError.OutOfMemory;
        const method: *CompiledMethod = @ptrCast(@alignCast(mem.ptr));
        method.header = mh;

        const lit_ptr: [*]Value = @ptrCast(@alignCast(mem.ptr + header_size));
        for (literals, 0..) |enc, i| {
            lit_ptr[i] = try decodeValue(enc, base_ptr, methods);
        }
        const bc_ptr: [*]u8 = @ptrCast(mem.ptr + header_size + literals_size);
        @memcpy(bc_ptr[0..bc_len], bytecodes);

        methods[method_idx] = method;
    }

    // Objects
    var obj_i: u32 = 0;
    while (obj_i < header.object_count) : (obj_i += 1) {
        const offset = readStruct(reader, u64) catch return SnapshotError.IoError;
        const obj_header = readStruct(reader, ObjectHeader) catch return SnapshotError.IoError;
        const obj_ptr = heap.from_space.ptr + @as(usize, @intCast(offset));
        const obj: *Object = @ptrCast(@alignCast(obj_ptr));
        obj.header = obj_header;

        switch (obj_header.getFormat()) {
            .normal, .variable, .weak => {
                const count = readStruct(reader, u32) catch return SnapshotError.IoError;
                const fields = obj.fields(count);
                for (fields, 0..) |*slot, i| {
                    const enc = readStruct(reader, EncodedValue) catch return SnapshotError.IoError;
                    slot.* = try decodeValue(enc, base_ptr, methods);
                    _ = i;
                }
            },
            .bytes => {
                const bytes = obj.bytes(obj_header.size);
                readExact(reader, bytes) catch return SnapshotError.IoError;
            },
            .words => {
                const bytes = obj.bytes(obj_header.size * 4);
                readExact(reader, bytes) catch return SnapshotError.IoError;
            },
            .compiled_method => {
                const lit_count = readStruct(reader, u32) catch return SnapshotError.IoError;
                const method: *CompiledMethod = @ptrCast(@alignCast(obj));
                const lit_ptr = method.getLiterals();
                for (lit_ptr[0..lit_count], 0..) |*slot, i| {
                    const enc = readStruct(reader, EncodedValue) catch return SnapshotError.IoError;
                    slot.* = try decodeValue(enc, base_ptr, methods);
                    _ = i;
                }
                const bc_len = readStruct(reader, u32) catch return SnapshotError.IoError;
                const bc_ptr = method.getBytecodes();
                if (bc_ptr.len < bc_len) return SnapshotError.InvalidImage;
                readExact(reader, bc_ptr[0..bc_len]) catch return SnapshotError.IoError;
            },
        }
    }

    // Rebuild class table
    for (encoded_classes) |enc| {
        const val = try decodeValue(enc, base_ptr, methods);
        heap.class_table.append(allocator, val) catch return SnapshotError.OutOfMemory;
    }

    // Rebuild symbols
    for (encoded_symbols.items) |entry| {
        const val = try decodeValue(entry.value, base_ptr, methods);
        try heap.symbol_table.put(allocator, try allocator.dupe(u8, entry.key), val);
    }

    // Rebuild globals
    for (encoded_globals.items) |entry| {
        const val = try decodeValue(entry.value, base_ptr, methods);
        try heap.globals.put(allocator, try allocator.dupe(u8, entry.key), val);
    }

    return heap;
}
