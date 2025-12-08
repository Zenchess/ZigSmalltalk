const std = @import("std");
const object = @import("../vm/object.zig");
const memory = @import("../vm/memory.zig");
const parser = @import("../compiler/parser.zig");
const codegen = @import("../compiler/codegen.zig");

const Value = object.Value;
const Heap = memory.Heap;
const Parser = parser.Parser;
const CodeGenerator = codegen.CodeGenerator;

/// Errors that can occur during file-in operations
pub const FileInError = error{
    InvalidChunkFormat,
    ClassNotFound,
    InvalidMethodDefinition,
    CompilationFailed,
    OutOfMemory,
    FileNotFound,
    InvalidEncoding,
};

/// Represents a parsed chunk from a .st file
pub const Chunk = struct {
    /// The class name (e.g., "Object", "Array")
    class_name: []const u8,
    /// Whether this is a class-side method (vs instance-side)
    is_class_side: bool,
    /// The method selector (e.g., "initialize", "at:put:")
    selector: []const u8,
    /// The method source code
    source: []const u8,
};

/// Result from parsing a class definition
pub const ClassDefinition = struct {
    superclass_name: []const u8,
    class_name: []const u8,
    instance_variables: []const []const u8,
    class_variables: []const []const u8,
    category: []const u8,
};

/// Chunk file parser for Dolphin/Squeak .st format
pub const ChunkParser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) ChunkParser {
        var start: usize = 0;
        // Skip UTF-8 BOM if present
        if (source.len >= 3 and source[0] == 0xEF and source[1] == 0xBB and source[2] == 0xBF) {
            start = 3;
        }
        return .{
            .allocator = allocator,
            .source = source,
            .pos = start,
        };
    }

    /// Read the next chunk from the file
    /// Returns null when no more chunks are available
    pub fn nextChunk(self: *ChunkParser) ?[]const u8 {
        // Skip whitespace and empty lines
        while (self.pos < self.source.len and isWhitespace(self.source[self.pos])) {
            self.pos += 1;
        }

        if (self.pos >= self.source.len) return null;

        // Check for chunk delimiter '!'
        if (self.source[self.pos] == '!') {
            self.pos += 1;
            // Skip whitespace after '!'
            while (self.pos < self.source.len and isWhitespace(self.source[self.pos])) {
                self.pos += 1;
            }
        }

        if (self.pos >= self.source.len) return null;

        // Read until next '!' or end of file
        const start = self.pos;
        var in_string = false;
        var prev_char: u8 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            // Track string literals to ignore '!' inside strings
            if (c == '\'' and prev_char != '\'') {
                in_string = !in_string;
            }

            // '!' ends the chunk (unless inside a string or escaped as '!!')
            if (c == '!' and !in_string) {
                // Check for escaped '!!'
                if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '!') {
                    self.pos += 2;
                    prev_char = '!';
                    continue;
                }
                break;
            }

            prev_char = c;
            self.pos += 1;
        }

        const chunk = self.source[start..self.pos];

        // Skip the terminating '!'
        if (self.pos < self.source.len and self.source[self.pos] == '!') {
            self.pos += 1;
        }

        // Return trimmed chunk
        const trimmed = std.mem.trim(u8, chunk, " \t\r\n");
        if (trimmed.len == 0) return self.nextChunk();

        return trimmed;
    }

    fn isWhitespace(c: u8) bool {
        return c == ' ' or c == '\t' or c == '\r' or c == '\n';
    }
};

/// Parse a method chunk header like "ClassName methodSelector" or "ClassName class methodSelector"
pub fn parseMethodHeader(chunk: []const u8) ?Chunk {
    // Find the first line (method header)
    var header_end: usize = 0;
    while (header_end < chunk.len and chunk[header_end] != '\n' and chunk[header_end] != '\r') {
        header_end += 1;
    }

    const header = std.mem.trim(u8, chunk[0..header_end], " \t");
    if (header.len == 0) return null;

    // Skip past the header to get the source
    var source_start = header_end;
    while (source_start < chunk.len and (chunk[source_start] == '\n' or chunk[source_start] == '\r')) {
        source_start += 1;
    }
    const source = if (source_start < chunk.len) chunk[source_start..] else "";

    // Parse header: "ClassName selector" or "ClassName class selector"
    var iter = std.mem.splitScalar(u8, header, ' ');

    const class_name = iter.next() orelse return null;
    if (class_name.len == 0) return null;

    var next_part = iter.next() orelse return null;
    var is_class_side = false;

    // Check for "class" keyword
    if (std.mem.eql(u8, next_part, "class")) {
        is_class_side = true;
        next_part = iter.next() orelse return null;
    }

    // The rest is the selector (may contain spaces for keyword selectors)
    var selector_buf: [256]u8 = undefined;
    var selector_len: usize = 0;

    // Copy first part of selector
    @memcpy(selector_buf[0..next_part.len], next_part);
    selector_len = next_part.len;

    // Append remaining parts (for keyword selectors like "at: put:")
    while (iter.next()) |part| {
        if (selector_len + 1 + part.len > selector_buf.len) break;
        selector_buf[selector_len] = ' ';
        selector_len += 1;
        @memcpy(selector_buf[selector_len .. selector_len + part.len], part);
        selector_len += part.len;
    }

    return Chunk{
        .class_name = class_name,
        .is_class_side = is_class_side,
        .selector = selector_buf[0..selector_len],
        .source = source,
    };
}

/// Check if a chunk is a class definition
pub fn isClassDefinition(chunk: []const u8) bool {
    // Class definitions typically start with a superclass name and "subclass:"
    return std.mem.indexOf(u8, chunk, "subclass:") != null or
        std.mem.indexOf(u8, chunk, "variableSubclass:") != null or
        std.mem.indexOf(u8, chunk, "variableByteSubclass:") != null or
        std.mem.indexOf(u8, chunk, "variableWordSubclass:") != null;
}

/// Check if a chunk is a method definition marker
pub fn isMethodDefinitionMarker(chunk: []const u8) bool {
    // Method definition markers are like "!ClassName methodsFor: 'category'!"
    return std.mem.indexOf(u8, chunk, "methodsFor:") != null;
}

/// File-in manager for loading Smalltalk source files
pub const FileIn = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    /// Current class being defined
    current_class: ?[]const u8,
    /// Whether current methods are class-side
    current_is_class_side: bool,
    /// Statistics
    methods_loaded: usize,
    classes_defined: usize,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) FileIn {
        return .{
            .allocator = allocator,
            .heap = heap,
            .current_class = null,
            .current_is_class_side = false,
            .methods_loaded = 0,
            .classes_defined = 0,
            .errors = .{},
        };
    }

    pub fn deinit(self: *FileIn) void {
        self.errors.deinit(self.allocator);
    }

    /// Load a .st file from disk
    pub fn loadFile(self: *FileIn, path: []const u8) !void {
        const file = std.fs.cwd().openFile(path, .{}) catch {
            return FileInError.FileNotFound;
        };
        defer file.close();

        const content = file.readToEndAlloc(self.allocator, 10 * 1024 * 1024) catch {
            return FileInError.OutOfMemory;
        };
        defer self.allocator.free(content);

        try self.loadSource(content);
    }

    /// Load Smalltalk source from a string
    pub fn loadSource(self: *FileIn, source: []const u8) !void {
        var chunk_parser = ChunkParser.init(self.allocator, source);

        while (chunk_parser.nextChunk()) |chunk| {
            self.processChunk(chunk) catch |err| {
                // Log error but continue processing
                const err_msg = switch (err) {
                    FileInError.ClassNotFound => "Class not found",
                    FileInError.InvalidMethodDefinition => "Invalid method definition",
                    FileInError.CompilationFailed => "Compilation failed",
                    else => "Unknown error",
                };
                self.errors.append(self.allocator, err_msg) catch {};
            };
        }
    }

    /// Process a single chunk
    fn processChunk(self: *FileIn, chunk: []const u8) !void {
        // Check for class definition
        if (isClassDefinition(chunk)) {
            try self.processClassDefinition(chunk);
            return;
        }

        // Check for method category marker (e.g., "!Object methodsFor: 'accessing'!")
        if (isMethodDefinitionMarker(chunk)) {
            try self.processMethodMarker(chunk);
            return;
        }

        // Otherwise treat as a method body
        try self.processMethodBody(chunk);
    }

    /// Process a class definition chunk
    fn processClassDefinition(self: *FileIn, chunk: []const u8) !void {
        // Parse class definition like:
        // Object subclass: #MyClass
        //     instanceVariableNames: 'x y z'
        //     classVariableNames: ''
        //     poolDictionaries: ''
        //     classInstanceVariableNames: ''

        // Find superclass name (first identifier before "subclass:")
        const subclass_idx = std.mem.indexOf(u8, chunk, "subclass:") orelse
            std.mem.indexOf(u8, chunk, "variableSubclass:") orelse
            std.mem.indexOf(u8, chunk, "variableByteSubclass:") orelse
            std.mem.indexOf(u8, chunk, "variableWordSubclass:") orelse
            return FileInError.InvalidChunkFormat;

        const superclass_name = std.mem.trim(u8, chunk[0..subclass_idx], " \t\r\n");
        if (superclass_name.len == 0) return FileInError.InvalidChunkFormat;

        // Find class name after "#"
        const hash_idx = std.mem.indexOf(u8, chunk[subclass_idx..], "#") orelse
            return FileInError.InvalidChunkFormat;
        const class_start = subclass_idx + hash_idx + 1;

        var class_end = class_start;
        while (class_end < chunk.len and isIdentChar(chunk[class_end])) {
            class_end += 1;
        }
        const class_name = chunk[class_start..class_end];
        if (class_name.len == 0) return FileInError.InvalidChunkFormat;

        // Find instance variable names
        var inst_var_names: []const u8 = "";
        if (std.mem.indexOf(u8, chunk, "instanceVariableNames:")) |idx| {
            if (std.mem.indexOfScalarPos(u8, chunk, idx, '\'')) |start| {
                if (std.mem.indexOfScalarPos(u8, chunk, start + 1, '\'')) |end| {
                    inst_var_names = chunk[start + 1 .. end];
                }
            }
        }

        // Look up superclass
        const superclass_value = self.heap.getGlobal(superclass_name) orelse {
            std.debug.print("Superclass not found: {s}\n", .{superclass_name});
            return FileInError.ClassNotFound;
        };

        if (!superclass_value.isObject()) {
            return FileInError.ClassNotFound;
        }

        // Create the new class
        const new_class = try createDynamicClass(
            self.heap,
            class_name,
            superclass_value.asObject(),
            inst_var_names,
        );

        // Register the class as a global
        try self.heap.setGlobal(class_name, Value.fromObject(new_class));

        self.classes_defined += 1;
        std.debug.print("Created class: {s} (subclass of {s})\n", .{ class_name, superclass_name });
    }

    /// Process a method category marker
    fn processMethodMarker(self: *FileIn, chunk: []const u8) !void {
        // Parse "ClassName methodsFor: 'category'" or "ClassName class methodsFor: 'category'"
        var iter = std.mem.splitScalar(u8, chunk, ' ');

        const class_name = iter.next() orelse return FileInError.InvalidMethodDefinition;
        self.current_class = class_name;

        const next = iter.next() orelse return FileInError.InvalidMethodDefinition;
        if (std.mem.eql(u8, next, "class")) {
            self.current_is_class_side = true;
            // Skip "methodsFor:"
        } else {
            self.current_is_class_side = false;
        }
    }

    /// Process a method body chunk and install it
    fn processMethodBody(self: *FileIn, chunk: []const u8) !void {
        const class_name = self.current_class orelse return FileInError.InvalidMethodDefinition;

        // Find the class
        const class_value = self.heap.getGlobal(class_name) orelse {
            return FileInError.ClassNotFound;
        };

        if (!class_value.isObject()) {
            return FileInError.ClassNotFound;
        }

        // Parse the method source to extract selector and body
        // First line is the method signature
        var first_line_end: usize = 0;
        while (first_line_end < chunk.len and chunk[first_line_end] != '\n' and chunk[first_line_end] != '\r') {
            first_line_end += 1;
        }

        const signature = std.mem.trim(u8, chunk[0..first_line_end], " \t");

        // Extract selector from signature
        const selector = extractSelector(signature) orelse return FileInError.InvalidMethodDefinition;

        // Compile the method
        var p = Parser.init(self.allocator, chunk);
        const ast = p.parseMethod() catch return FileInError.CompilationFailed;

        var gen = CodeGenerator.init(self.allocator, self.heap);
        defer gen.deinit();

        // Store the source code in the compiled method
        gen.source_code = chunk;

        const method = gen.compileMethod(ast) catch return FileInError.CompilationFailed;

        // Install the method in the class
        const class_obj = class_value.asObject();
        try installMethodInClass(self.heap, class_obj, selector, method, self.current_is_class_side);

        self.methods_loaded += 1;
    }
};

/// Extract the selector from a method signature
fn extractSelector(signature: []const u8) ?[]const u8 {
    if (signature.len == 0) return null;

    // Unary selector: just an identifier
    // Binary selector: operator characters
    // Keyword selector: words ending with ':'

    var i: usize = 0;

    // Check for binary selector (starts with operator)
    if (isBinaryChar(signature[0])) {
        while (i < signature.len and isBinaryChar(signature[i])) {
            i += 1;
        }
        return signature[0..i];
    }

    // Check for keyword or unary selector
    var has_keyword = false;
    while (i < signature.len) {
        const c = signature[i];
        if (c == ':') {
            has_keyword = true;
            i += 1;
            // Skip whitespace and parameter name
            while (i < signature.len and (signature[i] == ' ' or signature[i] == '\t')) {
                i += 1;
            }
            // Skip parameter name
            while (i < signature.len and isIdentChar(signature[i])) {
                i += 1;
            }
            // Skip whitespace before next keyword
            while (i < signature.len and (signature[i] == ' ' or signature[i] == '\t')) {
                i += 1;
            }
        } else if (isIdentChar(c)) {
            i += 1;
        } else if (c == ' ' or c == '\t') {
            if (!has_keyword) {
                // End of unary selector
                return signature[0..i];
            }
            i += 1;
        } else {
            break;
        }
    }

    // Build keyword selector by extracting just the keywords
    if (has_keyword) {
        return buildKeywordSelector(signature);
    }

    return if (i > 0) signature[0..i] else null;
}

fn buildKeywordSelector(signature: []const u8) ?[]const u8 {
    // For keyword selectors, we need to extract just the keyword parts
    // "at: index put: value" -> "at:put:"
    // This is a simplified version - in practice we'd build this properly
    var end: usize = 0;
    var i: usize = 0;

    while (i < signature.len) {
        if (signature[i] == ':') {
            end = i + 1;
            // Skip to next keyword
            i += 1;
            while (i < signature.len and !isIdentStartChar(signature[i])) {
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    return if (end > 0) signature[0..end] else null;
}

fn isBinaryChar(c: u8) bool {
    return switch (c) {
        '+', '-', '*', '/', '\\', '<', '>', '=', '@', '%', '|', '&', '?', '!', '~' => true,
        else => false,
    };
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

fn isIdentStartChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

/// Install a compiled method into a class's method dictionary
fn installMethodInClass(heap: *Heap, class_obj: *object.Object, selector: []const u8, method: *object.CompiledMethod, is_class_side: bool) !void {
    _ = is_class_side; // TODO: handle metaclass methods

    // Get or create method dictionary
    const method_dict_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);

    if (method_dict_val.isNil()) {
        // Create new method dictionary
        const new_dict = try heap.allocateObject(Heap.CLASS_ARRAY, 2, .variable);
        const sym = heap.internSymbol(selector) catch return error.OutOfMemory;
        new_dict.setField(0, sym, 2);
        new_dict.setField(1, Value.fromObject(@ptrCast(@alignCast(method))), 2);
        class_obj.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(new_dict), Heap.CLASS_NUM_FIELDS);
    } else if (method_dict_val.isObject()) {
        // Extend existing method dictionary
        const dict = method_dict_val.asObject();
        const old_size = dict.header.size;

        // Check if selector already exists
        var i: usize = 0;
        while (i < old_size) : (i += 2) {
            const sel_val = dict.getField(i, old_size);
            if (sel_val.isObject()) {
                const sel_obj = sel_val.asObject();
                if (sel_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    const sel_bytes = sel_obj.bytes(sel_obj.header.size);
                    if (std.mem.eql(u8, sel_bytes, selector)) {
                        // Update existing method
                        dict.setField(i + 1, Value.fromObject(@ptrCast(@alignCast(method))), old_size);
                        return;
                    }
                }
            }
        }

        // Add new entry - need to resize dictionary
        const new_size = old_size + 2;
        const new_dict = try heap.allocateObject(Heap.CLASS_ARRAY, new_size, .variable);

        // Copy old entries
        i = 0;
        while (i < old_size) : (i += 1) {
            new_dict.setField(i, dict.getField(i, old_size), new_size);
        }

        // Add new entry
        const sym = heap.internSymbol(selector) catch return error.OutOfMemory;
        new_dict.setField(old_size, sym, new_size);
        new_dict.setField(old_size + 1, Value.fromObject(@ptrCast(@alignCast(method))), new_size);

        class_obj.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(new_dict), Heap.CLASS_NUM_FIELDS);
    }
}

/// Create a new class dynamically at runtime
fn createDynamicClass(heap: *Heap, name: []const u8, superclass: *object.Object, inst_var_names: []const u8) !*object.Object {
    // Allocate a new class object
    const class = try heap.allocateObject(Heap.CLASS_CLASS, Heap.CLASS_NUM_FIELDS, .normal);

    // Set superclass
    class.setField(Heap.CLASS_FIELD_SUPERCLASS, Value.fromObject(superclass), Heap.CLASS_NUM_FIELDS);

    // Set class name
    const name_sym = heap.internSymbol(name) catch return error.OutOfMemory;
    class.setField(Heap.CLASS_FIELD_NAME, name_sym, Heap.CLASS_NUM_FIELDS);

    // Parse instance variable names and count them
    var inst_var_count: u32 = 0;
    if (inst_var_names.len > 0) {
        var iter = std.mem.splitScalar(u8, inst_var_names, ' ');
        while (iter.next()) |part| {
            if (part.len > 0) inst_var_count += 1;
        }
    }

    // Get superclass instance variable count
    const super_format = superclass.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
    var super_inst_vars: u32 = 0;
    if (super_format.isSmallInt()) {
        const format_int = super_format.asSmallInt();
        if (format_int >= 0) {
            super_inst_vars = @intCast(format_int >> 8);
        }
    }

    // Set format: encode instance size and format type
    // Format is encoded as: (instSize << 8) | formatType
    const total_inst_vars = super_inst_vars + inst_var_count;
    const format_value: i61 = (@as(i61, total_inst_vars) << 8) | 0; // 0 = normal format
    class.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(format_value), Heap.CLASS_NUM_FIELDS);

    // Initialize method dictionary to nil (will be created when first method is added)
    class.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.nil, Heap.CLASS_NUM_FIELDS);

    // Store instance variable names (create an array)
    if (inst_var_count > 0) {
        const names_array = try heap.allocateObject(Heap.CLASS_ARRAY, inst_var_count, .variable);
        var idx: usize = 0;
        var iter = std.mem.splitScalar(u8, inst_var_names, ' ');
        while (iter.next()) |part| {
            if (part.len > 0) {
                const var_sym = heap.internSymbol(part) catch return error.OutOfMemory;
                names_array.setField(idx, var_sym, inst_var_count);
                idx += 1;
            }
        }
        class.setField(Heap.CLASS_FIELD_INST_VARS, Value.fromObject(names_array), Heap.CLASS_NUM_FIELDS);
    } else {
        class.setField(Heap.CLASS_FIELD_INST_VARS, Value.nil, Heap.CLASS_NUM_FIELDS);
    }

    return class;
}

// Tests
test "chunk parser - basic" {
    const allocator = std.testing.allocator;

    const source =
        \\!Object methodsFor: 'accessing'!
        \\yourself
        \\    ^self! !
        \\
    ;

    var parser_inst = ChunkParser.init(allocator, source);

    const chunk1 = parser_inst.nextChunk();
    try std.testing.expect(chunk1 != null);
    try std.testing.expect(std.mem.indexOf(u8, chunk1.?, "methodsFor:") != null);

    const chunk2 = parser_inst.nextChunk();
    try std.testing.expect(chunk2 != null);
    try std.testing.expect(std.mem.indexOf(u8, chunk2.?, "yourself") != null);
}

test "chunk parser - with BOM" {
    const allocator = std.testing.allocator;

    // UTF-8 BOM followed by content
    const source = "\xEF\xBB\xBF!Object methodsFor: 'test'!";

    var parser_inst = ChunkParser.init(allocator, source);
    const chunk = parser_inst.nextChunk();
    try std.testing.expect(chunk != null);
    try std.testing.expect(std.mem.indexOf(u8, chunk.?, "Object") != null);
}

test "extract selector - unary" {
    const selector = extractSelector("yourself");
    try std.testing.expect(selector != null);
    try std.testing.expectEqualStrings("yourself", selector.?);
}

test "extract selector - binary" {
    const selector = extractSelector("+ aNumber");
    try std.testing.expect(selector != null);
    try std.testing.expectEqualStrings("+", selector.?);
}

test "extract selector - keyword" {
    const selector = extractSelector("at: index");
    try std.testing.expect(selector != null);
    try std.testing.expectEqualStrings("at:", selector.?);
}
