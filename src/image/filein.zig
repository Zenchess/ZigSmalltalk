const std = @import("std");
const object = @import("../vm/object.zig");
const memory = @import("../vm/memory.zig");
const bytecodes = @import("../vm/bytecodes.zig");
const parser = @import("../compiler/parser.zig");
const codegen = @import("../compiler/codegen.zig");
const interpreter = @import("../vm/interpreter.zig");

const Value = object.Value;
const Heap = memory.Heap;
const Parser = parser.Parser;
const CodeGenerator = codegen.CodeGenerator;

// Debug flag - set to false to disable verbose debug output
const DEBUG_VERBOSE = false;

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
        var in_string = false; // Inside single-quoted string literal
        var in_comment = false; // Inside double-quoted comment
        var prev_char: u8 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            // Track double-quoted comments - ignore everything inside them
            // In Smalltalk, "" inside a comment is an escaped double quote
            if (c == '"') {
                if (in_comment) {
                    // Inside a comment - check for escaped quote ""
                    if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '"') {
                        // Escaped quote - skip both
                        self.pos += 2;
                        prev_char = '"';
                        continue;
                    } else {
                        // End of comment
                        in_comment = false;
                    }
                } else if (!in_string) {
                    // Start of comment (only if not in string)
                    in_comment = true;
                }
            }

            // Track string literals to ignore '!' inside strings
            // In Smalltalk, '' inside a string is an escaped quote, but '' outside is empty string
            // Only track strings if not inside a comment (apostrophes in comments are just text)
            if (c == '\'' and !in_comment) {
                if (in_string) {
                    // Inside a string - check for escaped quote ''
                    if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '\'') {
                        // Escaped quote - skip both
                        self.pos += 2;
                        prev_char = '\'';
                        continue;
                    } else {
                        // End of string
                        in_string = false;
                    }
                } else {
                    // Start of string
                    in_string = true;
                }
            }

            // '!' ends the chunk (unless inside a string/comment or escaped as '!!')
            if (c == '!' and !in_string and !in_comment) {
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
    // Class definitions must have both subclass: and instanceVariableNames:
    // This distinguishes them from method bodies that might use subclass:
    const has_subclass = std.mem.indexOf(u8, chunk, "subclass:") != null or
        std.mem.indexOf(u8, chunk, "variableSubclass:") != null or
        std.mem.indexOf(u8, chunk, "variableByteSubclass:") != null or
        std.mem.indexOf(u8, chunk, "variableWordSubclass:") != null;

    if (!has_subclass) return false;

    // Class definitions must also have instanceVariableNames: (even if empty)
    return std.mem.indexOf(u8, chunk, "instanceVariableNames:") != null;
}

/// Check if a chunk is a method definition marker
pub fn isMethodDefinitionMarker(chunk: []const u8) bool {
    // Method definition markers are like "!ClassName methodsFor: 'category'!" or "!ClassName methodsFor!"
    return std.mem.indexOf(u8, chunk, "methodsFor") != null;
}

fn isIgnorableChunk(chunk: []const u8) bool {
    // Skip metadata chunks we don't need yet (guid, comments, category maps, file-out headers)
    // Also skip standalone comment chunks (strings that are just comments)
    if (chunk.len > 0 and chunk[0] == '"') {
        // Find the end of the comment - look for closing quote
        // A chunk that's entirely a comment should be ignored
        var depth: usize = 0;
        var i: usize = 0;
        var in_comment = false;
        while (i < chunk.len) : (i += 1) {
            if (chunk[i] == '"') {
                if (!in_comment) {
                    in_comment = true;
                    depth += 1;
                } else {
                    depth -= 1;
                    if (depth == 0) {
                        in_comment = false;
                    }
                }
            }
        }
        // If the entire chunk is just the comment, ignore it
        if (!in_comment and depth == 0) {
            // Check if after parsing comments, there's any non-comment content
            // Simple heuristic: if chunk starts with " and most of it is inside quotes, skip
            const trimmed = std.mem.trim(u8, chunk, " \t\r\n");
            if (trimmed.len > 0 and trimmed[0] == '"' and trimmed[trimmed.len - 1] == '"') {
                return true;
            }
        }
    }

    return std.mem.startsWith(u8, chunk, "\"Filed out from Dolphin Smalltalk\"") or
        std.mem.indexOf(u8, chunk, " guid:") != null or
        std.mem.indexOf(u8, chunk, " comment:") != null or
        std.mem.indexOf(u8, chunk, "categoriesForClass") != null or
        std.mem.indexOf(u8, chunk, "categoriesForMethods") != null or
        std.mem.indexOf(u8, chunk, " methodProtocol:") != null or
        std.mem.indexOf(u8, chunk, " isAbstract:") != null or
        std.mem.indexOf(u8, chunk, " isNullTerminated:") != null;
}

/// File-in manager for loading Smalltalk source files
pub const FileIn = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    /// Current class being defined
    current_class: ?[]const u8,
    /// Whether current methods are class-side
    current_is_class_side: bool,
    /// Whether we're inside a categoriesForMethods section (should skip chunks until next methodsFor)
    in_categories_section: bool,
    /// Statistics
    methods_loaded: usize,
    classes_defined: usize,
    expressions_evaluated: usize,
    errors: std.ArrayListUnmanaged([]const u8),
    /// Interpreter for evaluating expressions (optional, lazily initialized)
    interp: ?*interpreter.Interpreter,
    /// If true, interp is owned externally and should not be destroyed
    external_interp: bool,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) FileIn {
        return .{
            .allocator = allocator,
            .heap = heap,
            .current_class = null,
            .current_is_class_side = false,
            .in_categories_section = false,
            .methods_loaded = 0,
            .classes_defined = 0,
            .expressions_evaluated = 0,
            .errors = .{},
            .interp = null,
            .external_interp = false,
        };
    }

    /// Create FileIn with an external interpreter (for JIT stats tracking)
    pub fn initWithInterpreter(allocator: std.mem.Allocator, heap: *Heap, interp_ptr: *interpreter.Interpreter) FileIn {
        return .{
            .allocator = allocator,
            .heap = heap,
            .current_class = null,
            .current_is_class_side = false,
            .in_categories_section = false,
            .methods_loaded = 0,
            .classes_defined = 0,
            .expressions_evaluated = 0,
            .errors = .{},
            .interp = interp_ptr,
            .external_interp = true,
        };
    }

    pub fn deinit(self: *FileIn) void {
        self.errors.deinit(self.allocator);
        // Only destroy interpreter if we created it (not external)
        if (!self.external_interp) {
            if (self.interp) |interp_ptr| {
                self.allocator.destroy(interp_ptr);
            }
        }
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
        // Reset class context for each file
        self.current_class = null;
        self.current_is_class_side = false;
        self.in_categories_section = false;

        var chunk_parser = ChunkParser.init(self.allocator, source);

        var chunks: std.ArrayListUnmanaged([]const u8) = .{};
        defer chunks.deinit(self.allocator);

        while (chunk_parser.nextChunk()) |chunk| {
            chunks.append(self.allocator, chunk) catch {};
        }

        // Track which chunks have been processed (class definitions first)
        var processed = try self.allocator.alloc(bool, chunks.items.len);
        defer self.allocator.free(processed);
        @memset(processed, false);

        var made_progress = true;
        while (made_progress) {
            made_progress = false;
            for (chunks.items, 0..) |chunk, i| {
                if (processed[i]) continue;
                if (!isClassDefinition(chunk)) continue;

                self.processClassDefinition(chunk) catch |err| {
                    if (err == FileInError.ClassNotFound) {
                        continue;
                    }
                    self.recordError(err);
                    processed[i] = true;
                    continue;
                };

                processed[i] = true;
                made_progress = true;
            }
        }

        // Second pass: process everything else in original order
        for (chunks.items, 0..) |chunk, i| {
            if (isClassDefinition(chunk)) {
                self.in_categories_section = false; // Class definition exits categories section
                if (!processed[i]) self.recordError(FileInError.ClassNotFound);
                continue;
            }

            self.processNonClassChunk(chunk) catch |err| {
                self.recordError(err);
            };
        }
    }

    /// Process a single chunk that is not a class definition
    fn processNonClassChunk(self: *FileIn, chunk: []const u8) !void {
        // Check for entering a categoriesForMethods section
        const has_categories_methods = std.mem.indexOf(u8, chunk, "categoriesForMethods") != null;
        const has_categories_class = std.mem.indexOf(u8, chunk, "categoriesForClass") != null;

        if (has_categories_methods or has_categories_class) {
            self.in_categories_section = true;
            return;
        }

        // Check for method category marker (e.g., "!Object methodsFor: 'accessing'!")
        // This exits the categories section
        if (isMethodDefinitionMarker(chunk)) {
            self.in_categories_section = false;
            try self.processMethodMarker(chunk);
            return;
        }

        // Empty or blank chunks exit the categories section and clear class context
        // In Dolphin format, "! !" marks the end of a class's method definitions
        const trimmed = std.mem.trim(u8, chunk, " \t\r\n");
        if (trimmed.len == 0) {
            self.in_categories_section = false;
            self.current_class = null; // End of class method section
            return;
        }

        // If chunk looks like an expression (starts with uppercase and has a message),
        // exit categories section and continue processing
        if (self.in_categories_section) {
            // Check if this looks like an expression that should exit categories
            if (isExpressionChunk(chunk)) {
                self.in_categories_section = false;
                self.current_class = null; // Clear class context so it's treated as expression
                // Fall through to evaluate the expression
            } else {
                // Skip category metadata chunks
                return;
            }
        }

        if (isIgnorableChunk(chunk)) {
            return;
        }

        if (try self.handleClassConstant(chunk)) return;

        // If we have a current class context but this looks like an expression (not a method body),
        // it's probably an initialization expression after "! !" that ends a class's methods section
        if (self.current_class != null and isExpressionChunk(chunk)) {
            // Check if this looks more like an expression than a method body
            // Method bodies typically start with a selector (lowercase) or a unary selector
            // Expressions like "Transcript := TranscriptShell new" have ":=" in them
            // Also, "ClassName message" where ClassName differs from current_class is an expression
            if (std.mem.indexOf(u8, chunk, ":=") != null) {
                self.current_class = null; // Clear class context
            } else {
                // Check if the expression starts with a different class name than current_class
                // Skip leading comments to find the actual expression start
                var effective_chunk = chunk;
                while (effective_chunk.len > 0) {
                    const trimmed2 = std.mem.trimLeft(u8, effective_chunk, " \t\r\n");
                    if (trimmed2.len > 0 and trimmed2[0] == '"') {
                        // Skip comment
                        var i: usize = 1;
                        while (i < trimmed2.len) : (i += 1) {
                            if (trimmed2[i] == '"') {
                                if (i + 1 < trimmed2.len and trimmed2[i + 1] == '"') {
                                    i += 1;
                                } else {
                                    effective_chunk = trimmed2[i + 1 ..];
                                    break;
                                }
                            }
                        }
                        if (i >= trimmed2.len) break;
                    } else {
                        effective_chunk = trimmed2;
                        break;
                    }
                }

                // If expression starts with a number, parens, block, string, or temp declaration,
                // it's definitely an expression, not a method body
                if (effective_chunk.len > 0) {
                    const first = effective_chunk[0];
                    if (std.ascii.isDigit(first) or first == '(' or first == '[' or first == '\'' or first == '|') {
                        self.current_class = null; // Clear class context - this is an expression
                    } else if (std.ascii.isUpper(first)) {
                        // Extract the class name from the expression
                        var name_end: usize = 0;
                        while (name_end < effective_chunk.len and (std.ascii.isAlphanumeric(effective_chunk[name_end]) or effective_chunk[name_end] == '_')) : (name_end += 1) {}
                        const expr_class_name = effective_chunk[0..name_end];
                        // If it's a different class name, this is an expression, not a method
                        if (!std.mem.eql(u8, expr_class_name, self.current_class.?)) {
                            self.current_class = null; // Clear class context
                        }
                    }
                }
            }
        }

        // If we don't have a current class context, treat as an expression to evaluate
        if (self.current_class == null) {
            // Check if this looks like an expression (starts with '(' or looks like a message send)
            if (isExpressionChunk(chunk)) {
                try self.evaluateExpression(chunk);
                return;
            }
            // Not an expression - skip it (this is probably metadata we don't understand)
            return;
        }
        // Otherwise treat as a method body
        try self.processMethodBody(chunk);
    }

    /// Check if a chunk looks like an executable expression rather than a method
    fn isExpressionChunk(chunk: []const u8) bool {
        if (chunk.len == 0) return false;

        // Skip leading comments (text between double quotes)
        var effective_chunk = chunk;
        while (effective_chunk.len > 0) {
            const trimmed = std.mem.trimLeft(u8, effective_chunk, " \t\r\n");
            if (trimmed.len > 0 and trimmed[0] == '"') {
                // Skip this comment - find closing quote
                var i: usize = 1;
                while (i < trimmed.len) : (i += 1) {
                    if (trimmed[i] == '"') {
                        // Check for escaped quote ""
                        if (i + 1 < trimmed.len and trimmed[i + 1] == '"') {
                            i += 1; // Skip escaped quote
                        } else {
                            // End of comment
                            effective_chunk = trimmed[i + 1 ..];
                            break;
                        }
                    }
                }
                if (i >= trimmed.len) {
                    // Unclosed comment - treat as non-expression
                    return false;
                }
            } else {
                effective_chunk = trimmed;
                break;
            }
        }

        if (effective_chunk.len == 0) return false;

        // Expressions often start with '(' for grouped message sends
        if (effective_chunk[0] == '(') return true;

        // Blocks are also expressions
        if (effective_chunk[0] == '[') return true;

        // Temporary variable declarations indicate an expression
        if (effective_chunk[0] == '|') return true;

        // Literals: strings, numbers (but NOT symbols starting with # - those could be categoriesForClass metadata)
        if (effective_chunk[0] == '\'' or std.ascii.isDigit(effective_chunk[0])) return true;

        // Check for ClassName followed by a proper message send pattern:
        // - ClassName unaryMessage (lowercase letter after class name + whitespace)
        // - ClassName keyword: args (has a colon indicating keyword message)
        if (std.ascii.isUpper(effective_chunk[0])) {
            // Find end of class name
            var i: usize = 0;
            while (i < effective_chunk.len and (std.ascii.isAlphanumeric(effective_chunk[i]) or effective_chunk[i] == '_')) : (i += 1) {}

            // Must have whitespace after class name
            if (i >= effective_chunk.len or !std.ascii.isWhitespace(effective_chunk[i])) return false;

            // Skip whitespace
            while (i < effective_chunk.len and std.ascii.isWhitespace(effective_chunk[i])) : (i += 1) {}

            if (i >= effective_chunk.len) return false;

            // The message must start with a lowercase letter (proper Smalltalk convention)
            // This distinguishes "DefaultSortAlgorithm initialize" from "Building Suites"
            if (std.ascii.isLower(effective_chunk[i])) {
                return true;
            }

            // Also check for keyword message (contains ':')
            if (std.mem.indexOfScalar(u8, effective_chunk, ':') != null) {
                return true;
            }
        }

        return false;
    }

    /// Evaluate an expression chunk
    fn evaluateExpression(self: *FileIn, chunk: []const u8) !void {
        // Lazily initialize interpreter
        if (self.interp == null) {
            const interp_ptr = self.allocator.create(interpreter.Interpreter) catch return;
            interp_ptr.* = interpreter.Interpreter.init(self.heap, self.allocator);
            // Register interpreter with heap for GC stack tracing
            self.heap.interpreter = interp_ptr;
            self.interp = interp_ptr;
        }

        const interp_ptr = self.interp.?;

        // Parse and compile using a per-expression arena so we don't leak memory
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const expr_alloc = arena.allocator();

        // Parse as an expression
        var p = Parser.init(expr_alloc, chunk);
        const ast = p.parseExpression() catch {
            // Not a valid expression, skip
            return;
        };

        // Generate code
        var gen = CodeGenerator.init(expr_alloc, self.heap, expr_alloc);
        defer gen.deinit();

        const method = gen.compileDoIt(ast) catch {
            // Compilation failed, skip
            return;
        };

        if (std.mem.startsWith(u8, chunk, "ProtocolSpec")) {
            if (self.heap.getGlobal("ProtocolSpec")) |pval| {
                if (pval.isObject()) {
                    const obj = pval.asObject();
                    const obj_num_fields = obj.header.size;
                    const meta_val = obj.getField(Heap.CLASS_FIELD_METACLASS, obj_num_fields);
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG ProtocolSpec global class_idx={} meta?{}\n", .{ obj.header.class_index, meta_val.isObject() });
                } else {
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG ProtocolSpec global not object\n", .{});
                }
            } else {
                if (DEBUG_VERBOSE) std.debug.print("DEBUG ProtocolSpec global missing\n", .{});
            }
        }

        // Execute - for a DoIt, receiver is nil and there are no args
        _ = interp_ptr.execute(method, Value.nil, &[_]Value{}) catch |err| {
            // Execution failed - log it for debugging
            if (DEBUG_VERBOSE) {
                std.debug.print("  Expression execution failed: {any}\n", .{err});
                std.debug.print("    Chunk: {s}...\n", .{chunk[0..@min(60, chunk.len)]});
            }
            return;
        };

        self.expressions_evaluated += 1;
    }

    fn recordError(self: *FileIn, err: FileInError) void {
        const err_msg = switch (err) {
            FileInError.ClassNotFound => "Class not found",
            FileInError.InvalidMethodDefinition => "Invalid method definition",
            FileInError.CompilationFailed => "Compilation failed",
            FileInError.InvalidChunkFormat => "Invalid chunk format",
            FileInError.InvalidEncoding => "Invalid encoding",
            FileInError.OutOfMemory => "Out of memory",
            FileInError.FileNotFound => "File not found",
        };
        self.errors.append(self.allocator, err_msg) catch {};
    }

    /// Process a class definition chunk
    fn processClassDefinition(self: *FileIn, chunk: []const u8) !void {
        // Parse class definition like:
        // Object subclass: #MyClass
        //     instanceVariableNames: 'x y z'
        //     classVariableNames: ''
        //     poolDictionaries: ''
        //     classInstanceVariableNames: ''

        // Skip any leading comments (text in double quotes)
        var effective_chunk = chunk;
        while (effective_chunk.len > 0) {
            const trimmed = std.mem.trimLeft(u8, effective_chunk, " \t\r\n");
            if (trimmed.len > 0 and trimmed[0] == '"') {
                // Skip this comment - find closing quote
                var i: usize = 1;
                while (i < trimmed.len) : (i += 1) {
                    if (trimmed[i] == '"') {
                        // Check for escaped quote ""
                        if (i + 1 < trimmed.len and trimmed[i + 1] == '"') {
                            i += 1; // Skip escaped quote
                        } else {
                            // End of comment
                            effective_chunk = trimmed[i + 1 ..];
                            break;
                        }
                    }
                }
                if (i >= trimmed.len) {
                    // Unclosed comment - use remaining text
                    effective_chunk = "";
                    break;
                }
            } else {
                effective_chunk = trimmed;
                break;
            }
        }

        // Determine the class format type
        var class_format: object.ClassFormat = .normal;
        var subclass_keyword: []const u8 = "subclass:";

        if (std.mem.indexOf(u8, effective_chunk, "variableByteSubclass:") != null) {
            class_format = .bytes;
            subclass_keyword = "variableByteSubclass:";
        } else if (std.mem.indexOf(u8, effective_chunk, "variableWordSubclass:") != null) {
            class_format = .words;
            subclass_keyword = "variableWordSubclass:";
        } else if (std.mem.indexOf(u8, effective_chunk, "variableSubclass:") != null) {
            class_format = .variable;
            subclass_keyword = "variableSubclass:";
        }

        // Find superclass name (first identifier before "subclass:")
        const subclass_idx = std.mem.indexOf(u8, effective_chunk, subclass_keyword) orelse
            return FileInError.InvalidChunkFormat;

        const superclass_name = std.mem.trim(u8, effective_chunk[0..subclass_idx], " \t\r\n");
        if (superclass_name.len == 0) return FileInError.InvalidChunkFormat;

        // Find class name after "#"
        const hash_idx = std.mem.indexOf(u8, effective_chunk[subclass_idx..], "#") orelse
            return FileInError.InvalidChunkFormat;
        const class_start = subclass_idx + hash_idx + 1;

        var class_end = class_start;
        while (class_end < effective_chunk.len and isIdentChar(effective_chunk[class_end])) {
            class_end += 1;
        }
        const class_name = effective_chunk[class_start..class_end];
        if (class_name.len == 0) return FileInError.InvalidChunkFormat;

        // Find instance variable names
        var inst_var_names: []const u8 = "";
        if (std.mem.indexOf(u8, effective_chunk, "instanceVariableNames:")) |idx| {
            if (std.mem.indexOfScalarPos(u8, effective_chunk, idx, '\'')) |start| {
                if (std.mem.indexOfScalarPos(u8, effective_chunk, start + 1, '\'')) |end| {
                    inst_var_names = effective_chunk[start + 1 .. end];
                }
            }
        }

        // Find class variable names
        var class_var_names: []const u8 = "";
        if (std.mem.indexOf(u8, effective_chunk, "classVariableNames:")) |idx| {
            if (std.mem.indexOfScalarPos(u8, effective_chunk, idx, '\'')) |start| {
                if (std.mem.indexOfScalarPos(u8, effective_chunk, start + 1, '\'')) |end| {
                    class_var_names = effective_chunk[start + 1 .. end];
                }
            }
        }

        // Find pool dictionaries
        var pool_dict_names: []const u8 = "";
        if (std.mem.indexOf(u8, effective_chunk, "poolDictionaries:")) |idx| {
            if (std.mem.indexOfScalarPos(u8, effective_chunk, idx, '\'')) |start| {
                if (std.mem.indexOfScalarPos(u8, effective_chunk, start + 1, '\'')) |end| {
                    pool_dict_names = effective_chunk[start + 1 .. end];
                }
            }
        }

        // Find class instance variable names
        var class_inst_var_names: []const u8 = "";
        if (std.mem.indexOf(u8, effective_chunk, "classInstanceVariableNames:")) |idx| {
            if (std.mem.indexOfScalarPos(u8, effective_chunk, idx, '\'')) |start| {
                if (std.mem.indexOfScalarPos(u8, effective_chunk, start + 1, '\'')) |end| {
                    class_inst_var_names = effective_chunk[start + 1 .. end];
                }
            }
        }

        // Look up superclass
        const superclass_value = if (std.mem.eql(u8, superclass_name, "nil"))
            Value.nil
        else
            self.heap.getGlobal(superclass_name) orelse {
                if (DEBUG_VERBOSE) std.debug.print("Superclass not found: {s}\n", .{superclass_name});
                return FileInError.ClassNotFound;
            };

        const superclass_obj: ?*object.Object = if (superclass_value.isObject()) superclass_value.asObject() else null;

        // Create or update the class
        const existing_class = blk: {
            if (self.heap.getGlobal(class_name)) |val| {
                if (val.isObject()) break :blk val.asObject();
            }
            break :blk null;
        };

        const new_class = try createDynamicClass(
            self.heap,
            class_name,
            superclass_obj,
            inst_var_names,
            class_var_names,
            pool_dict_names,
            class_inst_var_names,
            class_format,
            existing_class,
            "Unpackaged", // Default category, can be updated later
        );

        // Register the class as a global
        try self.heap.setGlobal(class_name, Value.fromObject(new_class));

        if (existing_class == null) self.classes_defined += 1;
    }

    /// Process a method category marker
    fn processMethodMarker(self: *FileIn, chunk: []const u8) !void {
        // Parse "ClassName methodsFor: 'category'" or "ClassName class methodsFor: 'category'"
        var iter = std.mem.tokenizeAny(u8, chunk, " \t");

        const class_name = iter.next() orelse return FileInError.InvalidMethodDefinition;
        self.current_class = class_name;

        const next = iter.next() orelse return FileInError.InvalidMethodDefinition;
        if (std.mem.eql(u8, next, "class")) {
            self.current_is_class_side = true;
            _ = iter.next(); // Skip methodsFor token if present
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

        // Get heap address range for validation
        const heap_base = @intFromPtr(self.heap.from_space.ptr);
        const heap_end = heap_base + self.heap.space_size;

        // Validate the class pointer is within heap range
        const class_ptr = @intFromPtr(class_value.asObject());
        if (class_ptr < heap_base or class_ptr >= heap_end) {
            std.debug.print("ERROR: Class pointer 0x{x} outside heap [0x{x}-0x{x}] for class '{s}'\n", .{ class_ptr, heap_base, heap_end, class_name });
            return FileInError.ClassNotFound;
        }

        const class_obj_for_meta = class_value.asObject();
        const class_num_fields = class_obj_for_meta.header.size;
        const target_class_val = if (self.current_is_class_side)
            class_obj_for_meta.getField(Heap.CLASS_FIELD_METACLASS, class_num_fields)
        else
            class_value;

        if (!target_class_val.isObject()) return FileInError.ClassNotFound;

        // Validate the target pointer is within heap range
        const target_ptr = @intFromPtr(target_class_val.asObject());
        if (target_ptr < heap_base or target_ptr >= heap_end) {
            std.debug.print("ERROR: Target class pointer 0x{x} outside heap [0x{x}-0x{x}] for class '{s}' (class_side={})\n", .{ target_ptr, heap_base, heap_end, class_name, self.current_is_class_side });
            return FileInError.ClassNotFound;
        }

        // Parse the method source to extract selector and body
        // Skip leading comments and whitespace to find the method signature
        var method_start: usize = 0;
        while (method_start < chunk.len) {
            // Skip whitespace
            while (method_start < chunk.len and (chunk[method_start] == ' ' or chunk[method_start] == '\t' or chunk[method_start] == '\r' or chunk[method_start] == '\n')) {
                method_start += 1;
            }
            if (method_start >= chunk.len) break;
            // Skip comments (strings starting with ")
            if (chunk[method_start] == '"') {
                method_start += 1;
                while (method_start < chunk.len and chunk[method_start] != '"') {
                    method_start += 1;
                }
                if (method_start < chunk.len) method_start += 1; // Skip closing "
            } else {
                break; // Found start of method signature
            }
        }

        // First line is the method signature
        var first_line_end: usize = method_start;
        while (first_line_end < chunk.len and chunk[first_line_end] != '\n' and chunk[first_line_end] != '\r') {
            first_line_end += 1;
        }

        const signature = std.mem.trim(u8, chunk[method_start..first_line_end], " \t");

        // Extract selector from signature
        const selector = extractSelector(signature) orelse {
            if (DEBUG_VERBOSE) std.debug.print("  Failed to extract selector from signature: '{s}'\n", .{signature});
            return FileInError.InvalidMethodDefinition;
        };

        // Debug: show what selectors are being compiled (disabled)
        // if (std.mem.eql(u8, selector, "beginsWith:") or std.mem.eql(u8, selector, "basicBeginsWith:") or std.mem.eql(u8, selector, ",") or std.mem.eql(u8, selector, "=")) {
        //     if (DEBUG_VERBOSE) std.debug.print("DEBUG filein: compiling {s}>>{s} (class_name.len={})\n", .{class_name, selector, class_name.len});
        // }

        // Compile the method
        // Use a per-method arena to avoid unbounded allocations across the whole file-in
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const method_alloc = arena.allocator();

        var p = Parser.init(method_alloc, chunk);
        const ast = p.parseMethod() catch {
            return FileInError.CompilationFailed;
        };

        // Compiled methods must live beyond this chunk, so allocate with the main allocator
        // Use page_allocator for compiled method storage to avoid gpa leak reports for long-lived methods
        var gen = CodeGenerator.init(self.allocator, self.heap, std.heap.page_allocator);
        defer gen.deinit();

        // Store the source code in the compiled method
        gen.source_code = chunk;

        // Set instance variable names for the code generator
        // Walk up the class hierarchy to get all instance variables
        var inst_var_list: std.ArrayListUnmanaged([]const u8) = .{};
        defer inst_var_list.deinit(self.allocator);

        var walk_class = target_class_val;
        while (walk_class.isObject()) {
            const walk_obj = walk_class.asObject();
            const walk_num_fields = walk_obj.header.size;
            const inst_vars = walk_obj.getField(Heap.CLASS_FIELD_INST_VARS, walk_num_fields);
            if (inst_vars.isObject()) {
                const vars_array = inst_vars.asObject();
                const vars_size = vars_array.header.size;
                // Insert at the beginning since we're walking up the hierarchy
                var i: usize = vars_size;
                while (i > 0) {
                    i -= 1;
                    const var_sym = vars_array.getField(i, vars_size);
                    if (var_sym.isObject()) {
                        const sym_obj = var_sym.asObject();
                        if (sym_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            const var_name = sym_obj.bytes(sym_obj.header.size);
                            inst_var_list.insert(self.allocator, 0, var_name) catch {};
                        }
                    }
                }
            }
            walk_class = walk_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, walk_num_fields);
        }
        gen.instance_variables = inst_var_list.items;

        const method = gen.compileMethod(ast) catch {
            return FileInError.CompilationFailed;
        };

        // Debug for Character = specifically (disabled)
        // if (std.mem.eql(u8, selector, "=") and std.mem.eql(u8, class_name, "Character")) {
        //     const bcs = method.getBytecodes();
        //     std.debug.print("  Character>>= header args={} temps={}\n", .{ method.header.num_args, method.header.num_temps });
        //     std.debug.print("  Character>>= bytecodes:", .{});
        //     for (bcs) |bc| {
        //         std.debug.print(" {d}", .{bc});
        //     }
        //     std.debug.print("\n", .{});
        // }

        if (DEBUG_VERBOSE and std.mem.eql(u8, selector, "initialize") and std.mem.eql(u8, class_name, "LookupTable")) {
            std.debug.print("DEBUG compile LookupTable>>initialize inst_vars count={}\n", .{gen.instance_variables.len});
            for (gen.instance_variables, 0..) |iv, idx| {
                std.debug.print("  inst_var[{}] = {s}\n", .{ idx, iv });
            }
            const bcs = method.getBytecodes();
            std.debug.print("  bytecodes:", .{});
            for (bcs) |bc| {
                std.debug.print(" {d}", .{bc});
            }
            std.debug.print("\n", .{});
        }
        if (DEBUG_VERBOSE and std.mem.eql(u8, selector, "tests") and std.mem.eql(u8, class_name, "TestSuite")) {
            std.debug.print("DEBUG compile TestSuite>>tests inst_vars count={}\n", .{gen.instance_variables.len});
            for (gen.instance_variables, 0..) |iv, idx| {
                std.debug.print("  inst_var[{}] = {s}\n", .{ idx, iv });
            }
        }

        if (DEBUG_VERBOSE and (std.mem.eql(u8, selector, "instanceSpec") or std.mem.eql(u8, selector, "isNullTerminated") or std.mem.eql(u8, selector, "isNullTerminated:") or std.mem.eql(u8, selector, "buildSuiteFromSelectors") or std.mem.eql(u8, selector, "shouldInheritSelectors") or std.mem.eql(u8, selector, "run") or std.mem.eql(u8, selector, "tests") or (std.mem.eql(u8, selector, "=") and std.mem.eql(u8, class_name, "SequenceableCollection")) or std.mem.eql(u8, selector, "basicBeginsWith:"))) {
            const bcs = method.getBytecodes();
            std.debug.print("  header args={} temps={}\n", .{ method.header.num_args, method.header.num_temps });
            std.debug.print("DEBUG compile {s}>>{s} bytecodes:", .{ class_name, selector });
            for (bcs) |bc| {
                std.debug.print(" {d}", .{bc});
            }
            std.debug.print("\n", .{});
            const lits_dbg = method.getLiterals();
            std.debug.print("  Lits:", .{});
            for (lits_dbg, 0..) |lit, idx| {
                std.debug.print(" {d}=", .{idx});
                if (lit.isObject()) {
                    const lobj = lit.asObject();
                    if (lobj.header.class_index == Heap.CLASS_SYMBOL or lobj.header.class_index == Heap.CLASS_STRING) {
                        const lit_bytes = lobj.bytes(lobj.header.size);
                        // Truncate long strings and avoid newlines
                        const max_len: usize = 50;
                        const print_len = @min(lit_bytes.len, max_len);
                        var i: usize = 0;
                        while (i < print_len) : (i += 1) {
                            if (lit_bytes[i] == '\n' or lit_bytes[i] == '\r') break;
                        }
                        std.debug.print("{s}", .{lit_bytes[0..i]});
                        if (i < lit_bytes.len) std.debug.print("...", .{});
                    } else {
                        std.debug.print("obj(class_idx={})", .{lobj.header.class_index});
                    }
                } else if (lit.isSmallInt()) {
                    std.debug.print("SmallInt({})", .{lit.asSmallInt()});
                } else {
                    std.debug.print("other", .{});
                }
                std.debug.print(";", .{});
            }
            std.debug.print("\n", .{});
        }

        // Install the method in the class
        const class_obj = target_class_val.asObject();
        try installMethodInClass(self.heap, class_obj, selector, method, self.current_is_class_side);

        // ProtocolSpec file defines protocol-building helpers as instance methods, but ANSI DB
        // sends them to the class. Install class-side copies for those selectors.
        if (!self.current_is_class_side and std.mem.eql(u8, class_name, "ProtocolSpec")) {
            if (std.mem.startsWith(u8, selector, "newProtocolNamed:conformsToProtocolNames:") or
                std.mem.startsWith(u8, selector, "newMessagePattern:forProtocolNamed:") or
                std.mem.startsWith(u8, selector, "protocolDescription:") or
                std.mem.startsWith(u8, selector, "protocolNamed:"))
            {
                const class_obj_size = class_obj.header.size;
                const meta_val = class_obj.getField(Heap.CLASS_FIELD_METACLASS, class_obj_size);
                if (meta_val.isObject()) {
                    const meta = meta_val.asObject();
                    try installMethodInClass(self.heap, meta, selector, method, true);
                }
            }
        }

        self.methods_loaded += 1;
    }

    /// Handle "ClassName addClassConstant: 'Foo' value: 123" chunks minimally.
    /// We now store the value (integers/booleans) and also expose it as a global for lookup.
    fn handleClassConstant(self: *FileIn, chunk: []const u8) !bool {
        if (std.mem.indexOf(u8, chunk, "addClassConstant:") == null) return false;

        var iter = std.mem.tokenizeAny(u8, chunk, " \t");
        const class_name = iter.next() orelse return true;
        const class_val = self.heap.getGlobal(class_name) orelse return true;
        if (!class_val.isObject()) return true;

        const name_start = std.mem.indexOfScalar(u8, chunk, '\'') orelse return true;
        const name_end = std.mem.indexOfScalarPos(u8, chunk, name_start + 1, '\'') orelse return true;
        const const_name = std.mem.trim(u8, chunk[name_start + 1 .. name_end], " \t\r\n");

        // Parse the value token after "value:"
        var value_val: Value = Value.nil;
        if (std.mem.indexOf(u8, chunk, "value:")) |vpos| {
            var rest = std.mem.trim(u8, chunk[vpos + "value:".len ..], " \t\r\n");
            // Strip trailing '!' if present
            if (rest.len > 0 and rest[rest.len - 1] == '!') rest = rest[0 .. rest.len - 1];

            // Check for ByteArray literal #[...]
            if (rest.len > 2 and rest[0] == '#' and rest[1] == '[') {
                if (std.mem.lastIndexOf(u8, rest, "]")) |close_idx| {
                    const byte_content = rest[2..close_idx];
                    // Count bytes
                    var byte_count: usize = 0;
                    var count_iter = std.mem.tokenizeAny(u8, byte_content, " \t\r\n");
                    while (count_iter.next()) |_| byte_count += 1;

                    // Allocate ByteArray
                    const byte_array = self.heap.allocateObject(Heap.CLASS_BYTE_ARRAY, byte_count, .bytes) catch return true;

                    // Parse and fill bytes
                    const byte_slice = byte_array.bytes(byte_count);
                    var byte_iter = std.mem.tokenizeAny(u8, byte_content, " \t\r\n");
                    var byte_idx: usize = 0;
                    while (byte_iter.next()) |tok| {
                        if (std.fmt.parseInt(u8, tok, 10)) |b| {
                            byte_slice[byte_idx] = b;
                            byte_idx += 1;
                        } else |_| {}
                    }
                    value_val = Value.fromObject(byte_array);
                }
            }
            // Check for Array literal #(...)
            else if (rest.len > 2 and rest[0] == '#' and rest[1] == '(') {
                if (std.mem.lastIndexOf(u8, rest, ")")) |close_idx| {
                    const arr_content = rest[2..close_idx];
                    // Count elements
                    var elem_count: usize = 0;
                    var count_iter = std.mem.tokenizeAny(u8, arr_content, " \t\r\n");
                    while (count_iter.next()) |_| elem_count += 1;

                    // Allocate Array
                    const array = self.heap.allocateObject(Heap.CLASS_ARRAY, elem_count, .variable) catch return true;

                    // Parse and fill elements (integers only for now)
                    var elem_iter = std.mem.tokenizeAny(u8, arr_content, " \t\r\n");
                    var elem_idx: usize = 0;
                    while (elem_iter.next()) |tok| {
                        if (std.fmt.parseInt(i61, tok, 10)) |n| {
                            array.setField(elem_idx, Value.fromSmallInt(n), elem_count);
                            elem_idx += 1;
                        } else |_| {
                            array.setField(elem_idx, Value.nil, elem_count);
                            elem_idx += 1;
                        }
                    }
                    value_val = Value.fromObject(array);
                }
            } else {
                // Take first token up to whitespace or !
                var end_idx: usize = 0;
                while (end_idx < rest.len and rest[end_idx] != ' ' and rest[end_idx] != '\t' and rest[end_idx] != '!' and rest[end_idx] != '\r' and rest[end_idx] != '\n') {
                    end_idx += 1;
                }
                const token = rest[0..end_idx];
                if (token.len > 0) {
                    if (std.mem.eql(u8, token, "true")) {
                        value_val = Value.fromBool(true);
                    } else if (std.mem.eql(u8, token, "false")) {
                        value_val = Value.fromBool(false);
                    } else if (token.len > 2 and token[0] == '1' and token[1] == '6' and token[2] == 'r') {
                        const hex_slice = token[3..];
                        if (std.fmt.parseInt(i61, hex_slice, 16)) |parsed| {
                            value_val = Value.fromSmallInt(parsed);
                        } else |_| {}
                    } else {
                        if (std.fmt.parseInt(i61, token, 10)) |parsed| {
                            value_val = Value.fromSmallInt(parsed);
                        } else |_| {}
                    }
                }
            }
        }

        try ensureClassVarPresent(self.heap, class_val.asObject(), const_name);
        try setClassVarValue(self.heap, class_val.asObject(), const_name, value_val);
        // Also expose as global for simple lookup by compiled code
        try self.heap.setGlobal(const_name, value_val);
        return true;
    }
};

/// Extract the selector from a method signature
pub fn extractSelector(signature: []const u8) ?[]const u8 {
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
    // For keyword selectors, we need to find where the first keyword ends
    // For "at: index put: value", we find the end of "at:" at position 3
    // For "at: index", we find the end of "at:" at position 3
    // The trick is that in valid Smalltalk syntax, after a keyword colon and parameter,
    // either there's another keyword starting, or the signature ends.
    //
    // We iterate to find the last colon that's part of a keyword (preceded by identifier chars).
    // The selector is everything from start up to and including that colon,
    // but only if no parameter names are between keywords.
    //
    // Simplest approach: find where the first colon is, that's a keyword selector ending there.
    // For multi-keyword: "at: idx put: val" - we need just "at:put:"
    //
    // Actually, we can't return a slice of the original since keywords are non-contiguous.
    // But since this function is only called when we already know we have keywords,
    // and the selector is used for method lookup, we need the actual concatenated selector.
    //
    // For now, find the position after the last colon that ends a keyword.
    // A keyword is an identifier followed by colon.
    // After a colon comes optional whitespace, then a parameter name (identifier),
    // then optional whitespace, then either another keyword or end.

    var i: usize = 0;
    var last_keyword_end: usize = 0;

    while (i < signature.len) {
        // Skip leading whitespace
        while (i < signature.len and (signature[i] == ' ' or signature[i] == '\t')) {
            i += 1;
        }
        if (i >= signature.len) break;

        // Check if we're at start of an identifier (keyword)
        if (isIdentStartChar(signature[i])) {
            // Read the identifier
            while (i < signature.len and isIdentChar(signature[i])) {
                i += 1;
            }
            // Check if followed by colon (making it a keyword)
            if (i < signature.len and signature[i] == ':') {
                i += 1; // include the colon
                last_keyword_end = i;

                // Skip whitespace after colon
                while (i < signature.len and (signature[i] == ' ' or signature[i] == '\t')) {
                    i += 1;
                }
                // Skip parameter name
                if (i < signature.len and isIdentStartChar(signature[i])) {
                    while (i < signature.len and isIdentChar(signature[i])) {
                        i += 1;
                    }
                }
                // Continue to look for more keywords
            } else {
                // Not a keyword, just an identifier - for single keyword case like "at: index"
                // we've already captured the keyword, so this is the parameter name
                break;
            }
        } else {
            break;
        }
    }

    // For single keyword like "at: index", return "at:"
    // For multi-keyword, we still have a problem: "at: index put: value" -> last_keyword_end points after "put:"
    // but signature[0..last_keyword_end] = "at: index put:" which still includes "index"
    //
    // The real fix requires building a new string. Since we don't have an allocator here,
    // let's use a different approach: for single-keyword selectors (only one colon),
    // we can return the slice up to and including the first colon.

    // Count colons to detect multi-keyword
    var colon_count: usize = 0;
    var first_colon_pos: usize = 0;
    for (signature, 0..) |c, idx| {
        if (c == ':') {
            colon_count += 1;
            if (colon_count == 1) {
                first_colon_pos = idx;
            }
        }
    }

    if (colon_count == 1) {
        // Single keyword: return up to and including the colon
        return signature[0 .. first_colon_pos + 1];
    } else if (colon_count > 1) {
        // Multi-keyword: we need to build selector without parameter names
        // Since we can't allocate, use a static buffer
        const Static = struct {
            var buf: [256]u8 = undefined;
        };
        var out_len: usize = 0;

        i = 0;
        while (i < signature.len and out_len < Static.buf.len - 1) {
            // Skip whitespace
            while (i < signature.len and (signature[i] == ' ' or signature[i] == '\t')) {
                i += 1;
            }
            if (i >= signature.len) break;

            // Read identifier
            if (isIdentStartChar(signature[i])) {
                const id_start = i;
                while (i < signature.len and isIdentChar(signature[i])) {
                    i += 1;
                }
                // Check for colon
                if (i < signature.len and signature[i] == ':') {
                    // This is a keyword - copy identifier and colon to buffer
                    const id_len = i - id_start + 1; // +1 for colon
                    if (out_len + id_len <= Static.buf.len) {
                        @memcpy(Static.buf[out_len .. out_len + i - id_start], signature[id_start..i]);
                        out_len += i - id_start;
                        Static.buf[out_len] = ':';
                        out_len += 1;
                    }
                    i += 1; // skip colon

                    // Skip whitespace
                    while (i < signature.len and (signature[i] == ' ' or signature[i] == '\t')) {
                        i += 1;
                    }
                    // Skip parameter name
                    if (i < signature.len and isIdentStartChar(signature[i])) {
                        while (i < signature.len and isIdentChar(signature[i])) {
                            i += 1;
                        }
                    }
                } else {
                    // Just an identifier (parameter name), skip it
                    break;
                }
            } else {
                break;
            }
        }

        return if (out_len > 0) Static.buf[0..out_len] else null;
    }

    return null;
}

fn isBinaryChar(c: u8) bool {
    return switch (c) {
        '+', '-', '*', '/', '\\', '<', '>', '=', '@', '%', '|', '&', '?', '!', '~', ',' => true,
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
/// Note: class_obj should already be the metaclass for class-side methods
pub fn installMethodInClass(heap: *Heap, class_obj: *object.Object, selector: []const u8, method: *object.CompiledMethod, is_class_side: bool) !void {
    _ = is_class_side; // class_obj is already resolved to metaclass when needed

    // Use actual object size for field access (old classes may have fewer fields)
    const num_fields = class_obj.header.size;

    // Debug: get class name
    const debug_name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, num_fields);
    var class_name: []const u8 = "???";
    if (debug_name_val.isObject()) {
        const debug_name_obj = debug_name_val.asObject();
        if (debug_name_obj.header.class_index == Heap.CLASS_SYMBOL) {
            class_name = debug_name_obj.bytes(debug_name_obj.header.size);
        }
    }

    // Get or create method dictionary - use hash-based storage
    const method_dict_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, num_fields);
    const sym = heap.internSymbol(selector) catch return error.OutOfMemory;
    const method_val = Value.fromObject(@ptrCast(@alignCast(method)));

    if (method_dict_val.isNil()) {
        // Create new method dictionary with good initial size for hashing
        if (DEBUG_VERBOSE) std.debug.print("DEBUG installMethodInClass: '{s}' >> {s} - creating new method dict at {*}\n", .{ class_name, selector, class_obj });
        const new_dict = try heap.allocateObject(Heap.CLASS_ARRAY, 64, .variable); // 32 slots
        _ = hashInsertMethod(new_dict, sym, method_val, selector);
        class_obj.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(new_dict), num_fields);
    } else if (method_dict_val.isObject()) {
        const dict = method_dict_val.asObject();

        // Check if method should be protected
        if (hashLookupMethod(dict, sym, selector)) |existing| {
            if (existing.header.primitive_index != 0) {
                if (std.mem.eql(u8, selector, "basicNew") or
                    std.mem.eql(u8, selector, "basicNew:") or
                    std.mem.eql(u8, selector, "perform:") or
                    std.mem.eql(u8, selector, "perform:withArguments:") or
                    std.mem.eql(u8, selector, "instSize"))
                {
                    return; // Protected primitive
                }
            }
        }

        // Try to insert/update
        if (hashInsertMethod(dict, sym, method_val, selector)) {
            // Success
            return;
        }

        // Dictionary full - need to grow and rehash
        const old_size = dict.header.size;
        const new_size = if (old_size < 64) 128 else old_size * 2;
        const new_dict = try heap.allocateObject(Heap.CLASS_ARRAY, new_size, .variable);

        // Rehash all existing entries
        var i: usize = 0;
        while (i < old_size) : (i += 2) {
            const old_key = dict.getField(i, old_size);
            if (!old_key.isNil()) {
                const old_val = dict.getField(i + 1, old_size);
                _ = hashInsertMethod(new_dict, old_key, old_val, "");
            }
        }

        // Insert the new method
        _ = hashInsertMethod(new_dict, sym, method_val, selector);
        class_obj.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(new_dict), num_fields);
    }
}

/// Hash-based method lookup in dictionary
fn hashLookupMethod(dict: *object.Object, selector_sym: Value, selector_str: []const u8) ?*object.CompiledMethod {
    const dict_size = dict.header.size;
    if (dict_size < 2) return null;

    const num_slots = dict_size / 2;
    const hash = selector_sym.bits *% 2654435761;
    var index = @as(usize, @intCast(hash)) % num_slots;

    var probes: usize = 0;
    while (probes < num_slots) : (probes += 1) {
        const slot_base = index * 2;
        const key = dict.getField(slot_base, dict_size);

        if (key.isNil()) return null;

        // Pointer equality first
        if (key.bits == selector_sym.bits) {
            const val = dict.getField(slot_base + 1, dict_size);
            if (val.isObject()) {
                return @ptrCast(@alignCast(val.asObject()));
            }
            return null;
        }

        // String comparison fallback
        if (selector_str.len > 0 and key.isObject()) {
            const key_obj = key.asObject();
            if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const k_bytes = key_obj.bytes(key_obj.header.size);
                if (std.mem.eql(u8, k_bytes, selector_str)) {
                    const val = dict.getField(slot_base + 1, dict_size);
                    if (val.isObject()) {
                        return @ptrCast(@alignCast(val.asObject()));
                    }
                    return null;
                }
            }
        }

        index = (index + 1) % num_slots;
    }
    return null;
}

/// Hash-based method insertion - returns true on success, false if full
fn hashInsertMethod(dict: *object.Object, selector_sym: Value, method_val: Value, selector_str: []const u8) bool {
    const dict_size = dict.header.size;
    if (dict_size < 2) return false;

    const num_slots = dict_size / 2;
    const fields = dict.fields(dict_size);

    const hash = selector_sym.bits *% 2654435761;
    var index = @as(usize, @intCast(hash)) % num_slots;

    var probes: usize = 0;
    while (probes < num_slots) : (probes += 1) {
        const slot_base = index * 2;

        // Empty slot - insert
        if (fields[slot_base].isNil()) {
            fields[slot_base] = selector_sym;
            fields[slot_base + 1] = method_val;
            return true;
        }

        // Existing key - update
        if (fields[slot_base].bits == selector_sym.bits) {
            fields[slot_base + 1] = method_val;
            return true;
        }

        // String comparison for update
        if (selector_str.len > 0 and fields[slot_base].isObject()) {
            const key_obj = fields[slot_base].asObject();
            if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const k_bytes = key_obj.bytes(key_obj.header.size);
                if (std.mem.eql(u8, k_bytes, selector_str)) {
                    fields[slot_base + 1] = method_val;
                    return true;
                }
            }
        }

        index = (index + 1) % num_slots;
    }
    return false; // Full
}

fn ensureClassVarPresent(heap: *Heap, class_obj: *object.Object, name: []const u8) !void {
    const num_fields = class_obj.header.size;
    var dict_val = class_obj.getField(Heap.CLASS_FIELD_CLASS_VARS, num_fields);

    if (dict_val.isNil()) {
        const dict = try heap.allocateObject(Heap.CLASS_ARRAY, 2, .variable);
        const sym = try heap.internSymbol(name);
        dict.setField(0, sym, 2);
        dict.setField(1, Value.nil, 2);
        class_obj.setField(Heap.CLASS_FIELD_CLASS_VARS, Value.fromObject(dict), num_fields);
        return;
    }

    if (!dict_val.isObject()) return;

    const dict = dict_val.asObject();
    const size = dict.header.size;

    var i: usize = 0;
    while (i < size) : (i += 2) {
        const key = dict.getField(i, size);
        if (key.isObject()) {
            const key_obj = key.asObject();
            if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const key_bytes = key_obj.bytes(key_obj.header.size);
                if (std.mem.eql(u8, key_bytes, name)) {
                    return; // already present
                }
            }
        }
    }

    const new_size = size + 2;
    const new_dict = try heap.allocateObject(Heap.CLASS_ARRAY, new_size, .variable);

    i = 0;
    while (i < size) : (i += 1) {
        new_dict.setField(i, dict.getField(i, size), new_size);
    }

    const sym = try heap.internSymbol(name);
    new_dict.setField(size, sym, new_size);
    new_dict.setField(size + 1, Value.nil, new_size);

    class_obj.setField(Heap.CLASS_FIELD_CLASS_VARS, Value.fromObject(new_dict), num_fields);
}

fn setClassVarValue(heap: *Heap, class_obj: *object.Object, name: []const u8, value: Value) !void {
    try ensureClassVarPresent(heap, class_obj, name);
    const num_fields = class_obj.header.size;
    const dict_val = class_obj.getField(Heap.CLASS_FIELD_CLASS_VARS, num_fields);
    if (!dict_val.isObject()) return;
    const dict = dict_val.asObject();
    const size = dict.header.size;
    var i: usize = 0;
    while (i < size) : (i += 2) {
        const key = dict.getField(i, size);
        if (key.isObject() and key.asObject().header.class_index == Heap.CLASS_SYMBOL) {
            const key_bytes = key.asObject().bytes(key.asObject().header.size);
            if (std.mem.eql(u8, key_bytes, name)) {
                dict.setField(i + 1, value, size);
                return;
            }
        }
    }
}

/// Create a new class dynamically at runtime
pub fn createDynamicClass(heap: *Heap, name: []const u8, superclass: ?*object.Object, inst_var_names: []const u8, class_var_names: []const u8, pool_dict_names: []const u8, class_inst_var_names: []const u8, class_format: object.ClassFormat, existing_class: ?*object.Object, category: []const u8) !*object.Object {
    // Allocate or reuse a class object
    const class = existing_class orelse try heap.allocateObject(Heap.CLASS_CLASS, Heap.CLASS_NUM_FIELDS, .normal);

    // Use actual object size for field access (old classes may have fewer fields)
    const num_fields = class.header.size;

    // Set superclass
    if (superclass) |super_obj| {
        class.setField(Heap.CLASS_FIELD_SUPERCLASS, Value.fromObject(super_obj), num_fields);
    } else {
        class.setField(Heap.CLASS_FIELD_SUPERCLASS, Value.nil, num_fields);
    }

    // Set class name
    const name_sym = heap.internSymbol(name) catch return error.OutOfMemory;
    class.setField(Heap.CLASS_FIELD_NAME, name_sym, num_fields);

    // Set category (package) - only if class has enough fields (new classes)
    // Old classes from images may have fewer fields
    if (num_fields >= Heap.CLASS_NUM_FIELDS) {
        const category_sym = heap.internSymbol(if (category.len > 0) category else "Unpackaged") catch return error.OutOfMemory;
        class.setField(Heap.CLASS_FIELD_CATEGORY, category_sym, num_fields);
    }

    // Parse instance variable names and count them
    var inst_var_count: u32 = 0;
    if (inst_var_names.len > 0) {
        var iter = std.mem.splitAny(u8, inst_var_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len > 0) inst_var_count += 1;
        }
    }

    // Get superclass instance variable count (only for non-indexed parts)
    var super_inst_vars: u32 = 0;
    if (superclass) |super_obj| {
        const super_num_fields = super_obj.header.size;
        const super_format = super_obj.getField(Heap.CLASS_FIELD_FORMAT, super_num_fields);
        if (super_format.isSmallInt()) {
            const format_int = super_format.asSmallInt();
            if (format_int >= 0) {
                // Low byte is inst var count, format type is in higher bits
                super_inst_vars = @intCast(format_int & 0xFF);
            }
        }
    }

    // Set instanceSpec using Dolphin-compatible encoding
    const total_inst_vars = super_inst_vars + inst_var_count;
    const format_value: i61 = Heap.encodeInstanceSpec(total_inst_vars, class_format);
    class.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(format_value), num_fields);

    // Initialize method dictionary to nil if unset (will be created when first method is added)
    if (class.getField(Heap.CLASS_FIELD_METHOD_DICT, num_fields).isNil()) {
        class.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.nil, num_fields);
    }

    // Register in globals so the class is discoverable by name
    try heap.setGlobal(name, Value.fromObject(class));

    // Store instance variable names (create an array)
    if (inst_var_count > 0) {
        const names_array = try heap.allocateObject(Heap.CLASS_ARRAY, inst_var_count, .variable);
        var idx: usize = 0;
        var iter = std.mem.splitAny(u8, inst_var_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len == 0) continue;
            const var_sym = heap.internSymbol(part) catch return error.OutOfMemory;
            names_array.setField(idx, var_sym, inst_var_count);
            idx += 1;
        }
        class.setField(Heap.CLASS_FIELD_INST_VARS, Value.fromObject(names_array), num_fields);
    } else {
        class.setField(Heap.CLASS_FIELD_INST_VARS, Value.nil, num_fields);
    }

    // Parse and store class variable names as a dictionary
    // Class variables are stored as an array of [name, value] pairs (like method dict)
    var class_var_count: u32 = 0;
    if (class_var_names.len > 0) {
        var iter = std.mem.splitAny(u8, class_var_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len > 0) class_var_count += 1;
        }
    }

    if (class_var_count > 0) {
        // Create a dictionary with [name, value] pairs - values initialized to nil
        const class_vars_dict = try heap.allocateObject(Heap.CLASS_ARRAY, class_var_count * 2, .variable);
        var idx: usize = 0;
        var iter = std.mem.splitAny(u8, class_var_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len == 0) continue;
            const var_sym = heap.internSymbol(part) catch return error.OutOfMemory;
            class_vars_dict.setField(idx, var_sym, class_var_count * 2);
            class_vars_dict.setField(idx + 1, Value.nil, class_var_count * 2); // Initialize to nil
            idx += 2;
        }
        class.setField(Heap.CLASS_FIELD_CLASS_VARS, Value.fromObject(class_vars_dict), num_fields);
        std.debug.print("  Class variables: {s}\n", .{class_var_names});
    } else {
        class.setField(Heap.CLASS_FIELD_CLASS_VARS, Value.nil, num_fields);
    }

    // Parse and store pool dictionaries
    // Pool dictionaries are stored as an array of dictionary references
    var pool_dict_count: u32 = 0;
    if (pool_dict_names.len > 0) {
        var iter = std.mem.splitAny(u8, pool_dict_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len > 0) pool_dict_count += 1;
        }
    }

    if (pool_dict_count > 0) {
        const pool_dicts_array = try heap.allocateObject(Heap.CLASS_ARRAY, pool_dict_count, .variable);
        var idx: usize = 0;
        var iter = std.mem.splitAny(u8, pool_dict_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len == 0) continue;
            // Look up the pool dictionary by name
            if (heap.getGlobal(part)) |pool_val| {
                pool_dicts_array.setField(idx, pool_val, pool_dict_count);
            } else {
                // Pool not found - store the name as a symbol for later resolution
                const pool_sym = heap.internSymbol(part) catch return error.OutOfMemory;
                pool_dicts_array.setField(idx, pool_sym, pool_dict_count);
            }
            idx += 1;
        }
        class.setField(Heap.CLASS_FIELD_POOL_DICTS, Value.fromObject(pool_dicts_array), num_fields);
        std.debug.print("  Pool dictionaries: {s}\n", .{pool_dict_names});
    } else {
        class.setField(Heap.CLASS_FIELD_POOL_DICTS, Value.nil, num_fields);
    }

    // Create a metaclass for this class and set up class-side new/basicNew methods
    const existing_meta_val = class.getField(Heap.CLASS_FIELD_METACLASS, num_fields);
    const metaclass = if (existing_meta_val.isObject())
        existing_meta_val.asObject()
    else
        try createDynamicMetaclass(heap, class, name, class_inst_var_names, class_format);
    class.setField(Heap.CLASS_FIELD_METACLASS, Value.fromObject(metaclass), num_fields);

    // Update metaclass's superclass to match the class hierarchy
    // Metaclass's superclass should be the superclass's metaclass
    if (superclass) |super_obj| {
        const super_meta_val = super_obj.getField(Heap.CLASS_FIELD_METACLASS, super_obj.header.size);
        if (super_meta_val.isObject()) {
            metaclass.setField(Heap.CLASS_FIELD_SUPERCLASS, super_meta_val, Heap.METACLASS_NUM_FIELDS);
        }
    } else {
        // If no superclass, metaclass's superclass is Class
        const class_class = heap.getClass(Heap.CLASS_CLASS);
        metaclass.setField(Heap.CLASS_FIELD_SUPERCLASS, class_class, Heap.METACLASS_NUM_FIELDS);
    }

    // Register the class in the class table to get a class index (only for new classes)
    if (existing_class == null) {
        const class_index = try heap.registerClass(Value.fromObject(class));
        if (DEBUG_VERBOSE) std.debug.print("DEBUG createDynamicClass: registered class '{s}' at index {}\n", .{ name, class_index });
    }

    return class;
}

/// Create a metaclass for a dynamically created class
fn createDynamicMetaclass(heap: *Heap, class: *object.Object, name: []const u8, class_inst_var_names: []const u8, class_format: object.ClassFormat) !*object.Object {
    const bootstrap = @import("bootstrap.zig");

    // Use actual object size for field access (old classes may have fewer fields)
    const num_fields = class.header.size;

    // Allocate metaclass object (same as Class but with extra thisClass field)
    const metaclass = try heap.allocateObject(Heap.CLASS_METACLASS, Heap.METACLASS_NUM_FIELDS, .normal);

    // Set metaclass's thisClass to point to this class
    metaclass.setField(Heap.METACLASS_FIELD_THIS_CLASS, Value.fromObject(class), Heap.METACLASS_NUM_FIELDS);

    // Set the metaclass's FORMAT field - metaclass instances (class objects) have CLASS_NUM_FIELDS fields
    // Format is encoded as: (formatType << 8) | instSize
    // Use .normal format type (0) with CLASS_NUM_FIELDS instance variables
    const meta_format: i61 = Heap.encodeInstanceSpec(Heap.CLASS_NUM_FIELDS, .normal);
    metaclass.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(meta_format), Heap.METACLASS_NUM_FIELDS);

    // Parse and store class instance variable names
    var class_inst_var_count: u32 = 0;
    if (class_inst_var_names.len > 0) {
        var iter = std.mem.splitAny(u8, class_inst_var_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len > 0) class_inst_var_count += 1;
        }
    }

    if (class_inst_var_count > 0) {
        const names_array = try heap.allocateObject(Heap.CLASS_ARRAY, class_inst_var_count, .variable);
        var idx: usize = 0;
        var iter = std.mem.splitAny(u8, class_inst_var_names, " \t\r\n");
        while (iter.next()) |part| {
            if (part.len == 0) continue;
            const var_sym = heap.internSymbol(part) catch return error.OutOfMemory;
            names_array.setField(idx, var_sym, class_inst_var_count);
            idx += 1;
        }
        class.setField(Heap.CLASS_FIELD_CLASS_INST_VARS, Value.fromObject(names_array), num_fields);
        std.debug.print("  Class instance variables: {s}\n", .{class_inst_var_names});
    } else {
        class.setField(Heap.CLASS_FIELD_CLASS_INST_VARS, Value.nil, num_fields);
    }

    // Metaclass name is "ClassName class"
    var meta_name_buf: [128]u8 = undefined;
    const meta_name = std.fmt.bufPrint(&meta_name_buf, "{s} class", .{name}) catch name;
    const meta_name_sym = try heap.internSymbol(meta_name);
    metaclass.setField(Heap.CLASS_FIELD_NAME, meta_name_sym, Heap.METACLASS_NUM_FIELDS);

    // Get superclass's metaclass to set as metaclass superclass
    const superclass_val = class.getField(Heap.CLASS_FIELD_SUPERCLASS, num_fields);
    if (superclass_val.isObject()) {
        const super_obj = superclass_val.asObject();
        const super_num_fields = super_obj.header.size;
        const super_metaclass = super_obj.getField(Heap.CLASS_FIELD_METACLASS, super_num_fields);
        metaclass.setField(Heap.CLASS_FIELD_SUPERCLASS, super_metaclass, Heap.METACLASS_NUM_FIELDS);
    } else {
        // Root class - superclass of metaclass is Class
        // Prefer the dynamically loaded Class if available, otherwise use bootstrap Class
        const class_class = heap.getGlobal("Class") orelse heap.getClass(Heap.CLASS_CLASS);
        metaclass.setField(Heap.CLASS_FIELD_SUPERCLASS, class_class, Heap.METACLASS_NUM_FIELDS);
    }

    // Create method dictionary for metaclass and install basicNew/basicNew:
    const dict = try heap.allocateObject(Heap.CLASS_ARRAY, 8, .variable); // Space for 4 methods
    metaclass.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(dict), Heap.METACLASS_NUM_FIELDS);

    // Install basicNew based on class format; higher-level new/new: come from Smalltalk side
    const basicnew_prim = try bootstrap.createPrimitiveMethod(heap, 0, @intFromEnum(bytecodes.Primitive.basic_new));
    const selector_basicnew = try heap.internSymbol("basicNew");
    dict.setField(0, selector_basicnew, 8);
    dict.setField(1, Value.fromObject(@ptrCast(@alignCast(basicnew_prim))), 8);

    // For variable/byte/word classes, also add basicNew:
    if (class_format == .variable or class_format == .bytes or class_format == .words) {
        const basicnewsize_prim = try bootstrap.createPrimitiveMethod(heap, 1, @intFromEnum(bytecodes.Primitive.basic_new_size));
        const selector_basicnewsize = try heap.internSymbol("basicNew:");
        dict.setField(2, selector_basicnewsize, 8);
        dict.setField(3, Value.fromObject(@ptrCast(@alignCast(basicnewsize_prim))), 8);
    }

    return metaclass;
}

/// Compile and install a method from source code into a class
/// This is a public API for dynamic method compilation at runtime
pub fn compileAndInstallMethod(heap: *Heap, class_obj: *object.Object, source: []const u8) !void {
    // Parse the method source to extract selector
    var first_line_end: usize = 0;
    while (first_line_end < source.len and source[first_line_end] != '\n' and source[first_line_end] != '\r') {
        first_line_end += 1;
    }

    const signature = std.mem.trim(u8, source[0..first_line_end], " \t");
    const selector = extractSelector(signature) orelse {
        return error.InvalidMethodDefinition;
    };

    // Use a temporary arena for parsing
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Parse the method
    var p = Parser.init(alloc, source);
    const ast = p.parseMethod() catch return error.CompilationFailed;

    // Generate code
    var gen = CodeGenerator.init(alloc, heap, std.heap.page_allocator);
    defer gen.deinit();
    gen.source_code = source;

    // Get instance variables from the class hierarchy
    var inst_var_list: std.ArrayListUnmanaged([]const u8) = .{};
    defer inst_var_list.deinit(alloc);

    var walk_class = Value.fromObject(class_obj);
    while (walk_class.isObject()) {
        const walk_obj = walk_class.asObject();
        const walk_num_fields = walk_obj.header.size;
        const inst_vars = walk_obj.getField(Heap.CLASS_FIELD_INST_VARS, walk_num_fields);
        if (inst_vars.isObject()) {
            const vars_array = inst_vars.asObject();
            const vars_size = vars_array.header.size;
            var i: usize = vars_size;
            while (i > 0) {
                i -= 1;
                const var_sym = vars_array.getField(i, vars_size);
                if (var_sym.isObject()) {
                    const sym_obj = var_sym.asObject();
                    if (sym_obj.header.class_index == Heap.CLASS_SYMBOL) {
                        const var_name = sym_obj.bytes(sym_obj.header.size);
                        inst_var_list.insert(alloc, 0, var_name) catch {};
                    }
                }
            }
        }
        walk_class = walk_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, walk_num_fields);
    }
    gen.instance_variables = inst_var_list.items;

    // Compile the method
    const method = gen.compileMethod(ast) catch return error.CompilationFailed;

    // Install the method
    try installMethodInClass(heap, class_obj, selector, method, false);
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

test "extract selector - multi-keyword" {
    const selector = extractSelector("at: index put: value");
    try std.testing.expect(selector != null);
    try std.testing.expectEqualStrings("at:put:", selector.?);
}

test "extract selector - three keywords" {
    const selector = extractSelector("copyFrom: start to: stop into: dest");
    try std.testing.expect(selector != null);
    try std.testing.expectEqualStrings("copyFrom:to:into:", selector.?);
}
