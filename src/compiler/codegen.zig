const std = @import("std");
const parser = @import("parser.zig");
const bytecodes = @import("../vm/bytecodes.zig");
const object = @import("../vm/object.zig");
const memory = @import("../vm/memory.zig");

const ASTNode = parser.ASTNode;
const NodeType = parser.NodeType;
const Opcode = bytecodes.Opcode;
const Value = object.Value;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;

pub const CompileError = error{
    OutOfMemory,
    TooManyLiterals,
    TooManyArguments,
    TooManyTemporaries,
    InvalidNode,
    VariableNotFound,
    BytecodeOverflow,
};

/// Code generator - compiles AST to bytecode
pub const CodeGenerator = struct {
    allocator: std.mem.Allocator,
    method_allocator: std.mem.Allocator,
    heap: *Heap,

    // Output buffers
    bytecodes_buf: std.ArrayList(u8),
    literals: std.ArrayList(Value),

    // Variable scopes
    arguments: []const []const u8,
    temporaries: std.ArrayList([]const u8),
    instance_variables: []const []const u8,

    // For nested blocks - track outer scope for closure variable access
    outer_arguments: []const []const u8,
    outer_temporaries: std.ArrayList([]const u8),
    block_depth: usize,
    // Boundary in outer_temporaries: indices < this are from outer-outer (level 2), >= are from immediate outer (level 1)
    outer_scope_boundary: usize,

    // Method source for storage
    source_code: ?[]const u8 = null,

    // Special selector indices for optimized sends
    selector_plus: ?usize,
    selector_minus: ?usize,
    selector_times: ?usize,
    selector_divide: ?usize,
    selector_less: ?usize,
    selector_greater: ?usize,
    selector_less_eq: ?usize,
    selector_greater_eq: ?usize,
    selector_equal: ?usize,
    selector_not_equal: ?usize,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, method_allocator: std.mem.Allocator) CodeGenerator {
        return .{
            .allocator = allocator,
            .method_allocator = method_allocator,
            .heap = heap,
            .bytecodes_buf = .{},
            .literals = .{},
            .arguments = &[_][]const u8{},
            .temporaries = .{},
            .instance_variables = &[_][]const u8{},
            .outer_arguments = &[_][]const u8{},
            .outer_temporaries = .{},
            .block_depth = 0,
            .outer_scope_boundary = 0,
            .selector_plus = null,
            .selector_minus = null,
            .selector_times = null,
            .selector_divide = null,
            .selector_less = null,
            .selector_greater = null,
            .selector_less_eq = null,
            .selector_greater_eq = null,
            .selector_equal = null,
            .selector_not_equal = null,
        };
    }

    pub fn deinit(self: *CodeGenerator) void {
        self.bytecodes_buf.deinit(self.allocator);
        self.literals.deinit(self.allocator);
        self.temporaries.deinit(self.allocator);
        self.outer_temporaries.deinit(self.allocator);
    }

    /// Compile a "doIt" expression for the REPL
    pub fn compileDoIt(self: *CodeGenerator, node: *ASTNode) CompileError!*CompiledMethod {
        // Special case: if the node is a block with temporaries but no parameters,
        // it's a doIt with local variables - compile inline instead of as a closure
        if (node.node_type == .block and node.data.block.parameters.len == 0) {
            // Register temporaries
            for (node.data.block.temporaries) |temp| {
                self.temporaries.append(self.allocator, temp) catch {
                    return CompileError.OutOfMemory;
                };
            }

            // Compile statements
            for (node.data.block.statements, 0..) |stmt, i| {
                try self.compileNode(stmt);
                // Pop intermediate results except the last
                if (i < node.data.block.statements.len - 1) {
                    try self.emit(.pop);
                }
            }

            // If no statements, push nil
            if (node.data.block.statements.len == 0) {
                try self.emit(.push_nil);
            }
        } else {
            // Generate bytecode for the expression
            try self.compileNode(node);
        }

        // Add implicit return of the result
        try self.emit(.return_top);

        // Create the CompiledMethod
        return self.buildMethod(0); // 0 arguments for doIt
    }

    /// Compile a method definition from a MethodNode AST
    pub fn compileMethod(self: *CodeGenerator, node: *ASTNode) CompileError!*CompiledMethod {
        if (node.node_type != .method) {
            return CompileError.InvalidNode;
        }

        const method_def = node.data.method_def;

        // Register arguments as the first temporaries (arguments come before locals)
        for (method_def.arguments) |arg| {
            self.temporaries.append(self.allocator, arg) catch {
                return CompileError.OutOfMemory;
            };
        }

        // Register local temporaries
        for (method_def.temporaries) |temp| {
            self.temporaries.append(self.allocator, temp) catch {
                return CompileError.OutOfMemory;
            };
        }

        // Handle primitive methods
        if (method_def.primitive_index != 0) {
            // For primitive methods, we still compile the fallback code
            // The primitive will be tried first, and if it fails, the bytecode runs
        }

        // Compile method body statements
        for (method_def.statements, 0..) |stmt, i| {
            try self.compileNode(stmt);
            // Pop intermediate results except the last
            if (i < method_def.statements.len - 1) {
                try self.emit(.pop);
            }
        }

        // If no statements or no explicit return, return self
        if (method_def.statements.len == 0) {
            try self.emit(.return_receiver);
        } else {
            // Check if the last statement is already a return
            const last_stmt = method_def.statements[method_def.statements.len - 1];
            if (last_stmt.node_type != .return_statement) {
                // Add implicit return self
                try self.emit(.pop); // Pop the last expression result
                try self.emit(.return_receiver);
            }
        }

        // Create the CompiledMethod with primitive index
        return self.buildMethodWithPrimitive(@intCast(method_def.arguments.len), method_def.primitive_index);
    }

    /// Compile an AST node
    pub fn compileNode(self: *CodeGenerator, node: *ASTNode) CompileError!void {
        switch (node.node_type) {
            .literal_integer => {
                const n = node.data.integer;
                if (n >= -128 and n <= 127) {
                    try self.emit(.push_integer);
                    try self.emitByte(@bitCast(@as(i8, @intCast(n))));
                } else if (n >= -32768 and n <= 32767) {
                    try self.emit(.push_integer_16);
                    const n16: i16 = @intCast(n);
                    const unsigned: u16 = @bitCast(n16);
                    try self.emitByte(@intCast(unsigned >> 8));
                    try self.emitByte(@intCast(unsigned & 0xFF));
                } else {
                    // Store as literal - check if fits in SmallInt range
                    const max_small_int: i64 = (1 << 60) - 1;
                    const min_small_int: i64 = -(1 << 60);
                    if (n >= min_small_int and n <= max_small_int) {
                        const index = try self.addLiteral(Value.fromSmallInt(@intCast(n)));
                        try self.emit(.push_literal);
                        try self.emitByte(@intCast(index));
                    } else {
                        // Number too large for SmallInt - would need LargeInteger
                        // For now, store as Float
                        const float_obj = self.heap.allocateFloat(@floatFromInt(n)) catch {
                            return CompileError.OutOfMemory;
                        };
                        const index = try self.addLiteral(float_obj);
                        try self.emit(.push_literal);
                        try self.emitByte(@intCast(index));
                    }
                }
            },

            .literal_float => {
                // Allocate a Float object and store as literal
                const float_obj = self.heap.allocateFloat(node.data.float) catch {
                    return CompileError.OutOfMemory;
                };
                const index = try self.addLiteral(float_obj);
                try self.emit(.push_literal);
                try self.emitByte(@intCast(index));
            },

            .literal_scaled_decimal => {
                // For now, treat scaled decimal as Float
                // TODO: Implement proper ScaledDecimal class
                const float_obj = self.heap.allocateFloat(node.data.scaled_decimal.value) catch {
                    return CompileError.OutOfMemory;
                };
                const index = try self.addLiteral(float_obj);
                try self.emit(.push_literal);
                try self.emitByte(@intCast(index));
            },

            .literal_string => {
                const str = self.heap.allocateString(node.data.string) catch {
                    return CompileError.OutOfMemory;
                };
                const index = try self.addLiteral(str);
                try self.emit(.push_literal);
                try self.emitByte(@intCast(index));
            },

            .literal_symbol => {
                const sym = self.heap.internSymbol(node.data.string) catch {
                    return CompileError.OutOfMemory;
                };
                const index = try self.addLiteral(sym);
                try self.emit(.push_literal);
                try self.emitByte(@intCast(index));
            },

            .literal_character => {
                const cp: u21 = @intCast(node.data.integer);
                const index = try self.addLiteral(Value.fromCharacter(cp));
                try self.emit(.push_literal);
                try self.emitByte(@intCast(index));
            },

            .literal_array => {
                // Compile array elements - push each onto stack
                for (node.data.elements) |elem| {
                    try self.compileNode(elem);
                }
                // Create array from stack values
                const size = node.data.elements.len;
                if (size <= 255) {
                    try self.emit(.make_array);
                    try self.emitByte(@intCast(size));
                }
            },

            .variable => {
                const name = node.data.name;

                // Check current scope arguments first
                for (self.arguments, 0..) |arg, i| {
                    if (std.mem.eql(u8, arg, name)) {
                        try self.emitPushTemp(@intCast(i));
                        return;
                    }
                }

                // Check current scope temporaries
                for (self.temporaries.items, 0..) |temp, i| {
                    if (std.mem.eql(u8, temp, name)) {
                        try self.emitPushTemp(@intCast(self.arguments.len + i));
                        return;
                    }
                }

                // If inside a block, check outer scope variables
                if (self.block_depth > 0) {
                    // Check outer arguments (usually empty since we flatten into outer_temporaries)
                    for (self.outer_arguments, 0..) |arg, i| {
                        if (std.mem.eql(u8, arg, name)) {
                            // Emit push_outer_temp: level=1, index=i
                            try self.emit(.push_outer_temp);
                            try self.emitByte(1); // Level (1 = immediate outer)
                            try self.emitByte(@intCast(i));
                            return;
                        }
                    }

                    // Check outer temporaries - determine level based on boundary
                    for (self.outer_temporaries.items, 0..) |temp, i| {
                        if (std.mem.eql(u8, temp, name)) {
                            try self.emit(.push_outer_temp);
                            if (i < self.outer_scope_boundary) {
                                // Variable from outer-outer scope (method) - level 2
                                try self.emitByte(2);
                                try self.emitByte(@intCast(i));
                            } else {
                                // Variable from immediate outer scope - level 1
                                try self.emitByte(1);
                                try self.emitByte(@intCast(i - self.outer_scope_boundary));
                            }
                            return;
                        }
                    }
                }

                // Check instance variables
                for (self.instance_variables, 0..) |ivar, i| {
                    if (std.mem.eql(u8, ivar, name)) {
                        try self.emitPushInstVar(@intCast(i));
                        return;
                    }
                }

                // Must be a global - store as association in literals
                const sym = self.heap.internSymbol(name) catch {
                    return CompileError.OutOfMemory;
                };
                const index = try self.addLiteral(sym);
                try self.emit(.push_literal_variable);
                try self.emitByte(@intCast(index));
            },

            .pseudo_variable => {
                const name = node.data.name;

                if (std.mem.eql(u8, name, "self")) {
                    try self.emit(.push_receiver);
                } else if (std.mem.eql(u8, name, "nil")) {
                    try self.emit(.push_nil);
                } else if (std.mem.eql(u8, name, "true")) {
                    try self.emit(.push_true);
                } else if (std.mem.eql(u8, name, "false")) {
                    try self.emit(.push_false);
                } else if (std.mem.eql(u8, name, "super")) {
                    // super is like self but affects message lookup
                    try self.emit(.push_receiver);
                } else if (std.mem.eql(u8, name, "thisContext")) {
                    try self.emit(.push_context);
                }
            },

            .assignment => {
                // Compile the value
                try self.compileNode(node.data.assignment.value);

                const name = node.data.assignment.name;

                // Find the variable and emit store - check current scope first
                for (self.arguments, 0..) |arg, i| {
                    if (std.mem.eql(u8, arg, name)) {
                        try self.emitStoreTemp(@intCast(i));
                        return;
                    }
                }

                for (self.temporaries.items, 0..) |temp, i| {
                    if (std.mem.eql(u8, temp, name)) {
                        try self.emitStoreTemp(@intCast(self.arguments.len + i));
                        return;
                    }
                }

                // If inside a block, check outer scope variables
                if (self.block_depth > 0) {
                    // Check outer arguments (usually empty since we flatten into outer_temporaries)
                    for (self.outer_arguments, 0..) |arg, i| {
                        if (std.mem.eql(u8, arg, name)) {
                            // Emit store_outer_temp: level=1, index=i
                            try self.emit(.store_outer_temp);
                            try self.emitByte(1); // Level (1 = immediate outer)
                            try self.emitByte(@intCast(i));
                            return;
                        }
                    }

                    // Check outer temporaries - determine level based on boundary
                    for (self.outer_temporaries.items, 0..) |temp, i| {
                        if (std.mem.eql(u8, temp, name)) {
                            try self.emit(.store_outer_temp);
                            if (i < self.outer_scope_boundary) {
                                // Variable from outer-outer scope (method) - level 2
                                try self.emitByte(2);
                                try self.emitByte(@intCast(i));
                            } else {
                                // Variable from immediate outer scope - level 1
                                try self.emitByte(1);
                                try self.emitByte(@intCast(i - self.outer_scope_boundary));
                            }
                            return;
                        }
                    }
                }

                for (self.instance_variables, 0..) |ivar, i| {
                    if (std.mem.eql(u8, ivar, name)) {
                        try self.emitStoreInstVar(@intCast(i));
                        return;
                    }
                }

                // Not found in local scope - treat as class variable
                // Store the symbol name as a literal and emit extended_store with type 2
                const sym = self.heap.internSymbol(name) catch {
                    return CompileError.OutOfMemory;
                };
                const index = try self.addLiteral(sym);
                try self.emit(.extended_store);
                try self.emitByte(2); // type 2 = class variable
                try self.emitByte(@intCast(index));
            },

            .message_send => {
                // Compile receiver
                try self.compileNode(node.data.message.receiver);

                // Compile arguments
                for (node.data.message.arguments) |arg| {
                    try self.compileNode(arg);
                }

                const selector = node.data.message.selector;
                const num_args = node.data.message.arguments.len;

                // Check for optimized sends
                if (num_args == 1) {
                    if (std.mem.eql(u8, selector, "+")) {
                        try self.emit(.send_plus);
                        return;
                    } else if (std.mem.eql(u8, selector, "-")) {
                        try self.emit(.send_minus);
                        return;
                    } else if (std.mem.eql(u8, selector, "*")) {
                        try self.emit(.send_times);
                        return;
                    } else if (std.mem.eql(u8, selector, "/")) {
                        try self.emit(.send_divide);
                        return;
                    } else if (std.mem.eql(u8, selector, "<")) {
                        try self.emit(.send_less_than);
                        return;
                    } else if (std.mem.eql(u8, selector, ">")) {
                        try self.emit(.send_greater_than);
                        return;
                    } else if (std.mem.eql(u8, selector, "<=")) {
                        try self.emit(.send_less_or_equal);
                        return;
                    } else if (std.mem.eql(u8, selector, ">=")) {
                        try self.emit(.send_greater_or_equal);
                        return;
                    } else if (std.mem.eql(u8, selector, "=")) {
                        try self.emit(.send_equal);
                        return;
                    } else if (std.mem.eql(u8, selector, "~=")) {
                        try self.emit(.send_not_equal);
                        return;
                    } else if (std.mem.eql(u8, selector, "==")) {
                        try self.emit(.send_identical);
                        return;
                    }
                }

                if (num_args == 0) {
                    if (std.mem.eql(u8, selector, "class")) {
                        try self.emit(.send_class);
                        return;
                    } else if (std.mem.eql(u8, selector, "size")) {
                        try self.emit(.send_size);
                        return;
                    }
                }

                // General send
                const sym = self.heap.internSymbol(selector) catch {
                    return CompileError.OutOfMemory;
                };
                const selector_index = try self.addLiteral(sym);

                if (node.data.message.is_super) {
                    try self.emit(.super_send);
                } else {
                    try self.emit(.send);
                }
                try self.emitByte(@intCast(selector_index));
                try self.emitByte(@intCast(num_args));
            },

            .cascade => {
                // Compile receiver
                try self.compileNode(node.data.cascade.receiver);

                // For each message except the last, dup receiver and pop result
                const messages = node.data.cascade.messages;
                for (messages, 0..) |msg, i| {
                    if (i < messages.len - 1) {
                        try self.emit(.dup);
                    }

                    // Compile arguments
                    for (msg.arguments) |arg| {
                        try self.compileNode(arg);
                    }

                    // Send message
                    const sym = self.heap.internSymbol(msg.selector) catch {
                        return CompileError.OutOfMemory;
                    };
                    const selector_index = try self.addLiteral(sym);
                    try self.emit(.send);
                    try self.emitByte(@intCast(selector_index));
                    try self.emitByte(@intCast(msg.arguments.len));

                    if (i < messages.len - 1) {
                        try self.emit(.pop);
                    }
                }
            },

            .block => {
                // Create a BlockClosure
                // For now, we compile blocks inline and wrap in a closure structure
                // The closure bytecode format is:
                //   push_closure <num_args> <num_temps> <bytecode_length_hi> <bytecode_length_lo>
                //   <block bytecodes>
                //   <after block>

                const num_args: u8 = @intCast(node.data.block.parameters.len);
                const num_temps: u8 = @intCast(node.data.block.temporaries.len);

                // Save current position to patch later
                try self.emit(.push_closure);
                const patch_pos = self.bytecodes_buf.items.len;
                try self.emitByte(num_args);
                try self.emitByte(num_temps);
                try self.emitByte(0); // Placeholder for bytecode length hi
                try self.emitByte(0); // Placeholder for bytecode length lo

                const block_start = self.bytecodes_buf.items.len;

                // Save outer scope's arguments and temporaries for outer context access
                const prev_outer_args = self.outer_arguments;

                // Save the current outer_temporaries content to restore later
                var saved_outer_temps = std.ArrayListUnmanaged([]const u8){};
                for (self.outer_temporaries.items) |temp| {
                    saved_outer_temps.append(self.allocator, temp) catch {
                        return CompileError.OutOfMemory;
                    };
                }

                // For nested blocks, we need to merge all outer scopes
                // Build a combined list: [prev_outer_args] + [prev_outer_temps] + [current_args] + [current_temps]
                // This flattens the scope chain so inner blocks can access outer-outer variables
                var combined_outer = std.ArrayListUnmanaged([]const u8){};

                // First add previous outer arguments (grandparent scope) - these are level 2
                for (prev_outer_args) |arg| {
                    combined_outer.append(self.allocator, arg) catch {
                        return CompileError.OutOfMemory;
                    };
                }
                // Then previous outer temporaries - also level 2
                for (saved_outer_temps.items) |temp| {
                    combined_outer.append(self.allocator, temp) catch {
                        return CompileError.OutOfMemory;
                    };
                }
                // Track boundary: indices before this are from level 2 (outer-outer / method scope)
                const saved_outer_scope_boundary = self.outer_scope_boundary;
                self.outer_scope_boundary = prev_outer_args.len + saved_outer_temps.items.len;

                // Then current arguments (parent scope) - these are level 1
                for (self.arguments) |arg| {
                    combined_outer.append(self.allocator, arg) catch {
                        return CompileError.OutOfMemory;
                    };
                }
                // Then current temporaries - also level 1
                for (self.temporaries.items) |temp| {
                    combined_outer.append(self.allocator, temp) catch {
                        return CompileError.OutOfMemory;
                    };
                }

                // Set combined outer scope - everything is in outer_temporaries for simplicity
                // (we put args there too since they're accessed the same way via push_outer_temp)
                self.outer_arguments = &[_][]const u8{};
                self.outer_temporaries.shrinkRetainingCapacity(0);
                for (combined_outer.items) |item| {
                    self.outer_temporaries.append(self.allocator, item) catch {
                        return CompileError.OutOfMemory;
                    };
                }
                combined_outer.deinit(self.allocator);

                // Save current scope
                const current_args = self.arguments;
                const current_temp_count = self.temporaries.items.len;

                // Set block parameters as arguments for the block scope
                self.arguments = node.data.block.parameters;

                // Clear temporaries for block scope (only block's own temps)
                self.temporaries.shrinkRetainingCapacity(0);

                // Add block temporaries to scope
                for (node.data.block.temporaries) |temp| {
                    self.temporaries.append(self.allocator, temp) catch {
                        return CompileError.OutOfMemory;
                    };
                }

                // Compile block body
                self.block_depth += 1;
                for (node.data.block.statements) |stmt| {
                    try self.compileNode(stmt);
                }
                self.block_depth -= 1;

                // Restore current scope
                self.arguments = current_args;
                self.temporaries.shrinkRetainingCapacity(0);
                // Re-add original temporaries
                var i: usize = 0;
                while (i < current_temp_count) : (i += 1) {
                    // We need to restore from outer_temporaries since that's where we saved them
                    if (i < self.outer_temporaries.items.len) {
                        self.temporaries.append(self.allocator, self.outer_temporaries.items[i]) catch {
                            return CompileError.OutOfMemory;
                        };
                    }
                }

                // Restore previous outer scope
                self.outer_arguments = prev_outer_args;
                self.outer_temporaries.shrinkRetainingCapacity(0);
                for (saved_outer_temps.items) |temp| {
                    self.outer_temporaries.append(self.allocator, temp) catch {
                        return CompileError.OutOfMemory;
                    };
                }
                saved_outer_temps.deinit(self.allocator);
                self.outer_scope_boundary = saved_outer_scope_boundary;

                // If block has no statements, push nil
                if (node.data.block.statements.len == 0) {
                    try self.emit(.push_nil);
                }

                // Add block return (returns value to block caller)
                try self.emit(.return_top);

                const block_end = self.bytecodes_buf.items.len;
                const block_size: u16 = @intCast(block_end - block_start);

                // Patch the bytecode length (after num_args and num_temps)
                self.bytecodes_buf.items[patch_pos + 2] = @intCast(block_size >> 8);
                self.bytecodes_buf.items[patch_pos + 3] = @intCast(block_size & 0xFF);
            },

            .return_statement => {
                try self.compileNode(node.data.return_value);
                if (self.block_depth > 0) {
                    try self.emit(.block_return);
                } else {
                    try self.emit(.return_top);
                }
            },

            .sequence => {
                for (node.data.statements, 0..) |stmt, i| {
                    try self.compileNode(stmt);
                    // Pop intermediate results except the last
                    if (i < node.data.statements.len - 1) {
                        try self.emit(.pop);
                    }
                }
            },

            .method => {
                // Method nodes should be compiled via compileMethod, not compileNode
                return CompileError.InvalidNode;
            },
        }
    }

    fn emit(self: *CodeGenerator, op: Opcode) CompileError!void {
        self.bytecodes_buf.append(self.allocator, @intFromEnum(op)) catch {
            return CompileError.BytecodeOverflow;
        };
    }

    fn emitByte(self: *CodeGenerator, byte: u8) CompileError!void {
        self.bytecodes_buf.append(self.allocator, byte) catch {
            return CompileError.BytecodeOverflow;
        };
    }

    fn emitPushTemp(self: *CodeGenerator, index: u8) CompileError!void {
        if (index < 16) {
            try self.emitByte(@intFromEnum(Opcode.push_temporary_0) + index);
        } else {
            try self.emit(.push_temporary);
            try self.emitByte(index);
        }
    }

    fn emitPushInstVar(self: *CodeGenerator, index: u8) CompileError!void {
        if (index < 16) {
            try self.emitByte(@intFromEnum(Opcode.push_receiver_variable_0) + index);
        } else {
            try self.emit(.push_receiver_variable);
            try self.emitByte(index);
        }
    }

    fn emitStoreTemp(self: *CodeGenerator, index: u8) CompileError!void {
        if (index < 16) {
            try self.emitByte(@intFromEnum(Opcode.store_temporary_0) + index);
        } else {
            // Extended store
            try self.emit(.extended_store);
            try self.emitByte(0); // temp type
            try self.emitByte(index);
        }
    }

    fn emitStoreInstVar(self: *CodeGenerator, index: u8) CompileError!void {
        if (index < 16) {
            try self.emitByte(@intFromEnum(Opcode.store_receiver_variable_0) + index);
        } else {
            try self.emit(.extended_store);
            try self.emitByte(1); // inst var type
            try self.emitByte(index);
        }
    }

    fn addLiteral(self: *CodeGenerator, value: Value) CompileError!usize {
        // Check if already in literals
        for (self.literals.items, 0..) |lit, i| {
            if (lit.eql(value)) {
                return i;
            }
        }

        if (self.literals.items.len >= 256) {
            return CompileError.TooManyLiterals;
        }

        self.literals.append(self.allocator, value) catch {
            return CompileError.OutOfMemory;
        };
        return self.literals.items.len - 1;
    }

    fn buildMethod(self: *CodeGenerator, num_args: u8) CompileError!*CompiledMethod {
        return self.buildMethodWithPrimitive(num_args, 0);
    }

    fn buildMethodWithPrimitive(self: *CodeGenerator, num_args: u8, primitive_index: u16) CompileError!*CompiledMethod {
        // If source code is set, add it as the last literal
        if (self.source_code) |source| {
            const source_str = self.heap.allocateString(source) catch {
                return CompileError.OutOfMemory;
            };
            self.literals.append(self.allocator, source_str) catch {
                return CompileError.OutOfMemory;
            };
        }

        const num_literals = self.literals.items.len;
        const bytecode_size = self.bytecodes_buf.items.len;

        // Calculate total size
        const header_size = @sizeOf(CompiledMethod.MethodHeader);
        const literals_size = num_literals * @sizeOf(Value);
        const total_size = header_size + literals_size + bytecode_size;

        // Allocate memory for the method with 8-byte alignment
        const mem = self.method_allocator.alignedAlloc(u8, std.mem.Alignment.of(CompiledMethod), total_size) catch {
            return CompileError.OutOfMemory;
        };
        const method: *CompiledMethod = @ptrCast(mem.ptr);

        // Fill in header
        method.header = .{
            .num_args = num_args,
            .num_temps = @intCast(self.temporaries.items.len),
            .num_literals = @intCast(num_literals),
            .primitive_index = primitive_index,
            .flags = .{ .has_source = self.source_code != null },
            .bytecode_size = @intCast(bytecode_size),
        };

        // Copy literals
        const literals_ptr: [*]Value = @ptrFromInt(@intFromPtr(method) + header_size);
        for (self.literals.items, 0..) |lit, i| {
            literals_ptr[i] = lit;
        }

        // Copy bytecodes
        const bytecodes_ptr: [*]u8 = @ptrFromInt(@intFromPtr(method) + header_size + literals_size);
        @memcpy(bytecodes_ptr[0..bytecode_size], self.bytecodes_buf.items);

        return method;
    }
};

test "CodeGenerator - simple integer" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var gen = CodeGenerator.init(allocator, heap, allocator);
    defer gen.deinit();

    // Create a simple integer node
    var node = ASTNode{
        .node_type = .literal_integer,
        .token = .{ .type = .integer, .text = "42", .line = 1, .column = 1 },
        .data = .{ .integer = 42 },
    };

    const method = try gen.compileDoIt(&node);
    defer allocator.free(@as([*]u8, @ptrCast(method))[0..100]); // Simplified cleanup

    try std.testing.expectEqual(@as(u8, 0), method.header.num_args);
}

test "CodeGenerator - binary message" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var gen = CodeGenerator.init(allocator, heap, allocator);
    defer gen.deinit();

    // Create "3 + 4" AST manually
    var left = ASTNode{
        .node_type = .literal_integer,
        .token = .{ .type = .integer, .text = "3", .line = 1, .column = 1 },
        .data = .{ .integer = 3 },
    };

    var right = ASTNode{
        .node_type = .literal_integer,
        .token = .{ .type = .integer, .text = "4", .line = 1, .column = 5 },
        .data = .{ .integer = 4 },
    };

    var args = [_]*ASTNode{&right};

    var send = ASTNode{
        .node_type = .message_send,
        .token = .{ .type = .binary_selector, .text = "+", .line = 1, .column = 3 },
        .data = .{
            .message = .{
                .receiver = &left,
                .selector = "+",
                .arguments = &args,
                .is_super = false,
            },
        },
    };

    const method = try gen.compileDoIt(&send);
    _ = method;
    // Verify bytecodes contain push 3, push 4, send_plus, return_top
}
