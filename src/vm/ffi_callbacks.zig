//! FFI Callback Support
//!
//! This module enables Smalltalk blocks to be passed to C functions as callback
//! function pointers. When C code invokes the function pointer, the Smalltalk
//! block executes with marshalled arguments.
//!
//! Uses libffi closures (ffi_prep_closure_loc) to create callable C function
//! pointers at runtime.

const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");
const ffi_runtime = @import("ffi_runtime.zig");
const interpreter_mod = @import("interpreter.zig");

pub const Value = object.Value;
pub const Heap = memory.Heap;
pub const Object = object.Object;
pub const FFIType = ffi_runtime.FFIType;
pub const Interpreter = interpreter_mod.Interpreter;
pub const InterpreterError = interpreter_mod.InterpreterError;

/// libffi C imports
const ffi = @cImport({
    @cInclude("ffi.h");
});

/// Maximum number of active callbacks
const MAX_CALLBACKS = 256;

/// Maximum arguments for a callback
const MAX_CALLBACK_ARGS = 16;

/// Errors from callback operations
pub const CallbackError = error{
    TooManyCallbacks,
    InvalidSignature,
    ClosureAllocationFailed,
    ClosurePreparationFailed,
    InvalidCallbackId,
    BlockInvocationFailed,
    NotOnMainThread,
    QueueFull,
};

/// A registered callback entry
pub const FFICallbackEntry = struct {
    /// Unique ID for this callback
    id: u32,
    /// The Smalltalk block to invoke
    block: Value,
    /// Return type
    return_type: FFIType,
    /// Argument types
    arg_types: [MAX_CALLBACK_ARGS]FFIType,
    /// Number of arguments
    arg_count: usize,
    /// libffi call interface
    cif: ffi.ffi_cif,
    /// libffi type pointers for arguments
    ffi_arg_types: [MAX_CALLBACK_ARGS]*ffi.ffi_type,
    /// The closure structure
    closure: *ffi.ffi_closure,
    /// The callable function pointer (executable code)
    function_pointer: *anyopaque,
    /// Is this entry active?
    active: bool,
    /// Pointer back to registry for handler access
    registry: *FFICallbackRegistry,
};

/// Pending callback event (for cross-thread invocation)
pub const CallbackEvent = struct {
    /// The callback entry
    entry: *FFICallbackEntry,
    /// Marshalled arguments as Smalltalk Values
    args: [MAX_CALLBACK_ARGS]Value,
    /// Number of arguments
    arg_count: usize,
    /// Return value (set after block execution)
    result: Value,
    /// Completion signal
    completed: std.Thread.ResetEvent,
};

/// Registry managing all active callbacks
pub const FFICallbackRegistry = struct {
    /// All callback entries
    entries: [MAX_CALLBACKS]FFICallbackEntry,
    /// Next ID to assign
    next_id: u32,
    /// Mutex for thread safety
    mutex: std.Thread.Mutex,
    /// The main thread ID (for detecting cross-thread calls)
    main_thread_id: std.Thread.Id,
    /// Allocator
    allocator: std.mem.Allocator,
    /// Reference to interpreter for block invocation
    interpreter: *anyopaque, // *Interpreter, but forward declared
    /// Heap reference for value marshalling
    heap: *Heap,
    /// Event queue for cross-thread callbacks
    event_queue: std.ArrayList(CallbackEvent),
    /// Event queue mutex
    event_mutex: std.Thread.Mutex,

    pub fn init(allocator: std.mem.Allocator, interpreter: *anyopaque, heap: *Heap) FFICallbackRegistry {
        var registry = FFICallbackRegistry{
            .entries = undefined,
            .next_id = 1,
            .mutex = .{},
            .main_thread_id = std.Thread.getCurrentId(),
            .allocator = allocator,
            .interpreter = interpreter,
            .heap = heap,
            .event_queue = .{},
            .event_mutex = .{},
        };
        // Initialize all entries as inactive
        for (&registry.entries) |*entry| {
            entry.active = false;
            entry.id = 0;
        }
        return registry;
    }

    pub fn deinit(self: *FFICallbackRegistry) void {
        // Free all active closures
        for (&self.entries) |*entry| {
            if (entry.active) {
                ffi.ffi_closure_free(entry.closure);
                entry.active = false;
            }
        }
        self.event_queue.deinit(self.allocator);
    }

    /// Create a callback from a Smalltalk block and signature string
    pub fn createCallback(self: *FFICallbackRegistry, block: Value, signature: []const u8) CallbackError!u32 {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Find a free slot
        var slot: ?*FFICallbackEntry = null;
        for (&self.entries) |*entry| {
            if (!entry.active) {
                slot = entry;
                break;
            }
        }
        if (slot == null) {
            return CallbackError.TooManyCallbacks;
        }
        var entry = slot.?;

        // Parse signature
        const paren_start = std.mem.indexOf(u8, signature, "(") orelse return CallbackError.InvalidSignature;
        const paren_end = std.mem.lastIndexOf(u8, signature, ")") orelse return CallbackError.InvalidSignature;

        const ret_str = signature[0..paren_start];
        const args_str = signature[paren_start + 1 .. paren_end];

        entry.return_type = ffi_runtime.parseType(ret_str) orelse return CallbackError.InvalidSignature;

        // Parse argument types
        entry.arg_count = 0;
        if (args_str.len > 0) {
            var iter = std.mem.splitSequence(u8, args_str, ",");
            while (iter.next()) |type_str| {
                if (entry.arg_count >= MAX_CALLBACK_ARGS) return CallbackError.InvalidSignature;
                entry.arg_types[entry.arg_count] = ffi_runtime.parseType(type_str) orelse return CallbackError.InvalidSignature;
                entry.arg_count += 1;
            }
        }

        // Set up libffi type pointers
        for (0..entry.arg_count) |i| {
            entry.ffi_arg_types[i] = toFFIType(entry.arg_types[i]);
        }

        // Prepare the CIF
        const ffi_arg_types_ptr: [*c][*c]ffi.ffi_type = if (entry.arg_count > 0) @ptrCast(&entry.ffi_arg_types) else null;
        const cif_status = ffi.ffi_prep_cif(
            &entry.cif,
            ffi.FFI_DEFAULT_ABI,
            @intCast(entry.arg_count),
            toFFIType(entry.return_type),
            ffi_arg_types_ptr,
        );
        if (cif_status != ffi.FFI_OK) {
            return CallbackError.ClosurePreparationFailed;
        }

        // Allocate the closure
        var code_ptr: *anyopaque = undefined;
        const closure = ffi.ffi_closure_alloc(@sizeOf(ffi.ffi_closure), @ptrCast(&code_ptr));
        if (closure == null) {
            return CallbackError.ClosureAllocationFailed;
        }
        entry.closure = @ptrCast(@alignCast(closure));
        entry.function_pointer = code_ptr;

        // Prepare the closure
        const prep_status = ffi.ffi_prep_closure_loc(
            entry.closure,
            &entry.cif,
            &genericCallbackHandler,
            entry, // user_data points to this entry
            code_ptr,
        );
        if (prep_status != ffi.FFI_OK) {
            ffi.ffi_closure_free(closure);
            return CallbackError.ClosurePreparationFailed;
        }

        // Store block and mark active
        entry.id = self.next_id;
        self.next_id += 1;
        entry.block = block;
        entry.active = true;
        entry.registry = self;

        return entry.id;
    }

    /// Get the C function pointer for a callback ID
    pub fn getFunctionPointer(self: *FFICallbackRegistry, id: u32) ?*anyopaque {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (&self.entries) |*entry| {
            if (entry.active and entry.id == id) {
                return entry.function_pointer;
            }
        }
        return null;
    }

    /// Get the block Value for a callback ID (for GC roots)
    pub fn getBlock(self: *FFICallbackRegistry, id: u32) ?Value {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (&self.entries) |*entry| {
            if (entry.active and entry.id == id) {
                return entry.block;
            }
        }
        return null;
    }

    /// Free a callback by ID
    pub fn freeCallback(self: *FFICallbackRegistry, id: u32) bool {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (&self.entries) |*entry| {
            if (entry.active and entry.id == id) {
                ffi.ffi_closure_free(entry.closure);
                entry.active = false;
                entry.block = Value.nil;
                return true;
            }
        }
        return false;
    }

    /// Get all active callback blocks (for GC root scanning)
    pub fn getActiveBlocks(self: *FFICallbackRegistry, out: []Value) usize {
        self.mutex.lock();
        defer self.mutex.unlock();

        var count: usize = 0;
        for (&self.entries) |*entry| {
            if (entry.active and count < out.len) {
                out[count] = entry.block;
                count += 1;
            }
        }
        return count;
    }

    /// Check if we're on the main thread
    pub fn isMainThread(self: *FFICallbackRegistry) bool {
        return std.Thread.getCurrentId() == self.main_thread_id;
    }

    /// Process pending callback events (called from main interpreter loop)
    pub fn processPendingEvents(self: *FFICallbackRegistry) void {
        self.event_mutex.lock();
        defer self.event_mutex.unlock();

        while (self.event_queue.items.len > 0) {
            var event = self.event_queue.orderedRemove(0);
            self.event_mutex.unlock();

            // Invoke the block on main thread
            const result = self.invokeBlockDirect(event.entry, event.args[0..event.arg_count]);
            event.result = result;

            // Signal completion
            event.completed.set();

            self.event_mutex.lock();
        }
    }

    /// Invoke block directly (must be on main thread)
    fn invokeBlockDirect(self: *FFICallbackRegistry, entry: *FFICallbackEntry, args: []const Value) Value {
        const interp: *Interpreter = @ptrCast(@alignCast(self.interpreter));

        // Use the block value primitive mechanism
        // We need to invoke the block with the provided arguments
        return invokeBlock(interp, entry.block, args) catch Value.nil;
    }
};

/// Convert FFIType to libffi type pointer
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

/// Generic callback handler - called by libffi when C invokes the function pointer
fn genericCallbackHandler(
    cif: [*c]ffi.ffi_cif,
    ret: ?*anyopaque,
    args: [*c]?*anyopaque,
    user_data: ?*anyopaque,
) callconv(.c) void {
    _ = cif;

    const entry: *FFICallbackEntry = @ptrCast(@alignCast(user_data orelse return));
    const registry = entry.registry;

    // Marshal C arguments to Smalltalk Values
    var st_args: [MAX_CALLBACK_ARGS]Value = undefined;
    for (0..entry.arg_count) |i| {
        const arg_ptr = args[i] orelse {
            st_args[i] = Value.nil;
            continue;
        };
        st_args[i] = cArgToValue(arg_ptr, entry.arg_types[i], registry.heap);
    }

    var result: Value = Value.nil;

    if (registry.isMainThread()) {
        // Direct invocation on main thread
        result = registry.invokeBlockDirect(entry, st_args[0..entry.arg_count]);
    } else {
        // Cross-thread: queue event and wait
        var event = CallbackEvent{
            .entry = entry,
            .args = st_args,
            .arg_count = entry.arg_count,
            .result = Value.nil,
            .completed = std.Thread.ResetEvent{},
        };

        registry.event_mutex.lock();
        registry.event_queue.append(registry.allocator, event) catch {
            registry.event_mutex.unlock();
            // Queue full, return nil
            valueToRet(Value.nil, entry.return_type, ret);
            return;
        };
        registry.event_mutex.unlock();

        // Wait for completion
        event.completed.wait();
        result = event.result;
    }

    // Marshal return value back to C
    valueToRet(result, entry.return_type, ret);
}

/// Convert a C argument pointer to Smalltalk Value
fn cArgToValue(arg_ptr: *anyopaque, arg_type: FFIType, heap: *Heap) Value {
    _ = heap;
    return switch (arg_type) {
        .void => Value.nil,
        .uint8 => Value.fromSmallInt(@as(*u8, @ptrCast(@alignCast(arg_ptr))).*),
        .sint8 => Value.fromSmallInt(@as(*i8, @ptrCast(@alignCast(arg_ptr))).*),
        .uint16 => Value.fromSmallInt(@as(*u16, @ptrCast(@alignCast(arg_ptr))).*),
        .sint16 => Value.fromSmallInt(@as(*i16, @ptrCast(@alignCast(arg_ptr))).*),
        .uint32 => Value.fromSmallInt(@as(*u32, @ptrCast(@alignCast(arg_ptr))).*),
        .sint32 => Value.fromSmallInt(@as(*i32, @ptrCast(@alignCast(arg_ptr))).*),
        .uint64 => blk: {
            // Truncate u64 to fit in i61 (SmallInt max)
            const val = @as(*u64, @ptrCast(@alignCast(arg_ptr))).*;
            const max_small_int: u64 = @as(u64, 1) << 60; // Max positive value for i61
            if (val < max_small_int) {
                break :blk Value.fromSmallInt(@intCast(val));
            } else {
                // Value too large - truncate (lossy)
                break :blk Value.fromSmallInt(@intCast(val & (max_small_int - 1)));
            }
        },
        .sint64 => blk: {
            // Truncate i64 to fit in i61 (SmallInt)
            const val = @as(*i64, @ptrCast(@alignCast(arg_ptr))).*;
            const min_small_int: i64 = -(@as(i64, 1) << 60);
            const max_small_int: i64 = (@as(i64, 1) << 60) - 1;
            if (val >= min_small_int and val <= max_small_int) {
                break :blk Value.fromSmallInt(@intCast(val));
            } else if (val > max_small_int) {
                break :blk Value.fromSmallInt(@intCast(max_small_int));
            } else {
                break :blk Value.fromSmallInt(@intCast(min_small_int));
            }
        },
        .float32 => blk: {
            // TODO: Create Float object
            const f: f64 = @as(*f32, @ptrCast(@alignCast(arg_ptr))).*;
            _ = f;
            break :blk Value.nil;
        },
        .float64 => blk: {
            // TODO: Create Float object
            const f: f64 = @as(*f64, @ptrCast(@alignCast(arg_ptr))).*;
            _ = f;
            break :blk Value.nil;
        },
        .pointer, .string, .string_array => blk: {
            // Return pointer as integer - truncate to fit i61 if needed
            const ptr = @as(*usize, @ptrCast(@alignCast(arg_ptr))).*;
            const max_small_int: usize = @as(usize, 1) << 60;
            if (ptr < max_small_int) {
                break :blk Value.fromSmallInt(@intCast(ptr));
            } else {
                // Pointer too large - this is bad but we'll truncate
                break :blk Value.fromSmallInt(@intCast(ptr & (max_small_int - 1)));
            }
        },
    };
}

/// Convert Smalltalk Value to C return value
fn valueToRet(value: Value, ret_type: FFIType, ret_ptr: ?*anyopaque) void {
    const ptr = ret_ptr orelse return;
    switch (ret_type) {
        .void => {},
        .uint8 => {
            const p: *u8 = @ptrCast(@alignCast(ptr));
            if (value.isSmallInt()) {
                const v = value.asSmallInt();
                p.* = if (v >= 0) @truncate(@as(u64, @intCast(v))) else 0;
            } else {
                p.* = 0;
            }
        },
        .sint8 => {
            const p: *i8 = @ptrCast(@alignCast(ptr));
            p.* = if (value.isSmallInt()) @truncate(value.asSmallInt()) else 0;
        },
        .uint16 => {
            const p: *u16 = @ptrCast(@alignCast(ptr));
            if (value.isSmallInt()) {
                const v = value.asSmallInt();
                p.* = if (v >= 0) @truncate(@as(u64, @intCast(v))) else 0;
            } else {
                p.* = 0;
            }
        },
        .sint16 => {
            const p: *i16 = @ptrCast(@alignCast(ptr));
            p.* = if (value.isSmallInt()) @truncate(value.asSmallInt()) else 0;
        },
        .uint32 => {
            const p: *u32 = @ptrCast(@alignCast(ptr));
            if (value.isSmallInt()) {
                const v = value.asSmallInt();
                p.* = if (v >= 0) @truncate(@as(u64, @intCast(v))) else 0;
            } else {
                p.* = 0;
            }
        },
        .sint32 => {
            const p: *i32 = @ptrCast(@alignCast(ptr));
            p.* = if (value.isSmallInt()) @truncate(value.asSmallInt()) else 0;
        },
        .uint64 => {
            const p: *u64 = @ptrCast(@alignCast(ptr));
            if (value.isSmallInt()) {
                const v = value.asSmallInt();
                // Sign-extend i61 to i64, then reinterpret as u64
                p.* = @bitCast(@as(i64, v));
            } else {
                p.* = 0;
            }
        },
        .sint64 => {
            const p: *i64 = @ptrCast(@alignCast(ptr));
            // Sign-extend i61 to i64
            p.* = if (value.isSmallInt()) @as(i64, value.asSmallInt()) else 0;
        },
        .float32 => {
            const p: *f32 = @ptrCast(@alignCast(ptr));
            // TODO: Handle Float objects
            p.* = 0.0;
        },
        .float64 => {
            const p: *f64 = @ptrCast(@alignCast(ptr));
            // TODO: Handle Float objects
            p.* = 0.0;
        },
        .pointer, .string, .string_array => {
            const p: *usize = @ptrCast(@alignCast(ptr));
            if (value.isSmallInt()) {
                const v = value.asSmallInt();
                // Pointers should be positive - interpret as unsigned
                p.* = if (v >= 0) @intCast(v) else 0;
            } else {
                p.* = 0;
            }
        },
    }
}

/// Invoke a Smalltalk block with arguments
/// This is a simplified version - a full implementation would need to handle
/// all the block invocation complexity from primitives.zig
fn invokeBlock(interp: anytype, block: Value, args: []const Value) !Value {
    const interpreter: *Interpreter = @ptrCast(@alignCast(interp));

    if (!block.isObject()) {
        return Value.nil;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        return Value.nil;
    }

    // Validate block has expected fields
    if (block_obj.header.size < Heap.BLOCK_NUM_FIELDS) {
        return Value.nil;
    }

    // Get block data
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return Value.nil;
    }

    const expected_args: usize = @intCast(num_args_val.asSmallInt());
    if (expected_args != args.len) {
        return Value.nil; // Argument count mismatch
    }

    // Save interpreter state
    const saved_ip = interpreter.ip;
    const saved_method = interpreter.method;
    const saved_sp = interpreter.sp;
    const saved_temp_base = interpreter.temp_base;
    const saved_outer_temp_base = interpreter.outer_temp_base;
    const saved_home_temp_base = interpreter.home_temp_base;
    const saved_receiver = interpreter.receiver;
    const saved_context_ptr = interpreter.context_ptr;
    const saved_heap_context = interpreter.heap_context;
    const saved_home_heap_context = interpreter.home_heap_context;

    // Set up for block execution
    interpreter.method = @ptrCast(@alignCast(method_val.asObject()));
    interpreter.ip = @intCast(start_pc.asSmallInt());
    interpreter.receiver = block_receiver;

    // Push receiver placeholder
    try interpreter.push(block_receiver);
    interpreter.temp_base = interpreter.sp - 1;

    // Push arguments
    for (args) |arg| {
        try interpreter.push(arg);
    }

    // Set up contexts
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        interpreter.heap_context = outer_context_val;
        interpreter.home_heap_context = home_context_val;
        interpreter.outer_temp_base = saved_temp_base;
        interpreter.home_temp_base = saved_home_temp_base;
    } else if (outer_context_val.isSmallInt()) {
        interpreter.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interpreter.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else saved_home_temp_base;
        interpreter.heap_context = Value.nil;
        interpreter.home_heap_context = Value.nil;
    } else {
        interpreter.outer_temp_base = saved_temp_base;
        interpreter.home_temp_base = saved_home_temp_base;
        interpreter.heap_context = Value.nil;
        interpreter.home_heap_context = Value.nil;
    }

    // Allocate temps
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var i: usize = 0;
    while (i < num_temps) : (i += 1) {
        try interpreter.push(Value.nil);
    }

    // Create heap context for temps if needed
    if (num_temps > 0) {
        const outer_ctx = interpreter.heap_context;
        const heap_ctx = try interpreter.createHeapContext(num_temps);
        if (!outer_ctx.isNil()) {
            heap_ctx.setField(Heap.CONTEXT_FIELD_SENDER, outer_ctx, heap_ctx.header.size);
        }
        interpreter.heap_context = Value.fromObject(heap_ctx);
        if (interpreter.home_heap_context.isNil()) {
            interpreter.home_heap_context = outer_ctx;
        }
    }

    // Track block depth
    if (interpreter.primitive_block_depth >= interpreter.primitive_block_bases.len) {
        return error.StackOverflow;
    }
    interpreter.primitive_block_bases[interpreter.primitive_block_depth] = interpreter.context_ptr;
    interpreter.primitive_block_depth += 1;

    // Execute block
    const result = interpreter.interpretLoop() catch |err| {
        interpreter.primitive_block_depth -= 1;
        interpreter.ip = saved_ip;
        interpreter.method = saved_method;
        if (err != InterpreterError.BlockNonLocalReturn) {
            interpreter.sp = saved_sp;
        }
        interpreter.temp_base = saved_temp_base;
        interpreter.outer_temp_base = saved_outer_temp_base;
        interpreter.home_temp_base = saved_home_temp_base;
        interpreter.receiver = saved_receiver;
        interpreter.context_ptr = saved_context_ptr;
        interpreter.heap_context = saved_heap_context;
        interpreter.home_heap_context = saved_home_heap_context;

        if (err == InterpreterError.BlockNonLocalReturn) {
            return try interpreter.pop();
        }
        return Value.nil;
    };

    // Restore state
    interpreter.primitive_block_depth -= 1;
    interpreter.ip = saved_ip;
    interpreter.method = saved_method;
    interpreter.sp = saved_sp;
    interpreter.temp_base = saved_temp_base;
    interpreter.outer_temp_base = saved_outer_temp_base;
    interpreter.home_temp_base = saved_home_temp_base;
    interpreter.receiver = saved_receiver;
    interpreter.context_ptr = saved_context_ptr;
    interpreter.heap_context = saved_heap_context;
    interpreter.home_heap_context = saved_home_heap_context;

    return result;
}
