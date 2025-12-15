const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");
const bytecodes = @import("bytecodes.zig");
const primitives = @import("primitives.zig");
const debugger = @import("debugger.zig");
const scheduler = @import("scheduler.zig");
const overlapped = @import("overlapped.zig");
const jit = @import("jit.zig");

const Value = object.Value;
const Object = object.Object;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;
const Opcode = bytecodes.Opcode;
const Scheduler = scheduler.Scheduler;
const Process = scheduler.Process;
const OverlappedPool = overlapped.OverlappedPool;
const JIT = jit.JIT;

var debug_receiver_idx2_count: usize = 0;
var debug_send_special_count: usize = 0;

// Debug flag - set to false to disable verbose debug output
const DEBUG_VERBOSE = false;
const DEBUG_STACK = true;
const DEBUG_STACK_TRACE = true; // Print last N method calls before overflow

pub const InterpreterError = error{
    StackOverflow,
    StackUnderflow,
    MessageNotUnderstood,
    PrimitiveFailed,
    InvalidBytecode,
    InvalidIndex,
    OutOfMemory,
    TypeError,
    BlockCannotReturn,
    BlockNonLocalReturn, // Non-local return from block - result is on stack
    SmalltalkException, // Exception signaled by Smalltalk code
    ContinueExecution, // Primitive set up new context - continue main loop
};

/// Exception handler entry for on:do: handling
pub const ExceptionHandler = struct {
    exception_class: Value, // The exception class being caught
    handler_block: Value, // The block to execute
    context_ptr: usize, // Context index when handler was installed
    sp: usize, // Stack pointer when handler was installed
    temp_base: usize, // temp_base when handler was installed
    method: *CompiledMethod, // Method when handler was installed
    ip: usize, // Instruction pointer when handler was installed
    receiver: Value, // Receiver when handler was installed
};

/// Execution context for a method activation
/// Optimized to remove unused fields (num_args, num_temps, outer_context, closure)
/// Size reduced from ~104 bytes to ~72 bytes for better cache performance
pub const Context = struct {
    method: *CompiledMethod,
    method_class: Value,
    ip: usize,
    receiver: Value,
    // Temporaries and arguments are stored on the stack
    temp_base: usize, // Base index in stack for temps/args
    outer_temp_base: usize, // Base index for outer temps (for blocks) - level 1
    home_temp_base: usize, // Base index for home method temps - level >= 2
    // Heap-allocated context for closure variable capture
    heap_context: Value,
    // Home method's heap context (for nested blocks)
    home_heap_context: Value,
};

/// The main Smalltalk interpreter
pub const Interpreter = struct {
    // ========================================================================
    // Method Cache Constants - must be at top of struct in Zig
    // ========================================================================
    const METHOD_CACHE_SIZE: usize = 2048; // Must be power of 2
    const MethodCacheEntry = struct {
        class: Value,
        selector: Value,
        method: ?*CompiledMethod,
        holder: Value, // The class that contains the method (for super sends)
    };

    // ========================================================================
    // Inline Cache Constants - per-callsite caching
    // ========================================================================
    const INLINE_CACHE_SIZE: usize = 4096; // Must be power of 2
    const InlineCacheEntry = struct {
        method_ptr: usize, // Pointer to CompiledMethod containing this send
        bytecode_offset: u16, // Offset of send bytecode within method
        cached_class: Value, // Receiver class when cache was filled
        cached_method: ?*CompiledMethod, // Method found for this class
        cached_holder: Value, // Class containing the method
        cached_version: u32, // Cache version when entry was created
    };

    heap: *Heap,

    // Execution stack
    stack: [8192]Value,
    sp: usize, // Stack pointer (points to next free slot)

    // Call stack
    contexts: [1024]Context,
    context_ptr: usize, // Current context index

    // Current execution state (cached from current context)
    method: *CompiledMethod,
    method_class: Value,
    ip: usize,
    receiver: Value,
    temp_base: usize,
    outer_temp_base: usize, // For closures: temp_base of enclosing context (level 1)
    home_temp_base: usize, // For closures: temp_base of home method context (level >= 2)
    // Heap-allocated context for closure variable capture (current activation's heap context)
    heap_context: Value,
    // Home method's heap context (for level >= 2 variable access in nested blocks)
    home_heap_context: Value,
    last_mnu_selector: []const u8,
    last_send_selector: []const u8,
    last_mnu_receiver: Value,
    last_mnu_method: []const u8,

    // Exception handling
    exception_handlers: [256]ExceptionHandler,
    handler_ptr: usize, // Current handler index (number of active handlers)
    current_exception: Value, // Current exception being handled (for handler block parameter)

    // Primitive block execution tracking - when > 0, return_top returns directly to primitive
    // but block_return still uses context chain for non-local returns
    primitive_block_depth: usize,
    // Stack of context_ptrs at which primitive blocks started - used to check if return_top
    // should return directly to primitive (when context_ptr matches top of stack)
    primitive_block_bases: [64]usize,
    // Target temp_base for non-local return - when BlockNonLocalReturn is signaled,
    // this stores the temp_base of the home context we want to return to
    non_local_return_target: usize,

    // Transcript output callback - if set, transcript primitives call this instead of stdout
    transcript_callback: ?*const fn ([]const u8) void,

    // Process scheduler for green thread support
    process_scheduler: Scheduler,

    // Overlapped FFI call pool for non-blocking native calls
    overlapped_pool: OverlappedPool,

    // Allocator for dynamic allocations
    allocator: std.mem.Allocator,

    // Instruction counter for preemption timing
    instruction_count: u64,

    // Method cache fields
    method_cache: [METHOD_CACHE_SIZE]MethodCacheEntry,
    method_cache_hits: u64,
    method_cache_misses: u64,

    // Inline cache fields (per-callsite caching)
    inline_cache: [INLINE_CACHE_SIZE]InlineCacheEntry,
    inline_cache_hits: u64,
    inline_cache_misses: u64,
    cache_version: u32, // Global version - increment when methods change to invalidate caches

    // Common selectors cached for fast comparison
    selector_plus: Value,
    selector_minus: Value,
    selector_times: Value,
    selector_divide: Value,
    selector_modulo: Value,
    selector_less: Value,
    selector_greater: Value,
    selector_less_equal: Value,
    selector_greater_equal: Value,
    selector_equal: Value,
    selector_not_equal: Value,
    selector_bitand: Value,
    selector_bitor: Value,
    selector_bitxor: Value,
    selector_bitshift: Value,
    // Boolean conditional selectors (for fast path)
    selector_ifTrue: Value,
    selector_ifFalse: Value,
    selector_ifTrueIfFalse: Value,
    selector_ifFalseIfTrue: Value,

    // JIT Compiler (optional - null if JIT disabled)
    jit_compiler: ?*JIT,
    jit_enabled: bool,
    jit_compiled_calls: u64,
    jit_interpreted_calls: u64,

    pub fn init(heap: *Heap, allocator: std.mem.Allocator) Interpreter {
        return .{
            .heap = heap,
            .stack = undefined,
            .sp = 0,
            .contexts = undefined,
            .context_ptr = 0,
            .method = undefined,
            .method_class = Value.nil,
            .ip = 0,
            .receiver = Value.nil,
            .temp_base = 0,
            .outer_temp_base = 0,
            .home_temp_base = 0,
            .heap_context = Value.nil,
            .home_heap_context = Value.nil,
            .last_mnu_selector = "<?>",
            .last_send_selector = "<?>",
            .last_mnu_receiver = Value.nil,
            .last_mnu_method = "<?>",
            .exception_handlers = undefined,
            .handler_ptr = 0,
            .current_exception = Value.nil,
            .primitive_block_depth = 0,
            .primitive_block_bases = [_]usize{0} ** 64,
            .non_local_return_target = 0,
            .transcript_callback = null,
            .process_scheduler = Scheduler.init(allocator),
            .overlapped_pool = OverlappedPool.init(allocator),
            .allocator = allocator,
            .instruction_count = 0,
            // Initialize method cache to empty
            .method_cache = [_]MethodCacheEntry{.{ .class = Value.nil, .selector = Value.nil, .method = null, .holder = Value.nil }} ** METHOD_CACHE_SIZE,
            .method_cache_hits = 0,
            .method_cache_misses = 0,
            // Initialize inline cache to empty
            .inline_cache = [_]InlineCacheEntry{.{ .method_ptr = 0, .bytecode_offset = 0, .cached_class = Value.nil, .cached_method = null, .cached_holder = Value.nil, .cached_version = 0 }} ** INLINE_CACHE_SIZE,
            .inline_cache_hits = 0,
            .inline_cache_misses = 0,
            .cache_version = 1, // Start at 1 so version 0 means "uninitialized"
            // Common selectors - will be initialized on first use
            .selector_plus = Value.nil,
            .selector_minus = Value.nil,
            .selector_times = Value.nil,
            .selector_divide = Value.nil,
            .selector_modulo = Value.nil,
            .selector_less = Value.nil,
            .selector_greater = Value.nil,
            .selector_less_equal = Value.nil,
            .selector_greater_equal = Value.nil,
            .selector_equal = Value.nil,
            .selector_not_equal = Value.nil,
            .selector_bitand = Value.nil,
            .selector_bitor = Value.nil,
            .selector_bitxor = Value.nil,
            .selector_bitshift = Value.nil,
            .selector_ifTrue = Value.nil,
            .selector_ifFalse = Value.nil,
            .selector_ifTrueIfFalse = Value.nil,
            .selector_ifFalseIfTrue = Value.nil,
            // JIT is disabled by default - call enableJit() to enable
            .jit_compiler = null,
            .jit_enabled = false,
            .jit_compiled_calls = 0,
            .jit_interpreted_calls = 0,
        };
    }

    /// Enable JIT compilation
    pub fn enableJit(self: *Interpreter) !void {
        if (self.jit_compiler == null) {
            const jit_ptr = try self.allocator.create(JIT);
            jit_ptr.* = JIT.init(self.allocator);
            self.jit_compiler = jit_ptr;
        }
        self.jit_enabled = true;
    }

    /// Disable JIT compilation
    pub fn disableJit(self: *Interpreter) void {
        self.jit_enabled = false;
    }

    /// Deinitialize JIT compiler
    pub fn deinitJit(self: *Interpreter) void {
        if (self.jit_compiler) |jit_ptr| {
            jit_ptr.deinit();
            self.allocator.destroy(jit_ptr);
            self.jit_compiler = null;
        }
    }

    /// Initialize common selectors for fast arithmetic dispatch
    pub fn initCommonSelectors(self: *Interpreter) void {
        self.selector_plus = self.heap.internSymbol("+") catch Value.nil;
        self.selector_minus = self.heap.internSymbol("-") catch Value.nil;
        self.selector_times = self.heap.internSymbol("*") catch Value.nil;
        self.selector_divide = self.heap.internSymbol("/") catch Value.nil;
        self.selector_modulo = self.heap.internSymbol("\\\\") catch Value.nil;
        self.selector_less = self.heap.internSymbol("<") catch Value.nil;
        self.selector_greater = self.heap.internSymbol(">") catch Value.nil;
        self.selector_less_equal = self.heap.internSymbol("<=") catch Value.nil;
        self.selector_greater_equal = self.heap.internSymbol(">=") catch Value.nil;
        self.selector_equal = self.heap.internSymbol("=") catch Value.nil;
        self.selector_not_equal = self.heap.internSymbol("~=") catch Value.nil;
        self.selector_bitand = self.heap.internSymbol("bitAnd:") catch Value.nil;
        self.selector_bitor = self.heap.internSymbol("bitOr:") catch Value.nil;
        self.selector_bitxor = self.heap.internSymbol("bitXor:") catch Value.nil;
        self.selector_bitshift = self.heap.internSymbol("bitShift:") catch Value.nil;
        // Boolean conditional selectors
        self.selector_ifTrue = self.heap.internSymbol("ifTrue:") catch Value.nil;
        self.selector_ifFalse = self.heap.internSymbol("ifFalse:") catch Value.nil;
        self.selector_ifTrueIfFalse = self.heap.internSymbol("ifTrue:ifFalse:") catch Value.nil;
        self.selector_ifFalseIfTrue = self.heap.internSymbol("ifFalse:ifTrue:") catch Value.nil;
    }

    /// Flush the method cache (call when methods are added/modified)
    pub fn flushMethodCache(self: *Interpreter) void {
        for (&self.method_cache) |*entry| {
            entry.class = Value.nil;
            entry.selector = Value.nil;
            entry.method = null;
            entry.holder = Value.nil;
        }
    }

    /// Invalidate the inline cache by incrementing version
    /// This is O(1) instead of O(n) for clearing all entries
    /// Call when methods are added/modified to any class
    pub fn invalidateInlineCache(self: *Interpreter) void {
        self.cache_version +%= 1; // Wrapping add to handle overflow
        if (self.cache_version == 0) self.cache_version = 1; // Skip 0 (uninitialized)
    }

    /// Flush the inline cache completely (clears all entries)
    /// Use invalidateInlineCache() for better performance
    pub fn flushInlineCache(self: *Interpreter) void {
        self.invalidateInlineCache();
        for (&self.inline_cache) |*entry| {
            entry.method_ptr = 0;
            entry.bytecode_offset = 0;
            entry.cached_class = Value.nil;
            entry.cached_method = null;
            entry.cached_holder = Value.nil;
            entry.cached_version = 0;
        }
    }

    /// Flush both method cache and inline cache
    pub fn flushAllCaches(self: *Interpreter) void {
        self.flushMethodCache();
        self.flushInlineCache();
    }

    pub fn deinit(self: *Interpreter) void {
        self.overlapped_pool.deinit();
        self.process_scheduler.deinit();
    }

    // ========================================================================
    // Heap-allocated Context Support
    // ========================================================================

    /// Create a heap-allocated MethodContext for closure variable capture.
    /// Copies temps from the current stack frame into the heap context.
    pub fn createHeapContext(self: *Interpreter, num_temps: usize) !*Object {
        const total_fields = Heap.CONTEXT_NUM_FIXED_FIELDS + num_temps;
        const ctx = try self.heap.allocateObject(Heap.CLASS_METHOD_CONTEXT, total_fields, .normal);

        // Set fixed fields
        ctx.setField(Heap.CONTEXT_FIELD_SENDER, Value.nil, total_fields); // sender - not used for now
        ctx.setField(Heap.CONTEXT_FIELD_PC, Value.fromSmallInt(@intCast(self.ip)), total_fields);
        ctx.setField(Heap.CONTEXT_FIELD_STACKP, Value.fromSmallInt(0), total_fields);
        ctx.setField(Heap.CONTEXT_FIELD_METHOD, Value.fromObject(@ptrCast(@alignCast(self.method))), total_fields);
        ctx.setField(Heap.CONTEXT_FIELD_CLOSURE_OR_RECEIVER, self.receiver, total_fields);

        // Copy temps from current stack frame to heap context
        var i: usize = 0;
        while (i < num_temps) : (i += 1) {
            const stack_idx = self.temp_base + 1 + i; // +1 to skip receiver
            const val = if (stack_idx < self.sp) self.stack[stack_idx] else Value.nil;
            ctx.setField(Heap.CONTEXT_NUM_FIXED_FIELDS + i, val, total_fields);
        }

        return ctx;
    }

    /// Get a temp variable from a heap context
    pub fn getHeapContextTemp(ctx: *Object, index: usize) Value {
        const total_fields = ctx.header.size;
        const field_index = Heap.CONTEXT_NUM_FIXED_FIELDS + index;
        if (field_index < total_fields) {
            return ctx.getField(field_index, total_fields);
        }
        return Value.nil;
    }

    /// Set a temp variable in a heap context
    pub fn setHeapContextTemp(ctx: *Object, index: usize, val: Value) void {
        const total_fields = ctx.header.size;
        const field_index = Heap.CONTEXT_NUM_FIXED_FIELDS + index;
        if (field_index < total_fields) {
            ctx.setField(field_index, val, total_fields);
        }
    }

    // ========================================================================
    // Process Context Switching
    // ========================================================================

    /// Save the current interpreter state to a Process
    pub fn saveContextToProcess(self: *Interpreter, process: *Process) void {
        const saved = &process.saved_context;

        // Save current execution state
        saved.method = self.method;
        saved.method_class = self.method_class;
        saved.ip = self.ip;
        saved.receiver = self.receiver;
        saved.temp_base = self.temp_base;
        saved.outer_temp_base = self.outer_temp_base;
        saved.home_temp_base = self.home_temp_base;
        saved.heap_context = self.heap_context;
        saved.home_heap_context = self.home_heap_context;

        // Save stack
        saved.sp = self.sp;
        @memcpy(saved.stack[0..self.sp], self.stack[0..self.sp]);

        // Save context stack
        saved.context_ptr = self.context_ptr;
        for (0..self.context_ptr) |i| {
            const ctx = &self.contexts[i];
            process.saved_contexts[i] = .{
                .method = ctx.method,
                .method_class = ctx.method_class,
                .ip = ctx.ip,
                .receiver = ctx.receiver,
                .temp_base = ctx.temp_base,
                .outer_temp_base = ctx.outer_temp_base,
                .home_temp_base = ctx.home_temp_base,
                .heap_context = ctx.heap_context,
                .home_heap_context = ctx.home_heap_context,
            };
        }

        // Save exception handlers
        saved.handler_ptr = self.handler_ptr;
        for (0..self.handler_ptr) |i| {
            const handler = &self.exception_handlers[i];
            process.saved_exception_handlers[i] = .{
                .exception_class = handler.exception_class,
                .handler_block = handler.handler_block,
                .context_ptr = handler.context_ptr,
                .sp = handler.sp,
                .temp_base = handler.temp_base,
                .method = handler.method,
                .ip = handler.ip,
                .receiver = handler.receiver,
            };
        }

        // Save other state
        saved.current_exception = self.current_exception;
        saved.primitive_block_depth = self.primitive_block_depth;
        saved.non_local_return_target = self.non_local_return_target;
    }

    /// Restore interpreter state from a Process
    pub fn restoreContextFromProcess(self: *Interpreter, process: *Process) void {
        const saved = &process.saved_context;

        // Restore current execution state
        if (saved.method) |m| {
            self.method = m;
        }
        self.method_class = saved.method_class;
        self.ip = saved.ip;
        self.receiver = saved.receiver;
        self.temp_base = saved.temp_base;
        self.outer_temp_base = saved.outer_temp_base;
        self.home_temp_base = saved.home_temp_base;
        self.heap_context = saved.heap_context;
        self.home_heap_context = saved.home_heap_context;

        // Restore stack
        self.sp = saved.sp;
        @memcpy(self.stack[0..saved.sp], saved.stack[0..saved.sp]);

        // Restore context stack
        self.context_ptr = saved.context_ptr;
        for (0..saved.context_ptr) |i| {
            const saved_ctx = &process.saved_contexts[i];
            self.contexts[i] = .{
                .method = saved_ctx.method.?,
                .method_class = saved_ctx.method_class,
                .ip = saved_ctx.ip,
                .receiver = saved_ctx.receiver,
                .temp_base = saved_ctx.temp_base,
                .outer_temp_base = saved_ctx.outer_temp_base,
                .home_temp_base = saved_ctx.home_temp_base,
                .heap_context = saved_ctx.heap_context,
                .home_heap_context = saved_ctx.home_heap_context,
            };
        }

        // Restore exception handlers
        self.handler_ptr = saved.handler_ptr;
        for (0..saved.handler_ptr) |i| {
            const saved_handler = &process.saved_exception_handlers[i];
            self.exception_handlers[i] = .{
                .exception_class = saved_handler.exception_class,
                .handler_block = saved_handler.handler_block,
                .context_ptr = saved_handler.context_ptr,
                .sp = saved_handler.sp,
                .temp_base = saved_handler.temp_base,
                .method = saved_handler.method.?,
                .ip = saved_handler.ip,
                .receiver = saved_handler.receiver,
            };
        }

        // Restore other state
        self.current_exception = saved.current_exception;
        self.primitive_block_depth = saved.primitive_block_depth;
        self.non_local_return_target = saved.non_local_return_target;
    }

    /// Switch from current process to a new process
    /// Returns the process that was running (now suspended)
    pub fn switchToProcess(self: *Interpreter, new_process: *Process) ?*Process {
        const old_process = self.process_scheduler.active_process;

        // Save current state if there's an active process
        if (old_process) |old| {
            self.saveContextToProcess(old);
            old.state = .ready;
        }

        // Check if this is a fresh process that needs to start executing a block
        if (new_process.saved_context.method == null) {
            // This is a fresh process - check if it has a block to execute
            if (self.setupFreshProcess(new_process)) {
                new_process.state = .running;
                self.process_scheduler.active_process = new_process;
                return old_process;
            }
            // No block to execute - can't switch to this process
            // Put it in terminated state
            new_process.state = .terminated;
            return old_process;
        }

        // Restore new process state
        self.restoreContextFromProcess(new_process);
        new_process.state = .running;
        self.process_scheduler.active_process = new_process;

        return old_process;
    }

    /// Set up a fresh process to execute its block
    /// Returns true if successful, false if no block found
    fn setupFreshProcess(self: *Interpreter, process: *Process) bool {
        // Get the Smalltalk Process object
        if (!process.object.isObject()) return false;
        const process_obj = process.object.asObject();

        // Get suspendedFrame (index 1) which should contain the block
        const suspended_frame = process_obj.getField(scheduler.ProcessFields.suspendedFrame, process_obj.header.size);
        if (!suspended_frame.isObject()) return false;

        const block_obj = suspended_frame.asObject();
        if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) return false;

        // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
        const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
        const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
        const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
        const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
        const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
        const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
        const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

        if (!start_pc.isSmallInt() or !method_val.isObject()) return false;

        // Check for no-arg block
        if (num_args_val.isSmallInt() and num_args_val.asSmallInt() != 0) return false;

        // Reset interpreter state for this fresh process
        self.sp = 0;
        self.context_ptr = 0;
        self.handler_ptr = 0;
        self.current_exception = Value.nil;
        self.primitive_block_depth = 0;
        self.non_local_return_target = 0;

        // Set up execution to directly run the block's bytecodes (like primBlockValue does)
        self.method = @ptrCast(@alignCast(method_val.asObject()));
        self.ip = @intCast(start_pc.asSmallInt());
        self.receiver = block_receiver;

        // Push a placeholder for the receiver slot (like primBlockValue does)
        // This ensures temp_base + 1 + index correctly addresses temps
        self.push(block_receiver) catch return false;
        self.temp_base = self.sp - 1; // temp_base points to receiver slot

        // Restore heap contexts from block for cross-process variable access
        // This is the key to allowing captured variables to work across processes
        if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
            // New format: heap contexts - this enables cross-process variable sharing
            self.heap_context = outer_context_val;
            self.home_heap_context = home_context_val;
            // Stack indices aren't valid in a different process
            self.outer_temp_base = self.temp_base;
            self.home_temp_base = self.temp_base;
        } else {
            // Old format or nil - no heap context support
            self.outer_temp_base = 0;
            self.home_temp_base = 0;
            self.heap_context = Value.nil;
            self.home_heap_context = Value.nil;
        }

        // Allocate space for block temporaries by pushing nil values
        const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
            @intCast(num_temps_val.asSmallInt())
        else
            0;
        var ti: usize = 0;
        while (ti < num_temps) : (ti += 1) {
            self.push(Value.nil) catch return false;
        }

        // If the forked block has temps, ALWAYS create a new heap context for them.
        // This is critical for nested blocks to access the forked block's temps via push_outer_temp.
        // The outer context (from OUTER_CONTEXT field) is kept in home_heap_context for level >= 2 access.
        if (num_temps > 0) {
            // Save outer context for home access
            const outer_ctx = self.heap_context;
            // Create new context for this block's temps
            const heap_ctx = self.createHeapContext(num_temps) catch return false;
            self.heap_context = Value.fromObject(heap_ctx);
            // Keep home context pointing to outer for level >= 2 access
            if (self.home_heap_context.isNil()) {
                self.home_heap_context = outer_ctx;
            }
        }

        return true;
    }

    /// Create and start the initial/main process
    pub fn createMainProcess(self: *Interpreter) !*Process {
        // Create a Process Smalltalk object
        // Process has 7 fields: nextLink (from Link), suspendedFrame, priority, myList, exceptionEnvironment, name, processId
        const process_class = self.heap.globals.get("Process") orelse self.heap.getClass(Heap.CLASS_OBJECT);
        const process_obj = try self.heap.allocateObject(
            if (process_class.isObject()) process_class.asObject().header.class_index else Heap.CLASS_OBJECT,
            7, // nextLink, suspendedFrame, priority, myList, exceptionEnvironment, name, processId
            .normal
        );

        // Set priority to user scheduling priority (5) - priority is at index 2
        process_obj.setField(scheduler.ProcessFields.priority, Value.fromSmallInt(5), 7);

        // Create the VM-side Process
        const process = try self.process_scheduler.createProcess(Value.fromObject(process_obj), 5);
        process.state = .running;
        self.process_scheduler.active_process = process;

        return process;
    }

    /// Yield to the next ready process (if any)
    /// Returns true if a switch occurred
    pub fn yieldToNextProcess(self: *Interpreter) bool {
        // Check for timer expiration first
        if (self.process_scheduler.checkTimer()) |sem| {
            // Signal the timing semaphore
            self.signalSemaphore(sem);
        }

        // Find highest priority ready process
        const next = self.process_scheduler.findHighestPriorityReady() orelse return false;

        // Get current process
        const current = self.process_scheduler.active_process orelse return false;

        // Only switch if next process has higher or equal priority
        if (next.priority >= current.priority) {
            // Remove from ready queue
            self.process_scheduler.removeFromReadyQueue(next);

            // Put current process back in ready queue
            self.process_scheduler.makeReady(current);

            // Switch to new process
            _ = self.switchToProcess(next);
            return true;
        }

        return false;
    }

    /// Signal a semaphore (wake up waiting process or increment excess signals)
    pub fn signalSemaphore(self: *Interpreter, sem: Value) void {
        _ = self; // Used for future process wakeup logic
        if (!sem.isObject()) return;

        const sem_obj = sem.asObject();

        // Check for waiting processes in the semaphore's wait queue
        // For now, just increment excess signals
        // Full implementation would check the linked list
        const signals_val = sem_obj.getField(scheduler.SemaphoreFields.signals, 3);
        if (signals_val.isSmallInt()) {
            const current = signals_val.asSmallInt();
            sem_obj.setField(scheduler.SemaphoreFields.signals, Value.fromSmallInt(current + 1), 3);
        }
    }

    /// Process any completed overlapped FFI calls
    /// This signals the completion semaphores, waking up waiting processes
    pub fn processCompletedOverlappedCalls(self: *Interpreter) void {
        const semaphores = self.overlapped_pool.processCompletedCalls() catch return;
        defer self.allocator.free(semaphores);

        // Signal each completion semaphore
        for (semaphores) |sem| {
            self.signalSemaphore(sem);
        }
    }

    /// Run pending background processes for a time slice.
    /// Call this from the main event loop to give background processes time to run.
    /// Returns true if any process made progress.
    pub fn runPendingProcesses(self: *Interpreter, max_instructions: u32) bool {
        _ = max_instructions;
        // Check for completed overlapped calls
        self.processCompletedOverlappedCalls();

        // Check for timer expiration
        if (self.process_scheduler.checkTimer()) |sem| {
            self.signalSemaphore(sem);
        }

        // Note: Full process switching is not implemented in this stub
        // For now, just return false indicating no process ran
        return false;
    }

    /// Submit an overlapped FFI call that runs on a native thread
    /// The calling Smalltalk process will be suspended until completion
    pub fn submitOverlappedCall(
        self: *Interpreter,
        func_ptr: *const anyopaque,
        args: []const Value,
        completion_semaphore: Value,
    ) !*overlapped.OverlappedCall {
        // Get the current process (may be null if no process scheduler active)
        const calling_process = self.process_scheduler.active_process;

        // If we have an active process, suspend it
        if (calling_process) |proc| {
            self.saveContextToProcess(proc);
            proc.state = .suspended;
        }

        // Submit the call to the overlapped pool
        return try self.overlapped_pool.submitCall(
            func_ptr,
            args,
            completion_semaphore,
            calling_process,
        );
    }

    /// Set transcript output callback (for TUI mode)
    pub fn setTranscriptCallback(self: *Interpreter, callback: ?*const fn ([]const u8) void) void {
        self.transcript_callback = callback;
    }

    /// Write to transcript (uses callback if set, otherwise stdout)
    pub fn writeTranscript(self: *Interpreter, text: []const u8) void {
        if (self.transcript_callback) |cb| {
            cb(text);
        } else {
            const stdout = std.fs.File.stdout();
            _ = stdout.write(text) catch {};
        }
    }

    /// Push an exception handler onto the handler stack
    pub fn pushExceptionHandler(self: *Interpreter, exception_class: Value, handler_block: Value) InterpreterError!void {
        if (self.handler_ptr >= self.exception_handlers.len) {
            return InterpreterError.StackOverflow;
        }
        self.exception_handlers[self.handler_ptr] = .{
            .exception_class = exception_class,
            .handler_block = handler_block,
            .context_ptr = self.context_ptr,
            .sp = self.sp,
            .temp_base = self.temp_base,
            .method = self.method,
            .ip = self.ip,
            .receiver = self.receiver,
        };
        self.handler_ptr += 1;
    }

    /// Pop an exception handler from the handler stack
    pub fn popExceptionHandler(self: *Interpreter) void {
        if (self.handler_ptr > 0) {
            self.handler_ptr -= 1;
        }
    }

    /// Check if an exception class matches a handler's exception class
    /// Returns true if exception_class is the same as or a subclass of handler_class
    pub fn exceptionMatches(_: *Interpreter, exception_class: Value, handler_class: Value) bool {
        // Check exact match first
        if (exception_class.eql(handler_class)) {
            return true;
        }

        // Check if exception_class is a subclass of handler_class
        var current = exception_class;
        while (current.isObject()) {
            const current_obj = current.asObject();
            const superclass = current_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, current_obj.header.size);
            if (superclass.eql(handler_class)) {
                return true;
            }
            if (superclass.isNil()) {
                break;
            }
            current = superclass;
        }

        return false;
    }

    /// Find a handler for the given exception and return its index, or null if not found
    pub fn findExceptionHandler(self: *Interpreter, exception: Value) ?usize {
        // Get the exception's class
        const exception_class = self.heap.classOf(exception);

        // Search handlers from top to bottom (most recent first)
        var i: usize = self.handler_ptr;
        while (i > 0) {
            i -= 1;
            const handler = self.exception_handlers[i];
            if (self.exceptionMatches(exception_class, handler.exception_class)) {
                return i;
            }
        }
        return null;
    }

    /// Execute a compiled method and return the result
    pub fn execute(self: *Interpreter, method: *CompiledMethod, recv: Value, args: []const Value) InterpreterError!Value {
        // Initialize common selectors for fast arithmetic dispatch (once)
        if (self.selector_plus.isNil()) {
            self.initCommonSelectors();
        }

        // Ensure we have a main process if none exists
        // This represents the "main thread" (REPL, file-in, etc.)
        if (self.process_scheduler.active_process == null) {
            // Create a dummy Process object for the main thread
            const process_obj = self.heap.allocateObject(Heap.CLASS_OBJECT, 7, .normal) catch {
                // If allocation fails, continue without a main process
                return self.executeWithoutMainProcess(method, recv, args);
            };
            // Initialize fields to nil/defaults
            var field_idx: usize = 0;
            while (field_idx < 7) : (field_idx += 1) {
                process_obj.setField(field_idx, Value.nil, 7);
            }
            // Set priority to 5 (user scheduling priority)
            process_obj.setField(scheduler.ProcessFields.priority, Value.fromSmallInt(5), 7);

            // Create the VM-side process
            const main_process = self.process_scheduler.createProcess(Value.fromObject(process_obj), 5) catch {
                return self.executeWithoutMainProcess(method, recv, args);
            };
            main_process.state = .running;
            self.process_scheduler.active_process = main_process;
        }

        return self.executeWithoutMainProcess(method, recv, args);
    }

    /// Internal execute without main process setup (called after main process is ensured)
    fn executeWithoutMainProcess(self: *Interpreter, method: *CompiledMethod, recv: Value, args: []const Value) InterpreterError!Value {
        // Set up initial context
        self.method = method;
        self.ip = 0;
        self.receiver = recv;
        self.temp_base = self.sp;
        self.outer_temp_base = self.temp_base;

        // Push receiver
        try self.push(recv);

        // Push arguments
        for (args) |arg| {
            try self.push(arg);
        }

        // Allocate space for temporaries
        const num_temps = method.header.num_temps - method.header.num_args;
        var i: usize = 0;
        while (i < num_temps) : (i += 1) {
            try self.push(Value.nil);
        }

        // Execute primitive if present
        if (method.header.primitive_index != 0) {
            if (primitives.executePrimitive(self, method.header.primitive_index)) |result| {
                // If primitive succeeded, return its result
                return result;
            } else |err| {
                // ContinueExecution means primitive set up new context - continue to interpretLoop
                if (err == InterpreterError.ContinueExecution) {
                    // Fall through to interpretLoop
                } else if (err != InterpreterError.PrimitiveFailed) {
                    return err;
                }
                // Fall through to bytecode execution on PrimitiveFailed
            }
        }

        // Main interpretation loop
        return self.interpretLoop() catch |err| {
            // Non-local return from block - result is on stack, return it as success
            if (err == InterpreterError.BlockNonLocalReturn) {
                return try self.pop();
            }
            if (err == InterpreterError.MessageNotUnderstood and std.mem.eql(u8, self.last_mnu_selector, "<?>")) {
                self.last_mnu_selector = self.last_send_selector;
                self.last_mnu_method = self.currentMethodSource();
            }
            if (err == InterpreterError.MessageNotUnderstood) {
                const recv_class = self.heap.classOf(self.last_mnu_receiver);
                var recv_name: []const u8 = "<?>";
                if (recv_class.isObject()) {
                    const rc_obj = recv_class.asObject();
                    const name_val = rc_obj.getField(Heap.CLASS_FIELD_NAME, rc_obj.header.size);
                    if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                        recv_name = name_val.asObject().bytes(name_val.asObject().header.size);
                    }
                } else if (self.last_mnu_receiver.isNil()) {
                    recv_name = "nil";
                } else if (self.last_mnu_receiver.isSmallInt()) {
                    recv_name = "SmallInteger";
                }
                if (DEBUG_VERBOSE) std.debug.print("DEBUG MNU catch: last_send={s} method={s} recv={s}\n", .{
                    self.last_send_selector,
                    self.currentMethodSource(),
                    recv_name,
                });
            }
            return err;
        };
    }

    pub fn interpretLoop(self: *Interpreter) InterpreterError!Value {
        while (true) {
            // Check debugger before each bytecode
            if (debugger.debugEnabled and debugger.shouldBreak()) {
                debugger.enterDebugger();
            }

            // Periodic preemption check (every 1000 instructions)
            self.instruction_count +%= 1;
            if (self.instruction_count % 1000 == 0) {
                // Process completed overlapped FFI calls
                self.processCompletedOverlappedCalls();

                // Check if time slice expired and yield if needed
                if (self.yieldToNextProcess()) {
                    // Process switch occurred - continue with new context
                    continue;
                }
            }

            const byte = self.fetchByte();

            // Temporary debug tracing for MainTestCase suite builders
            if (DEBUG_VERBOSE and self.method.header.flags.has_source) {
                const lits = self.method.getLiterals();
                if (lits.len > 0) {
                    const last = lits[lits.len - 1];
                    if (last.isObject() and last.asObject().header.class_index == Heap.CLASS_STRING) {
                        const src = last.asObject().bytes(last.asObject().header.size);
                        if (std.mem.startsWith(u8, src, "buildSuiteFrom") or std.mem.startsWith(u8, src, "buildSuite") or std.mem.startsWith(u8, src, "allTestSelectors") or std.mem.startsWith(u8, src, "tests")) {
                            std.debug.print("TRACE2 ip={} byte=0x{x}\n", .{ self.ip - 1, byte });
                            if (byte == @intFromEnum(Opcode.send)) {
                                const lit_idx = self.fetchByte();
                                const arg_count = self.fetchByte();
                                var lit_info: []const u8 = "<?>"; 
                                if (lit_idx < lits.len) {
                                    const l = lits[lit_idx];
                                    if (l.isObject()) {
                                        const lobj = l.asObject();
                                        if (lobj.header.class_index == Heap.CLASS_SYMBOL or lobj.header.class_index == Heap.CLASS_STRING) {
                                            lit_info = lobj.bytes(lobj.header.size);
                                        }
                                    } else if (l.isSmallInt()) {
                                        lit_info = "SmallInt";
                                    }
                                }
                                std.debug.print("TRACE_SEND ip={} lit={} ({s}) args={} sp={}\n", .{ self.ip - 3, lit_idx, lit_info, arg_count, self.sp });
                                // rewind ip to before the fetches
                                self.ip -= 2;
                            } else {
                                std.debug.print("TRACE ip={} opcode=0x{x}\n", .{ self.ip - 1, byte });
                            }
                        }
                    }
                }
            }

            // Handle short-form opcodes first
            if (Opcode.isPushReceiverVariable(byte)) {
                const index = Opcode.getEmbeddedIndex(byte);
                try self.pushReceiverVariable(index);
                continue;
            }
            if (Opcode.isPushTemporary(byte)) {
                const index = Opcode.getEmbeddedIndex(byte);
                try self.pushTemporary(index);
                continue;
            }
            if (Opcode.isStoreReceiverVariable(byte)) {
                const index = byte - 0x40;
                if (DEBUG_VERBOSE) std.debug.print("DEBUG storeRecVar before idx={} ip={} sp={}\n", .{index, self.ip - 1, self.sp});
                try self.storeReceiverVariable(index);
                if (DEBUG_VERBOSE) std.debug.print("DEBUG storeRecVar after\n", .{});
                continue;
            }
            if (Opcode.isStoreTemporary(byte)) {
                const index = byte - 0x50;
                try self.storeTemporary(index);
                continue;
            }
            if (Opcode.isPopStoreReceiverVariable(byte)) {
                const index = byte - 0x60;
                try self.storeReceiverVariable(index);
                _ = try self.pop();
                continue;
            }
            if (Opcode.isPopStoreTemporary(byte)) {
                const index = byte - 0x70;
                try self.storeTemporary(index);
                _ = try self.pop();
                continue;
            }
            if (Opcode.isShortJump(byte)) {
                const offset = Opcode.getShortJumpOffset(byte);
                self.ip += offset;
                continue;
            }

            const opcode: Opcode = @enumFromInt(byte);

            switch (opcode) {
                // Push operations
                .push_literal => {
                    const index = self.fetchByte();
                    const literals = self.method.getLiterals();
                    const lit_val = literals[index];
                    try self.push(lit_val);
                },
                .push_literal_variable => {
                    const index = self.fetchByte();
                    const literals = self.method.getLiterals();
                    const literal = literals[index];

                    // The literal is a symbol - look it up as class variable or global
                    if (literal.isObject()) {
                        const lit_obj = literal.asObject();
                        if (lit_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            // It's a symbol - first try class variables, then globals
                            const name_bytes = lit_obj.bytes(lit_obj.header.size);

                            // Try to find as class variable in receiver's class hierarchy (only if receiver is not nil)
                            var found_val: ?Value = null;
                            if (!self.receiver.isNil()) {
                                found_val = self.lookupClassVariable(name_bytes);
                            }
                            
                            // If not found as class variable, try globals
                            if (found_val == null) {
                                found_val = self.heap.getGlobal(name_bytes);
                            }
                            
                            if (found_val) |val| {
                                try self.push(val);
                            } else {
                                // Not found
                                try self.push(Value.nil);
                            }
                        } else if (lit_obj.header.class_index == Heap.CLASS_ARRAY and lit_obj.header.size == 2) {
                            // It's an Association (2-element array): value is second field
                            const val = lit_obj.getField(1, 2);
                            try self.push(val);
                        } else {
                            try self.push(Value.nil);
                        }
                    } else {
                        try self.push(Value.nil);
                    }
                },
                .push_receiver => try self.push(self.receiver),
                .push_nil => try self.push(Value.nil),
                .push_true => try self.push(Value.@"true"),
                .push_false => try self.push(Value.@"false"),
                .push_context => {
                    // thisContext - for now push nil since we don't have full context objects
                    // This allows exception signaling code to run without crashing
                    try self.push(Value.nil);
                },
                .push_integer => {
                    const n: i8 = @bitCast(self.fetchByte());
                    try self.push(Value.fromSmallInt(n));
                },
                .push_integer_16 => {
                    const hi: u16 = self.fetchByte();
                    const lo: u16 = self.fetchByte();
                    const n: i16 = @bitCast((hi << 8) | lo);
                    try self.push(Value.fromSmallInt(n));
                },
                .push_receiver_variable => {
                    const index = self.fetchByte();
                    try self.pushReceiverVariable(index);
                },
                .push_temporary => {
                    const index = self.fetchByte();
                    try self.pushTemporary(index);
                },

                // Sends
                .send => {
                    const selector_index = self.fetchByte();
                    const num_args = self.fetchByte();
                    self.sendMessage(selector_index, num_args, false, self.ip - 3) catch |err| {
                        if (err == InterpreterError.BlockNonLocalReturn) {
                            // Non-local return from a block - check if the CURRENT method is the target
                            // IMPORTANT: Check temp_base BEFORE restoring caller's context
                            const current_temp_base = self.temp_base;
                            if (DEBUG_VERBOSE) std.debug.print("DEBUG .send NLR check: temp_base={} target={}\n", .{ current_temp_base, self.non_local_return_target });

                            if (current_temp_base == self.non_local_return_target) {
                                // This method is the target - get result and return
                                const nlr_result = self.pop() catch Value.nil;
                                if (DEBUG_VERBOSE) std.debug.print("DEBUG .send NLR intercepted: temp_base={} target={}\n", .{ current_temp_base, self.non_local_return_target });

                                // Unwind primitive_block_bases stack to match current context level
                                while (self.primitive_block_depth > 0 and
                                    self.primitive_block_bases[self.primitive_block_depth - 1] > self.context_ptr)
                                {
                                    self.primitive_block_depth -= 1;
                                }

                                if (try self.returnFromMethod(nlr_result)) |final_result| {
                                    return final_result;
                                }
                                // Continue execution in caller (after return)
                            } else {
                                // Not the target - restore caller's context and propagate
                                const nlr_result = self.pop() catch Value.nil;
                                if (self.context_ptr > 0) {
                                    self.context_ptr -= 1;
                                    const ctx = self.contexts[self.context_ptr];
                                    self.method = ctx.method;
                                    self.method_class = ctx.method_class;
                                    self.ip = ctx.ip;
                                    self.receiver = ctx.receiver;
                                    self.sp = ctx.temp_base;
                                    self.temp_base = ctx.temp_base;
                                    self.outer_temp_base = ctx.outer_temp_base;
                                    self.home_temp_base = ctx.home_temp_base;
                                    self.heap_context = ctx.heap_context;
                                    self.home_heap_context = ctx.home_heap_context;
                                }
                                try self.push(nlr_result);
                                return err;
                            }
                        } else {
                            return err;
                        }
                    };
                },
                .super_send => {
                    const selector_index = self.fetchByte();
                    const num_args = self.fetchByte();
                    self.sendMessage(selector_index, num_args, true, self.ip - 3) catch |err| {
                        if (err == InterpreterError.BlockNonLocalReturn) {
                            // Non-local return from a block - check if we're the target
                            if (self.temp_base == self.non_local_return_target) {
                                // This method is the target - return the result normally
                                const nlr_result = self.pop() catch Value.nil;
                                if (DEBUG_VERBOSE) std.debug.print("DEBUG .super_send NLR intercepted: temp_base={} target={}\n", .{ self.temp_base, self.non_local_return_target });

                                // Unwind primitive_block_bases stack to match current context level
                                while (self.primitive_block_depth > 0 and
                                    self.primitive_block_bases[self.primitive_block_depth - 1] > self.context_ptr)
                                {
                                    self.primitive_block_depth -= 1;
                                }

                                if (try self.returnFromMethod(nlr_result)) |final_result| {
                                    return final_result;
                                }
                                // Continue execution in caller
                            } else {
                                // Not the target - propagate
                                return err;
                            }
                        } else {
                            return err;
                        }
                    };
                },

                // Optimized sends
                .send_plus => try self.sendSpecialBinary(.add, "+"),
                .send_minus => try self.sendSpecialBinary(.subtract, "-"),
                .send_times => try self.sendSpecialBinary(.multiply, "*"),
                .send_divide => try self.sendSpecialBinary(.divide, "/"),
                .send_less_than => try self.sendSpecialBinary(.less_than, "<"),
                .send_greater_than => try self.sendSpecialBinary(.greater_than, ">"),
                .send_less_or_equal => try self.sendSpecialBinary(.less_or_equal, "<="),
                .send_greater_or_equal => try self.sendSpecialBinary(.greater_or_equal, ">="),
                .send_equal => try self.sendSpecialBinary(.equal, "="),
                .send_not_equal => try self.sendSpecialBinary(.not_equal, "~="),
                .send_identical => try self.sendIdentical(),
                .send_not_identical => try self.sendNotIdentical(),
                .send_class => {
                    const recv = try self.pop();
                    const class = self.heap.classOf(recv);
                    try self.push(class);
                },
                .send_size => {
                    // Always use method lookup for size - many classes override it
                    // (e.g., OrderedCollection returns lastIndex - firstIndex + 1, not basicSize)
                    try self.sendUnary("size");
                },

                // Returns
                .return_receiver => {
                    const result = self.receiver;
                    if (try self.returnFromMethod(result)) |final_result| {
                        return final_result;
                    }
                },
                .return_true => {
                    if (try self.returnFromMethod(Value.@"true")) |final_result| {
                        return final_result;
                    }
                },
                .return_false => {
                    if (try self.returnFromMethod(Value.@"false")) |final_result| {
                        return final_result;
                    }
                },
                .return_nil => {
                    if (try self.returnFromMethod(Value.nil)) |final_result| {
                        return final_result;
                    }
                },
                .return_top => {
                    const result = try self.pop();
                    // If we're in a primitive-controlled block AND at the block's context level,
                    // return directly (the primitive will restore state, don't pop contexts).
                    // If we're in a nested method call (context_ptr > top of primitive_block_bases),
                    // use normal return to pop contexts properly.
                    if (self.primitive_block_depth > 0 and
                        self.context_ptr == self.primitive_block_bases[self.primitive_block_depth - 1])
                    {
                        return result;
                    }
                    if (try self.returnFromMethod(result)) |final_result| {
                        return final_result;
                    }
                },
                .block_return => {
                    // Non-local return - return from enclosing method's home context
                    const result = try self.pop();
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG block_return primitive_block_depth={} home_temp_base={} result_is_obj={}\n", .{ self.primitive_block_depth, self.home_temp_base, result.isObject() });

                    // If we're inside a primitive block execution, signal non-local return
                    // so that the primitive can unwind properly
                    if (self.primitive_block_depth > 0) {
                        // Store the target temp_base so unwinding code knows where to stop
                        // home_temp_base points to the home method context
                        self.non_local_return_target = self.home_temp_base;
                        // Push result back so primitive can retrieve it
                        try self.push(result);
                        if (DEBUG_VERBOSE) std.debug.print("DEBUG block_return signaling BlockNonLocalReturn target={}\n", .{self.non_local_return_target});
                        return InterpreterError.BlockNonLocalReturn;
                    }

                    // Otherwise, just return from the current method normally
                    if (try self.returnFromMethod(result)) |final_result| {
                        return final_result;
                    }
                },

                // Jumps
                .jump => {
                    const offset = self.fetchSignedShort();
                    self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                },
                .jump_if_true => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (val.isTrue()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },
                .jump_if_false => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (val.isFalse()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },
                .jump_if_nil => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (val.isNil()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },
                .jump_if_not_nil => {
                    const offset = self.fetchSignedShort();
                    const val = try self.pop();
                    if (!val.isNil()) {
                        self.ip = @intCast(@as(isize, @intCast(self.ip)) + offset);
                    }
                },

                // Stack operations
                .pop => {
                    _ = try self.pop();
                },
                .dup => {
                    const val = self.peek();
                    try self.push(val);
                },

                // Primitive
                .primitive => {
                    const prim_hi: u16 = self.fetchByte();
                    const prim_lo: u16 = self.fetchByte();
                    const prim_index = (prim_hi << 8) | prim_lo;
                    if (primitives.executePrimitive(self, prim_index)) |result| {
                        try self.push(result);
                    } else |err| {
                        // ContinueExecution means a process switch happened - just continue with new context
                        if (err != InterpreterError.ContinueExecution) {
                            return err;
                        }
                        // Don't push anything, just continue the loop with the new process's context
                    }
                },

                .nop => {},

                .thread => {
                    // Thread execution - currently a no-op placeholder
                    // This could be used for concurrent execution or continuations
                    // For now, just continue to next instruction
                },

                .push_closure => {
                    // Create a BlockClosure object
                    // Format: push_closure <num_args> <num_temps> <size_hi> <size_lo> <bytecodes...>
                    const block_num_args = self.fetchByte();
                    const block_num_temps = self.fetchByte();
                    const size_hi: u16 = self.fetchByte();
                    const size_lo: u16 = self.fetchByte();
                    const bytecode_size: usize = @intCast((size_hi << 8) | size_lo);

                    if (DEBUG_VERBOSE) {
                        const prim_ctx_base = if (self.primitive_block_depth > 0) self.primitive_block_bases[self.primitive_block_depth - 1] else 0;
                        std.debug.print("DEBUG push_closure: temp_base={} home_temp_base={} ctx_ptr={} prim_ctx_base={} prim_depth={} block_temps={}\n", .{ self.temp_base, self.home_temp_base, self.context_ptr, prim_ctx_base, self.primitive_block_depth, block_num_temps });
                    }

                    // Lazily create heap context for closure variable capture
                    // This allows captured variables to be shared across processes
                    if (self.heap_context.isNil()) {
                        // Calculate number of temps to capture from current frame
                        const method_num_temps = self.method.header.num_temps;
                        const method_num_args = self.method.header.num_args;
                        const num_to_capture = method_num_args + method_num_temps;

                        // Create heap context and copy temps from stack
                        const heap_ctx = self.createHeapContext(num_to_capture) catch {
                            return InterpreterError.OutOfMemory;
                        };
                        self.heap_context = Value.fromObject(heap_ctx);

                        // At method level, home_heap_context == heap_context
                        if (self.home_heap_context.isNil()) {
                            self.home_heap_context = self.heap_context;
                        }
                    }

                    // Create BlockClosure object
                    // Fields: outerContext, startPC, numArgs, method, receiver, homeContext, numTemps
                    const closure = self.heap.allocateObject(Heap.CLASS_BLOCK_CLOSURE, Heap.BLOCK_NUM_FIELDS, .normal) catch {
                        return InterpreterError.OutOfMemory;
                    };

                    // Store heap context references for cross-process variable access
                    // OUTER_CONTEXT: immediate enclosing context (for level 1 access)
                    // HOME_CONTEXT: home method's context (for level >= 2 access and non-local returns)
                    closure.setField(Heap.BLOCK_FIELD_OUTER_CONTEXT, self.heap_context, Heap.BLOCK_NUM_FIELDS);
                    closure.setField(Heap.BLOCK_FIELD_START_PC, Value.fromSmallInt(@intCast(self.ip)), Heap.BLOCK_NUM_FIELDS);
                    closure.setField(Heap.BLOCK_FIELD_NUM_ARGS, Value.fromSmallInt(block_num_args), Heap.BLOCK_NUM_FIELDS);
                    closure.setField(Heap.BLOCK_FIELD_METHOD, Value.fromObject(@ptrCast(@alignCast(self.method))), Heap.BLOCK_NUM_FIELDS);
                    closure.setField(Heap.BLOCK_FIELD_RECEIVER, self.receiver, Heap.BLOCK_NUM_FIELDS);
                    closure.setField(Heap.BLOCK_FIELD_HOME_CONTEXT, self.home_heap_context, Heap.BLOCK_NUM_FIELDS);
                    closure.setField(Heap.BLOCK_FIELD_NUM_TEMPS, Value.fromSmallInt(block_num_temps), Heap.BLOCK_NUM_FIELDS);

                    try self.push(Value.fromObject(closure));

                    // Skip over the block's bytecodes
                    self.ip += bytecode_size;
                },

                .push_outer_temp => {
                    // Format: push_outer_temp <level> <index>
                    // Level 1 = immediate outer scope, Level 2 = method scope (home)
                    const level = self.fetchByte();
                    const index = self.fetchByte();

                    // Try heap context first (for cross-process variable access)
                    const heap_ctx = if (level >= 2) self.home_heap_context else self.heap_context;
                    if (!heap_ctx.isNil()) {
                        // Read from heap-allocated context
                        const ctx_obj = heap_ctx.asObject();
                        const outer_val = getHeapContextTemp(ctx_obj, index);
                        try self.push(outer_val);
                    } else {
                        // Fall back to stack-based access
                        const base = if (level >= 2) self.home_temp_base else self.outer_temp_base;
                        const slot = base + 1 + index;
                        const outer_val = self.stack[slot];
                        try self.push(outer_val);
                    }
                },

                .store_outer_temp => {
                    // Format: store_outer_temp <level> <index>
                    // Level 1 = immediate outer scope, Level 2 = method scope (home)
                    const level = self.fetchByte();
                    const index = self.fetchByte();

                    // Get value but leave on stack (store doesn't pop in Smalltalk)
                    const val = self.peek();

                    // Try heap context first (for cross-process variable access)
                    const heap_ctx = if (level >= 2) self.home_heap_context else self.heap_context;
                    if (!heap_ctx.isNil()) {
                        // Write to heap-allocated context
                        const ctx_obj = heap_ctx.asObject();
                        setHeapContextTemp(ctx_obj, index, val);
                    } else {
                        // Fall back to stack-based access
                        const base = if (level >= 2) self.home_temp_base else self.outer_temp_base;
                        const slot = base + 1 + index;
                        self.stack[slot] = val;
                    }
                },

                .make_array => {
                    // Format: make_array <count>
                    // Pop count values from stack, create array, push array
                    const count = self.fetchByte();

                    // Allocate array
                    const array = self.heap.allocateObject(Heap.CLASS_ARRAY, count, .variable) catch {
                        return InterpreterError.OutOfMemory;
                    };

                    // Pop values in reverse order (first pushed = first element)
                    // Values are on stack in order: [elem0, elem1, ..., elemN-1] with elemN-1 on top
                    var i: usize = count;
                    while (i > 0) {
                        i -= 1;
                        const val = try self.pop();
                        array.setField(i, val, count);
                    }

                    try self.push(Value.fromObject(array));
                },

                .extended_store => {
                    // Format: extended_store <type> <index>
                    // type: 0=temp, 1=inst var, 2=class var (literal index)
                    const store_type = self.fetchByte();
                    const index = self.fetchByte();
                    const val = self.peek(); // Store leaves value on stack

                    switch (store_type) {
                        0 => {
                            // Store to temporary
                            // temp_base points to receiver slot, temps start at temp_base + 1
                            self.stack[self.temp_base + 1 + index] = val;
                            // Also update heap context if present (for cross-process/nested block access)
                            if (!self.heap_context.isNil()) {
                                setHeapContextTemp(self.heap_context.asObject(), index, val);
                            }
                        },
                        1 => {
                            // Store to instance variable
                            if (self.receiver.isObject()) {
                                const recv_obj = self.receiver.asObject();
                                const class = self.heap.classOf(self.receiver);
                                var inst_size: usize = 0;
                                if (class.isObject()) {
                                    const cls_obj = class.asObject();
                                    const format_val = cls_obj.getField(Heap.CLASS_FIELD_FORMAT, cls_obj.header.size);
                                    if (format_val.isSmallInt()) {
                                        // Extract inst var count from format (low byte)
                                        inst_size = @intCast(format_val.asSmallInt() & 0xFF);
                                    }
                                }
                                recv_obj.setField(index, val, inst_size);
                            }
                        },
                        2 => {
                            // Store to class variable - index is literal index containing name
                            const literals = self.method.getLiterals();
                            const literal = literals[index];
                            if (literal.isObject()) {
                                const lit_obj = literal.asObject();
                                if (lit_obj.header.class_index == Heap.CLASS_SYMBOL) {
                                    const name_bytes = lit_obj.bytes(lit_obj.header.size);
                                    // Try class variables first, then globals
                                    if (!self.storeClassVariable(name_bytes, val)) {
                                        // Not found in class variables, store as global
                                        try self.heap.setGlobal(name_bytes, val);
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                },

                else => return InterpreterError.InvalidBytecode,
            }
        }
    }

    // Stack operations
    pub fn push(self: *Interpreter, value: Value) InterpreterError!void {
        if (self.sp >= self.stack.len) {
            return InterpreterError.StackOverflow;
        }
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    pub fn pop(self: *Interpreter) InterpreterError!Value {
        if (self.sp == 0) {
            return InterpreterError.StackUnderflow;
        }
        self.sp -= 1;
        return self.stack[self.sp];
    }

    // Unchecked stack operations for hot paths (caller must ensure bounds)
    inline fn pushUnchecked(self: *Interpreter, value: Value) void {
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    inline fn popUnchecked(self: *Interpreter) Value {
        self.sp -= 1;
        return self.stack[self.sp];
    }

    inline fn peekUnchecked(self: *Interpreter) Value {
        return self.stack[self.sp - 1];
    }

    // Replace top of stack without push/pop (for binary operations)
    inline fn replaceTop(self: *Interpreter, value: Value) void {
        self.stack[self.sp - 1] = value;
    }

    // Pop N values and push result (for N-ary operations returning 1 value)
    inline fn popNPush1(self: *Interpreter, n: usize, value: Value) void {
        self.sp -= n;
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    pub fn peek(self: *Interpreter) Value {
        if (self.sp == 0) return Value.nil;
        return self.stack[self.sp - 1];
    }

    pub fn peekN(self: *Interpreter, n: usize) Value {
        if (self.sp <= n) return Value.nil;
        return self.stack[self.sp - 1 - n];
    }

    pub fn currentMethodSource(self: *Interpreter) []const u8 {
        const lits = self.method.getLiterals();
        if (lits.len > 0) {
            const last = lits[lits.len - 1];
            if (last.isObject()) {
                const obj = last.asObject();
                if (obj.header.class_index == Heap.CLASS_STRING) {
                    return obj.bytes(obj.header.size);
                }
            }
        }
        return "<?>"; // Unknown
    }

    fn fetchByte(self: *Interpreter) u8 {
        const bytecodes_slice = self.method.getBytecodes();
        if (self.ip >= bytecodes_slice.len) return 0;
        const byte = bytecodes_slice[self.ip];
        self.ip += 1;
        return byte;
    }

    fn fetchSignedShort(self: *Interpreter) i16 {
        const hi: u16 = self.fetchByte();
        const lo: u16 = self.fetchByte();
        return @bitCast((hi << 8) | lo);
    }

    fn pushReceiverVariable(self: *Interpreter, index: u8) InterpreterError!void {
        if (self.receiver.isObject()) {
            const obj = self.receiver.asObject();
            // Get number of fields from class format
            var num_fields: usize = 16; // Default fallback
            const class = self.heap.classOf(self.receiver);
            if (class.isObject()) {
                const cls_obj = class.asObject();
                const format_val = cls_obj.getField(Heap.CLASS_FIELD_FORMAT, cls_obj.header.size);
                if (format_val.isSmallInt()) {
                    const spec = Heap.decodeInstanceSpec(format_val.asSmallInt());
                    num_fields = spec.inst_size;
                    if (num_fields == 0) num_fields = obj.header.size; // fallback to actual object size
                }
            }
            // Debug: trace some receiver variable accesses on class objects
            if (DEBUG_VERBOSE and obj.header.class_index == Heap.CLASS_CLASS and debug_receiver_idx2_count < 20) {
                debug_receiver_idx2_count += 1;
                var name_buf: []const u8 = "<?>"; // Default if lookup fails
                if (num_fields > Heap.CLASS_FIELD_NAME) {
                    const name_val = obj.getField(Heap.CLASS_FIELD_NAME, num_fields);
                    if (name_val.isObject()) {
                        const name_obj = name_val.asObject();
                        if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            name_buf = name_obj.bytes(name_obj.header.size);
                        }
                    }
                }
                std.debug.print("DEBUG pushRecVar idx={}, recv name={s}, obj.class_idx={}, num_fields={}, class.class_idx={}\n", .{
                    index,
                    name_buf,
                    obj.header.class_index,
                    num_fields,
                    if (class.isObject()) class.asObject().header.class_index else 0xFFFF_FFFF,
                });
                const val_dbg = if (index < num_fields) obj.getField(index, num_fields) else Value.nil;
                std.debug.print("  field[{}] type=", .{index});
                if (val_dbg.isSmallInt()) {
                    std.debug.print("SmallInt({})\n", .{val_dbg.asSmallInt()});
                } else if (val_dbg.isObject()) {
                    std.debug.print("Object(class_idx={})\n", .{val_dbg.asObject().header.class_index});
                } else if (val_dbg.isNil()) {
                    std.debug.print("nil\n", .{});
                } else {
                    std.debug.print("other\n", .{});
                }
            }
            if (index < num_fields) {
                const val = obj.getField(index, num_fields);
                try self.push(val);
            } else {
                try self.push(Value.nil);
            }
        } else {
            try self.push(Value.nil);
        }
    }

    fn pushTemporary(self: *Interpreter, index: u8) InterpreterError!void {
        // Read from heap context if present (this allows inner blocks to update outer temps
        // via store_outer_temp and have the outer block see the updated value)
        if (!self.heap_context.isNil()) {
            const val = getHeapContextTemp(self.heap_context.asObject(), index);
            try self.push(val);
            return;
        }
        // Fall back to stack access
        // temp_base points to receiver slot, temps start at temp_base + 1
        const stack_index = self.temp_base + 1 + index;
        if (stack_index < self.sp) {
            try self.push(self.stack[stack_index]);
        } else {
            try self.push(Value.nil);
        }
    }

    fn storeReceiverVariable(self: *Interpreter, index: u8) InterpreterError!void {
        const val = self.peek();
        if (self.receiver.isObject()) {
            const obj = self.receiver.asObject();
            // Get number of fields from class format
            var num_fields: usize = 16; // Default
            const class = self.heap.classOf(self.receiver);
            if (class.isObject()) {
                const cls_obj = class.asObject();
                const format_val = cls_obj.getField(Heap.CLASS_FIELD_FORMAT, cls_obj.header.size);
                if (format_val.isSmallInt()) {
                    const spec = Heap.decodeInstanceSpec(format_val.asSmallInt());
                    num_fields = spec.inst_size;
                    if (num_fields == 0) num_fields = obj.header.size;
                }
            }
            if (index < num_fields) {
                obj.setField(index, val, num_fields);
            }
        }
    }

    fn storeTemporary(self: *Interpreter, index: u8) InterpreterError!void {
        const val = self.peek();
        // temp_base points to receiver slot, temps start at temp_base + 1
        const stack_index = self.temp_base + 1 + index;
        if (stack_index < self.sp) {
            self.stack[stack_index] = val;
        }
        // Also update heap context if present (for cross-process/nested block access)
        if (!self.heap_context.isNil()) {
            setHeapContextTemp(self.heap_context.asObject(), index, val);
        }
    }

    pub fn sendMessage(self: *Interpreter, selector_index: u8, num_args: u8, is_super: bool, bytecode_offset: usize) InterpreterError!void {
        // Get selector from literals
        const literals = self.method.getLiterals();
        const selector = literals[selector_index];
        var selector_name: []const u8 = "<?>";
        if (selector.isObject()) {
            const sel_obj = selector.asObject();
            if (sel_obj.header.class_index == Heap.CLASS_SYMBOL) {
                selector_name = sel_obj.bytes(sel_obj.header.size);
            }
        }
        if (selector.isSmallInt()) {
            if (DEBUG_VERBOSE) std.debug.print("DEBUG sendMessage selector smallInt={} literal index={}\n", .{selector.asSmallInt(), selector_index});
        } else {
            if (DEBUG_VERBOSE) std.debug.print("DEBUG sendMessage selector non-object bits=0x{x} literal index={}\n", .{selector.bits, selector_index});
        }
        self.last_send_selector = selector_name;

        // ========================================================================
        // OPTIMIZATION: Leave receiver+args on stack, reference by offset
        // ========================================================================
        // Stack layout: [..., recv, arg0, arg1, ...], sp points past last arg
        // recv_pos = sp - num_args - 1 (position of receiver)
        // args are at recv_pos+1, recv_pos+2, etc.
        const recv_pos = self.sp - @as(usize, num_args) - 1;
        const recv = self.stack[recv_pos];

        // ========================================================================
        // SmallInteger Arithmetic Fast Paths
        // ========================================================================
        // Bypass full method lookup for common integer operations.
        // Only active if selectors have been initialized (checked via selector_plus).
        if (recv.isSmallInt() and num_args == 1 and !self.selector_plus.isNil()) {
            const arg = self.stack[recv_pos + 1]; // args[0]
            if (arg.isSmallInt()) {
                const a = recv.asSmallInt();
                const b = arg.asSmallInt();

                // Binary arithmetic operations with overflow checking
                if (selector.bits == self.selector_plus.bits) {
                    const result = @addWithOverflow(a, b);
                    if (result[1] == 0) {
                        // Write result to recv position, adjust sp
                        self.stack[recv_pos] = Value.fromSmallInt(result[0]);
                        self.sp = recv_pos + 1;
                        return;
                    }
                    // Overflow - fall through to normal send (will use LargeInteger)
                } else if (selector.bits == self.selector_minus.bits) {
                    const result = @subWithOverflow(a, b);
                    if (result[1] == 0) {
                        self.stack[recv_pos] = Value.fromSmallInt(result[0]);
                        self.sp = recv_pos + 1;
                        return;
                    }
                } else if (selector.bits == self.selector_times.bits) {
                    const result = @mulWithOverflow(a, b);
                    if (result[1] == 0) {
                        self.stack[recv_pos] = Value.fromSmallInt(result[0]);
                        self.sp = recv_pos + 1;
                        return;
                    }
                } else if (selector.bits == self.selector_divide.bits) {
                    // Integer division - fall through if b == 0
                    if (b != 0) {
                        self.stack[recv_pos] = Value.fromSmallInt(@divTrunc(a, b));
                        self.sp = recv_pos + 1;
                        return;
                    }
                } else if (selector.bits == self.selector_modulo.bits) {
                    // Modulo - fall through if b == 0
                    if (b != 0) {
                        self.stack[recv_pos] = Value.fromSmallInt(@rem(a, b));
                        self.sp = recv_pos + 1;
                        return;
                    }
                }
                // Comparison operations
                else if (selector.bits == self.selector_less.bits) {
                    self.stack[recv_pos] = if (a < b) Value.@"true" else Value.@"false";
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_greater.bits) {
                    self.stack[recv_pos] = if (a > b) Value.@"true" else Value.@"false";
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_less_equal.bits) {
                    self.stack[recv_pos] = if (a <= b) Value.@"true" else Value.@"false";
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_greater_equal.bits) {
                    self.stack[recv_pos] = if (a >= b) Value.@"true" else Value.@"false";
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_equal.bits) {
                    self.stack[recv_pos] = if (a == b) Value.@"true" else Value.@"false";
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_not_equal.bits) {
                    self.stack[recv_pos] = if (a != b) Value.@"true" else Value.@"false";
                    self.sp = recv_pos + 1;
                    return;
                }
                // Bitwise operations
                else if (selector.bits == self.selector_bitand.bits) {
                    self.stack[recv_pos] = Value.fromSmallInt(a & b);
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_bitor.bits) {
                    self.stack[recv_pos] = Value.fromSmallInt(a | b);
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_bitxor.bits) {
                    self.stack[recv_pos] = Value.fromSmallInt(a ^ b);
                    self.sp = recv_pos + 1;
                    return;
                } else if (selector.bits == self.selector_bitshift.bits) {
                    // bitShift: - positive shifts left, negative shifts right
                    if (b >= 0 and b < 61) {
                        const shift_amt: u6 = @intCast(b);
                        const shifted = a << shift_amt;
                        // Check for overflow
                        if ((shifted >> shift_amt) == a) {
                            self.stack[recv_pos] = Value.fromSmallInt(shifted);
                            self.sp = recv_pos + 1;
                            return;
                        }
                    } else if (b < 0 and b > -61) {
                        const shift_amt: u6 = @intCast(-b);
                        self.stack[recv_pos] = Value.fromSmallInt(a >> shift_amt);
                        self.sp = recv_pos + 1;
                        return;
                    }
                    // Fall through for large shifts
                }
            }
        }

        // ========================================================================
        // Boolean Conditional Fast Paths (using cached selectors - NO string compare!)
        // ========================================================================
        // Only if receiver is boolean and selectors are initialized
        if ((recv.isTrue() or recv.isFalse()) and !self.selector_ifTrue.isNil()) {
            if (selector.bits == self.selector_ifTrueIfFalse.bits and num_args == 2) {
                const chosen_block = if (recv.isTrue()) self.stack[recv_pos + 1] else self.stack[recv_pos + 2];
                // Pop recv+args, push block for primBlockValue
                self.sp = recv_pos;
                try self.push(chosen_block);
                const res = primitives.primBlockValue(self) catch |err| {
                    return err;
                };
                try self.push(res);
                return;
            }
            if (selector.bits == self.selector_ifFalseIfTrue.bits and num_args == 2) {
                const chosen_block = if (recv.isFalse()) self.stack[recv_pos + 1] else self.stack[recv_pos + 2];
                self.sp = recv_pos;
                try self.push(chosen_block);
                const res = primitives.primBlockValue(self) catch |err| {
                    return err;
                };
                try self.push(res);
                return;
            }
            if (selector.bits == self.selector_ifTrue.bits and num_args == 1) {
                if (recv.isTrue()) {
                    const block = self.stack[recv_pos + 1];
                    self.sp = recv_pos;
                    try self.push(block);
                    const res = primitives.primBlockValue(self) catch |err| {
                        return err;
                    };
                    try self.push(res);
                } else {
                    // Pop recv+args, push nil
                    self.sp = recv_pos;
                    try self.push(Value.nil);
                }
                return;
            }
            if (selector.bits == self.selector_ifFalse.bits and num_args == 1) {
                if (recv.isFalse()) {
                    const block = self.stack[recv_pos + 1];
                    self.sp = recv_pos;
                    try self.push(block);
                    const res = primitives.primBlockValue(self) catch |err| {
                        return err;
                    };
                    try self.push(res);
                } else {
                    // Pop recv+args, push nil
                    self.sp = recv_pos;
                    try self.push(Value.nil);
                }
                return;
            }
        }

        // Fast-path: update class instanceSpec flags directly for setSpecialBehavior:to:
        if (selector.isObject()) {
            const sel_obj = selector.asObject();
            if (sel_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const sel_name = sel_obj.bytes(sel_obj.header.size);

                // Fast-path: Behavior>>allSubclasses without needing closure captures
                if (std.mem.eql(u8, sel_name, "allSubclasses") and recv.isObject() and num_args == 0) {
                    const recv_obj = recv.asObject();

                    var stack_list = std.ArrayListUnmanaged(*object.Object){};
                    defer stack_list.deinit(std.heap.page_allocator);
                    var result_list = std.ArrayListUnmanaged(Value){};
                    defer result_list.deinit(std.heap.page_allocator);

                    try stack_list.append(std.heap.page_allocator, recv_obj);
                    while (stack_list.items.len > 0) {
                        const cls = stack_list.items[stack_list.items.len - 1];
                        stack_list.items.len -= 1;

                        for (self.heap.class_table.items) |candidate| {
                            if (!candidate.isObject()) continue;
                            const cand_obj = candidate.asObject();
                            const super_val = cand_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, cand_obj.header.size);
                            if (super_val.eql(Value.fromObject(cls))) {
                                try result_list.append(std.heap.page_allocator, candidate);
                                try stack_list.append(std.heap.page_allocator, cand_obj);
                            }
                        }
                    }

                    const arr_val = try self.heap.allocateArray(result_list.items.len);
                    const arr_obj = arr_val.asObject();
                    const fields = arr_obj.fields(result_list.items.len);
                    for (result_list.items, 0..) |v, fi| {
                        fields[fi] = v;
                    }
                    // Pop recv (num_args=0), push result
                    self.stack[recv_pos] = arr_val;
                    self.sp = recv_pos + 1;
                    return;
                }

                // ProtocolSpec protocol builder expressions - treat as no-ops
                if (std.mem.startsWith(u8, sel_name, "newProtocolNamed:conformsToProtocolNames:") or
                    std.mem.startsWith(u8, sel_name, "newMessagePattern:forProtocolNamed:") or
                    std.mem.eql(u8, sel_name, "protocolDescription:"))
                {
                    // Pop recv+args, push nil
                    self.stack[recv_pos] = Value.nil;
                    self.sp = recv_pos + 1;
                    return;
                }
                if (std.mem.eql(u8, sel_name, "setSpecialBehavior:to:") and recv.isObject() and num_args == 2) {
                    const mask_val = self.stack[recv_pos + 1]; // args[0]
                    const bool_val = self.stack[recv_pos + 2]; // args[1]
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG fastpath setSpecialBehavior args: mask smallInt?={} bool true?={} false?={}\n", .{ mask_val.isSmallInt(), bool_val.isTrue(), bool_val.isFalse() });

                    var mask: i61 = Heap.INSTSPEC_NULLTERM_MASK; // default to NullTerm mask if we can't parse
                    if (mask_val.isSmallInt()) {
                        mask = mask_val.asSmallInt();
                    }

                    const recv_obj = recv.asObject();
                    const current = recv_obj.getField(Heap.CLASS_FIELD_FORMAT, recv_obj.header.size);
                    if (current.isSmallInt()) {
                        var new_spec = current.asSmallInt();
                        if (bool_val.isTrue()) {
                            new_spec |= mask;
                        } else if (bool_val.isFalse()) {
                            new_spec &= ~mask;
                        }
                        recv_obj.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(new_spec), recv_obj.header.size);
                        // Pop recv+args, push result
                        self.stack[recv_pos] = Value.fromSmallInt(new_spec);
                        self.sp = recv_pos + 1;
                        return;
                    }
                    // If we can't handle, fall through to normal send
                }
            }
        }

        if (debug_send_special_count < 12 and selector.isObject()) {
            const sel_obj = selector.asObject();
            if (sel_obj.header.class_index == Heap.CLASS_SYMBOL) {
                const sel_name = sel_obj.bytes(sel_obj.header.size);
                if (std.mem.eql(u8, sel_name, "isBytes") or std.mem.eql(u8, sel_name, "setSpecialBehavior:to:") or std.mem.eql(u8, sel_name, "isPointers")) {
                    debug_send_special_count += 1;
                    var recv_name: []const u8 = "<?>"; // default if lookup fails
                    if (recv.isObject()) {
                        const recv_obj = recv.asObject();
                        const nf = recv_obj.header.size;
                        if (nf > Heap.CLASS_FIELD_NAME) {
                            const name_val = recv_obj.getField(Heap.CLASS_FIELD_NAME, nf);
                            if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                recv_name = name_val.asObject().bytes(name_val.asObject().header.size);
                            }
                        }
                    }
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG send {s} recv={s} num_args={d}\n", .{ sel_name, recv_name, num_args });
                    if (std.mem.eql(u8, sel_name, "setSpecialBehavior:to:") and recv.isObject()) {
                        const obj = recv.asObject();
                        const class_of = self.heap.classOf(recv);
                        if (class_of.isObject()) {
                            const cof_obj = class_of.asObject();
                            const spec_val = cof_obj.getField(Heap.CLASS_FIELD_FORMAT, cof_obj.header.size);
                            if (spec_val.isSmallInt()) {
                                const info = Heap.decodeInstanceSpec(spec_val.asSmallInt());
                                std.debug.print("  (meta spec) size={}, pointers={}, variable={}\n", .{ info.inst_size, info.is_pointers, info.is_variable });
                            }
                        }
                        const spec_val2 = obj.getField(Heap.CLASS_FIELD_FORMAT, obj.header.size);
                        if (spec_val2.isSmallInt()) {
                            const info2 = Heap.decodeInstanceSpec(spec_val2.asSmallInt());
                            std.debug.print("  (recv spec) size={}, pointers={}, variable={}, raw={}\n", .{ info2.inst_size, info2.is_pointers, info2.is_variable, spec_val2.asSmallInt() });
                        }
                    }
                }
            }
        }

        // Get the class to start lookup from
        const receiver_class = self.heap.classOf(recv);

        // ========================================================================
        // Monomorphic Inline Cache (MIC) - per-callsite caching
        // ========================================================================
        // For monomorphic sends (95%+ of sends), we want just:
        //   1 load (receiver class) - already done above
        //   1 compare (vs cached class)
        //   1 conditional jump (to cached method)
        // This avoids the hash computation of the global cache on hot paths.
        const method_ptr = @intFromPtr(self.method);
        const ic_hash = method_ptr ^ @as(usize, bytecode_offset);
        const ic_index = ic_hash & (INLINE_CACHE_SIZE - 1);
        const ic_entry = &self.inline_cache[ic_index];

        // Fast path: check if this exact callsite has a cached lookup
        // Also check cache version to handle method dictionary changes
        if (!is_super and ic_entry.method_ptr == method_ptr and
            ic_entry.bytecode_offset == @as(u16, @intCast(bytecode_offset)) and
            ic_entry.cached_class.bits == receiver_class.bits and
            ic_entry.cached_version == self.cache_version)
        {
            // Inline cache HIT - use cached method directly
            self.inline_cache_hits += 1;
            if (ic_entry.cached_method) |found_method| {
                const method_holder = ic_entry.cached_holder;
                // Execute the cached method (same logic as below)
                if (found_method.header.primitive_index != 0) {
                    if (primitives.executePrimitive(self, found_method.header.primitive_index)) |result| {
                        try self.push(result);
                        return;
                    } else |err| {
                        if (err == InterpreterError.ContinueExecution) return;
                        if (err == InterpreterError.MessageNotUnderstood) {
                            self.last_mnu_selector = selector_name;
                            self.last_mnu_receiver = recv;
                            self.last_mnu_method = self.currentMethodSource();
                            return err;
                        }
                        if (err == InterpreterError.BlockNonLocalReturn) return err;
                        // Primitive failed - restore stack and fall through
                        self.sp = recv_pos;
                        try self.push(self.stack[recv_pos]);
                        var arg_idx: usize = 0;
                        while (arg_idx < num_args) : (arg_idx += 1) {
                            try self.push(self.stack[recv_pos + 1 + arg_idx]);
                        }
                    }
                }
                // Execute method bytecode (inlined from below for speed)
                if (self.context_ptr >= self.contexts.len - 1) {
                    return InterpreterError.StackOverflow;
                }
                self.contexts[self.context_ptr] = .{
                    .method = self.method,
                    .method_class = self.method_class,
                    .ip = self.ip,
                    .receiver = self.receiver,
                    .temp_base = self.temp_base,
                    .outer_temp_base = self.outer_temp_base,
                    .home_temp_base = self.home_temp_base,
                    .heap_context = self.heap_context,
                    .home_heap_context = self.home_heap_context,
                };
                self.context_ptr += 1;
                self.method = found_method;
                self.method_class = method_holder;
                self.ip = 0;
                self.receiver = recv;
                self.temp_base = recv_pos;
                self.outer_temp_base = self.temp_base;
                self.home_temp_base = self.temp_base;
                self.heap_context = Value.nil;
                self.home_heap_context = Value.nil;
                const total_temps = found_method.header.num_temps;
                const local_temps = if (total_temps > num_args) total_temps - @as(u8, @intCast(num_args)) else 0;
                var k: usize = 0;
                while (k < local_temps) : (k += 1) {
                    try self.push(Value.nil);
                }
                return;
            }
        }

        // Inline cache MISS - fall through to global cache lookup
        self.inline_cache_misses += 1;

        const super_lookup_class = if (!self.method_class.isNil()) self.method_class else receiver_class;
        const start_class = if (is_super)
            self.getSuperclass(super_lookup_class)
        else
            self.heap.classOf(recv);
        // Look up method in class hierarchy (uses global method cache)
        if (self.lookupMethodWithHolder(start_class, selector)) |method_lookup| {
            // Update inline cache for non-super sends
            if (!is_super) {
                ic_entry.* = .{
                    .method_ptr = method_ptr,
                    .bytecode_offset = @intCast(bytecode_offset),
                    .cached_class = receiver_class,
                    .cached_method = method_lookup.method,
                    .cached_holder = method_lookup.holder,
                    .cached_version = self.cache_version,
                };
            }
            const found_method = method_lookup.method;
            const method_holder = method_lookup.holder;
            // Execute the found method
            if (found_method.header.primitive_index != 0) {
                // Primitives pop their arguments from the stack, so we need to ensure
                // recv+args are at the top of stack. Since they're already there, the
                // primitive can pop them directly.
                if (primitives.executePrimitive(self, found_method.header.primitive_index)) |result| {
                    // Primitive consumed recv+args via pop, push result
                    try self.push(result);
                    return;
                } else |err| {
                    // ContinueExecution means primitive set up new context - just return to continue main loop
                    if (err == InterpreterError.ContinueExecution) {
                        return;
                    }
                    // Check if this is a fatal error that should propagate
                    if (err == InterpreterError.MessageNotUnderstood) {
                        if (std.mem.eql(u8, self.last_mnu_selector, "<?>")) {
                            self.last_mnu_selector = selector_name;
                            self.last_mnu_receiver = recv;
                            self.last_mnu_method = self.currentMethodSource();
                        }
                        if (DEBUG_VERBOSE) std.debug.print("DEBUG primitive MNU selector={s}\n", .{selector_name});
                        return err;
                    }
                    // Propagate block non-local returns up the call chain
                    if (err == InterpreterError.BlockNonLocalReturn) {
                        return err;
                    }
                    // Primitive failed, fall through to execute bytecode
                    // Most primitives pop all args then fail immediately without pushing,
                    // so the original values are still in the stack array at their positions.
                    // We restore recv+args by reading from those positions.
                    self.sp = recv_pos;  // Reset sp to before recv
                    try self.push(self.stack[recv_pos]);  // Push recv
                    var arg_idx: usize = 0;
                    while (arg_idx < num_args) : (arg_idx += 1) {
                        try self.push(self.stack[recv_pos + 1 + arg_idx]);
                    }
                }
            }

            // ========================================================================
            // JIT Execution Path
            // ========================================================================
            // If JIT is enabled and method is compiled, execute native code
            if (self.jit_enabled) {
                if (self.jit_compiler) |jit_ptr| {
                    if (jit_ptr.getCompiled(found_method)) |compiled| {
                        // Set up receiver for JIT code
                        self.receiver = recv;
                        // Receiver and args are already on stack at recv_pos
                        // JIT code will access them via interpreter's stack

                        // Call the JIT-compiled code
                        const result = compiled.entry(self);

                        // Pop receiver+args from stack
                        self.sp = recv_pos;

                        // Push result
                        try self.push(result);

                        self.jit_compiled_calls += 1;
                        return;
                    }
                }
            }
            self.jit_interpreted_calls += 1;

            // Execute the method bytecode
            // Save current context
            if (self.context_ptr >= self.contexts.len - 1) {
            if (DEBUG_STACK) {
                std.debug.print("STACK OVERFLOW at context_ptr={} method={s}\n", .{self.context_ptr, self.currentMethodSource()});
            }
                return InterpreterError.StackOverflow;
            }

            // Debug: show which method is being saved
            var method_name: []const u8 = "<no source>";
            if (self.method.header.flags.has_source) {
                const lits_dbg = self.method.getLiterals();
                if (lits_dbg.len > 0 and lits_dbg[lits_dbg.len - 1].isObject()) {
                    const src_obj = lits_dbg[lits_dbg.len - 1].asObject();
                    if (src_obj.header.class_index == Heap.CLASS_STRING) {
                        const src_bytes = src_obj.bytes(src_obj.header.size);
                        method_name = src_bytes[0..@min(src_bytes.len, 20)];
                    }
                }
            }
            if (DEBUG_VERBOSE) std.debug.print("DEBUG push context ip={} ctx_ptr={} temp_base={} method={s}\n", .{ self.ip, self.context_ptr, self.temp_base, method_name });
                self.contexts[self.context_ptr] = .{
                    .method = self.method,
                    .method_class = self.method_class,
                    .ip = self.ip,
                    .receiver = self.receiver,
                    .temp_base = self.temp_base,
                    .outer_temp_base = self.outer_temp_base,
                    .home_temp_base = self.home_temp_base,
                    .heap_context = self.heap_context,
                    .home_heap_context = self.home_heap_context,
                };
            self.context_ptr += 1;

            // Set up new context for the called method
            self.method = found_method;
            self.method_class = method_holder;
            self.ip = 0;
            self.receiver = recv;
            // OPTIMIZATION: recv+args already on stack at recv_pos
            // temp_base points to receiver position
            self.temp_base = recv_pos;
            self.outer_temp_base = self.temp_base;
            self.home_temp_base = self.temp_base; // Blocks in this method return to this context
            // Clear heap contexts - method has its own fresh temps, not shared with calling block
            self.heap_context = Value.nil;
            self.home_heap_context = Value.nil;

            // OPTIMIZATION: recv+args already on stack, no need to push them
            // Receiver is at stack[temp_base], args at stack[temp_base+1..]

            // Allocate space for temporaries
            const total_temps = found_method.header.num_temps;
            const local_temps = if (total_temps > num_args) total_temps - @as(u8, @intCast(num_args)) else 0;
            var k: usize = 0;
            while (k < local_temps) : (k += 1) {
                try self.push(Value.nil);
            }
        } else {
            // Method not found - try to send doesNotUnderstand:
            var sel_name_dbg: []const u8 = "<?>";
            if (selector.isObject()) {
                const sel_obj = selector.asObject();
                if (sel_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    sel_name_dbg = sel_obj.bytes(sel_obj.header.size);
                }
            }
            var class_name_dbg: []const u8 = "<unknown>";
            if (start_class.isObject()) {
                const cls_obj = start_class.asObject();
                const name_val = cls_obj.getField(Heap.CLASS_FIELD_NAME, cls_obj.header.size);
                if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                    class_name_dbg = name_val.asObject().bytes(name_val.asObject().header.size);
                }
            }
            self.last_mnu_selector = sel_name_dbg;
            self.last_mnu_receiver = recv;
            self.last_mnu_method = self.currentMethodSource();
            if (DEBUG_VERBOSE) std.debug.print("DEBUG lookup miss selector={s} class={s}\n", .{sel_name_dbg, class_name_dbg});
            // Create a Message object to pass to doesNotUnderstand:
            const dnu_selector = self.heap.internSymbol("doesNotUnderstand:") catch {
                return InterpreterError.MessageNotUnderstood;
            };

            // Only try DNU if we're not already handling DNU (avoid infinite recursion)
            if (selector.eql(dnu_selector)) {
                return InterpreterError.MessageNotUnderstood;
            }

            // Look up doesNotUnderstand: in the class hierarchy
            const dnu_method = self.lookupMethod(start_class, dnu_selector);
            if (dnu_method) |found_dnu| {
                // Create a Message object
                const message = self.heap.allocateObject(Heap.CLASS_MESSAGE, 3, .normal) catch {
                    return InterpreterError.OutOfMemory;
                };
                // Message has: selector, arguments, lookupClass
                message.setField(0, selector, 3); // selector
                // Create arguments array - read args from stack (they're still there)
                const args_array = self.heap.allocateObject(Heap.CLASS_ARRAY, num_args, .variable) catch {
                    return InterpreterError.OutOfMemory;
                };
                var j: usize = 0;
                while (j < num_args) : (j += 1) {
                    args_array.setField(j, self.stack[recv_pos + 1 + j], num_args);
                }
                message.setField(1, Value.fromObject(args_array), 3); // arguments
                message.setField(2, start_class, 3); // lookupClass

                // Pop original recv+args from stack before calling DNU
                self.sp = recv_pos;

                // If doesNotUnderstand: is a primitive, execute it directly
                if (found_dnu.header.primitive_index != 0) {
                    try self.push(recv);
                    try self.push(Value.fromObject(message));
                    if (primitives.executePrimitive(self, found_dnu.header.primitive_index)) |result| {
                        try self.push(result);
                        return;
                    } else |err| {
                        return err; // Propagate all errors from DNU primitive
                    }
                }

                // Call doesNotUnderstand: with the Message (non-primitive case)
                // Save context
                if (self.context_ptr >= self.contexts.len - 1) {
                    return InterpreterError.StackOverflow;
                }

                self.contexts[self.context_ptr] = .{
                    .method = self.method,
                    .method_class = self.method_class,
                    .ip = self.ip,
                    .receiver = self.receiver,
                    .temp_base = self.temp_base,
                    .outer_temp_base = self.outer_temp_base,
                    .home_temp_base = self.home_temp_base,
                    .heap_context = self.heap_context,
                    .home_heap_context = self.home_heap_context,
                };
                self.context_ptr += 1;

                self.method = found_dnu;
                self.ip = 0;
                self.receiver = recv;
                self.temp_base = self.sp;
                self.outer_temp_base = self.temp_base;
                self.home_temp_base = self.temp_base;
                // Clear heap contexts - method has its own fresh temps, not shared with calling block
                self.heap_context = Value.nil;
                self.home_heap_context = Value.nil;

                // Push receiver and message argument
                try self.push(recv);
                try self.push(Value.fromObject(message));

                // Allocate temps
                const total_temps = found_dnu.header.num_temps;
                const local_temps = if (total_temps > 1) total_temps - 1 else 0;
                var k: usize = 0;
                while (k < local_temps) : (k += 1) {
                    try self.push(Value.nil);
                }
            } else {
                // No doesNotUnderstand: method - raise error
                var sel_name: []const u8 = "<?>"; 
                if (selector.isObject()) { 
                    const sel_obj = selector.asObject(); 
                    if (sel_obj.header.class_index == Heap.CLASS_SYMBOL) { 
                        sel_name = sel_obj.bytes(sel_obj.header.size); 
                    } else {
                        if (DEBUG_VERBOSE) std.debug.print("DEBUG MNU selector class_idx={}\n", .{sel_obj.header.class_index});
                    } 
                }
                var recv_name: []const u8 = "<?>";
                const recv_class = self.heap.classOf(recv);
                if (recv_class.isObject()) {
                    const recv_class_obj = recv_class.asObject();
                    const name_val = recv_class_obj.getField(Heap.CLASS_FIELD_NAME, recv_class_obj.header.size);
                    if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                        recv_name = name_val.asObject().bytes(name_val.asObject().header.size);
                    }
                }
                if (selector.isSmallInt()) {
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG MNU selector smallInt={}\n", .{selector.asSmallInt()});
                } else if (!selector.isObject()) {
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG MNU selector raw bits=0x{x}\n", .{selector.bits});
                }
                self.last_mnu_selector = sel_name;
                self.last_mnu_method = self.currentMethodSource();
                if (std.debug.runtime_safety) {
                    // Help diagnose missing doesNotUnderstand: definitions by showing the lookup chain.
                    var c = start_class;
                    while (c.isObject()) {
                        const c_obj = c.asObject();
                        const name_val = c_obj.getField(Heap.CLASS_FIELD_NAME, c_obj.header.size);
                        if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                            const nm = name_val.asObject().bytes(name_val.asObject().header.size);
                            std.debug.print("  DNU lookup chain class={s}\n", .{nm});
                        }
                        c = c_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, c_obj.header.size);
                    }
                }
                if (DEBUG_VERBOSE) std.debug.print("DEBUG MessageNotUnderstood recv={s} selector={s}\n", .{ recv_name, sel_name }); 
                return InterpreterError.MessageNotUnderstood;
            }
        }
    }

    /// Return from a method, restoring the previous context.
    /// Returns the final value if this was the outermost call, or null to continue execution.
    fn returnFromMethod(self: *Interpreter, result: Value) InterpreterError!?Value {
        const is_true = result.bits == Value.@"true".bits;
        const is_false = result.bits == Value.@"false".bits;
        const is_nil = result.bits == Value.nil.bits;
        if (DEBUG_VERBOSE) std.debug.print("DEBUG returnFromMethod ctx_ptr={} result: is_obj={} is_true={} is_false={} is_nil={}\n", .{ self.context_ptr, result.isObject(), is_true, is_false, is_nil });
        if (self.context_ptr == 0) {
            // No more contexts - this process is finished
            // Check if we can switch to another process
            const current = self.process_scheduler.active_process;
            if (current) |proc| {
                proc.state = .terminated;
                self.process_scheduler.active_process = null;

                // Find next ready process
                if (self.process_scheduler.findHighestPriorityReady()) |next| {
                    self.process_scheduler.removeFromReadyQueue(next);
                    self.restoreContextFromProcess(next);
                    next.state = .running;
                    self.process_scheduler.active_process = next;
                    // Continue execution with the restored process - don't push anything
                    return null;
                }
            }
            // No more processes - this is the final return
            if (DEBUG_VERBOSE) std.debug.print("DEBUG returnFromMethod final return is_true={} is_false={}\n", .{ is_true, is_false });
            return result;
        }

        // Pop the current activation's stack frame
        self.sp = self.temp_base;

        // Restore previous context
        self.context_ptr -= 1;
        const ctx = self.contexts[self.context_ptr];
        if (DEBUG_VERBOSE) std.debug.print("DEBUG returnFromMethod restoring ip={} temp_base={}\n", .{ ctx.ip, ctx.temp_base });
        self.method = ctx.method;
        self.method_class = ctx.method_class;
        self.ip = ctx.ip;
        self.receiver = ctx.receiver;
        self.temp_base = ctx.temp_base;
        self.outer_temp_base = ctx.outer_temp_base;
        self.home_temp_base = ctx.home_temp_base;
        self.heap_context = ctx.heap_context;
        self.home_heap_context = ctx.home_heap_context;

        // Push the return value onto the caller's stack
        try self.push(result);

        return null; // Continue execution in restored context
    }

    fn getSuperclass(_: *Interpreter, class: Value) Value {
        if (class.isObject()) {
            const class_obj = class.asObject();
            // Use actual object size for field access (old classes may have fewer fields)
            return class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, class_obj.header.size);
        }
        return Value.nil;
    }

    pub fn lookupMethod(self: *Interpreter, start_class: Value, selector: Value) ?*CompiledMethod {
        // Use lookupMethodWithHolder and discard holder
        if (self.lookupMethodWithHolder(start_class, selector)) |result| {
            return result.method;
        }
        return null;
    }

    const MethodLookup = struct {
        method: *CompiledMethod,
        holder: Value,
    };

    fn lookupMethodWithHolder(self: *Interpreter, start_class: Value, selector: Value) ?MethodLookup {
        // Check method cache first
        const hash = start_class.bits ^ selector.bits;
        const index = hash & (METHOD_CACHE_SIZE - 1);
        const entry = &self.method_cache[index];

        if (entry.class.bits == start_class.bits and entry.selector.bits == selector.bits) {
            // Cache hit!
            self.method_cache_hits += 1;
            if (entry.method) |method| {
                return MethodLookup{
                    .method = method,
                    .holder = entry.holder,
                };
            }
            return null; // Cached negative result
        }

        // Cache miss - do full lookup
        self.method_cache_misses += 1;
        const result = self.lookupMethodWithHolderUncached(start_class, selector);

        // Update cache
        entry.class = start_class;
        entry.selector = selector;
        if (result) |r| {
            entry.method = r.method;
            entry.holder = r.holder;
        } else {
            entry.method = null;
            entry.holder = Value.nil;
        }

        return result;
    }

    /// Uncached method lookup with holder - walks the class hierarchy
    fn lookupMethodWithHolderUncached(self: *Interpreter, start_class: Value, selector: Value) ?MethodLookup {
        var class = start_class;

        while (!class.isNil()) {
            if (class.isObject()) {
                const class_obj = class.asObject();
                // Use actual object size for field access (old classes may have fewer fields)
                const num_fields = class_obj.header.size;

                const method_dict = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, num_fields);
                if (method_dict.isObject()) {
                    if (self.lookupInMethodDict(method_dict, selector)) |method| {
                        return MethodLookup{
                            .method = method,
                            .holder = class,
                        };
                    }
                }
                class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, num_fields);
            } else {
                break;
            }
        }

        return null;
    }

    fn lookupInMethodDict(self: *Interpreter, dict: Value, selector: Value) ?*CompiledMethod {
        _ = self;
        // Method dictionaries are Arrays of [selector, method] pairs.
        // We use hash-based lookup with linear probing for O(1) average case.
        // Hash is computed from selector's bits (pointer value for interned symbols).

        if (!dict.isObject()) return null;

        const dict_obj = dict.asObject();
        const dict_size = dict_obj.header.size;

        if (dict_size < 2) return null;

        // Number of slots (each slot is 2 entries: selector + method)
        const num_slots = dict_size / 2;
        if (num_slots == 0) return null;

        // Hash from selector bits - use a simple but effective hash
        const hash = selector.bits *% 2654435761; // Knuth's multiplicative hash
        var index = @as(usize, @intCast(hash)) % num_slots;

        // Linear probe with wrap-around
        var probes: usize = 0;
        while (probes < num_slots) : (probes += 1) {
            const slot_base = index * 2;
            const key = dict_obj.getField(slot_base, dict_size);

            // Empty slot means selector not found
            if (key.isNil()) {
                return null;
            }

            // Check for match - first try pointer equality (fast for interned symbols)
            if (key.bits == selector.bits) {
                const method_val = dict_obj.getField(slot_base + 1, dict_size);
                if (method_val.isObject()) {
                    const found_method: *CompiledMethod = @ptrCast(@alignCast(method_val.asObject()));
                    return found_method;
                }
                return null;
            }

            // Fallback: compare symbol bytes if both are symbols (for non-interned case)
            if (key.isObject() and selector.isObject()) {
                const key_obj = key.asObject();
                const sel_obj = selector.asObject();
                if (key_obj.header.class_index == Heap.CLASS_SYMBOL and sel_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    const k_bytes = key_obj.bytes(key_obj.header.size);
                    const s_bytes = sel_obj.bytes(sel_obj.header.size);
                    if (std.mem.eql(u8, k_bytes, s_bytes)) {
                        const method_val = dict_obj.getField(slot_base + 1, dict_size);
                        if (method_val.isObject()) {
                            const found_method: *CompiledMethod = @ptrCast(@alignCast(method_val.asObject()));
                            return found_method;
                        }
                        return null;
                    }
                }
            }

            // Move to next slot (linear probing)
            index = (index + 1) % num_slots;
        }

        return null;
    }

    /// Look up a class variable by name in the receiver's class hierarchy
    fn lookupClassVariable(self: *Interpreter, name: []const u8) ?Value {
        // Get the class to search for class variables
        // If the receiver is a class (i.e., its class is a metaclass), we need to
        // search the receiver's class hierarchy, not its metaclass hierarchy.
        var class = self.heap.classOf(self.receiver);

        // Check if the receiver's class is a metaclass by looking for thisClass field
        // A metaclass has METACLASS_NUM_FIELDS (10) and thisClass at index 9
        if (class.isObject()) {
            const class_of_receiver = class.asObject();
            if (class_of_receiver.header.size >= Heap.METACLASS_NUM_FIELDS) {
                // This might be a metaclass - check if thisClass points back to receiver
                const this_class = class_of_receiver.getField(Heap.METACLASS_FIELD_THIS_CLASS, Heap.METACLASS_NUM_FIELDS);
                if (this_class.isObject() and self.receiver.isObject() and
                    this_class.asObject() == self.receiver.asObject())
                {
                    // The receiver IS a class - search class variables on it directly
                    class = self.receiver;
                }
            }
        }

        // Walk up the superclass chain looking for the class variable
        while (!class.isNil() and class.isObject()) {
            const class_obj = class.asObject();
            const obj_size = class_obj.header.size;

            // Get class variables dictionary from this class
            const class_vars = class_obj.getField(Heap.CLASS_FIELD_CLASS_VARS, obj_size);

            if (class_vars.isObject()) {
                const vars_obj = class_vars.asObject();
                const vars_size = vars_obj.header.size;

                // Search for the name in the [name, value] pairs
                var i: usize = 0;
                while (i + 1 < vars_size) : (i += 2) {
                    const key = vars_obj.getField(i, vars_size);
                    if (key.isNil()) break;

                    // Compare symbol names
                    if (key.isObject()) {
                        const key_obj = key.asObject();
                        if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            const key_bytes = key_obj.bytes(key_obj.header.size);
                            if (std.mem.eql(u8, key_bytes, name)) {
                                return vars_obj.getField(i + 1, vars_size);
                            }
                        }
                    }
                }
            }

            // Move to superclass
            class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, obj_size);
        }

        return null;
    }

    /// Store a value to a class variable by name in the receiver's class hierarchy
    fn storeClassVariable(self: *Interpreter, name: []const u8, value: Value) bool {
        // Get the class to search for class variables
        // If the receiver is a class (i.e., its class is a metaclass), we need to
        // search the receiver's class hierarchy, not its metaclass hierarchy.
        var class = self.heap.classOf(self.receiver);

        // Check if the receiver's class is a metaclass by looking for thisClass field
        if (class.isObject()) {
            const class_of_receiver = class.asObject();
            if (class_of_receiver.header.size >= Heap.METACLASS_NUM_FIELDS) {
                const this_class = class_of_receiver.getField(Heap.METACLASS_FIELD_THIS_CLASS, Heap.METACLASS_NUM_FIELDS);
                if (this_class.isObject() and self.receiver.isObject() and
                    this_class.asObject() == self.receiver.asObject())
                {
                    // The receiver IS a class - search class variables on it directly
                    class = self.receiver;
                }
            }
        }

        // Walk up the superclass chain looking for the class variable
        while (!class.isNil() and class.isObject()) {
            const class_obj = class.asObject();
            const obj_size = class_obj.header.size;

            // Get class variables dictionary from this class
            const class_vars = class_obj.getField(Heap.CLASS_FIELD_CLASS_VARS, obj_size);

            if (class_vars.isObject()) {
                const vars_obj = class_vars.asObject();
                const vars_size = vars_obj.header.size;

                // Search for the name in the [name, value] pairs
                var i: usize = 0;
                while (i + 1 < vars_size) : (i += 2) {
                    const key = vars_obj.getField(i, vars_size);
                    if (key.isNil()) break;

                    // Compare symbol names
                    if (key.isObject()) {
                        const key_obj = key.asObject();
                        if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            const key_bytes = key_obj.bytes(key_obj.header.size);
                            if (std.mem.eql(u8, key_bytes, name)) {
                                // Found it - store the new value
                                vars_obj.setField(i + 1, value, vars_size);
                                return true;
                            }
                        }
                    }
                }
            }

            // Move to superclass
            class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, obj_size);
        }

        return false;
    }

    fn sendSpecialBinary(self: *Interpreter, prim: bytecodes.Primitive, selector: []const u8) InterpreterError!void {
        const arg = try self.pop();
        const recv = try self.pop();

        // Try primitive first (fast path for SmallIntegers)
        try self.push(recv);
        try self.push(arg);

        const prim_result = primitives.executePrimitive(self, @intFromEnum(prim));
        if (prim_result) |result| {
            try self.push(result);
            return;
        } else |_| {
            // Primitive failed - fall back to regular method lookup
            _ = try self.pop(); // Remove arg
            _ = try self.pop(); // Remove recv

            // Look up the selector in the class hierarchy
            const class = self.heap.classOf(recv);
            const selector_sym = self.heap.internSymbol(selector) catch {
                return InterpreterError.OutOfMemory;
            };

            if (self.lookupMethod(class, selector_sym)) |method| {
                // Found method - call it
                if (method.header.primitive_index != 0) {
                    // Push receiver and arg for primitive
                    try self.push(recv);
                    try self.push(arg);
                    if (primitives.executePrimitive(self, method.header.primitive_index)) |result| {
                        try self.push(result);
                        return;
                    } else |_| {
                        // Pop the args we just pushed
                        _ = try self.pop();
                        _ = try self.pop();
                    }
                }

                // Execute method bytecode - save context and set up new frame
                if (self.context_ptr >= self.contexts.len - 1) {
                    return InterpreterError.StackOverflow;
                }

                self.contexts[self.context_ptr] = .{
                    .method = self.method,
                    .method_class = self.method_class,
                    .ip = self.ip,
                    .receiver = self.receiver,
                    .temp_base = self.temp_base,
                    .outer_temp_base = self.outer_temp_base,
                    .home_temp_base = self.home_temp_base,
                    .heap_context = self.heap_context,
                    .home_heap_context = self.home_heap_context,
                };
                self.context_ptr += 1;

                self.method = method;
                self.ip = 0;
                self.receiver = recv;
                self.temp_base = self.sp;
                self.outer_temp_base = self.temp_base;
                self.home_temp_base = self.temp_base;
                // Clear heap contexts - method has its own fresh temps, not shared with calling block
                self.heap_context = Value.nil;
                self.home_heap_context = Value.nil;

                // Push receiver and argument
                try self.push(recv);
                try self.push(arg);

                // Allocate temps
                const total_temps = method.header.num_temps;
                const local_temps = if (total_temps > 1) total_temps - 1 else 0;
                var k: usize = 0;
                while (k < local_temps) : (k += 1) {
                    try self.push(Value.nil);
                }
                return;
            }

            // No method found
            self.last_mnu_selector = selector;
            self.last_mnu_receiver = recv;
            self.last_mnu_method = self.currentMethodSource();
            const recv_class = self.heap.classOf(recv);
            var recv_name: []const u8 = "<?>";
            if (recv_class.isObject()) {
                const rc_obj = recv_class.asObject();
                const name_val = rc_obj.getField(Heap.CLASS_FIELD_NAME, rc_obj.header.size);
                if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                    recv_name = name_val.asObject().bytes(name_val.asObject().header.size);
                }
            }
            const arg_class = self.heap.classOf(arg);
            var arg_name: []const u8 = "<?>";
            if (arg_class.isObject()) {
                const ac_obj = arg_class.asObject();
                const name_val = ac_obj.getField(Heap.CLASS_FIELD_NAME, ac_obj.header.size);
                if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                    arg_name = name_val.asObject().bytes(name_val.asObject().header.size);
                }
            }
            const src = self.currentMethodSource();
            if (DEBUG_VERBOSE) std.debug.print("DEBUG binary lookup miss selector={s} recv={s} arg={s} in {s}\n", .{selector, recv_name, arg_name, src});
            if (std.mem.eql(u8, selector, "+")) {
                const base = self.outer_temp_base;
                if (base + 2 < self.stack.len) {
                    const slot0 = self.stack[base + 1];
                    const slot1 = self.stack[base + 2];
                    const s0 = blk: {
                        const cls = self.heap.classOf(slot0);
                        if (cls.isObject()) {
                            const cls_obj = cls.asObject();
                            const nv = cls_obj.getField(Heap.CLASS_FIELD_NAME, cls_obj.header.size);
                            if (nv.isObject() and nv.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                break :blk nv.asObject().bytes(nv.asObject().header.size);
                            }
                        }
                        if (slot0.isSmallInt()) break :blk "SmallInteger";
                        if (slot0.isNil()) break :blk "nil";
                        break :blk "<?>";
                    };
                    const s1 = blk: {
                        const cls = self.heap.classOf(slot1);
                        if (cls.isObject()) {
                            const cls_obj = cls.asObject();
                            const nv = cls_obj.getField(Heap.CLASS_FIELD_NAME, cls_obj.header.size);
                            if (nv.isObject() and nv.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                break :blk nv.asObject().bytes(nv.asObject().header.size);
                            }
                        }
                        if (slot1.isSmallInt()) break :blk "SmallInteger";
                        if (slot1.isNil()) break :blk "nil";
                        break :blk "<?>";
                    };
                    std.debug.print("  outer_temp_base={} slot0={s} slot1={s}\n", .{ base, s0, s1 });
                }
            }
            return InterpreterError.MessageNotUnderstood;
        }
    }

    fn sendIdentical(self: *Interpreter) InterpreterError!void {
        const arg = try self.pop();
        const recv = try self.pop();
        try self.push(Value.fromBool(recv.eql(arg)));
    }

    fn sendNotIdentical(self: *Interpreter) InterpreterError!void {
        const arg = try self.pop();
        const recv = try self.pop();
        try self.push(Value.fromBool(!recv.eql(arg)));
    }

    fn sendUnary(self: *Interpreter, selector: []const u8) InterpreterError!void {
        const recv = try self.pop();

        // Look up the selector in the class hierarchy
        const class = self.heap.classOf(recv);
        const selector_sym = self.heap.internSymbol(selector) catch {
            return InterpreterError.OutOfMemory;
        };

        if (self.lookupMethod(class, selector_sym)) |method| {
            // Found method - check for primitive
            if (method.header.primitive_index != 0) {
                // Push receiver for primitive
                try self.push(recv);
                if (primitives.executePrimitive(self, method.header.primitive_index)) |result| {
                    try self.push(result);
                    return;
                } else |_| {
                    // Primitive failed - pop receiver and execute bytecode
                    _ = try self.pop();
                }
            }

            // Execute method bytecode - save context and set up new frame
            if (self.context_ptr >= self.contexts.len - 1) {
                return InterpreterError.StackOverflow;
            }

            self.contexts[self.context_ptr] = .{
                .method = self.method,
                .method_class = self.method_class,
                .ip = self.ip,
                .receiver = self.receiver,
                .temp_base = self.temp_base,
                .outer_temp_base = self.outer_temp_base,
                .home_temp_base = self.home_temp_base,
                .heap_context = self.heap_context,
                .home_heap_context = self.home_heap_context,
            };
            self.context_ptr += 1;

            self.method = method;
            self.ip = 0;
            self.receiver = recv;
            self.temp_base = self.sp;
            self.outer_temp_base = self.temp_base;
            self.home_temp_base = self.temp_base;
            // Clear heap contexts - method has its own fresh temps, not shared with calling block
            self.heap_context = Value.nil;
            self.home_heap_context = Value.nil;

            // Push receiver (no arguments for unary)
            try self.push(recv);

            // Allocate temps
            const total_temps = method.header.num_temps;
            var k: usize = 0;
            while (k < total_temps) : (k += 1) {
                try self.push(Value.nil);
            }
            return;
        }

        // No method found
        self.last_mnu_selector = selector;
        self.last_mnu_receiver = recv;
        self.last_mnu_method = self.currentMethodSource();
        const recv_class = self.heap.classOf(recv);
        var recv_name: []const u8 = "<?>";
        if (recv_class.isObject()) {
            const rc_obj = recv_class.asObject();
            const name_val = rc_obj.getField(Heap.CLASS_FIELD_NAME, rc_obj.header.size);
            if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                recv_name = name_val.asObject().bytes(name_val.asObject().header.size);
            }
        }
        if (DEBUG_VERBOSE) std.debug.print("DEBUG lookup miss selector={s} recv={s}\n", .{ selector, recv_name });
        return InterpreterError.MessageNotUnderstood;
    }
};

test "Interpreter - push and pop" {
    const allocator = std.testing.allocator;
    var heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var interp = Interpreter.init(&heap, allocator);
    defer interp.deinit();

    try interp.push(Value.fromSmallInt(42));
    try interp.push(Value.fromSmallInt(100));

    try std.testing.expectEqual(@as(i61, 100), (try interp.pop()).asSmallInt());
    try std.testing.expectEqual(@as(i61, 42), (try interp.pop()).asSmallInt());
}
