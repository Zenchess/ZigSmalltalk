/// Process Scheduler for Zig Smalltalk VM
/// Implements Dolphin-compatible green thread scheduling with priority queues
///
/// Key concepts:
/// - Processes are green threads (cooperative multitasking in single OS thread)
/// - Priority-based scheduling with 10 priority levels
/// - Semaphores for synchronization
/// - Timer support for Delay class

const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");

const Value = object.Value;
const Object = object.Object;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;

/// Process states
pub const ProcessState = enum {
    suspended, // Not in any queue, can be resumed
    ready, // In a priority queue, waiting to be scheduled
    running, // Currently executing (activeProcess)
    waiting, // Blocked on a semaphore
    terminated, // Dead, cannot be resumed
};

/// Saved execution context for a suspended process
pub const SavedContext = struct {
    // Method execution state
    method: ?*CompiledMethod = null,
    method_class: Value = Value.nil,
    ip: usize = 0,
    receiver: Value = Value.nil,
    temp_base: usize = 0,
    outer_temp_base: usize = 0,
    home_temp_base: usize = 0,
    // Heap-allocated context for closure variable capture
    heap_context: Value = Value.nil,
    // Home method's heap context
    home_heap_context: Value = Value.nil,

    // Stack state
    sp: usize = 0,
    stack: [8192]Value = undefined,

    // Context stack state
    context_ptr: usize = 0,
    // We'll save contexts separately since they contain pointers

    // Exception handler state
    handler_ptr: usize = 0,
    current_exception: Value = Value.nil,

    // Block execution state
    primitive_block_depth: usize = 0,
    non_local_return_target: usize = 0,
};

/// A Process in the VM
/// Corresponds to Dolphin's Process class structure
pub const Process = struct {
    /// The Smalltalk Process object (has instVars: suspendedFrame, priority, myList, etc.)
    object: Value,

    /// Current state
    state: ProcessState,

    /// Priority level (1-10, higher = more important)
    priority: u8,

    /// Saved execution context when suspended
    saved_context: SavedContext,

    /// Saved context stack (array of contexts)
    saved_contexts: [1024]SavedContextEntry = undefined,

    /// Saved exception handlers
    saved_exception_handlers: [256]SavedExceptionHandler = undefined,

    /// Link for linked list (ready queue or semaphore wait queue)
    next: ?*Process = null,
    prev: ?*Process = null,

    /// The list this process is currently on (for debugging)
    suspending_list: ?*anyopaque = null,

    pub fn init(obj: Value, prio: u8) Process {
        return .{
            .object = obj,
            .state = .suspended,
            .priority = prio,
            .saved_context = .{},
        };
    }
};

/// Saved context entry (mirrors interpreter.Context but with Values that survive GC)
pub const SavedContextEntry = struct {
    method: ?*CompiledMethod = null,
    method_class: Value = Value.nil,
    ip: usize = 0,
    receiver: Value = Value.nil,
    temp_base: usize = 0,
    outer_temp_base: usize = 0,
    home_temp_base: usize = 0,
    num_args: usize = 0,
    num_temps: usize = 0,
    closure: ?Value = null,
    // Heap-allocated context for closure variable capture
    heap_context: Value = Value.nil,
    // Home method's heap context
    home_heap_context: Value = Value.nil,
};

/// Saved exception handler entry
pub const SavedExceptionHandler = struct {
    exception_class: Value = Value.nil,
    handler_block: Value = Value.nil,
    context_ptr: usize = 0,
    sp: usize = 0,
    temp_base: usize = 0,
    method: ?*CompiledMethod = null,
    ip: usize = 0,
    receiver: Value = Value.nil,
};

/// A linked list of processes (used for ready queues and semaphore wait queues)
pub const ProcessList = struct {
    first: ?*Process = null,
    last: ?*Process = null,
    count: usize = 0,

    pub fn isEmpty(self: *const ProcessList) bool {
        return self.first == null;
    }

    pub fn addLast(self: *ProcessList, process: *Process) void {
        process.next = null;
        process.prev = self.last;
        if (self.last) |last| {
            last.next = process;
        } else {
            self.first = process;
        }
        self.last = process;
        self.count += 1;
    }

    pub fn addFirst(self: *ProcessList, process: *Process) void {
        process.prev = null;
        process.next = self.first;
        if (self.first) |first| {
            first.prev = process;
        } else {
            self.last = process;
        }
        self.first = process;
        self.count += 1;
    }

    pub fn remove(self: *ProcessList, process: *Process) void {
        if (process.prev) |prev| {
            prev.next = process.next;
        } else {
            self.first = process.next;
        }
        if (process.next) |next| {
            next.prev = process.prev;
        } else {
            self.last = process.prev;
        }
        process.next = null;
        process.prev = null;
        self.count -= 1;
    }

    pub fn removeFirst(self: *ProcessList) ?*Process {
        const process = self.first orelse return null;
        self.remove(process);
        return process;
    }
};

/// Timer request for Delay support
pub const TimerRequest = struct {
    /// When to fire (in microseconds since VM start)
    fire_time_us: i64,
    /// Semaphore to signal when timer fires
    semaphore: Value,
    /// Whether this request is active
    active: bool = false,
};

/// Number of priority levels (Dolphin uses 10)
pub const NUM_PRIORITIES: usize = 10;

/// Priority level constants (matching Dolphin)
pub const Priority = struct {
    pub const systemBase: u8 = 1;
    pub const systemBackground: u8 = 2;
    pub const userBackground: u8 = 3;
    pub const userScheduling: u8 = 5;
    pub const userInterrupt: u8 = 7;
    pub const lowIO: u8 = 8;
    pub const highIO: u8 = 9;
    pub const timing: u8 = 10;
};

/// The main process scheduler
pub const Scheduler = struct {
    /// Ready queues - one per priority level (index 0 = priority 1, index 9 = priority 10)
    ready_queues: [NUM_PRIORITIES]ProcessList,

    /// The currently active process
    active_process: ?*Process,

    /// All known processes (for GC tracing)
    all_processes: std.ArrayList(*Process),

    /// Timer request (only one at a time, like Dolphin)
    timer: TimerRequest,

    /// VM start time (for microsecond clock)
    start_time_ns: i128,

    /// Allocator for process structures
    allocator: std.mem.Allocator,

    /// Whether async events (process switching) are enabled
    async_events_enabled: bool,

    /// The timing semaphore (signaled by timer)
    timing_semaphore: Value,

    pub fn init(allocator: std.mem.Allocator) Scheduler {
        var ready_queues: [NUM_PRIORITIES]ProcessList = undefined;
        for (&ready_queues) |*q| {
            q.* = .{};
        }

        return .{
            .ready_queues = ready_queues,
            .active_process = null,
            .all_processes = .{},
            .timer = .{
                .fire_time_us = 0,
                .semaphore = Value.nil,
                .active = false,
            },
            .start_time_ns = std.time.nanoTimestamp(),
            .allocator = allocator,
            .async_events_enabled = true,
            .timing_semaphore = Value.nil,
        };
    }

    pub fn deinit(self: *Scheduler) void {
        for (self.all_processes.items) |proc| {
            self.allocator.destroy(proc);
        }
        self.all_processes.deinit(self.allocator);
    }

    /// Get current microsecond clock value (relative to VM start)
    pub fn microsecondClockValue(self: *Scheduler) i64 {
        const now_ns = std.time.nanoTimestamp();
        const elapsed_ns = now_ns - self.start_time_ns;
        return @intCast(@divFloor(elapsed_ns, 1000));
    }

    /// Create a new process
    pub fn createProcess(self: *Scheduler, obj: Value, priority: u8) !*Process {
        const process = try self.allocator.create(Process);
        process.* = Process.init(obj, priority);
        try self.all_processes.append(self.allocator, process);
        return process;
    }

    /// Find process by its Smalltalk object
    pub fn findProcess(self: *Scheduler, obj: Value) ?*Process {
        for (self.all_processes.items) |proc| {
            if (proc.object.eql(obj)) {
                return proc;
            }
        }
        return null;
    }

    /// Add process to ready queue at its priority
    pub fn makeReady(self: *Scheduler, process: *Process) void {
        if (process.state == .terminated) return;

        // Remove from any current list
        if (process.suspending_list) |_| {
            // Already in a list, need to remove first
            // This is handled by the caller typically
        }

        const queue_idx = @as(usize, process.priority) - 1;
        if (queue_idx < NUM_PRIORITIES) {
            self.ready_queues[queue_idx].addLast(process);
            process.state = .ready;
            process.suspending_list = @ptrCast(&self.ready_queues[queue_idx]);
        }
    }

    /// Remove process from ready queue
    pub fn removeFromReadyQueue(self: *Scheduler, process: *Process) void {
        const queue_idx = @as(usize, process.priority) - 1;
        if (queue_idx < NUM_PRIORITIES) {
            self.ready_queues[queue_idx].remove(process);
            process.suspending_list = null;
        }
    }

    /// Find the highest priority ready process
    pub fn findHighestPriorityReady(self: *Scheduler) ?*Process {
        // Search from highest priority (index 9) to lowest (index 0)
        var i: usize = NUM_PRIORITIES;
        while (i > 0) {
            i -= 1;
            if (!self.ready_queues[i].isEmpty()) {
                return self.ready_queues[i].first;
            }
        }
        return null;
    }

    /// Check if we should preempt the current process
    pub fn shouldPreempt(self: *Scheduler) bool {
        if (!self.async_events_enabled) return false;

        const current = self.active_process orelse return true;

        // Check if there's a higher priority process ready
        var i: usize = NUM_PRIORITIES;
        while (i > current.priority) {
            i -= 1;
            if (!self.ready_queues[i].isEmpty()) {
                return true;
            }
        }
        return false;
    }

    /// Set timer to signal semaphore after milliseconds
    /// milliseconds < 0 cancels the timer
    /// milliseconds = 0 signals immediately
    pub fn setTimer(self: *Scheduler, milliseconds: i32, semaphore: Value) void {
        if (milliseconds < 0) {
            // Cancel timer
            self.timer.active = false;
            self.timer.semaphore = Value.nil;
        } else if (milliseconds == 0) {
            // Signal immediately - handled by caller
            self.timer.active = false;
        } else {
            // Schedule timer
            const fire_time = self.microsecondClockValue() + @as(i64, milliseconds) * 1000;
            self.timer.fire_time_us = fire_time;
            self.timer.semaphore = semaphore;
            self.timer.active = true;
        }
    }

    /// Check if timer has fired
    pub fn checkTimer(self: *Scheduler) ?Value {
        if (!self.timer.active) return null;

        const now = self.microsecondClockValue();
        if (now >= self.timer.fire_time_us) {
            const sem = self.timer.semaphore;
            self.timer.active = false;
            self.timer.semaphore = Value.nil;
            return sem;
        }
        return null;
    }

    /// Trace all process-related values for GC
    pub fn traceForGC(self: *Scheduler, copyFn: *const fn (Value) anyerror!Value) !void {
        // Trace timing semaphore
        if (!self.timing_semaphore.isNil()) {
            self.timing_semaphore = try copyFn(self.timing_semaphore);
        }

        // Trace timer semaphore
        if (!self.timer.semaphore.isNil()) {
            self.timer.semaphore = try copyFn(self.timer.semaphore);
        }

        // Trace all processes
        for (self.all_processes.items) |proc| {
            // Trace the process object itself
            proc.object = try copyFn(proc.object);

            // Trace saved context values
            proc.saved_context.method_class = try copyFn(proc.saved_context.method_class);
            proc.saved_context.receiver = try copyFn(proc.saved_context.receiver);
            proc.saved_context.current_exception = try copyFn(proc.saved_context.current_exception);
            if (!proc.saved_context.heap_context.isNil()) {
                proc.saved_context.heap_context = try copyFn(proc.saved_context.heap_context);
            }
            if (!proc.saved_context.home_heap_context.isNil()) {
                proc.saved_context.home_heap_context = try copyFn(proc.saved_context.home_heap_context);
            }

            // Trace saved stack
            for (proc.saved_context.stack[0..proc.saved_context.sp]) |*val| {
                val.* = try copyFn(val.*);
            }

            // Trace saved contexts
            for (proc.saved_contexts[0..proc.saved_context.context_ptr]) |*ctx| {
                ctx.method_class = try copyFn(ctx.method_class);
                ctx.receiver = try copyFn(ctx.receiver);
                if (ctx.closure) |closure| {
                    ctx.closure = try copyFn(closure);
                }
                if (!ctx.heap_context.isNil()) {
                    ctx.heap_context = try copyFn(ctx.heap_context);
                }
                if (!ctx.home_heap_context.isNil()) {
                    ctx.home_heap_context = try copyFn(ctx.home_heap_context);
                }
            }

            // Trace saved exception handlers
            for (proc.saved_exception_handlers[0..proc.saved_context.handler_ptr]) |*handler| {
                handler.exception_class = try copyFn(handler.exception_class);
                handler.handler_block = try copyFn(handler.handler_block);
                handler.receiver = try copyFn(handler.receiver);
            }
        }
    }
};

/// Semaphore state tracked in VM
/// The actual Semaphore object is a Smalltalk LinkedList with 'signals' instvar
pub const SemaphoreState = struct {
    /// The Smalltalk Semaphore object
    object: Value,
    /// Queue of processes waiting on this semaphore
    wait_queue: ProcessList,

    pub fn init(obj: Value) SemaphoreState {
        return .{
            .object = obj,
            .wait_queue = .{},
        };
    }
};

// Field indices for Process object (must match Dolphin Process class)
// Process inherits from Link which has 'nextLink' at index 0
pub const ProcessFields = struct {
    pub const nextLink: usize = 0; // from Link superclass
    pub const suspendedFrame: usize = 1;
    pub const priority: usize = 2;
    pub const myList: usize = 3;
    pub const exceptionEnvironment: usize = 4;
    pub const name: usize = 5;
    pub const processId: usize = 6;
};

// Field indices for Semaphore object
pub const SemaphoreFields = struct {
    // LinkedList fields first (firstLink, lastLink)
    pub const firstLink: usize = 0;
    pub const lastLink: usize = 1;
    // Semaphore's own field
    pub const signals: usize = 2;
};
