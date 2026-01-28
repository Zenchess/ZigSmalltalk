/// Overlapped FFI Call Support
/// Enables non-blocking FFI calls by executing them on native worker threads
/// while the calling Smalltalk process is suspended.
///
/// Architecture:
/// 1. When an overlapped FFI call is made:
///    - Current Smalltalk process is suspended
///    - A native thread is spawned to execute the FFI call
///    - When complete, a semaphore is signaled to wake the process
/// 2. This allows other Smalltalk processes to run during blocking FFI calls
const std = @import("std");
const object = @import("object.zig");
const scheduler = @import("scheduler.zig");

const Value = object.Value;
const Process = scheduler.Process;

/// Result of an overlapped FFI call
pub const OverlappedResult = struct {
    /// The result value from the FFI call
    result: Value,
    /// Error code (0 = success)
    error_code: i32,
    /// Whether the call completed
    completed: bool,
};

/// An overlapped call request
pub const OverlappedCall = struct {
    /// Function pointer to call
    func_ptr: *const anyopaque,
    /// Arguments for the call
    args: []const Value,
    /// Semaphore to signal when complete
    completion_semaphore: Value,
    /// Process that initiated the call
    calling_process: ?*Process,
    /// Result storage
    result: OverlappedResult,
    /// Thread handle
    thread: ?std.Thread,
    /// State
    state: State,

    pub const State = enum {
        pending,
        running,
        completed,
        error_state,
    };
};

/// Thread pool for overlapped FFI calls
pub const OverlappedPool = struct {
    /// Active overlapped calls
    active_calls: std.ArrayList(*OverlappedCall),
    /// Completed calls waiting to be processed
    completed_calls: std.ArrayList(*OverlappedCall),
    /// Mutex for thread-safe access
    mutex: std.Thread.Mutex,
    /// Allocator
    allocator: std.mem.Allocator,
    /// Whether the pool is running
    running: bool,

    pub fn init(allocator: std.mem.Allocator) OverlappedPool {
        return .{
            .active_calls = .{},
            .completed_calls = .{},
            .mutex = .{},
            .allocator = allocator,
            .running = true,
        };
    }

    pub fn deinit(self: *OverlappedPool) void {
        self.running = false;

        // Wait for and clean up active calls
        self.mutex.lock();
        for (self.active_calls.items) |call| {
            if (call.thread) |thread| {
                thread.join();
            }
            self.allocator.destroy(call);
        }
        self.active_calls.deinit(self.allocator);

        for (self.completed_calls.items) |call| {
            self.allocator.destroy(call);
        }
        self.completed_calls.deinit(self.allocator);
        self.mutex.unlock();
    }

    /// Submit an overlapped call
    /// The calling process will be suspended until the call completes
    pub fn submitCall(
        self: *OverlappedPool,
        func_ptr: *const anyopaque,
        args: []const Value,
        completion_semaphore: Value,
        calling_process: ?*Process,
    ) !*OverlappedCall {
        const call = try self.allocator.create(OverlappedCall);
        call.* = .{
            .func_ptr = func_ptr,
            .args = args,
            .completion_semaphore = completion_semaphore,
            .calling_process = calling_process,
            .result = .{
                .result = Value.nil,
                .error_code = 0,
                .completed = false,
            },
            .thread = null,
            .state = .pending,
        };

        self.mutex.lock();
        defer self.mutex.unlock();

        try self.active_calls.append(self.allocator, call);

        // Spawn worker thread
        call.thread = try std.Thread.spawn(.{}, workerThread, .{ self, call });
        call.state = .running;

        return call;
    }

    /// Worker thread function
    fn workerThread(self: *OverlappedPool, call: *OverlappedCall) void {
        // Execute the FFI call
        // For now, just mark as completed - actual FFI execution would go here
        // The real implementation would:
        // 1. Marshal arguments
        // 2. Call the native function
        // 3. Marshal the return value

        // Simulate work (in real implementation, this calls the actual FFI function)
        // For blocking calls like network I/O, this is where the blocking occurs
        call.result.result = Value.nil; // Would be actual result
        call.result.error_code = 0;
        call.result.completed = true;
        call.state = .completed;

        // Move to completed list
        self.mutex.lock();
        defer self.mutex.unlock();

        // Remove from active
        for (self.active_calls.items, 0..) |c, i| {
            if (c == call) {
                _ = self.active_calls.orderedRemove(i);
                break;
            }
        }

        // Add to completed
        self.completed_calls.append(self.allocator, call) catch {};
    }

    /// Check for and process completed calls
    /// Returns the semaphores that should be signaled
    pub fn processCompletedCalls(self: *OverlappedPool) ![]Value {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.completed_calls.items.len == 0) {
            return &[_]Value{};
        }

        var semaphores: std.ArrayList(Value) = .{};
        errdefer semaphores.deinit(self.allocator);

        for (self.completed_calls.items) |call| {
            if (!call.completion_semaphore.isNil()) {
                try semaphores.append(self.allocator, call.completion_semaphore);
            }
            self.allocator.destroy(call);
        }
        self.completed_calls.clearRetainingCapacity();

        return try semaphores.toOwnedSlice(self.allocator);
    }

    /// Wait for a specific call to complete (blocking)
    pub fn waitForCall(self: *OverlappedPool, call: *OverlappedCall) OverlappedResult {
        if (call.thread) |thread| {
            thread.join();
        }

        self.mutex.lock();
        defer self.mutex.unlock();

        // Remove from lists if still there
        for (self.active_calls.items, 0..) |c, i| {
            if (c == call) {
                _ = self.active_calls.orderedRemove(i);
                break;
            }
        }
        for (self.completed_calls.items, 0..) |c, i| {
            if (c == call) {
                _ = self.completed_calls.orderedRemove(i);
                break;
            }
        }

        const result = call.result;
        self.allocator.destroy(call);
        return result;
    }
};

/// FFI call wrapper that can execute synchronously or overlapped
pub const FFICallContext = struct {
    /// Whether this call should be overlapped
    overlapped: bool,
    /// Library name
    library: []const u8,
    /// Function name
    function: []const u8,
    /// Argument types
    arg_types: []const u8,
    /// Return type
    return_type: u8,

    pub fn init(library: []const u8, function: []const u8, overlapped_flag: bool) FFICallContext {
        return .{
            .overlapped = overlapped_flag,
            .library = library,
            .function = function,
            .arg_types = &[_]u8{},
            .return_type = 0,
        };
    }
};

test "overlapped pool basic" {
    const allocator = std.testing.allocator;
    var pool = OverlappedPool.init(allocator);
    defer pool.deinit();

    // Pool should start empty
    try std.testing.expect(pool.active_calls.items.len == 0);
    try std.testing.expect(pool.completed_calls.items.len == 0);
}
