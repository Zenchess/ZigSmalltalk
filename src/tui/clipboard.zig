const std = @import("std");
const builtin = @import("builtin");

const is_windows = builtin.os.tag == .windows;
const posix = if (!is_windows) std.posix else undefined;

/// Cross-platform clipboard support
/// Uses OSC 52 escape sequences (works with most modern terminals)
/// Falls back to system utilities (wl-copy/xclip/xsel) if OSC 52 fails

fn getStdout() std.fs.File {
    if (is_windows) {
        const handle = std.os.windows.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) catch unreachable;
        return std.fs.File{ .handle = handle };
    } else {
        return std.fs.File{ .handle = posix.STDOUT_FILENO };
    }
}

pub const ClipboardError = error{
    CommandFailed,
    NoClipboardTool,
    OutOfMemory,
    Timeout,
};

// Module-level TTY file for OSC 52 operations
var g_tty: ?std.fs.File = null;

/// Initialize clipboard with TTY file for OSC 52 support
pub fn init(tty: std.fs.File) void {
    g_tty = tty;
}

/// Copy text to system clipboard
pub fn copy(text: []const u8) ClipboardError!void {
    // Try OSC 52 first (works without external tools)
    if (tryOSC52Copy(text)) return;
    // Fall back to external tools
    if (tryWaylandCopy(text)) return;
    if (tryXclipCopy(text)) return;
    if (tryXselCopy(text)) return;
    return ClipboardError.NoClipboardTool;
}

/// Paste text from system clipboard
pub fn paste(allocator: std.mem.Allocator) ClipboardError![]u8 {
    // Try OSC 52 first
    if (tryOSC52Paste(allocator)) |text| return text;
    // Fall back to external tools
    if (tryWaylandPaste(allocator)) |text| return text;
    if (tryXclipPaste(allocator)) |text| return text;
    if (tryXselPaste(allocator)) |text| return text;
    return ClipboardError.NoClipboardTool;
}

/// Paste from primary selection (middle-click)
pub fn pastePrimary(allocator: std.mem.Allocator) ClipboardError![]u8 {
    // Try OSC 52 primary selection first
    if (tryOSC52PastePrimary(allocator)) |text| return text;
    // Fall back to external tools
    if (tryWaylandPastePrimary(allocator)) |text| return text;
    if (tryXclipPastePrimary(allocator)) |text| return text;
    if (tryXselPastePrimary(allocator)) |text| return text;
    return ClipboardError.NoClipboardTool;
}

// ============================================================================
// OSC 52 Implementation (Terminal escape sequences)
// ============================================================================

fn tryOSC52Copy(text: []const u8) bool {
    _ = g_tty orelse return false; // Just check if initialized

    // Base64 encode the text
    const base64 = std.base64.standard;
    const encoded_len = base64.Encoder.calcSize(text.len);

    var buf: [8192]u8 = undefined;
    if (encoded_len > buf.len - 10) return false; // Too large

    // Build OSC 52 sequence: \x1b]52;c;<base64>\x07
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    writer.writeAll("\x1b]52;c;") catch return false;
    _ = base64.Encoder.encode(buf[7..][0..encoded_len], text);
    stream.pos = 7 + encoded_len;
    writer.writeAll("\x07") catch return false;

    // Write to stdout (which goes to the terminal)
    const stdout = getStdout();
    _ = stdout.write(buf[0..stream.pos]) catch return false;
    return true;
}

fn tryOSC52Paste(allocator: std.mem.Allocator) ?[]u8 {
    return tryOSC52PasteSelection(allocator, "c"); // clipboard
}

fn tryOSC52PastePrimary(allocator: std.mem.Allocator) ?[]u8 {
    return tryOSC52PasteSelection(allocator, "p"); // primary
}

fn tryOSC52PasteSelection(allocator: std.mem.Allocator, selection: []const u8) ?[]u8 {
    const tty = g_tty orelse return null;

    // Send query: \x1b]52;<selection>;?\x07
    var query_buf: [16]u8 = undefined;
    const query = std.fmt.bufPrint(&query_buf, "\x1b]52;{s};?\x07", .{selection}) catch return null;
    _ = tty.write(query) catch return null;

    // Read response with timeout
    var response_buf: [16384]u8 = undefined;
    var total_read: usize = 0;

    // Set a short timeout for reading
    const start_time = std.time.milliTimestamp();
    const timeout_ms: i64 = 100;

    while (total_read < response_buf.len) {
        const elapsed = std.time.milliTimestamp() - start_time;
        if (elapsed > timeout_ms) break;

        const bytes_read = tty.read(response_buf[total_read..]) catch |err| {
            if (err == error.WouldBlock) {
                std.Thread.sleep(5 * std.time.ns_per_ms);
                continue;
            }
            break;
        };

        if (bytes_read == 0) {
            std.Thread.sleep(5 * std.time.ns_per_ms);
            continue;
        }

        total_read += bytes_read;

        // Check if we have the terminator
        if (std.mem.indexOfScalar(u8, response_buf[0..total_read], 0x07) != null) break;
        if (std.mem.indexOf(u8, response_buf[0..total_read], "\x1b\\") != null) break;
    }

    if (total_read == 0) return null;

    // Parse response: \x1b]52;c;<base64>\x07 or \x1b]52;c;<base64>\x1b\\
    const response = response_buf[0..total_read];

    // Find the start of base64 data (after "52;X;")
    const prefix = std.fmt.bufPrint(&query_buf, "\x1b]52;{s};", .{selection}) catch return null;
    const start = std.mem.indexOf(u8, response, prefix) orelse return null;
    const data_start = start + prefix.len;

    // Find the end (either \x07 or \x1b\\)
    var data_end = std.mem.indexOfScalar(u8, response[data_start..], 0x07);
    if (data_end == null) {
        data_end = std.mem.indexOf(u8, response[data_start..], "\x1b\\");
    }
    if (data_end == null) return null;

    const base64_data = response[data_start..][0..data_end.?];
    if (base64_data.len == 0) return null;

    // Check for "?" which means paste not supported
    if (std.mem.eql(u8, base64_data, "?")) return null;

    // Decode base64
    const base64 = std.base64.standard;
    const decoded_len = base64.Decoder.calcSizeForSlice(base64_data) catch return null;
    const decoded = allocator.alloc(u8, decoded_len) catch return null;

    base64.Decoder.decode(decoded, base64_data) catch {
        allocator.free(decoded);
        return null;
    };

    return decoded;
}

// ============================================================================
// External Tool Fallbacks
// ============================================================================

fn tryWaylandCopy(text: []const u8) bool {
    var child = std.process.Child.init(&.{ "wl-copy", "--" }, std.heap.page_allocator);
    child.stdin_behavior = .Pipe;
    child.spawn() catch return false;

    if (child.stdin) |stdin| {
        _ = stdin.write(text) catch {};
        stdin.close();
    }

    _ = child.wait() catch return false;
    return true;
}

fn tryXclipCopy(text: []const u8) bool {
    var child = std.process.Child.init(&.{ "xclip", "-selection", "clipboard" }, std.heap.page_allocator);
    child.stdin_behavior = .Pipe;
    child.spawn() catch return false;

    if (child.stdin) |stdin| {
        _ = stdin.write(text) catch {};
        stdin.close();
    }

    _ = child.wait() catch return false;
    return true;
}

fn tryXselCopy(text: []const u8) bool {
    var child = std.process.Child.init(&.{ "xsel", "--clipboard", "--input" }, std.heap.page_allocator);
    child.stdin_behavior = .Pipe;
    child.spawn() catch return false;

    if (child.stdin) |stdin| {
        _ = stdin.write(text) catch {};
        stdin.close();
    }

    _ = child.wait() catch return false;
    return true;
}

fn tryWaylandPaste(allocator: std.mem.Allocator) ?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "wl-paste", "--no-newline" },
    }) catch return null;

    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        return result.stdout;
    }
    allocator.free(result.stdout);
    return null;
}

fn tryWaylandPastePrimary(allocator: std.mem.Allocator) ?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "wl-paste", "--no-newline", "--primary" },
    }) catch return null;

    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        return result.stdout;
    }
    allocator.free(result.stdout);
    return null;
}

fn tryXclipPaste(allocator: std.mem.Allocator) ?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "xclip", "-selection", "clipboard", "-o" },
    }) catch return null;

    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        return result.stdout;
    }
    allocator.free(result.stdout);
    return null;
}

fn tryXclipPastePrimary(allocator: std.mem.Allocator) ?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "xclip", "-selection", "primary", "-o" },
    }) catch return null;

    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        return result.stdout;
    }
    allocator.free(result.stdout);
    return null;
}

fn tryXselPaste(allocator: std.mem.Allocator) ?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "xsel", "--clipboard", "--output" },
    }) catch return null;

    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        return result.stdout;
    }
    allocator.free(result.stdout);
    return null;
}

fn tryXselPastePrimary(allocator: std.mem.Allocator) ?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "xsel", "--primary", "--output" },
    }) catch return null;

    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        return result.stdout;
    }
    allocator.free(result.stdout);
    return null;
}
