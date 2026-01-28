const ffi_generated = @import("ffi_generated.zig");
const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.writeAll("=== Raylib Structs ===\n");
    if (ffi_generated.getLibraryStructNames("Raylib")) |names| {
        try stdout.print("Found {d} structs\n", .{names.len});
        for (names) |name| {
            try stdout.print("  {s}\n", .{name});
        }
    }

    try stdout.writeAll("\n=== Raylib Functions ===\n");
    if (ffi_generated.getLibraryFunctionNames("Raylib")) |names| {
        try stdout.print("Found {d} functions\n", .{names.len});
    }
}
