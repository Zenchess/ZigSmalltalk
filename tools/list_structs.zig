//! Tool to list auto-generated structs for a library

const std = @import("std");
const ffi_generated = @import("../src/vm/ffi_generated.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // List Raylib structs
    try stdout.writeAll("=== Raylib Auto-Generated Structs ===\n\n");
    if (ffi_generated.getLibraryStructNames("Raylib")) |names| {
        try stdout.print("Found {d} structs:\n", .{names.len});
        for (names) |name| {
            try stdout.print("  - {s}", .{name});
            if (ffi_generated.getStructInfo("Raylib", name)) |info| {
                try stdout.print(" ({d} bytes, {d} fields)\n", .{ info.size, info.fields.len });
            } else {
                try stdout.writeAll("\n");
            }
        }
    } else {
        try stdout.writeAll("No structs found.\n");
    }

    try stdout.writeAll("\n=== Raylib Auto-Generated Functions ===\n\n");
    if (ffi_generated.getLibraryFunctionNames("Raylib")) |names| {
        try stdout.print("Found {d} functions:\n", .{names.len});
        for (names[0..@min(20, names.len)]) |name| {
            try stdout.print("  - {s}\n", .{name});
        }
        if (names.len > 20) {
            try stdout.print("  ... and {d} more\n", .{names.len - 20});
        }
    }

    // Generate Smalltalk struct code sample
    try stdout.writeAll("\n=== Sample Smalltalk Struct Code (first 3) ===\n\n");
    if (ffi_generated.getLibraryStructNames("Raylib")) |names| {
        for (names[0..@min(3, names.len)]) |name| {
            if (ffi_generated.getStructInfo("Raylib", name)) |info| {
                try stdout.print("\"Struct: {s} ({d} bytes)\"\n", .{ info.name, info.size });
                try stdout.print("ByteArray subclass: #{s}\n", .{info.name});
                try stdout.writeAll("    instanceVariableNames: ''\n");
                try stdout.writeAll("    classVariableNames: ''\n    poolDictionaries: ''!\n\n");

                try stdout.print("!{s} class methodsFor: 'instance creation'!\n", .{info.name});
                try stdout.print("new\n    ^super new: {d}!\n! !\n\n", .{info.size});

                try stdout.print("!{s} methodsFor: 'accessing'!\n", .{info.name});
                for (info.fields) |field| {
                    try stdout.print("{s}\n    \"{s} at offset {d}\"\n    ^self {s}At: {d}!\n\n", .{
                        field.name,
                        field.type_name,
                        field.offset,
                        field.accessor_type,
                        field.offset,
                    });
                    try stdout.print("{s}: value\n    self {s}At: {d} put: value!\n\n", .{
                        field.name,
                        field.accessor_type,
                        field.offset,
                    });
                }
                try stdout.writeAll("! !\n\n");
            }
        }
    }
}
