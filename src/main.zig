const std = @import("std");

// VM modules
pub const object = @import("vm/object.zig");
pub const memory = @import("vm/memory.zig");
pub const bytecodes = @import("vm/bytecodes.zig");
pub const interpreter = @import("vm/interpreter.zig");
pub const primitives = @import("vm/primitives.zig");

// Compiler modules
pub const lexer = @import("compiler/lexer.zig");
pub const parser = @import("compiler/parser.zig");
pub const codegen = @import("compiler/codegen.zig");

// Image modules
pub const bootstrap = @import("image/bootstrap.zig");
pub const filein = @import("image/filein.zig");

const Value = object.Value;
const Heap = memory.Heap;
const Interpreter = interpreter.Interpreter;
const Parser = parser.Parser;
const CodeGenerator = codegen.CodeGenerator;

const banner =
    \\
    \\  ____  _         ____                  _ _ _        _ _
    \\ |_  / (_) __ _  / ___| _ __ ___   __ _| | | |_ __ _| | | __
    \\  / /  | |/ _` | \___ \| '_ ` _ \ / _` | | | __/ _` | | |/ /
    \\ / /_ _| | (_| |  ___) | | | | | | (_| | | | || (_| | |   <
    \\/___|(_)_|\__, | |____/|_| |_| |_|\__,_|_|_|\__\__,_|_|_|\_\
    \\          |___/
    \\
    \\Zig Smalltalk v0.1 - A Smalltalk implementation in Zig
    \\Type expressions to evaluate. Use 'quit' to exit.
    \\
    \\
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.fs.File.stdout();
    const stdin = std.fs.File.stdin();

    // Initialize heap (64MB)
    const heap = try Heap.init(allocator, 64 * 1024 * 1024);
    defer heap.deinit();

    // Bootstrap core classes
    try bootstrap.bootstrap(heap);

    // Create interpreter
    var interp = Interpreter.init(heap);

    // Print banner
    _ = try stdout.write(banner);

    // REPL loop
    var line_buf: [4096]u8 = undefined;

    while (true) {
        _ = try stdout.write("st> ");

        // Simple line reading using direct file operations
        var line_len: usize = 0;
        while (line_len < line_buf.len) {
            var byte: [1]u8 = undefined;
            const n = stdin.read(&byte) catch {
                _ = try stdout.write("\nGoodbye!\n");
                return;
            };
            if (n == 0) {
                _ = try stdout.write("\nGoodbye!\n");
                return;
            }
            if (byte[0] == '\n') break;
            line_buf[line_len] = byte[0];
            line_len += 1;
        }
        const line = line_buf[0..line_len];

        const trimmed = std.mem.trim(u8, line, " \t\r\n");

        if (trimmed.len == 0) continue;

        // Check for special commands
        if (std.mem.eql(u8, trimmed, "quit") or std.mem.eql(u8, trimmed, "exit")) {
            _ = try stdout.write("Goodbye!\n");
            break;
        }

        if (std.mem.eql(u8, trimmed, "help")) {
            try printHelp(stdout);
            continue;
        }

        if (std.mem.eql(u8, trimmed, "gc")) {
            _ = try stdout.write("Running garbage collection...\n");
            heap.collectGarbage() catch {
                _ = try stdout.write("GC error\n");
            };
            _ = try stdout.write("Done.\n");
            continue;
        }

        // Check for filein command
        if (std.mem.startsWith(u8, trimmed, "filein ")) {
            const path = std.mem.trim(u8, trimmed[7..], " \t");
            _ = try stdout.write("Loading ");
            _ = try stdout.write(path);
            _ = try stdout.write("...\n");

            var file_in = filein.FileIn.init(allocator, heap);
            defer file_in.deinit();

            file_in.loadFile(path) catch |err| {
                _ = try stdout.write("Error loading file: ");
                const err_msg = switch (err) {
                    filein.FileInError.FileNotFound => "File not found",
                    filein.FileInError.ClassNotFound => "Class not found",
                    filein.FileInError.InvalidMethodDefinition => "Invalid method definition",
                    filein.FileInError.CompilationFailed => "Compilation failed",
                    else => "Unknown error",
                };
                _ = try stdout.write(err_msg);
                _ = try stdout.write("\n");
                continue;
            };

            var buf: [64]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Loaded {d} methods, {d} classes.\n", .{ file_in.methods_loaded, file_in.classes_defined }) catch "Done.\n";
            _ = try stdout.write(msg);
            continue;
        }

        // Parse and evaluate expression
        const result = compileAndExecute(allocator, &interp, trimmed) catch |err| {
            _ = try stdout.write("Error: ");
            const err_msg = switch (err) {
                error.UnexpectedToken => "Unexpected token",
                error.OutOfMemory => "Out of memory",
                error.StackOverflow => "Stack overflow",
                error.StackUnderflow => "Stack underflow",
                error.MessageNotUnderstood => "Message not understood",
                error.PrimitiveFailed => "Primitive failed",
                error.InvalidBytecode => "Invalid bytecode",
                error.InvalidIndex => "Invalid index",
                error.TypeError => "Type error",
                error.BlockCannotReturn => "Block cannot return",
                else => "Unknown error",
            };
            _ = try stdout.write(err_msg);
            _ = try stdout.write("\n");
            continue;
        };

        // Print result
        try printValue(stdout, heap, result);
        _ = try stdout.write("\n");
    }
}

fn compileAndExecute(allocator: std.mem.Allocator, interp: *Interpreter, source: []const u8) !Value {
    // Parse the expression
    var p = Parser.init(allocator, source);
    const ast = try p.parseExpression();

    // Compile to bytecode
    var gen = CodeGenerator.init(allocator, interp.heap);
    defer gen.deinit();

    const method = try gen.compileDoIt(ast);

    // Execute with nil as receiver
    return try interp.execute(method, Value.nil, &[_]Value{});
}

fn printValue(file: std.fs.File, heap: *Heap, value: Value) !void {
    var buf: [256]u8 = undefined;
    if (value.isNil()) {
        _ = try file.write("nil");
    } else if (value.isTrue()) {
        _ = try file.write("true");
    } else if (value.isFalse()) {
        _ = try file.write("false");
    } else if (value.isSmallInt()) {
        const s = std.fmt.bufPrint(&buf, "{d}", .{value.asSmallInt()}) catch "?";
        _ = try file.write(s);
    } else if (value.isCharacter()) {
        const cp = value.asCharacter();
        if (cp < 128) {
            const s = std.fmt.bufPrint(&buf, "${c}", .{@as(u8, @intCast(cp))}) catch "?";
            _ = try file.write(s);
        } else {
            const s = std.fmt.bufPrint(&buf, "$\\u{{{x}}}", .{cp}) catch "?";
            _ = try file.write(s);
        }
    } else if (value.isObject()) {
        const obj = value.asObject();
        const class_index = obj.header.class_index;

        if (class_index == Heap.CLASS_STRING or class_index == Heap.CLASS_SYMBOL) {
            // Print string/symbol content
            const class = heap.getClass(class_index);
            if (class.isObject()) {
                const class_obj = class.asObject();
                const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
                if (format_val.isSmallInt()) {
                    const raw_format = format_val.asSmallInt();
                    const size: usize = @intCast(raw_format & 0xFF);
                    if (size > 0) {
                        const bytes_slice = obj.bytes(size);
                        if (class_index == Heap.CLASS_SYMBOL) {
                            _ = try file.write("#");
                        }
                        _ = try file.write("'");
                        _ = try file.write(bytes_slice);
                        _ = try file.write("'");
                        return;
                    }
                }
            }
            // Fallback
            if (class_index == Heap.CLASS_SYMBOL) {
                _ = try file.write("a Symbol");
            } else {
                _ = try file.write("a String");
            }
        } else if (class_index == Heap.CLASS_ARRAY) {
            _ = try file.write("#(...)");
        } else {
            // Generic object print
            const s = std.fmt.bufPrint(&buf, "<Object @{x}>", .{@intFromPtr(obj)}) catch "?";
            _ = try file.write(s);
        }
    } else {
        const s = std.fmt.bufPrint(&buf, "<Unknown: {x}>", .{value.bits}) catch "?";
        _ = try file.write(s);
    }
}

fn printHelp(file: std.fs.File) !void {
    _ = try file.write(
        \\
        \\Zig Smalltalk Commands:
        \\  quit, exit    - Exit the REPL
        \\  help          - Show this help
        \\  gc            - Run garbage collection
        \\
        \\Expression Examples:
        \\  3 + 4           - Arithmetic
        \\  'Hello'         - String literal
        \\  #symbol         - Symbol literal
        \\  true            - Boolean
        \\  nil             - Nil object
        \\  1 < 2           - Comparison (returns true/false)
        \\  5 * (3 + 2)     - Parenthesized expression
        \\
        \\
    );
}

// Include all tests from submodules
test {
    std.testing.refAllDecls(@This());
}
