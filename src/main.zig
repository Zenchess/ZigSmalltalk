const std = @import("std");

// VM modules
pub const object = @import("vm/object.zig");
pub const memory = @import("vm/memory.zig");
pub const bytecodes = @import("vm/bytecodes.zig");
pub const interpreter = @import("vm/interpreter.zig");
pub const primitives = @import("vm/primitives.zig");
pub const debugger = @import("vm/debugger.zig");

// Compiler modules
pub const lexer = @import("compiler/lexer.zig");
pub const parser = @import("compiler/parser.zig");
pub const codegen = @import("compiler/codegen.zig");

// Image modules
pub const bootstrap = @import("image/bootstrap.zig");
pub const filein = @import("image/filein.zig");
pub const snapshot = @import("image/snapshot.zig");

// TUI module
pub const tui = @import("tui/app.zig");

// FFI module
pub const ffi_gen = @import("vm/ffi_generated.zig");

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
    const allocator = std.heap.c_allocator;

    const stdout = std.fs.File.stdout();
    const stdin = std.fs.File.stdin();

    // Process command line arguments for files to load
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Handle --gen-structs early (doesn't need heap)
    for (args, 0..) |arg, i| {
        if (std.mem.eql(u8, arg, "--gen-structs") and i + 1 < args.len) {
            const lib_name = args[i + 1];

            // Write the base ExternalStructure class first
            _ = try stdout.write(
                \\!ByteArray methodsFor!
                \\
                \\uint8At: offset
                \\    "Read unsigned 8-bit value at 0-based byte offset"
                \\    <primitive: 770>
                \\    ^self primitiveFailed!
                \\
                \\uint16At: offset
                \\    "Read unsigned 16-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 771>
                \\    ^self primitiveFailed!
                \\
                \\uint32At: offset
                \\    "Read unsigned 32-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 772>
                \\    ^self primitiveFailed!
                \\
                \\uint64At: offset
                \\    "Read unsigned 64-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 778>
                \\    ^self primitiveFailed!
                \\
                \\int8At: offset
                \\    "Read signed 8-bit value at 0-based byte offset"
                \\    <primitive: 773>
                \\    ^self primitiveFailed!
                \\
                \\int16At: offset
                \\    "Read signed 16-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 774>
                \\    ^self primitiveFailed!
                \\
                \\int32At: offset
                \\    "Read signed 32-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 775>
                \\    ^self primitiveFailed!
                \\
                \\int64At: offset
                \\    "Read signed 64-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 779>
                \\    ^self primitiveFailed!
                \\
                \\float32At: offset
                \\    "Read 32-bit float value at 0-based byte offset"
                \\    <primitive: 776>
                \\    ^self primitiveFailed!
                \\
                \\float64At: offset
                \\    "Read 64-bit float value at 0-based byte offset"
                \\    <primitive: 777>
                \\    ^self primitiveFailed!
                \\
                \\uint8At: offset put: value
                \\    "Write unsigned 8-bit value at 0-based byte offset"
                \\    <primitive: 780>
                \\    ^self primitiveFailed!
                \\
                \\uint16At: offset put: value
                \\    "Write unsigned 16-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 781>
                \\    ^self primitiveFailed!
                \\
                \\uint32At: offset put: value
                \\    "Write unsigned 32-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 782>
                \\    ^self primitiveFailed!
                \\
                \\uint64At: offset put: value
                \\    "Write unsigned 64-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 789>
                \\    ^self primitiveFailed!
                \\
                \\int8At: offset put: value
                \\    "Write signed 8-bit value at 0-based byte offset"
                \\    <primitive: 783>
                \\    ^self primitiveFailed!
                \\
                \\int16At: offset put: value
                \\    "Write signed 16-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 784>
                \\    ^self primitiveFailed!
                \\
                \\int32At: offset put: value
                \\    "Write signed 32-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 785>
                \\    ^self primitiveFailed!
                \\
                \\int64At: offset put: value
                \\    "Write signed 64-bit value at 0-based byte offset (little-endian)"
                \\    <primitive: 793>
                \\    ^self primitiveFailed!
                \\
                \\float32At: offset put: value
                \\    "Write 32-bit float value at 0-based byte offset"
                \\    <primitive: 786>
                \\    ^self primitiveFailed!
                \\
                \\float64At: offset put: value
                \\    "Write 64-bit float value at 0-based byte offset"
                \\    <primitive: 787>
                \\    ^self primitiveFailed!
                \\
                \\address
                \\    "Return pointer to the byte data as an integer (for FFI)"
                \\    <primitive: 788>
                \\    ^self primitiveFailed!
                \\
                \\!
                \\
                \\Object subclass: #ExternalStructure
                \\    instanceVariableNames: 'bytes'
                \\    classVariableNames: ''
                \\    poolDictionaries: ''
                \\    classInstanceVariableNames: ''!
                \\
                \\!ExternalStructure class methodsFor!
                \\
                \\new
                \\    "Create a new instance with a properly sized byte buffer"
                \\    ^self basicNew initialize!
                \\
                \\byteSize
                \\    "Subclasses must override to return their struct size in bytes"
                \\    ^0!
                \\
                \\fromBytes: aByteArray
                \\    "Create an instance wrapping existing byte data"
                \\    ^self basicNew bytes: aByteArray!
                \\
                \\!
                \\
                \\!ExternalStructure methodsFor!
                \\
                \\initialize
                \\    "Initialize with a zero-filled byte buffer of the correct size"
                \\    bytes := ByteArray new: self class byteSize.
                \\    ^self!
                \\
                \\bytes
                \\    "Return the raw byte data"
                \\    ^bytes!
                \\
                \\bytes: aByteArray
                \\    "Set the raw byte data"
                \\    bytes := aByteArray.
                \\    ^self!
                \\
                \\byteSize
                \\    "Return the size in bytes of this structure"
                \\    ^self class byteSize!
                \\
                \\address
                \\    "Return the memory address of the byte data (for FFI calls)"
                \\    ^bytes address!
                \\
                \\copy
                \\    "Return a copy of this structure with its own byte buffer"
                \\    ^self class fromBytes: bytes copy!
                \\
                \\uint8At: offset ^bytes uint8At: offset!
                \\uint16At: offset ^bytes uint16At: offset!
                \\uint32At: offset ^bytes uint32At: offset!
                \\uint64At: offset ^bytes uint64At: offset!
                \\int8At: offset ^bytes int8At: offset!
                \\int16At: offset ^bytes int16At: offset!
                \\int32At: offset ^bytes int32At: offset!
                \\int64At: offset ^bytes int64At: offset!
                \\float32At: offset ^bytes float32At: offset!
                \\float64At: offset ^bytes float64At: offset!
                \\uint8At: offset put: value ^bytes uint8At: offset put: value!
                \\uint16At: offset put: value ^bytes uint16At: offset put: value!
                \\uint32At: offset put: value ^bytes uint32At: offset put: value!
                \\uint64At: offset put: value ^bytes uint64At: offset put: value!
                \\int8At: offset put: value ^bytes int8At: offset put: value!
                \\int16At: offset put: value ^bytes int16At: offset put: value!
                \\int32At: offset put: value ^bytes int32At: offset put: value!
                \\int64At: offset put: value ^bytes int64At: offset put: value!
                \\float32At: offset put: value ^bytes float32At: offset put: value!
                \\float64At: offset put: value ^bytes float64At: offset put: value!
                \\
                \\!
                \\
                \\
            );

            // Generate struct code for the library
            // Use a fixed buffer for struct output
            var struct_buf: [1024 * 1024]u8 = undefined; // 1MB buffer
            var fbs = std.io.fixedBufferStream(&struct_buf);
            const struct_writer = fbs.writer();

            const success = ffi_gen.generateStructCode(lib_name, struct_writer) catch {
                std.debug.print("Error generating struct code for {s}\n", .{lib_name});
                return;
            };

            // Write generated struct code to stdout
            _ = try stdout.write(fbs.getWritten());

            if (!success) {
                std.debug.print("Unknown library: {s}\n", .{lib_name});
                std.debug.print("Available libraries with struct support: Raylib\n", .{});
                return;
            }

            return;
        }
    }

    // Handle --image <path> and --load-order <path> flags
    var heap: *Heap = undefined;
    var loaded_from_image = false;
    var load_order_path: ?[]const u8 = null;
    var repl_mode = true;
    var tui_mode = false;
    var arg_index: usize = 1;
    while (arg_index < args.len) : (arg_index += 1) {
        if (std.mem.eql(u8, args[arg_index], "--image") and arg_index + 1 < args.len) {
            const img_path = args[arg_index + 1];
            heap = snapshot.loadFromFile(allocator, img_path) catch {
                std.debug.print("Failed to load image {s}, falling back to bootstrap\n", .{img_path});
                loaded_from_image = false;
                break;
            };
            loaded_from_image = true;
            arg_index += 1; // skip path
        } else if (std.mem.eql(u8, args[arg_index], "--load-order") and arg_index + 1 < args.len) {
            load_order_path = args[arg_index + 1];
            arg_index += 1; // skip path
        } else if (std.mem.eql(u8, args[arg_index], "--no-repl")) {
            repl_mode = false;
        } else if (std.mem.eql(u8, args[arg_index], "--tui")) {
            tui_mode = true;
        }
    }

    if (!loaded_from_image) {
        // Initialize heap (64MB default)
        heap = try Heap.init(allocator, 512 * 1024 * 1024); // 512MB to reduce GC frequency
        // Bootstrap core classes
        try bootstrap.bootstrap(heap);
    }
    try bootstrap.ensureCorePrimitives(heap);
    defer heap.deinit();

    // Create interpreter
    var interp = Interpreter.init(heap);
    // Register interpreter with heap for GC stack tracing
    heap.interpreter = &interp;

    // Process load-order file if specified
    if (load_order_path) |order_path| {
        const order_file = std.fs.cwd().openFile(order_path, .{}) catch |err| {
            std.debug.print("Failed to open load-order file {s}: {any}\n", .{ order_path, err });
            return;
        };
        defer order_file.close();

        const order_content = order_file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch {
            std.debug.print("Failed to read load-order file\n", .{});
            return;
        };
        defer allocator.free(order_content);

        // Process each line as a file path
        var lines = std.mem.splitAny(u8, order_content, "\n\r");
        while (lines.next()) |line| {
            const trimmed_line = std.mem.trim(u8, line, " \t");
            // Skip empty lines and comments
            if (trimmed_line.len == 0) continue;
            if (trimmed_line[0] == '#') continue;

            var file_in = filein.FileIn.init(allocator, heap);
            defer file_in.deinit();

            file_in.loadFile(trimmed_line) catch |err| {
                if (!tui_mode) {
                    const err_msg = switch (err) {
                        filein.FileInError.FileNotFound => "File not found",
                        filein.FileInError.ClassNotFound => "Class not found",
                        filein.FileInError.InvalidMethodDefinition => "Invalid method definition",
                        filein.FileInError.CompilationFailed => "Compilation failed",
                        else => "Unknown error",
                    };
                    std.debug.print("Loading: {s} - Error: {s}\n", .{ trimmed_line, err_msg });
                }
                continue;
            };
        }
    }

    var skip_next: bool = false;
    for (args[1..]) |arg| {
        if (skip_next) {
            skip_next = false;
            continue;
        }
        // Skip flags
        if (arg.len > 0 and arg[0] == '-') {
            if (std.mem.eql(u8, arg, "--image") or std.mem.eql(u8, arg, "--load-order")) {
                // Skip the following path (already handled in the earlier loop)
                skip_next = true;
                continue;
            }
            if (std.mem.eql(u8, arg, "--tui") or std.mem.eql(u8, arg, "--no-repl")) {
                continue;
            }
            continue;
        }

        // Load file (silently for TUI mode)
        var file_in = filein.FileIn.init(allocator, heap);
        defer file_in.deinit();

        file_in.loadFile(arg) catch |err| {
            if (!tui_mode) {
                const err_msg = switch (err) {
                    filein.FileInError.FileNotFound => "File not found",
                    filein.FileInError.ClassNotFound => "Class not found",
                    filein.FileInError.InvalidMethodDefinition => "Invalid method definition",
                    filein.FileInError.CompilationFailed => "Compilation failed",
                    else => "Unknown error",
                };
                std.debug.print("Error loading {s}: {s}\n", .{ arg, err_msg });
            }
            continue;
        };
    }

    // Re-register main interpreter with heap after file loading
    // (FileIn may have temporarily registered its own interpreter)
    heap.interpreter = &interp;

    if (!repl_mode) {
        return;
    }

    // TUI mode
    if (tui_mode) {
        try tui.runTUI(allocator, heap, &interp);
        return;
    }

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

        if (std.mem.eql(u8, trimmed, "debug")) {
            if (debugger.debugEnabled) {
                _ = try stdout.write("Disabling debugger.\n");
                debugger.disableDebugger();
            } else {
                _ = try stdout.write("Enabling debugger. Next expression will start in step mode.\n");
                _ = debugger.enableDebugger(&interp);
            }
            continue;
        }

        if (std.mem.startsWith(u8, trimmed, "saveimage")) {
            var tok = std.mem.tokenizeAny(u8, trimmed, " \t");
            _ = tok.next(); // command
            const path = tok.next() orelse "zig-smalltalk.img";
            _ = try stdout.write("Saving image to ");
            _ = try stdout.write(path);
            _ = try stdout.write("...\n");
            snapshot.saveToFile(heap, allocator, path) catch {
                _ = try stdout.write("Failed to save image\n");
                continue;
            };
            _ = try stdout.write("Image saved.\n");
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
            if (err == error.MessageNotUnderstood) {
                _ = try stdout.write(" (selector: ");
                const sel = if (std.mem.eql(u8, interp.last_mnu_selector, "<?>"))
                    interp.last_send_selector
                else
                    interp.last_mnu_selector;
                _ = try stdout.write(sel);
                _ = try stdout.write(")");
                // Print receiver class for easier debugging
                const recv_class = interp.heap.classOf(interp.last_mnu_receiver);
                if (recv_class.isObject()) {
                    const rc_obj = recv_class.asObject();
                    const rc_name_val = rc_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                    if (rc_name_val.isObject()) {
                        const rc_name_obj = rc_name_val.asObject();
                        if (rc_name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                            _ = try stdout.write(" on ");
                            _ = try stdout.write(rc_name_obj.bytes(rc_name_obj.header.size));
                        }
                    }
                }
                if (!std.mem.eql(u8, interp.last_mnu_method, "<?>")) {
                    _ = try stdout.write(" raised in ");
                    _ = try stdout.write(interp.last_mnu_method);
                }
                _ = try stdout.write(" in method: ");
                const meth = interp.method;
                const lits = meth.getLiterals();
                if (lits.len > 0 and lits[lits.len-1].isObject()) {
                    const obj = lits[lits.len-1].asObject();
                    if (obj.header.class_index == Heap.CLASS_STRING) {
                        _ = try stdout.write(obj.bytes(obj.header.size));
                    }
                } else {
                    _ = try stdout.write("<unknown>");
                }
            }
            _ = try stdout.write("\n");
            continue;
        };

        // Print result
        try printValue(stdout, heap, result);
        _ = try stdout.write("\n");
    }
}

fn compileAndExecute(allocator: std.mem.Allocator, interp: *Interpreter, source: []const u8) !Value {
    // Use a short-lived arena so temporary compiler allocations (including the
    // compiled method) are cleaned up after execution
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const temp_alloc = arena.allocator();

    // Parse the expression
    var p = Parser.init(temp_alloc, source);
    const ast = try p.parseExpression();

    // Compile to bytecode
    var gen = CodeGenerator.init(temp_alloc, interp.heap, temp_alloc);
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
            // Print string/symbol content - use the object's header size, not class format
            const size = obj.header.size;
            if (size > 0) {
                const bytes_slice = obj.bytes(size);
                if (class_index == Heap.CLASS_SYMBOL) {
                    _ = try file.write("#");
                }
                _ = try file.write("'");
                _ = try file.write(bytes_slice);
                _ = try file.write("'");
            } else {
                // Empty string/symbol
                if (class_index == Heap.CLASS_SYMBOL) {
                    _ = try file.write("#''");
                } else {
                    _ = try file.write("''");
                }
            }
        } else if (class_index == Heap.CLASS_ARRAY) {
            _ = try file.write("#(...)");
        } else {
            // Generic object print - try to get class name
            const class = heap.getClass(class_index);
            var class_name: []const u8 = "Object";
            if (class.isObject()) {
                const class_obj = class.asObject();
                const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                if (name_val.isObject()) {
                    const name_obj = name_val.asObject();
                    if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                        class_name = name_obj.bytes(name_obj.header.size);
                    }
                }
            }
            const s = std.fmt.bufPrint(&buf, "<{s} @{x}>", .{ class_name, @intFromPtr(obj) }) catch "?";
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
        \\  debug         - Toggle debugger (step-by-step execution)
        \\  saveimage <path> - Save image snapshot
        \\  filein <path> - Load Smalltalk source file
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
