# ZigSmalltalk

A 64-bit Smalltalk implementation written in Zig, featuring a terminal-based development environment with mouse support, automatic C library binding generation at compile time, and a semi-space garbage collector.

This project was inspired by limitations encountered when attempting to load an OpenGL chess scene with 8K textures (~55MB) in Dolphin Smalltalk's 32-bit environment. ZigSmalltalk aims to be a highly performant, ANSI Smalltalk-inspired implementation that makes external interfacing straightforward through Zig's compile-time capabilities.

**Note:** This project is not based on or related to "Zag Smalltalk." The goal is not to load Squeak/Pharo images, but to provide a clean 64-bit Smalltalk with excellent FFI capabilities.

## Features

- **64-bit tagged pointer VM** with 61-bit SmallIntegers
- **Semi-space copying garbage collector** with interpreter stack tracing
- **Terminal UI (TUI)** with mouse support, multiple tabs, and syntax highlighting
- **Compile-time C FFI generation** configurable through the TUI
- **Automatic C struct bindings** with getter/setter method generation
- **OBJ file loader** for 3D model loading
- **ANSI Smalltalk compliance** (~88% of ANSI test suite passing)
- **Supports both Dolphin and ANSI Smalltalk syntax**

## Building

Requires Zig 0.13+ and a C compiler.

```bash
# Generate FFI bindings (required before first build)
zig build gen-ffi

# Build the project
zig build

# Run the REPL
zig build run

# Run the TUI
zig build run -- --tui

# Run the TUI with ANSI classes pre-loaded
./tui.sh

# Load Smalltalk files
zig build run -- file1.st file2.st

# Load from a file list
zig build run -- --load-order files.txt

# Load a saved image
zig build run -- --image snapshot.img
```

### Dependencies

System libraries that may need to be installed:
- `libffi` - For runtime FFI calls
- `glfw` - For OpenGL windowing (optional, for graphics demos)
- `glew` - OpenGL extension loading (optional)
- `raylib` - Alternative graphics library (optional)

On Arch Linux:
```bash
pacman -S libffi glfw glew
```

On Ubuntu/Debian:
```bash
apt install libffi-dev libglfw3-dev libglew-dev
```

## Terminal UI

The TUI provides an integrated development environment in the terminal with full mouse support.

### Tabs

| Key | Tab | Description |
|-----|-----|-------------|
| `F1` | Transcript | System output and messages (read-only) |
| `F2` | Workspace | Interactive code editor for evaluating expressions |
| `F3` | Browser | Class and method browser with source editing |
| `F4` | FFI Config | Configure external library bindings |

You can also use `Ctrl+1` through `Ctrl+4` to switch tabs.

### Global Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+C` or `Ctrl+Q` | Quit application |
| `F1` - `F4` | Switch tabs |
| `Ctrl+1` - `Ctrl+9` | Switch to tab N |

### Workspace Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+D` | **Do It** - Execute selected code (or current line) |
| `Ctrl+P` | **Print It** - Execute and print result to Transcript |
| `Ctrl+I` | **Inspect It** - Execute and inspect result |

### Browser Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+S` | Save method source |
| `Tab` | Switch between panes (class list, method list, source) |
| Arrow keys | Navigate lists |
| `Escape` | Exit browser |

### Text Editor Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+A` | Select all |
| `Ctrl+C` | Copy selection |
| `Ctrl+X` | Cut selection |
| `Ctrl+V` | Paste |
| `Ctrl+Arrow` | Word-based navigation |
| `Shift+Arrow` | Extend selection |
| `Home` / `End` | Line start/end |
| `Page Up` / `Page Down` | Scroll |

## FFI System

ZigSmalltalk can automatically generate bindings for C libraries at compile time.

### Configuration via TUI

The easiest way to configure FFI bindings is through the **FFI Config** tab (`F4`) in the TUI:

1. Press `F4` to open the FFI Config tab
2. Press `A` to add a new library
3. Enter the library name (e.g., `Raylib`)
4. Enter the **full path** to the header file (e.g., `C:\raylib\include\raylib.h` or `/usr/include/raylib.h`)
5. Enter the **full path** to the library file (e.g., `C:\raylib\lib\libraylib.dll.a` or `/usr/lib/libraylib.so`)
6. Press `Enter` to add, then `Ctrl+S` to save
7. Regenerate bindings and rebuild:

```bash
zig build gen-ffi
zig build
```

**Note (Windows):** Ensure the DLL file is in your PATH at runtime.

### Advanced: Direct JSON Configuration

The configuration is stored in `ffi-config.json`:

```json
{
  "libraries": [
    {
      "name": "Raylib",
      "headers": ["C:\\raylib\\include\\raylib.h"],
      "link": "C:\\raylib\\lib\\libraylib.dll.a",
      "enabled": true
    }
  ]
}
```

After editing, regenerate bindings:

```bash
zig build gen-ffi
zig build
```

### Calling C Functions from Smalltalk

```smalltalk
"Call a C function"
'GLFW' ffiCall: #glfwInit with: {}.

"Call with arguments"
'LibC' ffiCall: #puts with: { 'Hello from C!' }.

"Store returned pointer"
window := 'GLFW' ffiCall: #glfwCreateWindow with: { 800. 600. 'Window'. 0. 0 }.
```

### C Struct Support

Generate Smalltalk wrapper classes for C structures:

```bash
zig build run -- --gen-structs GLFW > glfw-structs.st
```

This generates ExternalStructure subclasses with accessor methods:

```smalltalk
"Access struct fields"
pos := GLFWvidmode new.
width := pos width.    "Getter"
pos width: 1920.       "Setter"
```

The struct wrappers use ByteArray primitives for field access:
- `uint8At:`, `uint16At:`, `uint32At:`, `uint64At:` - Unsigned reads
- `int8At:`, `int16At:`, `int32At:`, `int64At:` - Signed reads
- `float32At:`, `float64At:` - Floating point reads
- Corresponding `put:` variants for writes
- `address` - Get pointer to ByteArray data for passing to C

## VM Architecture

### Value Encoding

64-bit tagged pointers using the lowest 3 bits:
- `xxx000` - Heap object pointer (8-byte aligned)
- `xxx001` - SmallInteger (61-bit signed)
- `xxx010` - Character (Unicode codepoint)
- `xxx110` - Special constants (nil, true, false)

### Object Layout

Objects have a 16-byte header followed by fields or bytes:
```
| class_index (4) | hash (4) | flags (1) | padding (3) | size (4) |
| field_0 | field_1 | ... | field_n |
```

### Garbage Collector

Semi-space copying collector using Cheney's algorithm:
- Two equal-sized memory spaces (default 256MB each)
- Traces interpreter stack, contexts, exception handlers as roots
- Forwarding pointers for object relocation
- C pointers stored as SmallInts are not traced (safe for FFI)

### Bytecode Interpreter

Stack-based interpreter with ~180 opcodes:
- Push/store operations for variables and literals
- Message sends with selector lookup
- Block closure creation and evaluation
- Non-local returns
- Exception handling

### Primitives

150+ primitives including:
- Arithmetic: `+`, `-`, `*`, `/`, `//`, `\\`, `bitAnd:`, `bitOr:`, `bitShift:`
- Comparison: `<`, `>`, `<=`, `>=`, `=`, `~=`
- Object: `at:`, `at:put:`, `size`, `basicNew`, `basicNew:`, `class`, `hash`
- Block: `value`, `value:`, `whileTrue:`, `whileFalse:`
- Collection: `do:`, `collect:`, `select:`, `inject:into:`
- FFI: ByteArray access primitives for struct manipulation

## Implemented Features

### Core Classes (50+)
- Object, Class, Metaclass, Behavior
- SmallInteger, Float, String, Symbol, Character
- Array, ByteArray, OrderedCollection, Set, Dictionary
- CompiledMethod, BlockClosure, Context
- True, False, UndefinedObject
- Exception, Error hierarchy
- ReadStream, WriteStream, FileStream
- Association, Message

### Smalltalk Syntax
- Class definitions with instance/class variables
- Method definitions with primitives
- Block closures with parameters
- Cascade messages (`;`)
- Non-local returns (`^`)
- Exception handling (`on:do:`)

### File Format

Supports Dolphin/Squeak chunk file format (`.st` files):

```smalltalk
Object subclass: #MyClass
    instanceVariableNames: 'x y'
    classVariableNames: ''
    poolDictionaries: ''!

!MyClass methodsFor!
initialize
    x := 0.
    y := 0!

add: n
    ^x + y + n!
!
```

## Project Structure

```
src/
  vm/
    object.zig        - Value encoding, object structures
    memory.zig        - Heap, garbage collector, symbol table
    bytecodes.zig     - Opcode definitions
    interpreter.zig   - Bytecode interpreter, method dispatch
    primitives.zig    - Built-in primitive operations
    ffi_runtime.zig   - Runtime FFI with libffi
    ffi_generated.zig - Auto-generated library bindings
    ffi_autogen.zig   - Comptime struct binding generation
    obj_loader.zig    - Wavefront OBJ file parser
  compiler/
    lexer.zig         - Tokenizer
    parser.zig        - Smalltalk parser
    codegen.zig       - Bytecode generation
  image/
    bootstrap.zig     - Core class creation
    filein.zig        - Chunk file loader
    snapshot.zig      - Image persistence
  tui/
    app.zig           - Main TUI application
    input.zig         - Keyboard/mouse input
    tabs/             - Tab implementations
    widgets/          - UI components
tools/
  gen_ffi.zig         - FFI binding generator
```

## Future Plans

- **JIT Compilation** - Compile hot methods to native code
- Improved debugging tools
- Image snapshots and persistence
- Additional ANSI Smalltalk compliance

## Contributing

Contributions and pull requests are welcome. Please open an issue to discuss major changes before submitting.

## License

MIT License - see [LICENSE](LICENSE) file.

### Third-Party Licenses

This project includes the ANSI Smalltalk class library from **Dolphin Smalltalk** by Object Arts, which is also MIT licensed. See [LICENSES/DOLPHIN-SMALLTALK.txt](LICENSES/DOLPHIN-SMALLTALK.txt) for details.

## Acknowledgments

This project uses the **ANSI Smalltalk class library** from [Dolphin Smalltalk](https://github.com/dolphinsmalltalk/Dolphin) by Object Arts. The class files in `ansi-tests/` are derived from Dolphin's implementation of the ANSI Smalltalk standard.

The VM implementation takes advantage of Zig's systems programming capabilities for performance and C interoperability.
