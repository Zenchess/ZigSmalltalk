# Zig Smalltalk

A 64-bit Smalltalk implementation written in Zig, featuring a terminal-based development environment with mouse support, automatic C library binding generation at compile time, and a semi-space garbage collector.

This project was inspired by limitations encountered when attempting to load an OpenGL chess scene with 8K textures (~55MB) in Dolphin Smalltalk's 32-bit environment. ZigSmalltalk aims to be a highly performant, ANSI Smalltalk-inspired implementation that makes external interfacing straightforward through Zig's compile-time capabilities.

**Note:** This project is not based on or related to "Zag Smalltalk." The goal is not to load Squeak/Pharo images, but to provide a clean 64-bit Smalltalk with excellent FFI capabilities.

Currently working:  Terminal editor with workspace/transcript/ffi page/browser.  FFI auto generation works, tested on OpenGL and Raylib.  

Benchmarks are around 20,000,000 message sends/second on some benchmarks, still slower than Cog/Visualworks but this is the current focus of development for more speed, utilizing more advanced Jit compilation techniques.

## Platforms
Linux, Windows (could run on mac maybe but haven't tested)

## Features

- **64-bit tagged pointer VM** with 61-bit SmallIntegers
- **Semi-space copying garbage collector** with interpreter stack tracing
- **Terminal UI (TUI)** with mouse support, multiple tabs, and syntax highlighting
- **Compile-time C FFI generation** configurable through the TUI
- **Automatic C struct bindings** with getter/setter method generation
- **ANSI Smalltalk compliance** (~100% of ANSI test suite passing)
- **Supports ANSI Smalltalk syntax**

## Building

Requires Zig 0.15+ and a C compiler.

```bash
# Build the project (FFI bindings generated automatically)
zig build

# Run the REPL
./tui.bat (windows)  ./tui.sh (Linux)




# Load Smalltalk files
zig build run -- file1.st file2.st

# Load from a file list
zig build run -- --load-order files.txt

# Load a saved image
zig build run -- --image snapshot.img
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



## FFI System

ZigSmalltalk can automatically generate bindings for C libraries at compile time.

### Configuration via TUI

The easiest way to configure FFI bindings is through the **FFI Config** tab (`F4`) in the TUI:

1. Press `F4` to open the FFI Config tab
2. Press `A` to add a new library
3. Enter the library name (e.g., `Raylib`)
4. Enter the **full path** to the header file (e.g., `c:/raylib/include/raylib.h` or `/usr/include/raylib.h`)
5. Enter the **full path** to the static library (e.g., `c:/raylib/lib/libraylib.a` or `/usr/lib/libraylib.a`)
6. Press `Enter` to add, then `Ctrl+S` to save
7. Rebuild (bindings are generated automatically):

```bash
zig build gen-ffi
zig build
```

**Note:** Using the static library (`.a`) is recommended as it links Raylib directly into the executable with no runtime dependencies.

### Advanced: Direct JSON Configuration

The configuration is stored in `ffi-config.json`:

```json
{
  "libraries": [
    {
      "name": "Raylib",
      "headers": ["c:/raylib/include/raylib.h"],
      "link": "c:/raylib/lib/libraylib.a",
      "enabled": true,
      "functions": ["InitWindow", "CloseWindow", "BeginDrawing", "EndDrawing", "ClearBackground"],
      "structs": ["Vector2", "Vector3", "Color", "Rectangle", "Camera3D"]
    }
  ]
}
```

The `functions` array lists C functions to expose, and `structs` lists C structures to generate Smalltalk wrapper classes for.



### Calling C Functions from Smalltalk

Example with Raylib

Raylib InitWindow: 800 with: 600 and: 'Raylib Window'.
Check the FFI package for methods generated on your library name, and External Structures you can use (variables generated, for instance you can send a Vector2 to raylib with:
Vector2 new x: 500; y: 500; yourself


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

## Contributing

Contributions and pull requests are welcome. Please open an issue to discuss major changes before submitting.

## License

MIT License - see [LICENSE](LICENSE) file.

### Third-Party Licenses

This project includes the ANSI Smalltalk class library from **Dolphin Smalltalk** by Object Arts, which is also MIT licensed. See [LICENSES/DOLPHIN-SMALLTALK.txt](LICENSES/DOLPHIN-SMALLTALK.txt) for details.

## Acknowledgments

This project uses the **ANSI Smalltalk class library** from [Dolphin Smalltalk](https://github.com/dolphinsmalltalk/Dolphin) by Object Arts. The class files in `ansi-tests/` are derived from Dolphin's implementation of the ANSI Smalltalk standard.

The VM implementation takes advantage of Zig's systems programming capabilities for performance and C interoperability.

