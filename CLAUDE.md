# ZigSmalltalk - Claude Code Reference

## Overview
A 64-bit Smalltalk implementation written in Zig with a terminal-based IDE. Features a bytecode interpreter, semi-space copying GC, JIT compiler, and compile-time FFI generation.

## Build & Run
```bash
zig build              # Build the VM with FFI support
zig build -Dno-ffi     # Build without FFI (for cross-platform CI)
zig build run          # Run the TUI
zig build test         # Run Zig unit tests
./zig-out/bin/zig-smalltalk file.st  # Load Smalltalk file
```

## Key Directories
| Directory | Purpose |
|-----------|---------|
| `src/vm/` | Virtual machine: interpreter, memory/GC, primitives, JIT |
| `src/compiler/` | Lexer, parser, bytecode codegen |
| `src/image/` | Bootstrap, Smalltalk class library (*.st files), snapshots |
| `src/tui/` | Terminal UI with tabs, widgets, syntax highlighting |
| `tools/` | FFI generator, header processor |
| `ansi-tests/` | ANSI Smalltalk compliance test suite |

## Important Files
- `src/main.zig` - Entry point
- `src/vm/interpreter.zig` - Main bytecode interpreter (211KB)
- `src/vm/primitives.zig` - Built-in primitive operations (432KB)
- `src/vm/memory.zig` - Heap and garbage collector
- `src/image/bootstrap.zig` - Core class hierarchy creation
- `ffi-config.json` - FFI library bindings configuration
- `load-order-fixed.txt` - ANSI test file load order

## Running ANSI Tests
```bash
# Load all ANSI files and run tests
FILES=$(grep -v '^$' load-order-fixed.txt | grep -v '^#' | tr '\n' ' ')
./zig-out/bin/zig-smalltalk $FILES ansi-tests/run-tests-simple.st
```

## Current Test Status (2026-01-27)

### REPL Feature Tests (comprehensive-test-results.txt)

**Working:**
- Integer arithmetic: `+`, `-`, `*`, `/`, `//`
- Comparisons: `<`, `>`, `<=`, `>=`, `=`, `~=`
- Boolean: `true`, `false`, `not`, `&`, `|` (eager evaluation)
- Characters: `$a`, `$Z`
- Strings: literals, `size`, `at:`, `,` (concat), `=`, `copyFrom:to:`
- Symbols: `#hello`
- Arrays: `#(1 2 3)`, `size`, `at:`, `includes:`, `indexOf:`, `reversed`
- nil: `nil`, `isNil`
- Unary: `abs`, `negated`, `factorial`
- Keywords: `max:`, `min:`
- Temps: `| x | x := 5. x + 3.`
- Simple blocks: `[3 + 4] value`
- Closure capture: `| x | x := 10. [:y | x + y] value: 5.`
- Control: `ifTrue:ifFalse:`
- Loops: `whileTrue:`, `timesRepeat:`
- Dictionary: `at:put:`, `at:`
- Type checks: `class`, `isKindOf:`, `respondsTo:`
- Integer: `even`, `odd`, `truncated`, `rounded`, `floor`, `ceiling`
- Float math: `sqrt`, `sin`, `cos`, `tan`, `exp`, `ln`, `log10`, `arcSin`, `arcCos`, `arcTan`
- Reflection: `respondsTo:`, `Smalltalk allClasses`

**FIXED (2026-01-24) - Block Arguments Now Work:**
- `[:x | x * 2] value: 5` => 10
- `[:x :y | x + y] value: 3 value: 4` => 7
- `to:do:` - now works correctly
- `#(1 2 3) do: [:each | ...]` - now works
- `collect:`, `select:` - should work now

**ADDED (2026-01-27):**
- Boolean `&` and `|` operators (eager evaluation)
- Float math functions: `sqrt`, `sin`, `cos`, `tan`, `exp`, `ln`, `log10`, `floor`, `ceiling`, `rounded`, `arcSin`, `arcCos`, `arcTan`
- `Object>>respondsTo:` - checks if object understands a selector
- `Smalltalk>>allClasses` - returns array of all classes in the system
- Array methods: `includes:`, `indexOf:`, `indexOf:ifAbsent:`, `reversed`

**Bug Fix Details:**
The bug was in `makeHandlerPushTemp` (interpreter.zig:4560). The fast-path handler
for `push_temporary_N` opcodes always read from the stack, ignoring `heap_context`.
Block arguments are stored in `heap_context` when a block is executed via `value:`,
so the fix was to check `heap_context` first before falling back to stack access.

**Missing Methods (MessageNotUnderstood):**
- `\\` (modulo)
- `asLowercase`, `asUppercase`, `squared`
- `ifNil:`
- `beginsWith:`, `endsWith:`
- `@` (Point creation)
- `OrderedCollection>>add:`

### ANSI Test Suite Status (FIXED 2026-01-24)
- Tests load and run correctly
- 79 test case classes found
- Boolean tests: 16 selectors
- Fixed issues:
  1. `--load-ansi` flag was incorrectly skipping stub file
  2. Core libraries were loaded after stub, overwriting stub methods
  3. `Processor-stub.st` was using `new` which throws error
  4. Inline cache didn't include selector, causing false cache hits
  5. Stack frame introspection implemented (primitives 230-236)

**Inline Cache Bug Fix:**
The inline cache was keyed by (method_ptr, bytecode_offset, receiver_class) but didn't
include the selector. This caused false cache hits when multiple REPL expressions had
the same bytecode structure but different selectors (e.g., `Processor testReturn42`
followed by `Processor activeProcess` would return 42 for both). Fixed by adding
`cached_selector` to `InlineCacheEntry` and checking it on cache hit.

**Stack Frame Introspection (2026-01-24):**
Added StackFrame class (`src/smalltalk/stack-frame.st`) with primitives for stack
introspection needed by exception handling:
- Primitive 230: `method` - returns CompiledMethod for frame
- Primitive 231: `receiver` - returns self for frame
- Primitive 232: `sender` - returns parent StackFrame or nil
- Primitive 233: `ip` - returns instruction pointer
- Primitive 234: `arguments` - returns Array of arguments
- Primitive 235: `temporaries` - returns Array of temps
- Primitive 236: `current` (class method) - returns current frame

Note: Primitives execute without pushing their own context, so `context_ptr` during
primitive execution already points to the caller's context. The StackFrame stores
an absolute context index, which remains valid as long as the frame hasn't returned.

## Architecture Notes

### Tagged Pointers (64-bit)
Values encoded in 64-bit words with 3-bit type tags:
- SmallIntegers get 61 bits
- Objects, Characters, and special constants have distinct tags

### Memory Management
- Semi-space copying garbage collector
- Interpreter stack tracing for GC roots
- Symbol and class tables for interning

### Method Dispatch
- 2048-entry method cache for fast dispatch
- 4096-entry per-callsite inline cache for JIT

### FFI System
- Compile-time generation from `ffi-config.json`
- Runtime support via libffi
- Supports LibC, LibMath, OpenGL, Raylib, GLFW, OpenXR

## Common Issues

### `MessageNotUnderstood` Errors
Check if the class/method is defined in bootstrap.zig or loaded .st files.

### Load Order Issues
Files must be loaded in dependency order. Use `load-order-fixed.txt` as reference.

### Heap/GC Issues
The VM uses a semi-space collector. Large allocations or deep recursion can trigger GC issues.
