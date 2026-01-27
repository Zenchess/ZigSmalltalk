# Primitives Refactoring Plan

The `primitives.zig` file (12,562 lines, 304 primitives) should be split into logical modules.

## Proposed Structure

```
src/vm/primitives/
├── README.md           # This file
├── arithmetic.zig      # Integer/Float arithmetic (~60 primitives)
├── objects.zig         # Object creation, access, identity (~25 primitives)
├── blocks.zig          # Block evaluation, control flow (~30 primitives)
├── collections.zig     # Array, Dict, Set, OC, Stream (~50 primitives)
├── strings.zig         # String manipulation (~25 primitives)
├── processes.zig       # Process, Semaphore (~15 primitives)
├── files.zig           # File I/O (~20 primitives)
├── ffi.zig             # FFI calls, memory access (~50 primitives)
├── terminal.zig        # TUI primitives (~30 primitives)
├── exceptions.zig      # Error/exception handling (~10 primitives)
└── reflection.zig      # Class introspection, stack frames (~15 primitives)
```

## Implementation Notes

1. Each module exports its primitive functions
2. Main `primitives.zig` imports all modules and contains `executePrimitive()` dispatch
3. Shared helpers (like `evaluateBlock`) go in a `helpers.zig` module
4. Constants and type aliases shared via a `common.zig` module

## Migration Strategy

1. Start with self-contained modules (ffi.zig, terminal.zig, files.zig)
2. Extract shared helpers as needed
3. Update imports incrementally
4. Verify each step with `zig build test`

## Risk Mitigation

- Keep dispatch table in main file to minimize breakage
- Run ANSI tests after each module extraction
- Git commit after each successful module split
