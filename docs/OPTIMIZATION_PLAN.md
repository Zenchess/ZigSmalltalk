# ZigSmalltalk VM Optimization Plan

## Current State (December 2024)

### What We Have
- **Baseline JIT compiler** exists in `src/vm/jit.zig`
- JIT enabled via `--jit` flag, activates after load-order processing
- Monomorphic inline caches (MIC) per call site
- GC invalidates JIT caches (avoids stale pointers)
- Threaded dispatch infrastructure in interpreter
- **Cross-platform**: JIT now works on Windows (VirtualAlloc) and POSIX (mmap)
- **Phase 1.2 complete**: Jump bytecodes with backpatching support
- **Phase 1.3 complete**: SP registerization (r12=sp, r13=stack base) - 12x speedup!
- **Windows x64 calling convention**: Fixed (uses RCX instead of RDI)
- **SmallInteger fast paths**: Implemented in interpreter for +, -, *, /, <, >, <=, >=, =, ~=

### Performance Baseline (Updated December 2024)

| Benchmark | Rate | Notes |
|-----------|------|-------|
| Fibonacci (fib 30) | **~9,000,000 sends/sec** | Current JIT baseline |
| Fibonacci (fib 35) | **~9,200,000 sends/sec** | Consistent |
| Raw sends (10M) | ~2,200,000 sends/sec | timesRepeat overhead |
| Previous baseline | ~1,050,000 sends/sec | Before SP registerization |
| Original baseline | ~176,000 sends/sec | Without fast paths |

### Comparison with Historical Smalltalks

| Implementation | Year | Sends/sec | vs ZigSmalltalk |
|----------------|------|-----------|-----------------|
| **ZigSmalltalk** | **2024** | **~9,000,000** | **1x** |
| VisualWorks 3.0 | 2000 | ~5,950,000 | 0.66x (we're 1.5x faster!) |
| Dolphin 2.1 | 2000 | ~1,112,000 | 0.12x (we're 8x faster!) |
| Squeak 2.5 | 1999 | ~850,000 | 0.09x (we're 11x faster!) |
| Squeak Stack VM | 2024 | ~20-50,000,000 | 2-5x faster than us |
| Pharo Cog JIT | 2024 | ~100-200,000,000 | 10-20x faster than us |

**Current status**: Faster than VisualWorks 3.0 and classic Smalltalks. 2-5x slower than modern Squeak, 10-20x slower than Pharo Cog.

### Compiler-Level Control Flow Inlining (NEW)

We've implemented compiler-level inlining of `ifTrue:`, `ifFalse:`, and `ifTrue:ifFalse:` in `src/compiler/codegen.zig`. When the compiler detects these patterns with niladic block arguments, it generates jump bytecodes instead of block creation/send:

**Pattern detected**:
```smalltalk
receiver ifTrue: [blockBody]
```

**Before (block-based)**:
```
push receiver
push_closure (blockBody)
send: ifTrue:
```

**After (inlined)**:
```
push receiver
jump_if_false else
  <block body inline>
  jump end
else:
  push_nil
end:
```

**Benefits**:
- Eliminates block allocation overhead
- Jump bytecodes are now JIT-eligible
- No message send overhead for control flow
- Nested control flow optimizes well

**Status**: ✓ Implemented and tested for:
- `ifTrue:` - returns block value or nil
- `ifFalse:` - returns block value or nil
- `ifTrue:ifFalse:` - returns appropriate block value
- `ifFalse:ifTrue:` - returns appropriate block value

### Why JIT Doesn't Help Fibonacci (Yet)

Analysis of `fibonacci` method execution:
1. **Arithmetic** (+, -, <=) → Handled by SmallInteger fast paths (primitives), not JIT-able
2. **Control flow** (ifTrue:ifFalse:) → NOW INLINED to jump bytecodes ✓
3. **Recursion** (fibonacci) → With inlined control, methods should now be JIT-eligible

**Key insight**: With compiler-level inlining of control structures:
- Methods using `ifTrue:/ifFalse:` are now JIT-eligible (no block bytecodes)
- JIT can emit efficient jump sequences instead of block sends
- Further optimization: JIT could inline SmallInteger arithmetic directly

The JIT IS working correctly for eligible methods (e.g., Transcript methods during output).

---

## Optimization Phases (Revised Roadmap)

### Phase 0: Platform & Stability (CURRENT PRIORITY)

**Goal**: JIT builds and runs on all platforms

#### 0.1 Cross-Platform Executable Memory
- Replace `std.posix.mmap` with platform-specific allocation
- Windows: `VirtualAlloc` with `PAGE_EXECUTE_READWRITE`
- POSIX: `mmap` with `PROT_READ | PROT_WRITE | PROT_EXEC`
- Add helper function `allocateExecutableMemory()` and `freeExecutableMemory()`

#### 0.2 Validation
- Build succeeds on Windows and Linux
- `--jit --no-repl --load-order load-order-fixed.txt` completes without crash
- Basic microbenchmarks work with JIT enabled

---

### Phase 1: Interpreter + JIT Foundation

**Goal**: 5-10x speedup through combined interpreter/JIT improvements

#### 1.1 SmallInteger Arithmetic Fast Paths (Interpreter)
**Benefits both interpreted and JIT code**

Check for SmallInteger receivers and handle +, -, *, <, >, =, etc. directly:
```zig
if (receiver.isSmallInt() and selector == self.specialSelectors.plus) {
    if (arg.isSmallInt()) {
        // Overflow-checked addition
        return receiver + arg;
    }
}
// Fall through to normal send
```
**Expected speedup**: 2-3x on arithmetic-heavy code

#### 1.2 JIT: Jumps & Backpatching
- Maintain map of bytecode IP → native code position
- Emit placeholder rel32 for jumps, patch once target known
- Support short jumps (0xB8–0xBF) and long jumps (0xB0–0xB4)
- Remove "no jumps" restriction from JIT eligibility

#### 1.3 JIT: Registerize Stack Pointer
- Keep `sp` in callee-saved register (e.g., `r12`) for entire compiled method
- Write back to `interpreter.sp` only at safepoints (calls, returns)
- Removes many loads/stores from hot paths

#### 1.4 Block Invocation Fast Path (Interpreter)
- Recognize BlockClosure receiver for `value`, `value:`, `value:value:`
- Invoke directly without method lookup

**Expected combined speedup**: 3-5x

---

### Phase 2: Inline Caching Improvements

**Goal**: Additional 2-4x speedup

#### 2.1 JIT: Inline Monomorphic Send Fast Path
For each send site in JIT code, emit:
```
load receiver (from stack via sp register)
classOf(receiver)  // inline tag check or helper call
compare with cache.expected_class and cache.version
on hit: call cached target entry directly
on miss: call runtime_patch_send() to update cache
```

#### 2.2 JIT: Inline SmallInt Ops
- Emit inline +, -, *, <, <=, >, >=, =, ≠ for SmallInts
- Tag checks with slow-path guards (call runtime on overflow/type mismatch)

#### 2.3 JIT: Inline Boolean ifTrue:/ifFalse:
- Check receiver == true/false
- Inline the chosen branch
- Slow path for non-boolean receivers

#### 2.4 PIC Light (2-3 entry cache)
- Expand call-site cache to hold 2-3 entries
- Chain class/version checks before miss
- Still indirect calls (no self-modifying code yet)

---

### Phase 3: Broader JIT Coverage

**Goal**: JIT handles more code patterns

#### 3.1 More Inline Primitives
- `at:`, `at:put:` for Arrays/ByteArrays (with bounds checks)
- Block `value`/`value:` when arity matches and no captured temps

#### 3.2 Stack Registerization Deeper
- Keep TOS/NOS in registers where profitable
- Spill around calls

#### 3.3 Blocks and Non-Local Return
- JIT block creation and invocation
- Safe path for NLR (deopt to interpreter)

#### 3.4 OSR Lite
- Allow jumping into JITted loops from interpreter when loop is hot

---

### Phase 4: Polish & Advanced

**Goal**: Production-quality JIT

#### 4.1 Code Cache Management
- Eviction/aging policies
- Per-class versioning for invalidation (instead of global bumps)

#### 4.2 Patch-in-Place
- For hottest monomorphic sites, patch direct call/jump once stable

#### 4.3 Optimizing JIT (Far Future)
- Profile-guided optimization
- Inlining of hot sends
- Type specialization
- Escape analysis
- Deoptimization support

---

## Implementation Notes

### Cross-Platform Executable Memory (Phase 0)

```zig
const builtin = @import("builtin");

fn allocateExecutableMemory(size: usize) ![]align(4096) u8 {
    if (builtin.os.tag == .windows) {
        const windows = std.os.windows;
        const ptr = windows.VirtualAlloc(
            null,
            size,
            windows.MEM_COMMIT | windows.MEM_RESERVE,
            windows.PAGE_EXECUTE_READWRITE,
        ) orelse return error.OutOfMemory;
        return @as([*]align(4096) u8, @ptrCast(ptr))[0..size];
    } else {
        return std.posix.mmap(
            null,
            size,
            std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
    }
}

fn freeExecutableMemory(mem: []align(4096) u8) void {
    if (builtin.os.tag == .windows) {
        const windows = std.os.windows;
        windows.VirtualFree(@ptrCast(mem.ptr), 0, windows.MEM_RELEASE);
    } else {
        std.posix.munmap(mem);
    }
}
```

### JIT Call Site Cache Structure (Existing)

```zig
pub const CallSiteCache = struct {
    expected_class: Value,           // For quick class comparison
    cached_method: ?*CompiledMethod, // Resolved method
    cached_holder: Value,            // Class that defines method
    version: u32,                    // For invalidation
    cached_jit_code: ?*CompiledCode, // JIT code if compiled
    method_ptr: usize,               // Owning method
    bytecode_offset: u16,            // Send location
};
```

### SmallInteger Fast Path Example

```zig
// In interpreter send handling:
if (receiver.isSmallInt() and argCount == 1) {
    const arg = self.stackAt(0);
    if (arg.isSmallInt()) {
        const a = receiver.asSmallInt();
        const b = arg.asSmallInt();
        if (selector == self.specialSelectors.plus) {
            const result = @addWithOverflow(a, b);
            if (result[1] == 0) {  // No overflow
                self.popN(2);
                self.push(Value.fromSmallInt(result[0]));
                return;
            }
        }
        // ... similar for -, *, <, >, =, etc.
    }
}
// Fall through to normal send
```

---

## Success Metrics

| Phase | Target Sends/sec | Speedup | Status |
|-------|------------------|---------|--------|
| Baseline | 176,000 | 1x | ✓ Original interpreter |
| Phase 0 | 176,000 | 1x | ✓ Windows/POSIX exec memory |
| Phase 1.1 | 1,000,000+ | 6x | ✓ SmallInteger fast paths |
| Phase 1.2 | - | - | ✓ JIT jumps & backpatching |
| Phase 1.3 | **9,000,000** | **51x** | ✓ **SP registerization** |
| Compiler | - | - | ✓ **ifTrue:/ifFalse: inlining** |
| Phase 2 | 20,000,000+ | 100x+ | ⟳ Inline caching improvements |
| Phase 3 | 50,000,000+ | 250x+ | Broader coverage (blocks) |
| Phase 4 | 100,000,000+ | 500x+ | Production polish (Cog-level) |

**Current**: ~9,000,000 sends/sec (51x improvement from baseline, 1.5x faster than VisualWorks 3.0!)

### Compiler Optimizations Completed

- **ifTrue:/ifFalse: inlining**: Control structures compile to jump bytecodes instead of block creation + message send
- This eliminates block allocation and method lookup overhead for control flow
- Methods using control structures are now JIT-eligible (no block bytecodes)

---

## Testing Checkpoints

After each phase:
1. `zig build` succeeds on Windows and Linux
2. `--jit --no-repl --load-order load-order-fixed.txt` completes without crash
3. Microbenchmarks show expected speedup (or no regression)
4. No crashes under debugger

---

## References

- Deutsch & Schiffman, "Efficient Implementation of the Smalltalk-80 System" (1984)
- Hölzle, Chambers, Ungar, "Optimizing Dynamically-Typed Object-Oriented Languages" (1991)
- Squeak Wiki: http://wiki.squeak.org/squeak/768
- Cog VM Blog: http://www.mirandabanda.org/cogblog/about-cog/
