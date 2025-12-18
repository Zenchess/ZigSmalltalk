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

## Architectural Decision: Zig-Native JIT

For Phases 2-4, we commit to a **Zig-native optimization approach**:

### Why Not LLVM ORC?

| Concern | Impact |
|---------|--------|
| **Latency** | LLVM optimizations take 10-100ms per method; Smalltalk's interactive nature requires sub-millisecond JIT response |
| **Memory** | LLVM's infrastructure is heavyweight vs our lean ~2600 LOC JIT |
| **Debugging** | Zig-native keeps stack traces and profiling coherent |
| **Precedent** | Cog achieves 100M+ sends/sec with hand-written optimizations, no LLVM |

### Zig's Strengths

- **Compile-time metaprogramming** (`comptime`) for specialized code stencils
- **Direct control** over register allocation and memory layout
- **Zero-overhead abstractions** - no runtime cost for type safety

LLVM remains a theoretical option for a hypothetical Phase 5+ "background compilation" tier (compiling in background after 100K+ invocations), but is **out of scope** for the current roadmap.

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

### Phase 2: Register & Cache Optimizations

**Goal**: 20-30M sends/sec (2-3x improvement)

#### 2.0 TOS/NOS Registerization (PRIORITY)

**Rationale**: SP registerization (Phase 1.3) alone gave 12x speedup. TOS/NOS registerization is the next highest-impact optimization with relatively low complexity.

Keep top-of-stack and next-on-stack in registers:
- **TOS** in `r14` (callee-saved)
- **NOS** in `r15` (callee-saved)
- Spill to memory only at calls/returns
- Reload after calls return

**Implementation in `src/vm/jit.zig`**:
```
Prologue changes:
- push r14, r15 (save callee-saved registers)
- Load TOS/NOS from stack[sp-1], stack[sp-2] into r14/r15

Push operation:
- mov [r13 + r12*8], r15    ; spill NOS to stack
- mov r15, r14              ; NOS = old TOS
- mov r14, <new value>      ; TOS = new value
- add r12, 1                ; sp++

Pop operation:
- mov r14, r15              ; TOS = old NOS
- sub r12, 1                ; sp--
- mov r15, [r13 + r12*8 - 8] ; reload NOS from stack

Before calls:
- mov [r13 + r12*8 - 8], r14  ; spill TOS
- mov [r13 + r12*8 - 16], r15 ; spill NOS

After calls:
- reload r14/r15 from stack
```

**Expected speedup**: 2-3x (target 15-20M sends/sec from this alone)

#### 2.1 JIT: Inline Monomorphic Send Fast Path
For each send site in JIT code, emit:
```
load receiver (from TOS register r14)
classOf(receiver)  // inline tag check or helper call
compare with cache.expected_class and cache.version
on hit: call cached target entry directly
on miss: call runtime_patch_send() to update cache
```

#### 2.2 JIT: Inline SmallInt Ops
- Emit inline +, -, *, <, <=, >, >=, =, ≠ for SmallInts
- Tag checks with slow-path guards (call runtime on overflow/type mismatch)
- Use TOS/NOS registers directly for operands

#### 2.3 JIT: Inline Boolean ifTrue:/ifFalse:
- Check receiver (TOS) == true/false
- Inline the chosen branch
- Slow path for non-boolean receivers

#### 2.4 2-Entry PIC
- Start with 2-entry polymorphic inline cache (not 3)
- Chain class/version checks before miss
- Add branch prediction hints where beneficial
- Expand to 3-4 entries only if profiling shows benefit

---

### Phase 3: Blocks & Broader Coverage

**Goal**: 50M sends/sec

#### 3.1 More Inline Primitives
- `at:`, `at:put:` for Arrays/ByteArrays (with bounds checks)
- Block `value`/`value:` when arity matches

#### 3.2 Block Specialization (KEY BOTTLENECK)

Blocks are fundamental to Smalltalk's control flow. This is a critical optimization target.

- **JIT-compile blocks as inline functions** where possible
- **Captured variables in registers** when block doesn't escape
- **Eliminate closure allocation** for non-escaping blocks (escape analysis lite)
- **Specialize common patterns**: `timesRepeat:`, `do:`, `collect:`, `select:`

**Implementation approach**:
```
When compiling a method with a non-escaping block:
1. Detect block doesn't escape (not stored, not returned)
2. Inline block bytecodes directly at call site
3. Map block temps to caller's temp space or registers
4. Emit guard for arity mismatch (deopt to interpreter)
```

#### 3.3 Profile-Based Invocation Counting (NEW)

Foundation for tiered compilation in Phase 4:
- **Count method invocations** at call sites (lightweight counter per CallSiteCache)
- **Mark methods as "hot"** after ~1000 invocations
- **Backedge counting** for hot loop detection
- **Profile data storage**: Extend CallSiteCache or separate profile table

This enables Phase 4's speculative inlining to target the right methods.

#### 3.4 Safe Non-Local Return
- JIT block creation and invocation
- Deopt path for NLR (non-local return)
- Track home context for proper unwinding

#### 3.5 OSR Lite
- Allow jumping into JITted loops from interpreter when loop is hot
- Requires stack frame translation

---

### Phase 4: Tiered JIT & Advanced Optimizations

**Goal**: 100M+ sends/sec (Cog-level performance)

#### 4.1 Lightweight Sea-of-Nodes IR

A minimal intermediate representation for optimization passes:
- **~500 lines of Zig code** - intentionally simple
- **Enables**: speculative inlining, escape analysis, global value numbering (GVN)
- **Compilation target** for hot methods (Tier 1)

**Design**:
```zig
const IRNode = struct {
    op: Op,           // phi, const, add, call, guard, etc.
    inputs: []u16,    // indices of input nodes
    type_info: Type,  // inferred type (SmallInt, Object, etc.)
};

const IRGraph = struct {
    nodes: []IRNode,
    // Sea-of-nodes: no explicit control flow graph
    // Control dependencies are just another edge type
};
```

This is NOT LLVM - it's a focused IR for the specific optimizations we need.

#### 4.2 Speculative Inlining with Guards

Use profile data from Phase 3.3 to inline hot monomorphic sends:
- **Inline callee code** at call site
- **Emit type guard** before inlined code
- **Deoptimization stub** on guard failure (fall back to interpreter)
- **Profile-guided site selection** - only inline proven-hot sites

**Example transformation**:
```
Before (send):
  call runtime_send(selector: #foo)

After (inlined with guard):
  cmp [receiver].class, ExpectedClass
  jne deopt_stub
  ; inlined body of ExpectedClass>>foo
  ...
```

#### 4.3 Code Cache Management
- Eviction/aging policies for JIT code
- Per-class versioning for invalidation (instead of global bumps)
- LRU or frequency-based eviction

#### 4.4 Patch-in-Place
- For hottest monomorphic sites, patch direct call/jump once stable
- Self-modifying code for ultimate performance
- Requires careful cache invalidation on class changes

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
| **Phase 2.0** | **15-20,000,000** | **85-113x** | ⟳ **TOS/NOS registerization (NEXT)** |
| Phase 2.1-2.4 | 25-30,000,000 | 140-170x | Inline caching improvements |
| Phase 3 | 50,000,000+ | 280x+ | Blocks & broader coverage |
| Phase 4 | 100,000,000+ | 560x+ | Tiered JIT (Cog-level) |

**Current**: ~9,000,000 sends/sec (51x improvement from baseline, 1.5x faster than VisualWorks 3.0!)

**Next milestone**: Phase 2.0 TOS/NOS registerization - expected 2-3x improvement

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
