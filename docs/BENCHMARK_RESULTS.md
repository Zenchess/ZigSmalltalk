# ZigSmalltalk Benchmark Results

## Baseline Measurements (Pre-Optimization)

**Date**: December 2024
**Platform**: WSL2 Linux, Zig 0.13
**Hardware**: Modern desktop CPU

### Message Send Performance

| Benchmark | Time | Rate |
|-----------|------|------|
| Message sends (100K) | 567 ms | **~176,000 sends/sec** |
| Arithmetic (100K adds) | 804 ms | ~124,000 ops/sec |
| Loop iterations (100K) | 1797 ms | ~55,600 iter/sec |
| Sieve of Eratosthenes (8191) | 777 ms | 1028 primes found |

### Test Code Used

```smalltalk
"Message send benchmark"
| start end count |
count := 0.
start := Delay millisecondClockValue.
100000 timesRepeat: [count yourself].
end := Delay millisecondClockValue.
Transcript show: 'Message sends: '; show: (end - start) printString; show: ' ms'; cr.

"Arithmetic benchmark"
| start end sum |
sum := 0.
start := Delay millisecondClockValue.
100000 timesRepeat: [sum := sum + 1].
end := Delay millisecondClockValue.
Transcript show: 'Arithmetic: '; show: (end - start) printString; show: ' ms'; cr.

"Loop benchmark"
| start end sum i |
sum := 0. i := 1.
start := Delay millisecondClockValue.
[i <= 100000] whileTrue: [sum := sum + i. i := i + 1].
end := Delay millisecondClockValue.
Transcript show: 'Loop: '; show: (end - start) printString; show: ' ms'; cr.

"Sieve benchmark"
| start end result |
start := Delay millisecondClockValue.
result := 8191 sieveBench.
end := Delay millisecondClockValue.
Transcript show: 'Sieve: '; show: (end - start) printString; show: ' ms, ';
           show: result printString; show: ' primes'; cr.
```

---

## Historical Comparison

| Implementation | Year | Sends/sec | Notes |
|----------------|------|-----------|-------|
| **ZigSmalltalk (baseline)** | 2024 | ~176,000 | Pre-optimization bytecode interpreter |
| **ZigSmalltalk (optimized)** | 2024 | ~262,000 | With method cache + fast paths |
| Squeak 2.5 | 1999 | ~850,000 | Pure interpreter, ~500MHz hardware |
| Dolphin 2.1 | 2000 | ~1,112,000 | Optimized interpreter |
| VisualWorks 3.0 | 2000 | ~5,950,000 | JIT compiler |
| Squeak/Cog | 2014 | ~180,000,000 | Modern JIT |

**Analysis**: With Phase 1 optimizations, ZigSmalltalk is now approximately 3-4x slower than classic 2000-era interpreters (improved from 5-6x slower).

---

## Optimization Progress

### Phase 1: Interpreter Optimizations

| Optimization | Status | Expected Speedup |
|--------------|--------|------------------|
| Global method cache | Implemented | 1.5-2x |
| SmallInteger fast paths | Implemented | 2-3x |
| Hash-based method dictionaries | Implemented | 1.1-1.3x |
| Block invocation primitives | Implemented | 1.2-1.5x |
| Improved bytecode dispatch | Pending | 1.3-1.5x |

**Phase 1 Target**: 800,000 - 1,500,000 sends/sec (5-8x improvement)

---

## Post-Optimization Results (Phase 1)

**Date**: December 2024
**Optimizations Applied**:
1. Global method cache (2048 entries, direct-mapped with holder tracking)
2. SmallInteger arithmetic fast paths (+, -, *, /, \\\\, <, >, <=, >=, =, ~=, bitAnd:, bitOr:, bitXor:, bitShift:)
3. Hash-based method dictionaries (Knuth multiplicative hash with linear probing)
4. Block invocation primitives (timesRepeat:, whileTrue:, whileFalse:)

| Benchmark | Before | After | Speedup | Rate |
|-----------|--------|-------|---------|------|
| Message sends (100K) | 567 ms | ~382 ms | **1.48x** | ~262,000 sends/sec |
| Arithmetic (100K adds) | 804 ms | ~547 ms | **1.47x** | ~183,000 ops/sec |
| Loop iterations (100K) | 1797 ms | ~1681 ms | **1.07x** | ~59,500 iter/sec |

### Analysis

- **Message sends**: 48% faster due to method cache eliminating class hierarchy walks
- **Arithmetic**: 47% faster due to SmallInteger fast paths bypassing method lookup entirely
- **Loop**: 7% faster - limited improvement because `whileTrue:` block overhead dominates
- **Hash dictionaries**: Marginal improvement since method cache handles most hot paths

### Comparison to Target

| Phase | Target | Achieved |
|-------|--------|----------|
| Baseline | 176,000 sends/sec | 176,000 sends/sec |
| Phase 1 (all optimizations) | 800K-1.5M sends/sec | ~262,000 sends/sec |

We achieved about 35% of the target Phase 1 improvement. The main bottleneck is now block invocation overhead:
- `timesRepeat:` and `whileTrue:` still require full context creation
- Each block evaluation involves stack frame setup/teardown

### Future Optimizations (Phase 2)

Further gains would require:
- **Inline caches at call sites**: Polymorphic inline caches (PICs) for monomorphic/megamorphic sends
- **Block inlining**: Compile common patterns like `whileTrue:` directly into bytecode
- **Threaded dispatch**: Replace switch-based dispatch with computed goto
- **JIT compilation**: Generate native code for hot methods
