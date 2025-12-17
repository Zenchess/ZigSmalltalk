# ZigSmalltalk Benchmark Results

## Sends Per Second

| Benchmark | Sends/sec | Notes |
|-----------|-----------|-------|
| Fibonacci (fib 30) | ~9.1M | Method dispatch heavy |
| Fibonacci (fib 35) | ~9.2M | Consistent with above |
| Raw sends (1M) | ~2.2M | timesRepeat: overhead |
| Raw sends (10M) | ~2.2M | Consistent |

### Comparison with Other Smalltalk VMs

| Implementation | Sends/sec | Notes |
|----------------|-----------|-------|
| **ZigSmalltalk JIT** | **~9M** | Baseline JIT, monomorphic IC |
| Squeak Stack VM | ~20-50M | Threaded interpreter |
| VisualWorks | ~50-100M | Optimizing interpreter |
| Pharo Cog JIT | ~100-200M | Polymorphic IC, inlining |

ZigSmalltalk is currently 2-5x slower than Squeak and 10-20x slower than Pharo Cog.
This is expected for a baseline JIT without aggressive optimizations.

---

## Test System
- Platform: Windows x86_64
- ZigSmalltalk: v0.1 with JIT baseline compiler
- Date: 2024-12-17

## ZigSmalltalk Results

### Fibonacci (recursive, tests method dispatch)
| Input | Result | Time |
|-------|--------|------|
| fib(25) | 75,025 | 52 ms |
| fib(30) | 832,040 | 584-603 ms |
| fib(35) | 9,227,465 | 6,525-6,654 ms |

### TAK Benchmark (Takeuchi function, deeply recursive)
| Input | Result | Time |
|-------|--------|------|
| tak(18,12,6) | 7 | 23 ms |

### Ackermann (extremely recursive)
| Input | Result | Time |
|-------|--------|------|
| A(3,7) | 1,021 | 318 ms |

### Loop Benchmark (arithmetic, loop overhead)
| Input | Result | Time |
|-------|--------|------|
| sum(1..1M) | 500,000,500,000 | 698-713 ms |

### Factorial (multiply, recursion)
| Input | Result | Time |
|-------|--------|------|
| 12! × 10,000 | 479,001,600 | 80-83 ms |

## Comparison with Other Implementations

### Are We Fast Yet Benchmarks (2016)
Reference: https://github.com/smarr/are-we-fast-yet

| Implementation | Relative to Java |
|----------------|------------------|
| Java 1.8 | 1.00x (baseline) |
| Crystal | 1.85x slower |
| SOMns | 2.00x slower |
| TruffleSOM | 2.20x slower |
| Graal.js | 2.23x slower |
| JRuby+Truffle | 2.36x slower |
| Node.js | 2.89x slower |
| JRuby | 12.83x slower |
| Rubinius | 25.09x slower |
| MRI Ruby | 45.62x slower |

### Pharo 12 Benchmarks Game Results
Reference: https://benchmarksgame-team.pages.debian.net/benchmarksgame/

| Benchmark | Time |
|-----------|------|
| pidigits | 7.3s |
| binary-trees | 35.2s |
| fasta | 43-93s |
| spectral-norm | 53.4s |
| n-body | 114.7s |
| mandelbrot | 344-366s |
| fannkuch-redux | 481s |
| k-nucleotide | 494-561s |
| reverse-complement | 152-565s |

### Fibonacci Comparison (Estimated)
Based on typical Smalltalk implementations:

| Implementation | fib(35) | Notes |
|----------------|---------|-------|
| **ZigSmalltalk JIT** | ~6.5s | Current implementation |
| Pharo/Cog JIT | ~2-4s | Optimizing JIT with polymorphic inline caches |
| Squeak (Stack VM) | ~8-15s | Stack-based interpreter |
| GNU Smalltalk | ~10-20s | Traditional interpreter |
| TruffleSOM | ~1-2s | Graal JIT with aggressive optimization |

## Analysis

ZigSmalltalk's baseline JIT compiler achieves reasonable performance:
- Fibonacci is competitive with traditional Smalltalk interpreters
- The inline cache system provides good method lookup performance
- SmallInteger arithmetic is optimized with tagged pointer operations

Areas for improvement:
1. **Loop optimization**: The loop benchmark shows overhead from block evaluation
2. **Polymorphic inline caches**: Currently using monomorphic caches
3. **Inlining**: No cross-method inlining yet
4. **Register allocation**: Currently using memory-based stack

## Running Benchmarks

```bash
# Run full benchmark suite
echo "filein run-bench-full.st" | ./zig-out/bin/zig-smalltalk --jit

# Run simple benchmark
echo "filein run-bench.st" | ./zig-out/bin/zig-smalltalk --jit
```

## References
- Are We Fast Yet: https://github.com/smarr/are-we-fast-yet
- Benchmarks Game: https://benchmarksgame-team.pages.debian.net/benchmarksgame/
- Pharo: https://pharo.org
- Squeak: https://squeak.org
