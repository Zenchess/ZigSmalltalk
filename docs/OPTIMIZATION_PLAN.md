# ZigSmalltalk VM Optimization Plan

## Current Performance Baseline

| Benchmark | Time | Rate |
|-----------|------|------|
| Message sends (100K) | 567 ms | ~176,000 sends/sec |
| Arithmetic (100K adds) | 804 ms | ~124,000 ops/sec |
| Loop iterations (100K) | 1797 ms | ~55,600 iter/sec |
| Sieve (8191) | 777 ms | - |

### Comparison with Historical Smalltalks

| Implementation | Year | Sends/sec | Notes |
|----------------|------|-----------|-------|
| ZigSmalltalk | 2024 | ~176,000 | Current bytecode interpreter |
| Squeak 2.5 | 1999 | ~850,000 | Pure interpreter, 500MHz |
| Dolphin 2.1 | 2000 | ~1,112,000 | Optimized interpreter |
| VisualWorks 3.0 | 2000 | ~5,950,000 | JIT compiler |
| Squeak/Cog | 2014 | ~180,000,000 | Modern JIT |

**Gap analysis**: We're roughly 5-6x slower than classic interpreters on equivalent hardware.

---

## Optimization Phases

### Phase 1: Interpreter Optimization (This Phase)

**Goal**: 5-10x speedup through interpreter improvements

#### 1.1 Global Method Cache

**Problem**: Every message send does a full method lookup through the class hierarchy.

**Solution**: A global cache mapping (class, selector) -> method.

```
Cache entry: { receiverClass, selector, method }
Lookup: hash(class ^ selector) & cacheMask
```

**Expected speedup**: 1.5-2x on method-heavy code

#### 1.2 SmallInteger Arithmetic Fast Paths

**Problem**: `1 + 2` currently goes through full message dispatch.

**Solution**: Check for SmallInteger receivers and handle +, -, *, <, >, =, etc. directly in the interpreter without method lookup.

```
if (receiver.isSmallInt() and selector == #+) {
    if (arg.isSmallInt()) {
        return receiver + arg;  // Direct arithmetic
    }
}
// Fall through to normal send
```

**Expected speedup**: 2-3x on arithmetic-heavy code

#### 1.3 Improved Bytecode Dispatch

**Problem**: Current switch-based dispatch has branch prediction issues.

**Solution**: Consider computed goto / direct threading (if Zig supports it), or at minimum ensure hot bytecodes are handled first.

**Expected speedup**: 1.3-1.5x

#### 1.4 Block Invocation Fast Path

**Problem**: Block value/value:/value:value: go through normal dispatch.

**Solution**: Recognize BlockClosure receiver and invoke directly.

**Expected speedup**: 1.2-1.5x on block-heavy code

---

### Phase 2: Inline Caches (Future)

**Goal**: Additional 3-5x speedup

#### 2.1 Monomorphic Inline Cache (MIC)

Cache the last class seen at each call site:
```
callsite:
  if (receiver.class == cached_class) goto cached_method
  else: slow_lookup_and_update_cache()
```

#### 2.2 Polymorphic Inline Cache (PIC)

For call sites that see 2-4 different classes:
```
callsite:
  switch (receiver.class) {
    cached_class1: goto method1
    cached_class2: goto method2
    cached_class3: goto method3
    default: megamorphic_lookup()
  }
```

#### 2.3 Cache Invalidation

When a method is added/changed:
- Flush affected cache entries
- Or use class version numbers

---

### Phase 3: Baseline JIT (Future)

**Goal**: Remove interpreter overhead entirely for hot methods

- Template-based code generation
- No optimization passes
- Direct register allocation
- Keep interpreter for cold code and debugging

---

### Phase 4: Optimizing JIT with LLVM (Far Future)

**Goal**: "Half the speed of C"

- Profile-guided optimization
- Inlining of hot sends
- Type specialization
- Escape analysis
- Deoptimization support

---

## Implementation Notes

### Method Cache Structure

```zig
const MethodCacheEntry = struct {
    selector: Value,      // Selector being looked up
    class: Value,         // Receiver's class
    method: ?*CompiledMethod,  // Cached method (null = negative cache)
};

const METHOD_CACHE_SIZE = 2048;  // Must be power of 2
var method_cache: [METHOD_CACHE_SIZE]MethodCacheEntry = undefined;

fn cachedLookup(class: Value, selector: Value) ?*CompiledMethod {
    const hash = @as(usize, @bitCast(class)) ^ @as(usize, @bitCast(selector));
    const index = hash & (METHOD_CACHE_SIZE - 1);
    const entry = &method_cache[index];

    if (entry.class == class and entry.selector == selector) {
        return entry.method;  // Cache hit
    }

    // Cache miss - do full lookup and cache result
    const method = fullMethodLookup(class, selector);
    entry.class = class;
    entry.selector = selector;
    entry.method = method;
    return method;
}
```

### SmallInteger Fast Path Example

```zig
fn executeSend(self: *Interpreter, selector: Value, argCount: usize) !void {
    const receiver = self.stackAt(argCount);

    // Fast path for SmallInteger arithmetic
    if (receiver.isSmallInt() and argCount == 1) {
        const arg = self.stackAt(0);
        if (arg.isSmallInt()) {
            if (selector == self.specialSelectors.plus) {
                // Overflow check
                const a = receiver.asSmallInt();
                const b = arg.asSmallInt();
                if (@addWithOverflow(a, b)) |result| {
                    self.popN(2);
                    self.push(Value.fromSmallInt(result));
                    return;
                }
            }
            // ... similar for -, *, <, >, =, etc.
        }
    }

    // Normal send path
    return self.normalSend(receiver, selector, argCount);
}
```

---

## Success Metrics

| Phase | Target Sends/sec | Speedup |
|-------|------------------|---------|
| Current | 176,000 | 1x |
| Phase 1 | 800,000 - 1,500,000 | 5-8x |
| Phase 2 | 3,000,000 - 5,000,000 | 17-28x |
| Phase 3 | 10,000,000+ | 50x+ |

---

## References

- Deutsch & Schiffman, "Efficient Implementation of the Smalltalk-80 System" (1984)
- Hölzle, Chambers, Ungar, "Optimizing Dynamically-Typed Object-Oriented Languages" (1991)
- Squeak Wiki: http://wiki.squeak.org/squeak/768
- Cog VM Blog: http://www.mirandabanda.org/cogblog/about-cog/
