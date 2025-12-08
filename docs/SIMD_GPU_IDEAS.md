# SIMD and GPU Acceleration for Smalltalk Execution

## The Challenge

Smalltalk is fundamentally **dynamic and object-oriented**:
- Every operation is a message send
- Types are determined at runtime
- Objects can change class
- Methods can be added/modified at runtime

This is the opposite of what GPU/SIMD excel at (uniform, predictable, data-parallel operations).

## Where It Could Work

### 1. Bulk Collection Operations

When you write:
```smalltalk
largeArray collect: [:x | x * 2 + 1]
```

If we can **prove at runtime** that:
- All elements are SmallIntegers
- The block is pure arithmetic
- No side effects

Then we could JIT-compile to SIMD:
```zig
// Process 8 integers at once with AVX2
const vec = @as(@Vector(8, i64), slice[i..i+8]);
const result = vec * @splat(8, 2) + @splat(8, 1);
```

### 2. Numeric Arrays (FloatArray, IntegerArray)

Specialized homogeneous collections could bypass message sends entirely:
```smalltalk
floatArray1 + floatArray2  "Element-wise add"
matrix multiplyBy: otherMatrix
```

### 3. Image Processing / Scientific Computing

If we add ByteArray or bitmap primitives:
```smalltalk
bitmap applyFilter: #gaussianBlur
pixels collect: [:rgb | rgb brightenBy: 0.2]
```

### 4. String Operations

SIMD is excellent for string searching, comparison, parsing:
```smalltalk
largeString occurrencesOf: $a
string1 compareTo: string2
```

## GPU Possibilities

GPU would need even more constraints but could work for:

### Massively Parallel Collection Operations
```smalltalk
"Process 1 million elements"
hugeArray parallelCollect: [:x | x expensiveComputation]
```

### Matrix/Tensor Operations
```smalltalk
matrix1 gpuMultiply: matrix2
neuralNet forwardPass: inputTensor
```

## Implementation Approaches

### Approach 1: Specialized Primitives

Add SIMD-accelerated primitives for specific operations:
```zig
// primitives.zig
fn primSIMDArrayAdd(interp: *Interpreter) !Value {
    // Check both are numeric arrays
    // Use @Vector operations
}
```

### Approach 2: Tracing JIT with Specialization

- Trace hot loops at runtime
- Detect type-stable numeric operations
- Generate SIMD code for the trace

### Approach 3: Explicit Parallel Collections

New classes that guarantee homogeneous types:
```smalltalk
SIMDFloatArray new: 1000000
GPUTensor shape: #(1024 1024)
```

## Practical Starting Points for ZigSmalltalk

### Easy Wins (1-2 weeks each)

1. **SIMD string search** (`indexOf:`, `occurrencesOf:`)
2. **SIMD array comparison**
3. **Vectorized arithmetic** on `FloatArray`/`IntegerArray` classes

### Medium Effort

1. **Bulk numeric operations** (`+`, `*` on homogeneous arrays)
2. **SIMD-accelerated hash table lookup** (method dispatch!)

### Hard but Interesting

1. **Tracing JIT** that detects SIMD opportunities
2. **GPU compute** for explicit parallel operations

## Method Dispatch Optimization

The **method dispatch** itself could benefit from SIMD:
- Cache lookup involves string/symbol comparison
- Multiple candidates could be checked in parallel
- Hash table probing is vectorizable

## Recommended Implementation Order

1. **SIMD string primitives** - immediate benefit, Zig makes it easy
2. **Numeric array classes** - `FloatArray`, `IntegerArray` with vectorized ops
3. **Parallel method cache lookup** - could speed up all message sends
4. **Tracing JIT** for hot loops (long-term goal)
5. **GPU tensor operations** (if scientific computing is a goal)
