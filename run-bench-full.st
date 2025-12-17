"Comprehensive benchmark suite"

!SmallInteger methodsFor: 'benchmarks'!
fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!
!

!SmallInteger methodsFor: 'benchmarks'!
loopBenchmark
    | sum i |
    sum := 0.
    i := 1.
    [i <= self] whileTrue: [
        sum := sum + i.
        i := i + 1].
    ^sum!
!

!SmallInteger methodsFor: 'benchmarks'!
factorialBench
    self <= 1 ifTrue: [^1].
    ^self * (self - 1) factorialBench!
!

!SmallInteger methodsFor: 'benchmarks'!
ackermann: n
    self = 0 ifTrue: [^n + 1].
    n = 0 ifTrue: [^(self - 1) ackermann: 1].
    ^(self - 1) ackermann: (self ackermann: n - 1)!
!

!SmallInteger methodsFor: 'benchmarks'!
takX: x y: y z: z
    y < x ifTrue: [
        ^self takX: (self takX: x - 1 y: y z: z)
              y: (self takX: y - 1 y: z z: x)
              z: (self takX: z - 1 y: x z: y)
    ].
    ^z!
!

| start end result |

Transcript show: ''; cr.
Transcript show: '╔════════════════════════════════════════════════════════════╗'; cr.
Transcript show: '║         ZigSmalltalk JIT Benchmark Suite                   ║'; cr.
Transcript show: '╠════════════════════════════════════════════════════════════╣'; cr.
Transcript show: '║ Comparison with other Smalltalk implementations            ║'; cr.
Transcript show: '╚════════════════════════════════════════════════════════════╝'; cr.
Transcript show: ''; cr.

Transcript show: '─────────────────────────────────────────────────────────────'; cr.
Transcript show: ' FIBONACCI BENCHMARKS (recursive, tests method dispatch)'; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.

"Fibonacci 25"
Transcript show: ' fib(25): '.
start := Delay millisecondClockValue.
result := 25 fibonacci.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

"Fibonacci 30"
Transcript show: ' fib(30): '.
start := Delay millisecondClockValue.
result := 30 fibonacci.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

"Fibonacci 35"
Transcript show: ' fib(35): '.
start := Delay millisecondClockValue.
result := 35 fibonacci.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

Transcript show: ''; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.
Transcript show: ' TAK BENCHMARK (deeply recursive, 3-way branching)'; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.

"TAK(18,12,6)"
Transcript show: ' tak(18,12,6): '.
start := Delay millisecondClockValue.
result := 0 takX: 18 y: 12 z: 6.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

Transcript show: ''; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.
Transcript show: ' ACKERMANN BENCHMARK (extremely recursive)'; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.

"Ackermann(3,7)"
Transcript show: ' A(3,7): '.
start := Delay millisecondClockValue.
result := 3 ackermann: 7.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

Transcript show: ''; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.
Transcript show: ' LOOP BENCHMARK (arithmetic, loop overhead)'; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.

"Loop 1M"
Transcript show: ' sum(1..1000000): '.
start := Delay millisecondClockValue.
result := 1000000 loopBenchmark.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

Transcript show: ''; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.
Transcript show: ' FACTORIAL BENCHMARK (multiply, recursion)'; cr.
Transcript show: '─────────────────────────────────────────────────────────────'; cr.

"Factorial 12 x 10000"
Transcript show: ' 12! x 10000: '.
start := Delay millisecondClockValue.
10000 timesRepeat: [result := 12 factorialBench].
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' = '; show: (end - start) printString; show: ' ms'; cr.

Transcript show: ''; cr.
Transcript show: '═════════════════════════════════════════════════════════════'; cr.
Transcript show: ' BENCHMARK COMPLETE'; cr.
Transcript show: '═════════════════════════════════════════════════════════════'; cr.
Transcript show: ''; cr.
Transcript show: ' Reference: Pharo 12 benchmarksgame results (Intel):'; cr.
Transcript show: '   n-body: 114.7s, spectral-norm: 53.4s'; cr.
Transcript show: '   binary-trees: 35.2s, mandelbrot: 344-366s'; cr.
Transcript show: ''; cr.
Transcript show: ' Reference: Are We Fast Yet (2016 data):'; cr.
Transcript show: '   Java baseline = 1.0x'; cr.
Transcript show: '   TruffleSOM = 2.2x slower'; cr.
Transcript show: '   SOMns = 2.0x slower'; cr.
Transcript show: '   MRI Ruby = 45.6x slower'; cr.
Transcript show: '═════════════════════════════════════════════════════════════'; cr.

quit!
