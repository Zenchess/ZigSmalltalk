"Simple benchmark runner"

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

| start end result |

Transcript show: 'ZigSmalltalk JIT Benchmark'; cr.
Transcript show: '=========================='; cr; cr.

"Fibonacci 30"
Transcript show: 'Fibonacci(30): '.
start := Delay millisecondClockValue.
result := 30 fibonacci.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' in '; show: (end - start) printString; show: ' ms'; cr.

"Fibonacci 35"
Transcript show: 'Fibonacci(35): '.
start := Delay millisecondClockValue.
result := 35 fibonacci.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' in '; show: (end - start) printString; show: ' ms'; cr.

"Loop 1000000"
Transcript show: 'Loop sum 1-1000000: '.
start := Delay millisecondClockValue.
result := 1000000 loopBenchmark.
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' in '; show: (end - start) printString; show: ' ms'; cr.

"Factorial 12 x 10000"
Transcript show: 'Factorial(12) x 10000: '.
start := Delay millisecondClockValue.
10000 timesRepeat: [result := 12 factorialBench].
end := Delay millisecondClockValue.
Transcript show: result printString; show: ' in '; show: (end - start) printString; show: ' ms'; cr.

Transcript show: ''; cr.
Transcript show: 'Benchmark complete.'; cr.

quit!
