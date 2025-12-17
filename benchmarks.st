"Classic Smalltalk Benchmarks"
"Based on benchmarks from: Are We Fast Yet, Byte Sieve, TAK function"
"Sources: https://github.com/smarr/are-we-fast-yet"
"         https://en.wikipedia.org/wiki/Tak_(function)"
"         https://rosettacode.org/wiki/Sieve_of_Eratosthenes"

"=== Fibonacci Benchmark ==="
"Recursive fibonacci - tests method invocation performance"

!SmallInteger methodsFor: 'benchmarks'!
fibonacci
    "Classic recursive fibonacci"
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!
!

"=== TAK Benchmark ==="
"Takeuchi function - tests recursive function calling"
"tak(x, y, z) = if y < x then tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y)) else z"

!SmallInteger methodsFor: 'benchmarks'!
takX: x y: y z: z
    "TAK benchmark - Takeuchi function"
    y < x ifTrue: [
        ^self takX: (self takX: x - 1 y: y z: z)
              y: (self takX: y - 1 y: z z: x)
              z: (self takX: z - 1 y: x z: y)
    ].
    ^z!
!

"=== Sieve of Eratosthenes ==="
"Prime number sieve - tests array access and loops"

!SmallInteger methodsFor: 'benchmarks'!
sieve
    "Sieve of Eratosthenes up to self, answer count of primes"
    | flags count i j prime |
    flags := Array new: self.

    "Initialize all to true (1)"
    i := 1.
    [i <= self] whileTrue: [
        flags at: i put: 1.
        i := i + 1].

    "Mark composites"
    count := 0.
    i := 2.
    [i <= self] whileTrue: [
        (flags at: i) = 1 ifTrue: [
            "i is prime"
            count := count + 1.
            prime := i.
            "Mark multiples as composite"
            j := i + i.
            [j <= self] whileTrue: [
                flags at: j put: 0.
                j := j + prime]].
        i := i + 1].
    ^count!
!

"=== Simple Loop Benchmark ==="
"Tests basic arithmetic and loop performance"

!SmallInteger methodsFor: 'benchmarks'!
loopBenchmark
    "Simple loop adding numbers"
    | sum i |
    sum := 0.
    i := 1.
    [i <= self] whileTrue: [
        sum := sum + i.
        i := i + 1].
    ^sum!
!

"=== Factorial Benchmark ==="
"Recursive factorial"

!SmallInteger methodsFor: 'benchmarks'!
factorialBench
    "Recursive factorial"
    self <= 1 ifTrue: [^1].
    ^self * (self - 1) factorialBench!
!

"=== Ackermann Benchmark ==="
"Ackermann function - extremely recursive"

!SmallInteger methodsFor: 'benchmarks'!
ackermann: n
    "Ackermann function A(m, n)"
    self = 0 ifTrue: [^n + 1].
    n = 0 ifTrue: [^(self - 1) ackermann: 1].
    ^(self - 1) ackermann: (self ackermann: n - 1)!
!

"=== Benchmark Runner ==="

Transcript show: ''; cr.
Transcript show: '========================================'; cr.
Transcript show: '  ZigSmalltalk Benchmark Suite'; cr.
Transcript show: '========================================'; cr.
Transcript show: ''; cr.

| startTime endTime elapsed iterations result |

"--- Fibonacci Benchmark ---"
Transcript show: '1. Fibonacci Benchmark'; cr.
Transcript show: '   Computing fib(30)...'; cr.

iterations := 1.
startTime := Delay millisecondClockValue.
iterations timesRepeat: [result := 30 fibonacci].
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: fib(30) = '; show: result printString; cr.
Transcript show: '   Time: '; show: elapsed printString; show: ' ms'; cr.
Transcript show: ''; cr.

"--- TAK Benchmark ---"
Transcript show: '2. TAK Benchmark'; cr.
Transcript show: '   Computing tak(18, 12, 6)...'; cr.

startTime := Delay millisecondClockValue.
result := 0 takX: 18 y: 12 z: 6.
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: tak(18,12,6) = '; show: result printString; cr.
Transcript show: '   Time: '; show: elapsed printString; show: ' ms'; cr.
Transcript show: ''; cr.

"--- Sieve Benchmark ---"
Transcript show: '3. Sieve of Eratosthenes'; cr.
Transcript show: '   Finding primes up to 10000...'; cr.

startTime := Delay millisecondClockValue.
result := 10000 sieve.
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: '; show: result printString; show: ' primes found'; cr.
Transcript show: '   Time: '; show: elapsed printString; show: ' ms'; cr.
Transcript show: ''; cr.

"--- Loop Benchmark ---"
Transcript show: '4. Loop Benchmark'; cr.
Transcript show: '   Summing 1 to 100000...'; cr.

startTime := Delay millisecondClockValue.
result := 100000 loopBenchmark.
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: sum = '; show: result printString; cr.
Transcript show: '   Time: '; show: elapsed printString; show: ' ms'; cr.
Transcript show: ''; cr.

"--- Factorial Benchmark ---"
Transcript show: '5. Factorial Benchmark'; cr.
Transcript show: '   Computing 12! x 1000 iterations...'; cr.

startTime := Delay millisecondClockValue.
1000 timesRepeat: [result := 12 factorialBench].
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: 12! = '; show: result printString; cr.
Transcript show: '   Time: '; show: elapsed printString; show: ' ms (1000 iterations)'; cr.
Transcript show: ''; cr.

"--- Ackermann Benchmark ---"
Transcript show: '6. Ackermann Benchmark'; cr.
Transcript show: '   Computing A(3, 6)...'; cr.

startTime := Delay millisecondClockValue.
result := 3 ackermann: 6.
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: A(3,6) = '; show: result printString; cr.
Transcript show: '   Time: '; show: elapsed printString; show: ' ms'; cr.
Transcript show: ''; cr.

"--- Multiple Fibonacci runs for average ---"
Transcript show: '7. Fibonacci Stability Test'; cr.
Transcript show: '   Running fib(25) x 10 iterations...'; cr.

| total |
total := 0.
startTime := Delay millisecondClockValue.
10 timesRepeat: [result := 25 fibonacci].
endTime := Delay millisecondClockValue.
elapsed := endTime - startTime.

Transcript show: '   Result: fib(25) = '; show: result printString; cr.
Transcript show: '   Total time: '; show: elapsed printString; show: ' ms'; cr.
Transcript show: '   Average: '; show: (elapsed // 10) printString; show: ' ms per run'; cr.
Transcript show: ''; cr.

Transcript show: '========================================'; cr.
Transcript show: '  Benchmark Suite Complete'; cr.
Transcript show: '========================================'; cr.

quit!
