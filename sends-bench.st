"Sends per second benchmark"

!SmallInteger methodsFor: 'benchmarks'!
fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!
!

!SmallInteger methodsFor: 'benchmarks'!
sendBenchmark
    "Empty method for measuring raw send overhead"
    ^self!
!

| start end result sends time sendsPerSec count |

Transcript show: 'Sends/Second Benchmark'; cr.
Transcript show: '======================'; cr; cr.

Transcript show: '1. Fibonacci sends/sec calculation'; cr.
Transcript show: '   fib(30): '.
start := Delay millisecondClockValue.
result := 30 fibonacci.
end := Delay millisecondClockValue.
time := end - start.
sends := 5263683.
sendsPerSec := sends * 1000 // time.
Transcript show: time printString; show: ' ms, ~5.3M sends'; cr.
Transcript show: '   => '; show: sendsPerSec printString; show: ' sends/sec'; cr; cr.

Transcript show: '   fib(35): '.
start := Delay millisecondClockValue.
result := 35 fibonacci.
end := Delay millisecondClockValue.
time := end - start.
sends := 58375139.
sendsPerSec := sends * 1000 // time.
Transcript show: time printString; show: ' ms, ~58M sends'; cr.
Transcript show: '   => '; show: sendsPerSec printString; show: ' sends/sec'; cr; cr.

Transcript show: '2. Raw send benchmark (1M sends)'; cr.
count := 0.
start := Delay millisecondClockValue.
1000000 timesRepeat: [count := count sendBenchmark].
end := Delay millisecondClockValue.
time := end - start.
time = 0 ifTrue: [time := 1].
sendsPerSec := 1000000 * 1000 // time.
Transcript show: '   1M sends in '; show: time printString; show: ' ms'; cr.
Transcript show: '   => '; show: sendsPerSec printString; show: ' sends/sec'; cr; cr.

Transcript show: '3. Raw send benchmark (10M sends)'; cr.
count := 0.
start := Delay millisecondClockValue.
10000000 timesRepeat: [count := count sendBenchmark].
end := Delay millisecondClockValue.
time := end - start.
time = 0 ifTrue: [time := 1].
sendsPerSec := 10000000 * 1000 // time.
Transcript show: '   10M sends in '; show: time printString; show: ' ms'; cr.
Transcript show: '   => '; show: sendsPerSec printString; show: ' sends/sec'; cr; cr.

Transcript show: '======================'; cr.
Transcript show: 'Reference: Classic Smalltalk VMs'; cr.
Transcript show: '  Squeak Stack VM: ~20-50M sends/sec'; cr.
Transcript show: '  Pharo Cog JIT:   ~100-200M sends/sec'; cr.
Transcript show: '  VisualWorks:     ~50-100M sends/sec'; cr.
Transcript show: '======================'; cr.

quit
