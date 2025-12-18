"Test JIT fibonacci"

!SmallInteger methodsFor: 'benchmarks'!
fibonacci
    "Classic recursive fibonacci"
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!
!

| result |
Transcript show: 'Computing fib(20)...'; cr.
result := 20 fibonacci.
Transcript show: 'Result: '; show: result printString; cr.
quit!
