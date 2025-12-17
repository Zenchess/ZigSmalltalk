"Isolated test for JIT bug - different primitives"

!SmallInteger methodsFor!

fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!

testE
    "Call size primitive on an array, then fib"
    | x arr |
    arr := Array new: 5.
    x := arr size.
    ^28 fibonacci!

testF
    "Call + primitive, then fib"
    | x |
    x := 1 + 2.
    ^28 fibonacci!

testG
    "Two primitives, then fib"
    | x y |
    x := 1 + 2.
    y := 3 + 4.
    ^28 fibonacci!

testH
    "Call Delay millisecondClockValue twice"
    | x y |
    x := Delay millisecondClockValue.
    y := Delay millisecondClockValue.
    ^y - x!

!
