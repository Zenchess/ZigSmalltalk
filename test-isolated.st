"Isolated test for JIT bug"

!SmallInteger methodsFor!

fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!

testA
    ^28 fibonacci!

testB
    | x |
    x := 999.
    ^28 fibonacci!

testC
    | x |
    x := Delay millisecondClockValue.
    ^28 fibonacci!

testD
    | x |
    x := self class.
    ^28 fibonacci!

!
