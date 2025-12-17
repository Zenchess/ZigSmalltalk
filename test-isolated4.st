"Isolated test - primitive vs non-primitive"

!SmallInteger methodsFor!

fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!

simpleReturn
    ^self!

testL
    "Call primitive (negated) then fib"
    | x |
    x := 5 negated.
    ^28 fibonacci!

testM
    "Call non-primitive method (simpleReturn) then fib"
    | x |
    x := 5 simpleReturn.
    ^28 fibonacci!

testN
    "Call abs (primitive) then fib"
    | x |
    x := -5 abs.
    ^28 fibonacci!

!
