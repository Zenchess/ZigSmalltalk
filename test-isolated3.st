"Isolated test - non-SmallInteger sends"

!SmallInteger methodsFor!

fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!

testI
    "Send to Array without primitive"
    | x arr |
    arr := Array new: 5.
    x := arr isEmpty.
    ^28 fibonacci!

testJ
    "Just create array"
    | arr |
    arr := Array new: 5.
    ^28 fibonacci!

testK
    "Send printString to an integer (non-primitive method)"
    | x |
    x := 42 printString.
    ^28 fibonacci!

!
