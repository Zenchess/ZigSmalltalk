Object subclass: #SimpleTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!SimpleTest methodsFor!

testBooleanOps
    | x y |
    x := (true and: [true]).
    y := (false or: [true]).
    ^x and: [y]!

testIfTrueIfFalse
    | result |
    result := true ifTrue: [1] ifFalse: [2].
    result = 1 ifFalse: [^false].
    result := false ifTrue: [1] ifFalse: [2].
    result = 2 ifFalse: [^false].
    ^true!

testMultiAssign
    | a b c |
    a := 1.
    b := 2.
    c := a + b.
    ^c = 3!

testNestedBlock
    | result |
    result := true ifTrue: [
        false ifTrue: [1] ifFalse: [2]
    ].
    ^result = 2!

testWhileLoop
    | count |
    count := 0.
    [count < 3] whileTrue: [count := count + 1].
    ^count = 3!
!
