Object subclass: #CoreTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!CoreTest methodsFor!

testSimple
    ^true!

testEquality
    self assert: 1 = 1.
    self assert: (2 = 3) not.
    ^true!

testArithmetic
    self assert: 2 + 2 = 4.
    self assert: 10 - 3 = 7.
    ^true!

testComparison
    self assert: 3 < 5.
    self assert: 5 > 3.
    ^true!

testBooleanOperations
    | x y |
    x := (true and: [true]).
    y := (false or: [true]).
    self assert: x.
    self assert: y.
    ^true!

testIfTrueIfFalse
    | result |
    result := true ifTrue: [1] ifFalse: [2].
    self assert: result = 1.
    result := false ifTrue: [1] ifFalse: [2].
    self assert: result = 2.
    ^true!

testBlocks
    | b |
    b := [42].
    self assert: b value = 42.
    ^true!

testBlockWithArg
    | b |
    b := [:x | x + 1].
    self assert: (b value: 5) = 6.
    ^true!

testMultipleTempAssignments
    | a b c |
    a := 1.
    b := 2.
    c := a + b.
    self assert: c = 3.
    ^true!

testNestedBlockReturn
    | result |
    result := true ifTrue: [
        false ifTrue: [1] ifFalse: [2]
    ].
    self assert: result = 2.
    ^true!

testWhileLoop
    | count |
    count := 0.
    [count < 3] whileTrue: [count := count + 1].
    self assert: count = 3.
    ^true!

testArrayAccess
    | arr |
    arr := #(10 20 30).
    self assert: (arr at: 1) = 10.
    self assert: (arr at: 2) = 20.
    ^true!

testStrings
    | s |
    s := 'hello'.
    self assert: s size = 5.
    ^true!

testClass
    self assert: 42 class = SmallInteger.
    ^true!

assert: condition
    condition ifFalse: [self error: 'Assertion failed']!

runAllTests
    | tests passed failed |
    tests := #(testSimple testEquality testArithmetic testComparison testBooleanOperations testIfTrueIfFalse testBlocks testBlockWithArg testMultipleTempAssignments testNestedBlockReturn testWhileLoop testArrayAccess testStrings testClass).
    passed := 0.
    failed := 0.
    tests do: [:testName |
        | result |
        Transcript show: 'Running '; show: testName; show: '... '.
        result := [self perform: testName] on: Error do: [:e | nil].
        result ifNotNil: [
            passed := passed + 1.
            Transcript show: 'PASSED'; cr
        ] ifNil: [
            failed := failed + 1.
            Transcript show: 'FAILED'; cr
        ]
    ].
    Transcript cr; show: 'Results: '; show: passed; show: ' passed, '; show: failed; show: ' failed'; cr.
    ^passed!
!
