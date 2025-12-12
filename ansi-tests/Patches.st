!Behavior methodsFor!

isAbstract
    ^self isNonInstantiable!
!

!Object methodsFor!

species
    ^self class!
!

!Object methodsFor!

ifNil: aBlock
    ^self
!

!Object methodsFor!

ifNotNil: aBlock
    ^aBlock value: self!
!

!Behavior methodsFor!

allSubclasses
    | subs answer |
    answer := OrderedCollection new.
    subs := subclasses.
    subs ifNotNil: [:arr |
        arr do: [:each |
            answer addLast: each.
            answer addAll: each allSubclasses]].
    ^answer asArray!
!

!Character methodsFor!

code
    "Answer the code point value of the receiver"
    <primitive: 520>
    ^self primitiveFailed!

= comperand
    "Answer whether the receiver is the same as the parameter, comperand."
    ^self == comperand or: [comperand class == Character and: [comperand code == self code]]!
!

!TestCase class methodsFor!
allTestSelectors
    "Return an OrderedCollection so MainTestCase can extend it with addAll:"
    | result |
    result := OrderedCollection new.
    self allSelectors do: [:each |
        ((each beginsWith: 'test') and: [(each identityIncludes: $:) not])
            ifTrue: [result add: each]].
    ^result!
!

!SortedCollection class methodsFor!
new
    "Answer a default SortedCollection with default sort algorithm"
    ^self sortAlgorithm: self defaultSortAlgorithm!

new: anInteger
    "Answer a SortedCollection with capacity anInteger and default sort algorithm"
    ^(super new: anInteger) setSortAlgorithm: self defaultSortAlgorithm; yourself!
!

!Set class methodsFor!
new
    "Answer an instance with default capacity - workaround for non-local return issue"
    | size set |
    size := self sizeFor: self defaultCapacity.
    set := self basicNew: size.
    set initialize.
    ^set!

new: count
    "Answer an instance with given capacity - workaround for non-local return issue"
    | size set |
    size := self sizeFor: count.
    set := self basicNew: size.
    set initialize.
    ^set!
!

!BlockClosure methodsFor!

ensure: terminationBlock
    "Simplified ensure - evaluate receiver then terminationBlock"
    | answer |
    answer := self value.
    terminationBlock value.
    ^answer!

ifCurtailed: terminationBlock
    "Simplified ifCurtailed - just evaluate receiver for now (no exception support)"
    ^self value!

"Note: on:do: implementation with primitive 812 is in SmalltalkSystem-stubs.st"
!

!TestCase methodsFor!

simpleRun
    "Run test without exception handling framework"
    | passed |
    passed := true.
    self setUp.
    self performTest.
    self tearDown.
    ^passed!
!

!TestCase class methodsFor!

runAllSimple
    "Run all tests without exception framework, print results"
    | passed failed selectors |
    passed := 0.
    failed := 0.
    selectors := self allTestSelectors.
    selectors do: [:sel |
        | test result |
        test := self selector: sel.
        result := test simpleRun.
        result ifTrue: [
            passed := passed + 1.
            Transcript show: '.']
        ifFalse: [
            failed := failed + 1.
            Transcript show: 'F']].
    Transcript cr.
    Transcript show: 'Passed: '; print: passed; show: ' Failed: '; print: failed; cr.
    ^passed!
!
