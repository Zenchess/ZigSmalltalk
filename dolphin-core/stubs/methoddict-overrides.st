"Helper overrides for array-backed method dictionaries"!

!Behavior methodsFor!
selectors
    "Return selectors from the selector/method pair array used as a method dictionary."

    methodDictionary isNil ifTrue: [^#()].
    | out idx size |
    out := OrderedCollection new.
    idx := 1.
    size := methodDictionary size.
    [idx <= size] whileTrue: [
        | sel |
        sel := methodDictionary at: idx.
        sel isNil ifFalse: [out add: sel].
        idx := idx + 2].
    ^out asArray!

selectorsDo: aBlock
    methodDictionary isNil ifTrue: [^self].
    | idx size |
    idx := 1.
    size := methodDictionary size.
    [idx <= size] whileTrue: [
        | sel |
        sel := methodDictionary at: idx.
        sel isNil ifFalse: [aBlock value: sel].
        idx := idx + 2]!
!

!Array methodsFor!
keys
    "Return every other element assuming selector/method pairs (used for methodDictionary)."
    | out idx size |
    out := OrderedCollection new.
    idx := 1.
    size := self size.
    [idx <= size] whileTrue: [
        | key |
        key := self at: idx.
        key isNil ifFalse: [out add: key].
        idx := idx + 2].
    ^out asArray!

keysDo: aBlock
    | idx size |
    idx := 1.
    size := self size.
    [idx <= size] whileTrue: [
        | key |
        key := self at: idx.
        key isNil ifFalse: [aBlock value: key].
        idx := idx + 2]!
!
