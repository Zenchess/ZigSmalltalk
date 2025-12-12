"Filed out stub support for SUnit in Zig Smalltalk"!

Object subclass: #Delay
    instanceVariableNames: 'duration'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Delay methodsFor!
duration
    ^duration!

duration: anInteger
    duration := anInteger!

wait
    "No-op delay; just answer self"
    ^self!
!

!Delay class methodsFor!
microsecondClockValue
    "Return a monotonic-ish counter; stubbed as 0 for now"
    ^0!

forMicroseconds: us
    ^self new duration: us!

forMilliseconds: ms
    ^self new duration: ms * 1000!

forSeconds: s
    ^self new duration: s * 1000000!
!

Object subclass: #Semaphore
    instanceVariableNames: 'count'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Semaphore methodsFor!
initialize
    count := 0!

signal
    count := count + 1.
    ^count!

wait
    count := (count - 1) max: 0.
    ^count!

excessSignals
    ^count!
!

Dictionary variableSubclass: #LookupTable
    instanceVariableNames: 'values'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!LookupTable methodsFor!
"Inherit Dictionary behaviour; no changes needed for SUnit use"!
!

!Class methodsFor!
isAbstract
    ^false!
!

!Exception methodsFor!
sunitExitWith: aValue
    ^aValue!
!

"Alias used by SUnit"
SUnitDelay := Delay!

"Lightweight sorted collection support without pulling in Dolphin's SortAlgorithm hierarchy."
!SequenceableCollection methodsFor!
asSortedCollection
	"Return a shallow copy; deterministic ordering isn't needed for SUnit stubs."
	^self asArray copy!

asSortedCollection: aComparator
	"Return a shallow copy; ignore comparator in this minimal stub."
	^self asArray copy!
!

!Behavior methodsFor!
isVariable
	"Treat all classes as fixed layout for now; adequate for SUnit core usage."

	^false!
!

!TestCase class methodsFor!
buildSuiteFromSelectors
	"Minimal suite builder that works with the current primitive set."

	| suite selectors |
	selectors := self shouldInheritSelectors
		ifTrue: [self allSelectors]
		ifFalse: [self selectors].
	suite := TestSuite new name: self name asString; yourself.
	selectors do: [:sel |
		((sel beginsWith: 'test') and: [sel numArgs isZero])
			ifTrue: [suite addTest: (self selector: sel)]].
	^suite!
!

!Set class methodsFor!
new
	"Answer a new Set with default capacity. Simplified stub that avoids class constants."
	^(self basicNew: 7) initialize!

new: count
	"Answer a new Set with given capacity. Use next larger prime-ish size."
	| size |
	size := count < 3 ifTrue: [3] ifFalse: [count * 2 + 1].
	^(self basicNew: size) initialize!
!

Array variableSubclass: #WeakArray
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!WeakArray methodsFor!
nonCorpsesDo: aBlock
	"Iterate over non-nil elements"
	self do: [:each | each isNil ifFalse: [aBlock value: each]]!
!

"Stub out dependency mechanism - _DependentsRegister is not initialized"
!Object methodsFor!
getDependents
	"Stub - return nil (no dependents)"
	^nil!

dependents
	"Answer empty collection - no dependency tracking"
	^#()!

changed: anAspect
	"Stub - do nothing, no observers to notify"
	^self!

changed: anAspect with: aParameter
	"Stub - do nothing"
	^self!
!

"Stub for Character codePoint - our Characters are immediates, not objects"
!Character methodsFor!
codePoint
	"Answer the Unicode code point as an integer. Uses VM primitive for immediate chars."
	<primitive: 520>
	^self primitiveFailed!

asInteger
	"Answer the code point as an integer."
	^self codePoint!
!

"Stub for String hash - VMConstants.SmallIntegerMax not available"
!String methodsFor!
hash
	"FNV1a hash folded to 30 bits for SmallInteger range."
	| utf8 h |
	utf8 := self asUtf8String.
	h := 2166136261.
	1 to: utf8 size do: [:i |
		h := ((h bitXor: (utf8 basicAt: i) codePoint) * 16777619) bitAnd: 16rFFFFFFFF].
	"Fold to 30 bits"
	^((h bitShift: -30) bitXor: h) bitAnd: 16r3FFFFFFF!
!

"Stub for chained exception handling - ExceptionHandlerSet isn't loaded"
!BlockClosure methodsFor!
on: selector1 do: action1 on: selector2 do: action2
	"Chain two exception handlers by nesting on:do: calls."
	^[[self value] on: selector1 do: action1] on: selector2 do: action2!

on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3
	"Chain three exception handlers."
	^[[[self value] on: selector1 do: action1] on: selector2 do: action2] on: selector3 do: action3!

on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3 on: selector4 do: action4
	"Chain four exception handlers."
	^[[[[self value] on: selector1 do: action1] on: selector2 do: action2] on: selector3 do: action3] on: selector4 do: action4!
!

"Simple printString and isZero for SmallInteger"
!SmallInteger methodsFor!
printString
	"Answer a string representation of the receiver using VM primitive."
	<primitive: 44>
	^self primitiveFailed!

displayString
	^self printString!

isZero
	"Answer whether the receiver is zero."
	^self == 0!
!

"Simple printString for Object using basic format"
!Object methodsFor!
printString
	"Answer a basic string representation of the receiver."
	| className |
	className := self class name.
	className isNil ifTrue: [^'anObject'].
	(className first isVowel)
		ifTrue: [^'an' , className]
		ifFalse: [^'a' , className]!

displayString
	^self printString!

ifNil: nilBlock
	"Answer self - receiver is not nil"
	^self!

ifNotNil: notNilBlock
	"Evaluate the block with self as argument"
	^notNilBlock value: self!

ifNil: nilBlock ifNotNil: notNilBlock
	"Evaluate notNilBlock with self"
	^notNilBlock value: self!

ifNotNil: notNilBlock ifNil: nilBlock
	"Evaluate notNilBlock with self"
	^notNilBlock value: self!
!

!UndefinedObject methodsFor!
ifNil: nilBlock
	"Evaluate and answer the result of the block"
	^nilBlock value!

ifNotNil: notNilBlock
	"Answer nil - receiver is nil"
	^nil!

ifNil: nilBlock ifNotNil: notNilBlock
	"Evaluate nilBlock"
	^nilBlock value!

ifNotNil: notNilBlock ifNil: nilBlock
	"Evaluate nilBlock"
	^nilBlock value!
!

"Helper for vowel checking"
!Character methodsFor!
isVowel
	^#($a $e $i $o $u $A $E $I $O $U) includes: self!

= aCharacter
	"Answer whether receiver equals the argument. Characters are immediate values so == works."
	^self == aCharacter!
!

"Simple numArgs for Symbol - counts colons"
!Symbol methodsFor!
numArgs
	"Answer the number of arguments required. Count colons."
	| count |
	count := 0.
	1 to: self size do: [:i | (self basicAt: i) = $: ifTrue: [count := count + 1]].
	^count!
!

"Fix beginsWith: to use simple index-based comparison without non-local returns"
!SequenceableCollection methodsFor!
beginsWith: aCollection
	"Answer whether the receiver begins with the sequence of objects in aCollection."
	| mySize theirSize matches |
	mySize := self size.
	theirSize := aCollection size.
	mySize < theirSize ifTrue: [^false].
	matches := true.
	1 to: theirSize do: [:i |
		matches ifTrue: [
			(self at: i) = (aCollection at: i) ifFalse: [matches := false]]].
	^matches!
!

Object subclass: #TranscriptStub
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!TranscriptStub methodsFor!
show: anObject
	"Output to stdout using primitive."
	<primitive: 612>
	^self!

nextPutAll: aString
	^self show: aString!

cr
	^self show: '
'!

print: anObject
	^self show: anObject printString!
!

"Initialize Transcript global"!
Transcript := TranscriptStub new!
