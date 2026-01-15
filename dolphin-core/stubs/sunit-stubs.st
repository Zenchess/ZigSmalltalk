"Filed out stub support for SUnit in Zig Smalltalk"!

"Simple doesNotUnderstand: to prevent infinite recursion during exception handling"
!Object methodsFor!
doesNotUnderstand: aMessage
    "Minimal handler - print error and return nil to prevent infinite recursion.
     The ANSI Object.cls version tries to signal MessageNotUnderstood which can
     cause infinite recursion if the exception handling itself triggers DNU."
    Transcript show: 'Message not understood: '; show: aMessage selector; cr.
    ^nil!

primitiveFailed
    "Called when a primitive operation fails. Stub version that returns nil."
    Transcript show: 'Primitive failed in: '; show: self class name; cr.
    ^nil!

primitiveFailed: errorCode
    "Called when a primitive operation fails with an error code."
    Transcript show: 'Primitive failed with code: '; show: errorCode printString; cr.
    ^nil!

topFrame
    "Stub - return nil for stack frame"
    ^nil!

sender
    "Stub - return nil for sender frame"
    ^nil!

method
    "Stub - return nil for method"
    ^nil!

selector
    "Stub - return nil for selector"
    ^nil!

signal:
    "Stub - do nothing"
    ^nil!

signal: signalerText
    "Stub - signal with text"
    Transcript show: 'Signal: '; show: signalerText; cr.
    ^nil!
!

"FFI methods for String - must be loaded after ANSI String class"
!String methodsFor!
ffiCall: functionName with: args
    "Call an FFI function by name with arguments.
     The receiver is the library name (e.g., 'Raylib', 'LibMath').
     Uses primitive 700 for FFI dispatch."
    <primitive: 700>
    ^self primitiveFailed!

ffiFunctions
    "Return an Array of function names available in this FFI library."
    <primitive: 761>
    ^#()!
!

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

topFrame
    "Stub - return nil for stack frame"
    ^nil!

sender
    "Stub - return nil for sender frame"
    ^nil!

method
    "Stub - return nil for method"
    ^nil!

selector
    "Stub - return nil for selector"
    ^nil!

signal:
    "Stub - signal exception by returning self"
    ^self!

signal: signalerText withFailureCode: errorCode
    "Stub - signal with text and code"
    Transcript show: 'Exception signaled: '; show: signalerText; cr.
    ^self!
!

!TestResult methodsFor!
testsRun
    "Answer the total number of tests run"
    ^passed size + failures size + errors size + skipped size!
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

"Fix for copyFrom:to: when primitive 50 fails"
!ArrayedCollection methodsFor!
copyFrom: startInteger to: stopInteger
	"Answer a new sequencedReadableCollection containing elements from startInteger to stopInteger.
	 Reimplemented without primitive to work around primitive 50 issues."
	| len newColl |
	len := stopInteger - startInteger + 1.
	len < 0 ifTrue: [^self species new: 0].
	newColl := self species new: len.
	newColl replaceFrom: 1 to: len with: self startingAt: startInteger.
	^newColl!
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

printString
	"Answer a string representation of nil"
	^'nil'!
!

"String concatenation operator"
!String methodsFor!
, aString
	"Concatenate with another string"
	| result newSize |
	newSize := self size + aString size.
	result := String new: newSize.
	result replaceFrom: 1 to: self size with: self startingAt: 1.
	result replaceFrom: self size + 1 to: newSize with: aString startingAt: 1.
	^result!
!

"Helper for vowel checking - uses simple comparisons (includes: may not work)"
!Character methodsFor!
isVowel
	self = $a ifTrue: [^true].
	self = $e ifTrue: [^true].
	self = $i ifTrue: [^true].
	self = $o ifTrue: [^true].
	self = $u ifTrue: [^true].
	self = $A ifTrue: [^true].
	self = $E ifTrue: [^true].
	self = $I ifTrue: [^true].
	self = $O ifTrue: [^true].
	self = $U ifTrue: [^true].
	^false!

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

"Initialize Transcript global only if not already set"!
(Transcript isNil or: [Transcript class name = #DeafObject]) ifTrue: [Transcript := TranscriptStub new]!

"Object >> respondsTo: - check if receiver understands a selector"
!Object methodsFor!
respondsTo: aSymbol
	"Answer whether the receiver can respond to the selector aSymbol."
	^self class canUnderstand: aSymbol!
!

"Behavior >> canUnderstand: - check if class understands a selector"
!Behavior methodsFor!
canUnderstand: aSymbol
	"Answer whether instances of the receiver can respond to aSymbol.
	 Search the method dictionary chain."
	| cls |
	cls := self.
	[cls isNil] whileFalse: [
		(cls includesSelector: aSymbol) ifTrue: [^true].
		cls := cls superclass].
	^false!

includesSelector: aSymbol
	"Answer whether the receiver's method dictionary contains aSymbol."
	methodDictionary isNil ifTrue: [^false].
	| idx size |
	idx := 1.
	size := methodDictionary size.
	[idx <= size] whileTrue: [
		(methodDictionary at: idx) == aSymbol ifTrue: [^true].
		idx := idx + 2].
	^false!
!

"String >> asLowercase / asUppercase using VM primitives"
!String methodsFor!
asLowercase
	"Answer a new String with all characters converted to lowercase."
	<primitive: 863>
	"Fallback - do character-by-character conversion"
	| result |
	result := self class new: self size.
	1 to: self size do: [:i |
		result at: i put: (self at: i) asLowercase].
	^result!

asUppercase
	"Answer a new String with all characters converted to uppercase."
	<primitive: 862>
	"Fallback - do character-by-character conversion"
	| result |
	result := self class new: self size.
	1 to: self size do: [:i |
		result at: i put: (self at: i) asUppercase].
	^result!
!

"Character >> asLowercase / asUppercase for ASCII range"
!Character methodsFor!
asLowercase
	"Answer the lowercase equivalent of the receiver."
	| code |
	code := self codePoint.
	(code >= 65 and: [code <= 90]) ifTrue: [^Character codePoint: code + 32].
	^self!

asUppercase
	"Answer the uppercase equivalent of the receiver."
	| code |
	code := self codePoint.
	(code >= 97 and: [code <= 122]) ifTrue: [^Character codePoint: code - 32].
	^self!
!
