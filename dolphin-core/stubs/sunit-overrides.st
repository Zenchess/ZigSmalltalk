"Temporary overrides for minimal SUnit execution"!

!Collection methodsFor!
select: discriminator
	| newCollection |
	newCollection := self newSelection.
	self do: [:each | (discriminator value: each) ifTrue: [newCollection add: each]].
	^newCollection!
!

!Collection methodsFor!
reject: discriminator
	| newCollection |
	newCollection := self newSelection.
	self do: [:each | | keep | keep := (discriminator value: each) not. keep ifTrue: [newCollection add: each]].
	^newCollection!
!

!OrderedCollection class methodsFor!
new
	"Answer a new empty OrderedCollection with large initial capacity"
	^self new: 100!
!

!String methodsFor!
beginsWith: aCollection
	"Answer whether the receiver begins with the sequence of objects in aCollection."
	^self basicBeginsWith: aCollection!
!

!Set methodsFor!
do: operation
	tally == 0 ifTrue: [^self].
	1 to: self basicSize do: [:index |
		| element |
		element := self basicAt: index.
		element isNil ifFalse: [operation value: element]]!
!

!Set class methodsFor!
new
	^(self basicNew: 3) initialize!

new: count
	^(self basicNew: (self sizeFor: count)) initialize!
!

!Dictionary class methodsFor!
new
	^(self basicNew: 3) initialize!

new: count
	^(self basicNew: (self sizeFor: count)) initialize!
!

!LookupTable class methodsFor!
new
	^(self basicNew: 3) initialize!

new: count
	^(self basicNew: (self sizeFor: count)) initialize!
!

!TestCase methodsFor!
runCase
	self setUp.
	self performTest.
	self tearDown.
	^self!
!

!TestCase class methodsFor!
buildSuiteFromSelectors
	"Minimal suite builder - use select: which is known to work"
	| suite selectors testSelectors |
	suite := TestSuite new.
	selectors := self selectors.
	testSelectors := selectors select: [:s | s beginsWith: 'test'].
	testSelectors := testSelectors select: [:s | (s includes: $:) not].
	testSelectors do: [:sel | suite addTest: (self selector: sel)].
	^suite!

buildSuite
	"Build a suite from selectors matching 'test*' pattern."
	^self buildSuiteFromSelectors!
!

!TestSuite methodsFor!
run
	| result |
	result := TestResult new.
	self run: result.
	^result!

run: aResult
	self tests do: [:each | each run: aResult].
	^aResult!
!

!Behavior methodsFor!
selectors
	"Return the selectors from the array-backed methodDictionary (selector/method pairs)."
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

!Character methodsFor!
codePoint
	"Answer the integer code point"
	<primitive: 520>
	^self primitiveFailed!

isLetter
	"Simple ASCII-only stub for isLetter"
	| code |
	code := self codePoint.
	^(code >= 65 and: [code <= 90]) or: [code >= 97 and: [code <= 122]]!

isDigit
	"Simple ASCII-only stub for isDigit"
	| code |
	code := self codePoint.
	^code >= 48 and: [code <= 57]!
!

!Symbol methodsFor!
argumentCount
	"Count colons to determine argument count for unary/keyword selectors"
	| len ch count |
	len := self size.
	len == 0 ifTrue: [^0].
	ch := self at: 1.
	"Binary selector - always 1 arg"
	((ch codePoint >= 65 and: [ch codePoint <= 90]) or: [ch codePoint >= 97 and: [ch codePoint <= 122]]) ifFalse: [^1].
	"Keyword or unary - count colons"
	count := 0.
	1 to: len do: [:i | (self at: i) == $: ifTrue: [count := count + 1]].
	^count!
!

!Array methodsFor!
keys
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
