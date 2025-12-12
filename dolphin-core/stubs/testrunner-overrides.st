"Headless overrides for TestRunner so the ANSI suite can run without a GUI"!

!TestRunner methodsFor!
createSchematicWiring
	^self
!

displayClear
	^self
!

displayColor: aSymbol
	^self
!

displayDefects: aCollection
	^self
!

displayDetails: aString
	^self
!

displayFail
	^self
!

displayGreen
	^self
!

displayMode: aString
	^self
!

displayPass
	^self
!

displayRed
	^self
!

displayReset
	^self
!

displayRunning
	^self
!

displayYellow
	^self
!

updateDefects
	^self
!

updateWindow
	^self
!

runSuite: aTestSuite
	"a lightweight headless runner."

	aTestSuite addDependentToHierachy: self.
	result := aTestSuite run.
	aTestSuite removeDependentFromHierachy: self.
	^result
! !

!TestSuite methodsFor!
tests
	tests isNil ifTrue: [tests := OrderedCollection new].
	^tests!

addTest: aTest
	tests isNil ifTrue: [tests := OrderedCollection new].
	tests add: aTest!

run
	"Run all tests headlessly and return the result."

	| result |
	result := TestResult new.
	self run: result.
	^result!
!

run: aResult
	"Execute each test and accumulate into the supplied result."

	self tests do: [:each | each run: aResult].
	^aResult!
! !
