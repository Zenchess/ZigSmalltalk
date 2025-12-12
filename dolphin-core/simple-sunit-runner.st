Object subclass: #SimpleSUnitRunner
	instanceVariableNames: ''
	classVariableNames: 'LastReport'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

!SimpleSUnitRunner methodsFor: 'running'!
run
	| classes selectors runCount passed failures errors skipped report |

	"Collect the canonical test suites we care about."
	classes := {
		SUnitTest.
		ExampleSetTest.
		ResumableTestFailureTestCase.
		SimpleTestResourceTestCase
	}.

	selectors := OrderedCollection new.
	classes do: [:cls |
		cls testSelectors do: [:sel |
			selectors add: (cls selector: sel)]].

	runCount := 0.
	passed := OrderedCollection new.
	failures := OrderedCollection new.
	errors := OrderedCollection new.
	skipped := OrderedCollection new.

	selectors do: [:test |
		runCount := runCount + 1.
		[test runCase.
			passed add: test]
			on: TestSkip do: [:skip |
				skipped add: test]
			on: TestFailure do: [:fail |
				failures add: test]
			on: Error do: [:err |
				errors add: test]]].

	report := self reportWithRunCount: runCount
		passed: passed
		failures: failures
		errors: errors
		skipped: skipped.
	SimpleSUnitRunner lastReport: report.
	^report!

!SimpleSUnitRunner methodsFor: 'reporting'!
	reportWithRunCount: runCount passed: passed failures: failures errors: errors skipped: skipped
	^String streamContents: [:stream |
		stream
			nextPutAll: 'run=' , runCount printString;
			cr.
		stream
			nextPutAll: 'passed=' , passed size printString;
			cr.
		stream
			nextPutAll: 'failures=' , failures size printString;
			cr.
		failures do: [:each |
			stream
				nextPutAll: '  failed: ' , each printString;
				cr].
		stream
			nextPutAll: 'errors=' , errors size printString;
			cr.
		errors do: [:each |
			stream
				nextPutAll: '  error: ' , each printString;
				cr].
		stream
			nextPutAll: 'skipped=' , skipped size printString;
			cr.
		skipped do: [:each |
			stream
				nextPutAll: '  skipped: ' , each printString;
				cr].
	]!
!
!SimpleSUnitRunner class methodsFor: 'reporting'!
lastReport
	^LastReport!

lastReport: aString
	LastReport := aString!
!
