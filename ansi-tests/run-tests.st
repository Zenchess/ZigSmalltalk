"Run all ANSI tests and report results"
| testSuite testResult | 

testSuite := TestSuite new.

"Add all ANSI test classes to the suite"
Smalltalk allClassesDo: [ :class |
    (class name asString endsWith: 'ANSITest') ifTrue: [
        testSuite addTest: class buildSuite
    ]
].

"Run the tests"
testResult := testSuite run.

"Report results"
Transcript show: '==========================================='; cr.
Transcript show: 'ANSI Test Results Summary'; cr.
Transcript show: '==========================================='; cr.
Transcript show: 'Total tests run: ', testResult testsRun printString; cr.
Transcript show: 'Failures: ', testResult failures size printString; cr.
Transcript show: 'Errors: ', testResult errors size printString; cr.
Transcript show: 'Skipped: ', testResult skipped size printString; cr.
Transcript show: ''; cr.

"List failures if any"
testResult failures do: [ :failure | 
    Transcript show: 'FAILURE: ', failure printString; cr
].

"List errors if any"  
testResult errors do: [ :error | 
    Transcript show: 'ERROR: ', error printString; cr
].

Transcript show: '==========================================='; cr.

"Exit with appropriate code"
(testResult failures size > 0 or: [ testResult errors size > 0]) 
    ifTrue: [ 
        Transcript show: 'Some tests failed!'; cr.
        Smalltalk exit: 1 
    ]
    ifFalse: [ 
        Transcript show: 'All tests passed!'; cr.
        Smalltalk exit: 0 
    ].
