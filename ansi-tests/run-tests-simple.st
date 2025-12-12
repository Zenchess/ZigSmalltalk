"Simple ANSI test runner that outputs to stdout"
| testSuite testResult passedCount failedCount errorCount |

"Initialize counters"
passedCount := 0.
failedCount := 0.
errorCount := 0.

"Create test suite and add ANSI test classes"
testSuite := TestSuite new.

"Find and add all ANSI test classes"
Smalltalk allClassesDo: [ :class |
    (class name endsWith: 'ANSITest') ifTrue: [
        testSuite addTest: class new
    ]
].

"Run tests and collect results"
testSuite tests do: [ :test |
    [
        test runCase.
        passedCount := passedCount + 1.
        stdout nextPutAll: 'PASS: '; nextPutAll: test printString; cr.
    ] on: TestFailure do: [ :failure |
        failedCount := failedCount + 1.
        stdout nextPutAll: 'FAIL: '; nextPutAll: test printString; nextPutAll: ' - '; nextPutAll: failure messageText; cr.
    ] on: Error do: [ :error |
        errorCount := errorCount + 1.
        stdout nextPutAll: 'ERROR: '; nextPutAll: test printString; nextPutAll: ' - '; nextPutAll: error messageText; cr.
    ]
].

"Print summary"
stdout nextPutAll: '==========================================='; cr.
stdout nextPutAll: 'ANSI Test Results Summary'; cr.
stdout nextPutAll: '==========================================='; cr.
stdout nextPutAll: 'Total tests: '; nextPutAll: testSuite tests size printString; cr.
stdout nextPutAll: 'Passed: '; nextPutAll: passedCount printString; cr.
stdout nextPutAll: 'Failed: '; nextPutAll: failedCount printString; cr.
stdout nextPutAll: 'Errors: '; nextPutAll: errorCount printString; cr.
stdout nextPutAll: '==========================================='; cr.

"Exit with appropriate code"
(failedCount > 0 or: [ errorCount > 0])
    ifTrue: [
        stdout nextPutAll: 'Some tests failed!'; cr.
        Smalltalk exit: 1
    ]
    ifFalse: [
        stdout nextPutAll: 'All tests passed!'; cr.
        Smalltalk exit: 0
    ].