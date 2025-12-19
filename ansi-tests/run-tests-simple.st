"Simple ANSI test runner that outputs to Transcript"
| testSuite testResult |

"Create test suite and add ANSI test classes"
testSuite := TestSuite new.

"Find and add all ANSI test classes"
Transcript show: 'Calling allClassesDo...'; cr.
Smalltalk allClassesDo: [ :class |
    | className suite allSelectors testSelectors |
    className := class name asString.
    (className endsWith: 'ANSITest') ifTrue: [
        allSelectors := class selectors.
        testSelectors := class testSelectors.
        Transcript show: '  Class: '; show: className;
            show: ' - total methods: '; show: allSelectors size printString;
            show: ', test methods: '; show: testSelectors size printString; cr.
        (testSelectors size > 0) ifTrue: [
            Transcript show: '    Building suite manually for '; show: className; cr.
            suite := TestSuite new.
            suite name: className.
            Transcript show: '      Adding '; show: testSelectors size printString; show: ' tests...'; cr.
            1 to: testSelectors size do: [:i |
                Transcript show: '        Test '; show: i printString; show: '...'; cr.
                suite addTest: (class selector: (testSelectors at: i)).
                Transcript show: '          Suite now has '; show: suite tests size printString; show: ' tests'; cr.
            ].
            Transcript show: '    Suite final size: '; show: suite tests size printString; cr.
            testSuite addTest: suite
        ]
    ]
].
Transcript show: 'Done finding test classes'; cr.

"Run all tests"
Transcript show: 'Running tests...'; cr.
testResult := testSuite run.

"Print summary"
Transcript show: '==========================================='; cr.
Transcript show: 'ANSI Test Results Summary'; cr.
Transcript show: '==========================================='; cr.
Transcript show: 'Total tests run: '; show: testResult runCount printString; cr.
Transcript show: 'Passed: '; show: testResult passedCount printString; cr.
Transcript show: 'Failed: '; show: testResult failureCount printString; cr.
Transcript show: 'Errors: '; show: testResult errorCount printString; cr.
Transcript show: '==========================================='; cr.

"Exit status"
testResult hasPassed
    ifTrue: [
        Transcript show: 'All tests passed!'; cr.
    ]
    ifFalse: [
        Transcript show: 'Some tests failed!'; cr.
    ].