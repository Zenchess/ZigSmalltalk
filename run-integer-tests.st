"ANSI Integer Tests"

| testClass testSelectors passed errors |
testClass := IntegerANSITest.
testSelectors := testClass selectors select: [:sel | sel asString beginsWith: 'test'].
Transcript show: 'Running '; show: testSelectors size printString; show: ' tests from IntegerANSITest'; cr.

passed := 0.
errors := 0.

testSelectors do: [:sel |
    | test |
    test := testClass new.
    [[
        test perform: sel.
        passed := passed + 1.
    ] on: TestFailure do: [:ex |
        Transcript show: 'F'.
    ]] on: Error do: [:ex |
        errors := errors + 1.
    ].
].

Transcript cr.
Transcript show: 'Passed: '; show: passed printString; cr.
Transcript show: 'Errors: '; show: errors printString; cr!
