Transcript show: '===== ANSI Test Suite ====='; cr!

| passed errors |
passed := 0.
errors := 0.
IntegerANSITest selectors do: [:sel |
    (sel asString beginsWith: 'test') ifTrue: [
        | test |
        test := IntegerANSITest new.
        [[test perform: sel. passed := passed + 1.] on: TestFailure do: [:ex |]] on: Error do: [:ex | errors := errors + 1.].
    ].
].
Transcript show: 'IntegerANSITest: '; show: passed printString; show: ' passed, '; show: errors printString; show: ' errors'; cr!

| passed errors |
passed := 0.
errors := 0.
BooleanANSITest selectors do: [:sel |
    (sel asString beginsWith: 'test') ifTrue: [
        | test |
        test := BooleanANSITest new.
        [[test perform: sel. passed := passed + 1.] on: TestFailure do: [:ex |]] on: Error do: [:ex | errors := errors + 1.].
    ].
].
Transcript show: 'BooleanANSITest: '; show: passed printString; show: ' passed, '; show: errors printString; show: ' errors'; cr!

| passed errors |
passed := 0.
errors := 0.
NilANSITest selectors do: [:sel |
    (sel asString beginsWith: 'test') ifTrue: [
        | test |
        test := NilANSITest new.
        [[test perform: sel. passed := passed + 1.] on: TestFailure do: [:ex |]] on: Error do: [:ex | errors := errors + 1.].
    ].
].
Transcript show: 'NilANSITest: '; show: passed printString; show: ' passed, '; show: errors printString; show: ' errors'; cr!

| passed errors |
passed := 0.
errors := 0.
CharacterANSITest selectors do: [:sel |
    (sel asString beginsWith: 'test') ifTrue: [
        | test |
        test := CharacterANSITest new.
        [[test perform: sel. passed := passed + 1.] on: TestFailure do: [:ex |]] on: Error do: [:ex | errors := errors + 1.].
    ].
].
Transcript show: 'CharacterANSITest: '; show: passed printString; show: ' passed, '; show: errors printString; show: ' errors'; cr!

| passed errors |
passed := 0.
errors := 0.
StringANSITest selectors do: [:sel |
    (sel asString beginsWith: 'test') ifTrue: [
        | test |
        test := StringANSITest new.
        [[test perform: sel. passed := passed + 1.] on: TestFailure do: [:ex |]] on: Error do: [:ex | errors := errors + 1.].
    ].
].
Transcript show: 'StringANSITest: '; show: passed printString; show: ' passed, '; show: errors printString; show: ' errors'; cr!

| passed errors |
passed := 0.
errors := 0.
ArrayANSITest selectors do: [:sel |
    (sel asString beginsWith: 'test') ifTrue: [
        | test |
        test := ArrayANSITest new.
        [[test perform: sel. passed := passed + 1.] on: TestFailure do: [:ex |]] on: Error do: [:ex | errors := errors + 1.].
    ].
].
Transcript show: 'ArrayANSITest: '; show: passed printString; show: ' passed, '; show: errors printString; show: ' errors'; cr!

Transcript show: '==========================='; cr!
