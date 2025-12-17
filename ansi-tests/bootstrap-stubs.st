"Bootstrap stubs to prevent infinite recursion in doesNotUnderstand:"

"Simple Object>>doesNotUnderstand: that doesn't create complex exceptions"
!Object methodsFor: 'error handling'!
doesNotUnderstand: aMessage
    "Minimal handler - just return nil to prevent infinite recursion"
    ^nil!
!

"Simple Exception class that doesn't require complex initialization"
!Exception class methodsFor: 'instance creation'!
new
    "Answer a new instance - simple version without initialize"
    ^self basicNew!
!

!Exception methodsFor: 'accessing'!
initialize
    "Simple initialize that does nothing"
    ^self!

messageText
    "Answer the message text"
    ^messageText!

messageText: aString
    "Set the message text"
    messageText := aString.
    ^self!

tag
    "Answer the tag"
    ^tag ifNil: [messageText]!

tag: anObject
    "Set the tag"
    tag := anObject.
    ^self!
!

!Exception methodsFor: 'signaling'!
signal
    "Simple signal - just answer self for now"
    ^self!

signal: aString
    "Signal with message"
    self messageText: aString.
    ^self signal!
!

"MessageNotUnderstood simple implementation"
!MessageNotUnderstood class methodsFor: 'instance creation'!
receiver: anObject message: aMessage
    "Create and signal - simplified"
    | ex |
    ex := self new.
    ex receiver: anObject.
    ex tag: aMessage.
    ^ex!
!

!MessageNotUnderstood methodsFor: 'accessing'!
receiver
    ^receiver!

receiver: anObject
    receiver := anObject.
    ^self!

message
    ^self tag!

selector
    ^self message selector!
!

"Error simple defaultAction"
!Error methodsFor: 'handling'!
defaultAction
    "Simple default action"
    Transcript show: 'Error: '; show: (self messageText ifNil: ['Unknown error']); cr.
    ^nil!
!

"TestResult simple implementation for running tests"
!TestResult methodsFor: 'accessing'!
runCount
    ^runCount ifNil: [0]!

passedCount
    ^passedCount ifNil: [0]!

failureCount
    ^failureCount ifNil: [0]!

errorCount
    ^errorCount ifNil: [0]!

initialize
    runCount := 0.
    passedCount := 0.
    failureCount := 0.
    errorCount := 0.
    ^self!
!

!TestResult methodsFor: 'recording'!
addPass
    passedCount := passedCount + 1.
    runCount := runCount + 1!

addFailure: aTestCase
    failureCount := failureCount + 1.
    runCount := runCount + 1!

addError: aTestCase
    errorCount := errorCount + 1.
    runCount := runCount + 1!
!

"TestCase simple run support"
!TestCase methodsFor: 'running'!
run
    "Run a single test and answer the result"
    | result |
    result := TestResult new initialize.
    self run: result.
    ^result!

run: aTestResult
    "Run myself and record in aTestResult"
    [self setUp.
     [self performTest] on: Error do: [:e | aTestResult addError: self. ^self].
     self tearDown.
     aTestResult addPass]
        on: Error do: [:e | aTestResult addError: self]!

setUp
    "Default setup does nothing"
    ^self!

tearDown
    "Default teardown does nothing"
    ^self!

performTest
    "Perform the actual test"
    self perform: self selector!
!

!TestCase class methodsFor: 'running'!
run
    "Run all tests in this class"
    ^self buildSuite run!

buildSuite
    "Answer a TestSuite with all tests"
    | suite |
    suite := TestSuite new.
    suite name: self name.
    self allTestSelectors do: [:sel | suite addTest: (self selector: sel)].
    ^suite!

selector: aSymbol
    "Answer a new test case for the given selector"
    ^self new setTestSelector: aSymbol!
!

!TestCase methodsFor: 'accessing'!
selector
    ^testSelector!

setTestSelector: aSymbol
    testSelector := aSymbol.
    ^self!
!

"TestSuite simple implementation"
!TestSuite methodsFor: 'running'!
run
    "Run all tests and answer the combined result"
    | result |
    result := TestResult new initialize.
    self run: result.
    ^result!

run: aTestResult
    "Run all tests and record in aTestResult"
    tests do: [:each | each run: aTestResult]!
!

!TestSuite methodsFor: 'accessing'!
name: aString
    name := aString!

tests
    ^tests ifNil: [tests := OrderedCollection new]!

addTest: aTest
    self tests add: aTest!
!

"Ensure Object has basic error method"
!Object methodsFor: 'error handling'!
error: aString
    "Signal an error"
    Transcript show: 'Error: '; show: aString; cr.
    Error signal: aString!

primitiveFailed
    "A primitive has failed"
    self error: 'Primitive failed'!

subclassResponsibility
    "Subclass should implement this"
    self error: 'Subclass responsibility'!
!

Transcript show: 'Bootstrap stubs loaded'; cr!
