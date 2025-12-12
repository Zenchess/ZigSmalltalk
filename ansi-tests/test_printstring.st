!TestResult methodsFor: 'testing'!
printString
    "Answer a <readableString> whose characters are a description of the receiver
    as a developer would want to see it."

    | stream |
    stream := String writeStream: 32.
    self printOn: stream.
    ^stream contents! !

"Test the printString method"
TestResult new printString