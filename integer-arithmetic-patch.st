"Minimal Integer arithmetic patch for Zig Smalltalk VM's built-in Integer"
"Adds basic arithmetic methods without loading full ANSI Integer.cls"

!Integer methodsFor: 'arithmetic'!

- aNumber
    "Subtract aNumber from the receiver"
    <primitive: 2>
    ^self primitiveFailed!

+ aNumber
    "Add aNumber to the receiver"
    <primitive: 1>
    ^self primitiveFailed!

* aNumber
    "Multiply the receiver by aNumber"
    <primitive: 9>
    ^self primitiveFailed!

// aNumber
    "Integer division"
    <primitive: 12>
    ^self primitiveFailed!

rem: aNumber
    "Remainder after integer division"
    <primitive: 13>
    ^self primitiveFailed!

negated
    "Return the negation of the receiver"
    ^0 - self!

<= aNumber
    "Answer whether the receiver is less than or equal to aNumber"
    <primitive: 4>
    ^self primitiveFailed!

to: stop do: aBlock
    "Evaluate aBlock with each integer from self to stop"
    | current |
    current := self.
    [current <= stop] whileTrue: [
        aBlock value: current.
        current := current + 1
    ]!

!

!Number methodsFor: 'enumerating'!

to: stop do: aBlock
    "Evaluate aBlock with each number from self to stop"
    | current |
    current := self.
    [current <= stop] whileTrue: [
        aBlock value: current.
        current := current + 1
    ]!

!

!SmallInteger methodsFor: 'arithmetic'!

- aNumber
    "Subtract aNumber from the receiver"
    <primitive: 2>
    ^self primitiveFailed!

+ aNumber
    "Add aNumber to the receiver"
    <primitive: 1>
    ^self primitiveFailed!

* aNumber
    "Multiply the receiver by aNumber"
    <primitive: 9>
    ^self primitiveFailed!

!
