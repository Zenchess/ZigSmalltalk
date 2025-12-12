"Character stubs for Zig Smalltalk"

!Character class methodsFor: 'instance creation'!
codePoint: anInteger
    "Answer a Character for the given code point"
    <primitive: 521>
    ^self primitiveFailed: #codePoint:!
!

!Character methodsFor: 'accessing'!
codePoint
    "Answer the Unicode code point"
    <primitive: 520>
    ^self primitiveFailed: #codePoint!

asString
    "Answer a String containing just this character"
    ^String with: self!
!

!Character methodsFor: 'converting'!
asLowercase
    "Answer the lowercase version of this character"
    | cp |
    cp := self codePoint.
    (cp >= 65 and: [cp <= 90]) ifTrue: [^Character codePoint: cp + 32].
    ^self!

asUppercase
    "Answer the uppercase version of this character"
    | cp |
    cp := self codePoint.
    (cp >= 97 and: [cp <= 122]) ifTrue: [^Character codePoint: cp - 32].
    ^self!
!

!Character methodsFor: 'testing'!
isDigit
    "Answer whether the receiver is a decimal digit (0-9)"
    | cp |
    cp := self codePoint.
    ^(cp >= 48) and: [cp <= 57]!

isAlphaNumeric
    "Answer whether the receiver is a letter or digit"
    ^self isLetter or: [self isDigit]!
!
