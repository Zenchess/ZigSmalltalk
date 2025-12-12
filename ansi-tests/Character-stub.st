!Character methodsFor: 'comparing'!

= comperand
    "Answer whether the receiver is the same as the parameter.
    Use the primitive == for immediate character comparison."
    ^self == comperand!

code
    "Return the ASCII/Unicode code point for this character.
    For now, use asciiValue as a fallback."
    ^self asciiValue!

! !
