"Stub methods for Dolphin-specific Object methods that aren't implemented in Zig Smalltalk"

!Object methodsFor: 'testing'!

isImmutable
    "Stub: Answer whether the receiver is a constant object.
    In Zig Smalltalk we don't track immutability, so always return false."
    ^false!

isImmutable: aBoolean
    "Stub: Set whether the receiver is a constant object.
    In Zig Smalltalk this is a no-op."
    ^self!

! !
