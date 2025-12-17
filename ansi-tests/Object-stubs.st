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

"Override doesNotUnderstand: to prevent infinite recursion during exception handling.
 The ANSI Object.cls version tries to signal MessageNotUnderstood which can
 cause infinite recursion if the exception handling itself triggers DNU."
!Object methodsFor: 'exception handling'!

doesNotUnderstand: aMessage
    "Minimal handler - use VM primitive to print error and return nil.
     This prevents infinite recursion from complex exception handling."
    <primitive: 800>
    ^nil!

! !
