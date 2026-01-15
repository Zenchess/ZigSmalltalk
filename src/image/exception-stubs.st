"Exception handling and missing method stubs"

!ProcessorScheduler methodsFor: 'exceptions'!
constWriteSignal
    "Return nil instead of a signal object"
    ^nil
! !

!Object methodsFor: 'testing'!
isImmutable
    "Stub - nothing is immutable"
    ^false
! !

!UndefinedObject methodsFor: 'error handling'!
signalWith: anObject
    "Stub for nil signalWith: - just continue"
    ^self
! !

!Character methodsFor: 'testing'!
isVowel
    "Answer whether receiver is a vowel"
    self = $a ifTrue: [^true].
    self = $e ifTrue: [^true].
    self = $i ifTrue: [^true].
    self = $o ifTrue: [^true].
    self = $u ifTrue: [^true].
    self = $A ifTrue: [^true].
    self = $E ifTrue: [^true].
    self = $I ifTrue: [^true].
    self = $O ifTrue: [^true].
    self = $U ifTrue: [^true].
    ^false
! !

!Behavior methodsFor: 'class variables'!
addClassVariable: aString
    "Stub - class variables are handled at load time"
    ^self
! !

Transcript show: 'Exception stubs loaded'; cr!
