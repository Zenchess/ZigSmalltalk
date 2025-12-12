"Stub for MessageNotUnderstood that doesn't rely on full exception handling"

!MessageNotUnderstood class methodsFor: 'instance creation'!

receiver: anObject message: aMessage
    "Simplified version that doesn't need Processor - just print and signal."
    | inst |
    inst := super new.
    inst receiver: anObject.
    inst tag: aMessage.
    "Instead of full signal, just use basic exception mechanism"
    Transcript show: 'MessageNotUnderstood: '; show: anObject class name; show: ' >> '; show: aMessage selector; cr.
    ^inst basicSignal!

! !

!MessageNotUnderstood methodsFor: 'signalling'!

basicSignal
    "Simple signal that doesn't require Processor"
    ^self defaultAction!

defaultAction
    "Default action - just return nil for now to avoid infinite loops"
    ^nil!

! !
