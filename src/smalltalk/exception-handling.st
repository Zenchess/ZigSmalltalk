"Exception handling for ZigSmalltalk"
"Sets up proper exception signaling and MessageNotUnderstood"

"MessageNotUnderstood - raised when a message is not understood"
Error subclass: #MessageNotUnderstood
    instanceVariableNames: 'receiver'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!MessageNotUnderstood methodsFor: 'accessing'!

receiver
    "Answer the object which did not understand the message"
    ^receiver!

receiver: anObject
    "Set the object which did not understand the message"
    receiver := anObject!

message
    "Answer the Message that was not understood"
    ^self tag!

selector
    "Answer the selector that was not understood"
    ^self message selector!

! !

!MessageNotUnderstood methodsFor: 'testing'!

isResumable
    "MessageNotUnderstood is resumable"
    ^true!

! !

!MessageNotUnderstood class methodsFor: 'instance creation'!

receiver: anObject message: aMessage
    "Create and signal a new MessageNotUnderstood"
    ^self new
        receiver: anObject;
        tag: aMessage;
        signal!

! !

"Make Exception>>signal use the primitive"
!Exception methodsFor: 'signaling'!

signal
    "Signal this exception"
    <primitive: 810>
    ^self!

signal: aString
    "Signal this exception with a message"
    self messageText: aString.
    ^self signal!

! !

"Add class-side signal methods"
!Exception class methodsFor: 'signaling'!

signal
    "Create a new instance and signal it"
    ^self new signal!

signal: aString
    "Create a new instance and signal it with a message"
    ^self new signal: aString!

! !

"Add tag/tag: methods to Exception"
!Exception methodsFor: 'accessing'!

tag
    "Answer the tag"
    ^tag!

tag: anObject
    "Set the tag"
    tag := anObject!

! !

"Update Object>>doesNotUnderstand: to signal the exception"
!Object methodsFor: 'error handling'!

doesNotUnderstand: aMessage
    "Handle a message that was not understood by signaling MessageNotUnderstood"
    ^MessageNotUnderstood receiver: self message: aMessage!

! !

"Block>>on:do: uses primitive 812"
!BlockClosure methodsFor: 'exception handling'!

on: anExceptionClass do: handlerBlock
    "Evaluate self, and if anExceptionClass is signaled, evaluate handlerBlock"
    <primitive: 812>
    ^self value!

! !

Transcript show: 'Exception handling loaded'; cr!
