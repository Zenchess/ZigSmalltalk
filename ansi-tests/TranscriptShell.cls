"TranscriptShell - Follows Dolphin pattern exactly"
"Transcript is initially set to DeafObject.current, then replaced by TranscriptShell when needed"

Object subclass: #TranscriptShell
    instanceVariableNames: 'outputStream'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

"TranscriptShell instance methods"
!TranscriptShell methodsFor!

initialize
    outputStream := WriteStream on: String new!

show: anObject
    outputStream nextPutAll: anObject printString!

nextPutAll: aString
    outputStream nextPutAll: aString!

cr
    outputStream nextPut: Character cr!

flush
    "Transcript is always flushed immediately"!

contents
    ^outputStream contents! !

"Class method - this is what replaces Transcript"
!TranscriptShell class methodsFor!

create
    "This is called to create the TranscriptShell instance"
    | newTranscript |
    newTranscript := super new initialize.
    Transcript := newTranscript.
    ^newTranscript! !

"Initialize Transcript to DeafObject.current (exact Dolphin pattern)"
Transcript := DeafObject current.

"Test the implementation"
Transcript show: 'Transcript initialized'; cr.

quit