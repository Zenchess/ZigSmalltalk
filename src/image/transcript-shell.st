Object subclass: #TranscriptShell
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!TranscriptShell methodsFor!

show: anObject
    <primitive: 650>
    ^self!

cr
    <primitive: 651>
    ^self!

nextPutAll: aString
    <primitive: 652>
    ^self!

flush
    <primitive: 653>
    ^self!

contents
    ^''!

!TranscriptShell class methodsFor!

create
    | newTranscript |
    newTranscript := super new.
    Transcript := newTranscript.
    ^newTranscript! !

Transcript := TranscriptShell create.