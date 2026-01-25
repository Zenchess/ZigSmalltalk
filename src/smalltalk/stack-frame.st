"Stack Frame implementation for ZigSmalltalk"
"Provides reflective access to the call stack for exception handling and debugging"

Object subclass: #StackFrame
    instanceVariableNames: 'process index'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!StackFrame methodsFor: 'accessing'!

process
    "Answer the process this frame belongs to"
    ^process!

process: aProcess
    process := aProcess!

index
    "Answer the index of this frame in the context stack (0 = top)"
    ^index!

index: anInteger
    index := anInteger!

method
    "Answer the CompiledMethod for this frame"
    <primitive: 230>
    ^nil!

receiver
    "Answer the receiver (self) for this frame"
    <primitive: 231>
    ^nil!

sender
    "Answer the StackFrame that called this frame, or nil if this is the bottom"
    <primitive: 232>
    ^nil!

ip
    "Answer the instruction pointer (bytecode offset) for this frame"
    <primitive: 233>
    ^0!

arguments
    "Answer an Array of the arguments passed to this frame"
    <primitive: 234>
    ^#()!

temporaries
    "Answer an Array of the temporary variables in this frame"
    <primitive: 235>
    ^#()!

! !

!StackFrame methodsFor: 'testing'!

isBlockFrame
    "Answer whether this is a block activation frame"
    ^false!

isDead
    "Answer whether this frame has returned"
    ^index isNil or: [self method isNil]!

! !

!StackFrame methodsFor: 'printing'!

printOn: aStream
    | m |
    m := self method.
    m ifNil: [
        aStream nextPutAll: 'DeadFrame'.
        ^self
    ].
    aStream
        nextPutAll: (m methodClass ifNil: ['?'] ifNotNil: [:c | c name]);
        nextPutAll: '>>';
        nextPutAll: (m selector ifNil: ['?']);
        nextPutAll: ' @ ';
        print: index!

displayOn: aStream
    self printOn: aStream!

! !

!StackFrame methodsFor: 'stack walking'!

stackTrace: depth
    "Answer a string with a stack trace to the given depth"
    | stream frame count |
    stream := WriteStream on: (String new: 200).
    frame := self.
    count := 0.
    [frame notNil and: [(depth < 0) or: [count < depth]]] whileTrue: [
        frame printOn: stream.
        stream cr.
        frame := frame sender.
        count := count + 1
    ].
    ^stream contents!

callstackDo: aBlock depth: maxDepth
    "Iterate over the stack frames up to maxDepth"
    | frame count |
    frame := self.
    count := 0.
    [frame notNil and: [(maxDepth < 0) or: [count < maxDepth]]] whileTrue: [
        aBlock value: frame.
        frame := frame sender.
        count := count + 1
    ]!

! !

!StackFrame class methodsFor: 'instance creation'!

process: aProcess index: anInteger
    "Create a new StackFrame for the given process and context index"
    ^self new
        process: aProcess;
        index: anInteger;
        yourself!

current
    "Answer the current (top) stack frame"
    <primitive: 236>
    ^nil!

! !
