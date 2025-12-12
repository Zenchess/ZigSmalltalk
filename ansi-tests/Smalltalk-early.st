Object subclass: #SmalltalkSystem
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!SmalltalkSystem methodsFor: 'accessing'!
at: aSymbol
    "Look up a global by name"
    <primitive: 910>
    ^nil!

at: aSymbol ifAbsent: aBlock
    "Look up a global, evaluate block if not found"
    <primitive: 911>
    ^aBlock value!
!

Smalltalk := SmalltalkSystem new!
