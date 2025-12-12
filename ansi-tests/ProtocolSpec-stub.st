!ProtocolSpec class methodsFor: 'accessing'!

protocolNamed: protocolName
    "Stub - return a dummy protocol that accepts everything"
    ^DummyProtocolSpec new!

! !

Object subclass: #DummyProtocolSpec
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!DummyProtocolSpec methodsFor: 'accessing'!

messageOrNilAtSelector: aSelector
    "Return a dummy message spec that accepts any return value"
    ^DummyMsgSpec new!

! !

Object subclass: #DummyMsgSpec
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!DummyMsgSpec methodsFor: 'testing'!

isConformingReturn: anObject
    "Accept any return value"
    ^true!

! !
