Object subclass: #ProcessorScheduler
    instanceVariableNames: 'activeProcess'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

Object subclass: #MinimalProcess
    instanceVariableNames: 'exceptionEnvironment'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ProcessorScheduler methodsFor: 'accessing'!

activeProcess
    activeProcess isNil ifTrue: [activeProcess := MinimalProcess new].
    ^activeProcess!

isActiveMain
    ^true!

yield
    "Give other processes a chance to run - stub does nothing"
    ^self!

! !

!MinimalProcess methodsFor: 'exception handling'!

exceptionEnvironment
    ^exceptionEnvironment!

exceptionEnvironment: anObject
    exceptionEnvironment := anObject!

terminate
    ^self!

! !

Processor := ProcessorScheduler new!
