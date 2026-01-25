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

testReturn42
    ^42!

activeProcess
    "Return the active process - always create new for now"
    ^MinimalProcess new!

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

topFrame
    "Return the top stack frame for this process"
    ^StackFrame current!

! !

Processor := ProcessorScheduler basicNew!
