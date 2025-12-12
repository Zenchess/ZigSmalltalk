Object subclass: #Tmp
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Tmp methodsFor!
test: aBoolean
    ^aBoolean ifFalse: [self]!
!

Tmp new test: false.
