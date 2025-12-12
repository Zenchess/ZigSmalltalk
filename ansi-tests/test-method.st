Object subclass: #VarTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!VarTest methodsFor!

testTemp
    | x |
    x := (true and: [true]).
    ^x!
!
