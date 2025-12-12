Object subclass: #StackTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!StackTest methodsFor: 'testing'!
test
    | results |
    results := OrderedCollection new.
    #object: beginsWith: 'test'.
    ^results!
!
