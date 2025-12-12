Object subclass: #SimpleClass
    instanceVariableNames: 'value'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!SimpleClass methodsFor!

value
    ^value!

value: anObject
    value := anObject!

test1
    | x |
    x := 5.
    ^x + 10!

test2
    ^self value: 42; value!

testCollect
    ^#(1 2 3) collect: [:x | x * 2]!

testDo
    | sum |
    sum := 0.
    #(1 2 3) do: [:x | sum := sum + x].
    ^sum!
!

