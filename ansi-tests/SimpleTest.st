TestCase subclass: #SimpleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

!SimpleTest methodsFor!

testTrue
	self assert: true!

testFalse
	self deny: false!

testEquality
	self assert: 1 = 1!

testInequality
	self deny: 1 = 2!

testAddition
	self assert: (2 + 3) = 5!

testSubtraction
	self assert: (5 - 3) = 2!

testMultiplication
	self assert: (3 * 4) = 12!

testDivision
	self assert: (12 / 4) = 3!

testNot
	self assert: false not.
	self deny: true not!

testAnd
	self assert: (true and: [true]).
	self deny: (true and: [false]).
	self deny: (false and: [true])!

testOr
	self assert: (true or: [false]).
	self assert: (false or: [true]).
	self deny: (false or: [false])!
!
