"Package: TestPackage"
"User Package - Test for package loading"

Object subclass: #TestClass
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestPackage'!

!TestClass methodsFor: 'accessing'!
value
	^ value! !

!TestClass methodsFor: 'accessing'!
value: aValue
	value := aValue! !

!TestClass methodsFor: 'testing'!
doubled
	^ value * 2! !
