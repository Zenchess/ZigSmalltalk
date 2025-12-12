Object subclass: #Delay
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

!Delay class methodsFor!

microsecondClockValue
	"Return current time in microseconds - stub returns 0"
	^0!

forMicroseconds: anInteger
	^self new! !

!Delay methodsFor!

wait
	"Stub - do nothing"
	^self! !

Object subclass: #SUnitDelay
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

!SUnitDelay class methodsFor!

forMicroseconds: anInteger
	^self new! !

!SUnitDelay methodsFor!

wait
	"Stub - do nothing"
	^self! !
