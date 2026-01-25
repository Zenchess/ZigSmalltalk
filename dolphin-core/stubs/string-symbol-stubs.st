"String and Symbol stubs for test infrastructure"

!Symbol methodsFor!

asString
	"Answer a new String containing the characters of the receiver."
	| len string |
	len := self size.
	string := String new: len.
	1 to: len do: [:i | string at: i put: (self at: i)].
	^string! !

!String methodsFor!

beginsWith: aString
	"Answer whether the receiver starts with the characters of the argument."
	| prefixSize |
	prefixSize := aString size.
	prefixSize > self size ifTrue: [^false].
	1 to: prefixSize do: [:i |
		(self at: i) = (aString at: i) ifFalse: [^false]
	].
	^true! !
