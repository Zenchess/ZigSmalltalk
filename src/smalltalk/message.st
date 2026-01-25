"Message methods for ZigSmalltalk"
"Add methods to the existing Message class (defined in bootstrap)"
"Message has instance variables: selector (0), arguments (1), lookupClass (2)"
"Note: Use instVarAt: since bootstrap classes don't have named inst vars"

!Message methodsFor: 'accessing'!

selector
    "Answer the selector of the message (field 0)"
    ^self instVarAt: 1!

selector: aSymbol
    "Set the selector of the message (field 0)"
    ^self instVarAt: 1 put: aSymbol!

arguments
    "Answer the arguments of the message (field 1)"
    ^self instVarAt: 2!

arguments: anArray
    "Set the arguments of the message (field 1)"
    ^self instVarAt: 2 put: anArray!

lookupClass
    "Answer the class in which lookup started (field 2)"
    ^self instVarAt: 3!

lookupClass: aClass
    "Set the class in which lookup started (field 2)"
    ^self instVarAt: 3 put: aClass!

! !

!Message methodsFor: 'sending'!

sendTo: anObject
    "Send this message to anObject and return the result"
    ^anObject perform: self selector withArguments: self arguments!

! !

!Message methodsFor: 'printing'!

printOn: aStream
    "Print the message on aStream"
    aStream nextPutAll: 'Message('.
    aStream nextPutAll: selector printString.
    aStream nextPut: $)!

! !

!Message class methodsFor: 'instance creation'!

selector: aSymbol
    "Create a message with no arguments"
    ^self selector: aSymbol arguments: #()!

selector: aSymbol arguments: anArray
    "Create a message with selector and arguments"
    ^self new
        selector: aSymbol;
        arguments: anArray;
        yourself!

! !

Transcript show: 'Message methods loaded'; cr!
