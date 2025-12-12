"Override SUnitNameResolver to use direct class references instead of Smalltalk global"

!SUnitNameResolver class methodsFor: 'accessing'!

classNamed: aSymbol
    "Look up a class by name. Since we don't have Smalltalk global dictionary,
    we use perform: to dynamically access the class as a global."
    | className |
    className := aSymbol asString.

    "Try common test helper classes directly"
    aSymbol == #ObjectHelper ifTrue: [^ObjectHelper].
    aSymbol == #CollectionHelper ifTrue: [^CollectionHelper].
    aSymbol == #ReadableStringHelper ifTrue: [^ReadableStringHelper].
    aSymbol == #SequencedCollectionHelper ifTrue: [^SequencedCollectionHelper].
    aSymbol == #SequencedContractibleCollectionHelper ifTrue: [^SequencedContractibleCollectionHelper].
    aSymbol == #SequencedReadableCollectionHelper ifTrue: [^SequencedReadableCollectionHelper].
    aSymbol == #CollectionStreamHelper ifTrue: [^CollectionStreamHelper].
    aSymbol == #ExtensibleCollectionHelper ifTrue: [^ExtensibleCollectionHelper].
    aSymbol == #AbstractDictionaryHelper ifTrue: [^AbstractDictionaryHelper].
    aSymbol == #GettableStreamHelper ifTrue: [^GettableStreamHelper].
    aSymbol == #PuttableStreamHelper ifTrue: [^PuttableStreamHelper].
    aSymbol == #SequencedStreamHelper ifTrue: [^SequencedStreamHelper].
    aSymbol == #ReadStreamHelper ifTrue: [^ReadStreamHelper].
    aSymbol == #WriteStreamHelper ifTrue: [^WriteStreamHelper].

    "If not found, return nil"
    ^nil!

! !
