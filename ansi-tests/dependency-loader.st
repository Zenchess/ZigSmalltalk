"ANSI Test Dependency-Aware Loader
This script analyzes dependencies and loads classes in the correct order"

| classDependencies fileList loadedClasses remainingClasses currentLoadCount |

"Define class dependencies based on inheritance hierarchy"
classDependencies := Dictionary new.

"Core Object hierarchy"
classDependencies at: #Object put: #().
classDependencies at: #ProtoObject put: #(Object).
classDependencies at: #Behavior put: #(Object).
classDependencies at: #ClassDescription put: #(Behavior).
classDependencies at: #Class put: #(ClassDescription).
classDependencies at: #Metaclass put: #(ClassDescription).
classDependencies at: #UndefinedObject put: #(Object).

"Magnitude hierarchy"
classDependencies at: #Magnitude put: #(Object).
classDependencies at: #Character put: #(Magnitude).
classDependencies at: #Association put: #(Magnitude).
classDependencies at: #Date put: #(Magnitude).
classDependencies at: #Time put: #(Magnitude).
classDependencies at: #DateAndTime put: #(Magnitude).
classDependencies at: #TimeStamp put: #(Magnitude).
classDependencies at: #Duration put: #(Magnitude).

"Number hierarchy"
classDependencies at: #Number put: #(Magnitude).
classDependencies at: #ArithmeticValue put: #(Magnitude).
classDependencies at: #Integer put: #(Number).
classDependencies at: #SmallInteger put: #(Integer).
classDependencies at: #Fraction put: #(Number).
classDependencies at: #Float put: #(ArithmeticValue).
classDependencies at: #ScaledDecimal put: #(Number).

"Collection hierarchy"
classDependencies at: #Collection put: #(Object).
classDependencies at: #SequenceableCollection put: #(Collection).
classDependencies at: #ArrayedCollection put: #(SequenceableCollection).
classDependencies at: #RunArray put: #(ArrayedCollection).
classDependencies at: #Interval put: #(SequenceableCollection).
classDependencies at: #Bag put: #(Collection).
classDependencies at: #Set put: #(Collection).
classDependencies at: #Dictionary put: #(Collection).
classDependencies at: #LinkedList put: #(SequenceableCollection).
classDependencies at: #Semaphore put: #(LinkedList).

"Boolean hierarchy"
classDependencies at: #Boolean put: #(Object).
classDependencies at: #True put: #(Boolean).
classDependencies at: #False put: #(Boolean).

"Stream hierarchy"
classDependencies at: #Stream put: #(Object).
classDependencies at: #PositionableStream put: #(Stream).
classDependencies at: #SequencedStream put: #(Stream).
classDependencies at: #ReadStream put: #(PositionableStream).
classDependencies at: #WriteStream put: #(PositionableStream).
classDependencies at: #ReadWriteStream put: #(ReadStream WriteStream).
classDependencies at: #FileStream put: #(ReadWriteStream).
classDependencies at: #NullStream put: #(Stream).

"Exception hierarchy"
classDependencies at: #Exception put: #(Object).
classDependencies at: #Error put: #(Exception).
classDependencies at: #ArithmeticError put: #(Error).
classDependencies at: #ZeroDivide put: #(ArithmeticError).
classDependencies at: #FloatingPointException put: #(ArithmeticError).
classDependencies at: #Notification put: #(Exception).
classDependencies at: #Signal put: #(Exception).
classDependencies at: #Warning put: #(Notification).

"Test hierarchy"
classDependencies at: #TestResult put: #(Object).
classDependencies at: #TestFailure put: #(Object).
classDependencies at: #TestResource put: #(Object).
classDependencies at: #TestCase put: #(Object).
classDependencies at: #TestCaseProtocol put: #(TestCase).
classDependencies at: #MainTestCase put: #(TestCaseProtocol).
classDependencies at: #TestCaseHelper put: #(Object).
classDependencies at: #SUnitNameResolver put: #(Object).

"Protocol specification classes"
classDependencies at: #ProtocolANYSpec put: #(Object).
classDependencies at: #ProtocolMsgSpec put: #(Object).
classDependencies at: #ProtocolSpec put: #(ProtocolANYSpec).

"Helper classes"
classDependencies at: #ObjectHelper put: #(TestCaseHelper).
classDependencies at: #CollectionHelper put: #(TestCaseHelper).
classDependencies at: #DictionaryHelper put: #(TestCaseHelper).
classDependencies at: #ExtensibleCollectionHelper put: #(TestCaseHelper).
classDependencies at: #ReadableStringHelper put: #(TestCaseHelper).
classDependencies at: #ReadStreamHelper put: #(TestCaseHelper).
classDependencies at: #WriteStreamHelper put: #(TestCaseHelper).
classDependencies at: #SequencedStreamHelper put: #(TestCaseHelper).
classDependencies at: #SequencedCollectionHelper put: #(TestCaseHelper).
classDependencies at: #SequencedReadableCollectionHelper put: #(TestCaseHelper).
classDependencies at: #GettableStreamHelper put: #(TestCaseHelper).
classDependencies at: #PuttableStreamHelper put: #(TestCaseHelper).
classDependencies at: #CollectionStreamHelper put: #(TestCaseHelper).

"Get all .cls files"
fileList := (Directory current filesMatching: '*.cls') select: [:file |
    | className |
    className := file baseName asSymbol.
    classDependencies includesKey: className
].

loadedClasses := Set new.
remainingClasses := fileList copy.

Transcript show: 'Starting dependency-aware loading...'; cr.

"Load files in dependency order"
[remainingClasses isEmpty] whileFalse: [
    currentLoadCount := remainingClasses size.
    
    remainingClasses do: [:file |
        | className dependencies |
        className := file baseName asSymbol.
        dependencies := classDependencies at: className ifAbsent: [#()].
        
        "Check if all dependencies are loaded"
        (dependencies allSatisfy: [:dep | loadedClasses includes: dep]) ifTrue: [
            Transcript show: 'Loading: ', file name; cr.
            file fileIn.
            loadedClasses add: className.
            remainingClasses remove: file.
        ]
    ].
    
    "Check if we're stuck (circular dependencies or missing files)"
    (remainingClasses size = currentLoadCount) ifTrue: [
        Transcript show: 'Warning: Could not resolve dependencies for remaining files:'; cr.
        remainingClasses do: [:file | Transcript show: '  ', file name; cr].
        remainingClasses do: [:file | file fileIn]. "Load remaining files anyway"
        remainingClasses removeAll: remainingClasses.
    ]
].

Transcript show: 'Dependency-aware loading completed'; cr.
Transcript show: 'Loaded classes: ', loadedClasses size printString; cr.