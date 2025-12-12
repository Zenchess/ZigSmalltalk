"ANSI Test Loader - Fixed Dependency Order
This script loads classes in the correct dependency order to avoid 'Not found' errors"

| coreClasses testClasses protocolClasses helperClasses remainingFiles loadedCount |

Transcript show: '=== Starting ANSI Test Loading with Fixed Dependencies ==='; cr.

"Phase 1: Core Object and Behavior hierarchy (must be first)"
coreClasses := #(
    'Object.cls'
    'ProtoObject.cls'
    'Behavior.cls'
    'ClassDescription.cls'
    'Class.cls'
    'Metaclass.cls'
    'UndefinedObject.cls'
).

Transcript show: 'Phase 1: Loading core Object hierarchy...'; cr.
coreClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found (may be built-in)'; cr
    ]
].

"Phase 2: Core Magnitude and Number hierarchy"
coreClasses := #(
    'Magnitude.cls'
    'Character.cls'
    'Association.cls'
    'Date.cls'
    'Time.cls'
    'DateAndTime.cls'
    'TimeStamp.cls'
    'Duration.cls'
    'Number.cls'
    'ArithmeticValue.cls'
    'Integer.cls'
    'SmallInteger.cls'
    'Fraction.cls'
    'Float.cls'
    'ScaledDecimal.cls'
    'Boolean.cls'
    'True.cls'
    'False.cls'
).

Transcript show: 'Phase 2: Loading core Magnitude and Number hierarchy...'; cr.
coreClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found (may be built-in)'; cr
    ]
].

"Phase 3: Core Collection hierarchy"
coreClasses := #(
    'Collection.cls'
    'SequenceableCollection.cls'
    'ArrayedCollection.cls'
    'RunArray.cls'
    'Interval.cls'
    'Bag.cls'
    'Set.cls'
    'Dictionary.cls'
    'LinkedList.cls'
    'Semaphore.cls'
    'SortedCollection.cls'
    'OrderedCollection.cls'
    'IdentityDictionary.cls'
    'LookupTable.cls'
    'PluggableSet.cls'
    'SharedSet.cls'
    'IdentitySet.cls'
    'Array.cls'
    'String.cls'
    'Symbol.cls'
    'ByteArray.cls'
).

Transcript show: 'Phase 3: Loading core Collection hierarchy...'; cr.
coreClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found (may be built-in)'; cr
    ]
].

"Phase 4: Core Stream hierarchy"
coreClasses := #(
    'Stream.cls'
    'PositionableStream.cls'
    'SequencedStream.cls'
    'ReadStream.cls'
    'WriteStream.cls'
    'ReadWriteStream.cls'
    'FileStream.cls'
    'NullStream.cls'
).

Transcript show: 'Phase 4: Loading core Stream hierarchy...'; cr.
coreClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found (may be built-in)'; cr
    ]
].

"Phase 5: Core Exception hierarchy"
coreClasses := #(
    'Exception.cls'
    'Error.cls'
    'ArithmeticError.cls'
    'ZeroDivide.cls'
    'FloatingPointException.cls'
    'Notification.cls'
    'Signal.cls'
    'Warning.cls'
    'ExceptionSet.cls'
    'MessageNotUnderstood.cls'
).

Transcript show: 'Phase 5: Loading core Exception hierarchy...'; cr.
coreClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found (may be built-in)'; cr
    ]
].

"Phase 6: Test framework classes"
testClasses := #(
    'TestResult.cls'
    'TestFailure.cls'
    'TestResource.cls'
    'TestCase.cls'
    'TestSuite.cls'
    'TestCaseProtocol.cls'
    'MainTestCase.cls'
    'TestCaseHelper.cls'
    'SUnitNameResolver.cls'
).

Transcript show: 'Phase 6: Loading test framework...'; cr.
testClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found'; cr
    ]
].

"Phase 7: Protocol specification classes"
protocolClasses := #(
    'ProtocolANYSpec.cls'
    'ProtocolMsgSpec.cls'
    'ProtocolSpec.cls'
).

Transcript show: 'Phase 7: Loading protocol specifications...'; cr.
protocolClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found'; cr
    ]
].

"Phase 8: Helper classes"
helperClasses := #(
    'ObjectHelper.cls'
    'CollectionHelper.cls'
    'DictionaryHelper.cls'
    'ExtensibleCollectionHelper.cls'
    'ReadableStringHelper.cls'
    'ReadStreamHelper.cls'
    'WriteStreamHelper.cls'
    'SequencedStreamHelper.cls'
    'SequencedCollectionHelper.cls'
    'SequencedReadableCollectionHelper.cls'
    'GettableStreamHelper.cls'
    'PuttableStreamHelper.cls'
    'CollectionStreamHelper.cls'
).

Transcript show: 'Phase 8: Loading helper classes...'; cr.
helperClasses do: [:file |
    (file asFileReference exists) ifTrue: [
        Transcript show: '  Loading: ', file; cr.
        file asFileReference fileIn
    ] ifFalse: [
        Transcript show: '  Warning: ', file, ' not found'; cr
    ]
].

"Phase 9: Load remaining test classes in dependency order"
remainingFiles := (Directory current filesMatching: '*.cls') select: [:file |
    | className |
    className := file baseName asSymbol.
    (className endsWith: 'ANSITest') or: [className = #SequencedStreamTest]
].

loadedCount := 0.
remainingFiles do: [:file |
    Transcript show: 'Phase 9: Loading test class: ', file name; cr.
    file fileIn.
    loadedCount := loadedCount + 1
].

Transcript show: '=== ANSI Test Loading Complete ==='; cr.
Transcript show: 'Loaded ', loadedCount printString, ' test classes'; cr.
Transcript show: 'Core classes and dependencies should now be properly resolved'; cr.