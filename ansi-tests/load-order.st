"ANSI Test Load Order Script
This script loads classes in the correct dependency order"

| loadList |

"Define the load order based on class hierarchy dependencies"
loadList := #(
    "Core Object hierarchy"
    'Object.cls'
    'ProtoObject.cls'
    'Behavior.cls'
    'ClassDescription.cls'
    'Class.cls'
    'Metaclass.cls'
    'UndefinedObject.cls'
    
    "Core Magnitude hierarchy"
    'Magnitude.cls'
    'Character.cls'
    'Association.cls'
    'Date.cls'
    'Time.cls'
    'DateAndTime.cls'
    'TimeStamp.cls'
    'Duration.cls'
    
    "Core Number hierarchy"
    'Number.cls'
    'ArithmeticValue.cls'
    'Integer.cls'
    'SmallInteger.cls'
    'Fraction.cls'
    'Float.cls'
    'ScaledDecimal.cls'
    
    "Core Collection hierarchy"
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
    
    "Core Boolean hierarchy"
    'Boolean.cls'
    'True.cls'
    'False.cls'
    
    "Core Stream hierarchy"
    'Stream.cls'
    'PositionableStream.cls'
    'SequencedStream.cls'
    'ReadStream.cls'
    'WriteStream.cls'
    'ReadWriteStream.cls'
    'FileStream.cls'
    'NullStream.cls'
    
    "Core Exception hierarchy"
    'Exception.cls'
    'Error.cls'
    'ArithmeticError.cls'
    'ZeroDivide.cls'
    'FloatingPointException.cls'
    'Notification.cls'
    'Signal.cls'
    'Warning.cls'
    
    "Core Test hierarchy"
    'TestResult.cls'
    'TestFailure.cls'
    'TestResource.cls'
    'TestCase.cls'
    'TestCaseProtocol.cls'
    'MainTestCase.cls'
    'TestCaseHelper.cls'
    'SUnitNameResolver.cls'
    
    "Protocol specification classes"
    'ProtocolANYSpec.cls'
    'ProtocolMsgSpec.cls'
    'ProtocolSpec.cls'
    
    "Helper classes"
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
    
    "ANSI Test classes - in logical grouping"
    'ArrayANSITest.cls'
    'ArrayFactoryANSITest.cls'
    'BagANSITest.cls'
    'BagFactoryANSITest.cls'
    'ByteArrayANSITest.cls'
    'ByteArrayFactoryANSITest.cls'
    'CharacterANSITest.cls'
    'CharacterFactoryANSITest.cls'
    'CollectionTest.cls'
    'DateAndTimeANSITest.cls'
    'DateAndTimeFactoryANSITest.cls'
    'DictionaryANSITest.cls'
    'DictionaryFactoryANSITest.cls'
    'DurationANSITest.cls'
    'DurationFactoryANSITest.cls'
    'ErrorANSITest.cls'
    'ErrorClassANSITest.cls'
    'ExceptionANSITest.cls'
    'ExceptionClassANSITest.cls'
    'ExceptionSetANSITest.cls'
    'FailedMessageANSITest.cls'
    'FileStreamFactoryANSITest.cls'
    'FloatANSITest.cls'
    'FloatCharacterizationANSITest.cls'
    'FractionANSITest.cls'
    'FractionFactoryANSITest.cls'
    'IdentityDictionaryANSITest.cls'
    'IdentityDictionaryFactoryANSITest.cls'
    'IntegerANSITest.cls'
    'IntervalANSITest.cls'
    'IntervalFactoryANSITest.cls'
    'MessageNotUnderstoodANSITest.cls'
    'MessageNotUnderstoodSelectorANSITest.cls'
    'MonadicBlockANSITest.cls'
    'NiladicBlockANSITest.cls'
    'NilANSITest.cls'
    'NotificationANSITest.cls'
    'NotificationClassANSITest.cls'
    'ObjectANSITest.cls'
    'ObjectClassANSITest.cls'
    'OrderedCollectionANSITest.cls'
    'OrderedCollectionFactoryANSITest.cls'
    'ReadFileStreamANSITest.cls'
    'ReadStreamANSITest.cls'
    'ReadStreamFactoryANSITest.cls'
    'ReadWriteStreamANSITest.cls'
    'ReadWriteStreamFactoryANSITest.cls'
    'ScaledDecimalANSITest.cls'
    'SelectorANSITest.cls'
    'SequencedReadableCollectionTest.cls'
    'SetANSITest.cls'
    'SetFactoryANSITest.cls'
    'SortedCollectionANSITest.cls'
    'SortedCollectionFactoryANSITest.cls'
    'StringANSITest.cls'
    'StringFactoryANSITest.cls'
    'SymbolANSITest.cls'
    'TranscriptANSITest.cls'
    'WarningANSITest.cls'
    'WarningClassANSITest.cls'
    'WriteFileStreamANSITest.cls'
    'WriteStreamANSITest.cls'
    'WriteStreamFactoryANSITest.cls'
    'ZeroDivideANSITest.cls'
    'ZeroDivideFactoryANSITest.cls'
).

"Load each file in order"
loadList do: [:fileName |
    (fileName endsWith: '.cls') ifTrue: [
        [fileName asFileReference exists] 
            ifTrue: [fileName asFileReference fileIn]
            ifFalse: [Transcript show: 'Warning: File not found: ', fileName; cr]
    ]
].

Transcript show: 'ANSI test classes loaded in dependency order'; cr.