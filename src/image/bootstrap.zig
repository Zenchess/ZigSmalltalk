const std = @import("std");
const memory = @import("../vm/memory.zig");
const object = @import("../vm/object.zig");
const bytecodes = @import("../vm/bytecodes.zig");
const build_options = @import("build_options");

const Heap = memory.Heap;
const Value = object.Value;
const Object = object.Object;
const ClassFormat = object.ClassFormat;
const Primitive = bytecodes.Primitive;

// FFI generated bindings (only available when FFI is enabled)
const ffi_generated = if (build_options.ffi_enabled) @import("../vm/ffi_generated.zig") else undefined;
const ffi_autogen = if (build_options.ffi_enabled) @import("../vm/ffi_autogen.zig") else undefined;
const ffi_enabled = build_options.ffi_enabled;

/// Bootstrap the core Smalltalk classes
/// This creates the minimal class hierarchy needed to start the system
pub fn bootstrap(heap: *Heap) !void {
    // The "tying the knot" problem: Class is an instance of Metaclass,
    // but Metaclass is a subclass of Class. We solve this by:
    // 1. Creating stub class objects first
    // 2. Fixing up the class pointers afterwards

    // Phase 1: Create all class objects with placeholder class references

    // Pre-allocate space in class table
    var i: usize = 0;
    while (i < 50) : (i += 1) {
        try heap.class_table.append(heap.allocator, Value.nil);
    }

    // Create ProtoObject (the root - though we might not use it)
    // For simplicity, we'll use Object as the root

    // Object class
    const object_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_OBJECT] = Value.fromObject(object_class);

    // Class class
    const class_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_CLASS] = Value.fromObject(class_class);

    // Metaclass class
    const metaclass_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_METACLASS] = Value.fromObject(metaclass_class);

    // Behavior class
    const behavior_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_BEHAVIOR] = Value.fromObject(behavior_class);

    // ClassDescription class
    const class_desc_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_CLASS_DESCRIPTION] = Value.fromObject(class_desc_class);

    // SmallInteger class
    const small_int_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_SMALL_INTEGER] = Value.fromObject(small_int_class);

    // String class
    const string_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_STRING] = Value.fromObject(string_class);

    // Symbol class
    const symbol_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_SYMBOL] = Value.fromObject(symbol_class);

    // Array class
    const array_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_ARRAY] = Value.fromObject(array_class);

    // ByteArray class
    const byte_array_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_BYTE_ARRAY] = Value.fromObject(byte_array_class);

    // CompiledMethod class
    const compiled_method_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_COMPILED_METHOD] = Value.fromObject(compiled_method_class);

    // BlockClosure class
    const block_closure_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_BLOCK_CLOSURE] = Value.fromObject(block_closure_class);

    // UndefinedObject class (nil's class)
    const undefined_object_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_UNDEFINED_OBJECT] = Value.fromObject(undefined_object_class);

    // True class
    const true_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_TRUE] = Value.fromObject(true_class);

    // False class
    const false_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_FALSE] = Value.fromObject(false_class);

    // Character class
    const character_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_CHARACTER] = Value.fromObject(character_class);

    // Interval class
    const interval_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_INTERVAL] = Value.fromObject(interval_class);

    // Float class
    const float_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_FLOAT] = Value.fromObject(float_class);

    // Exception class
    const exception_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_EXCEPTION] = Value.fromObject(exception_class);

    // Error class (subclass of Exception)
    const error_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_ERROR] = Value.fromObject(error_class);

    // Message class (for doesNotUnderstand:)
    const message_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_MESSAGE] = Value.fromObject(message_class);

    // MessageNotUnderstood class (subclass of Error)
    const mnu_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_MESSAGE_NOT_UNDERSTOOD] = Value.fromObject(mnu_class);

    // Dictionary class
    const dictionary_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_DICTIONARY] = Value.fromObject(dictionary_class);

    // Set class
    const set_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_SET] = Value.fromObject(set_class);

    // OrderedCollection class
    const ordered_collection_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_ORDERED_COLLECTION] = Value.fromObject(ordered_collection_class);

    // ReadStream class
    const read_stream_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_READ_STREAM] = Value.fromObject(read_stream_class);

    // WriteStream class
    const write_stream_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_WRITE_STREAM] = Value.fromObject(write_stream_class);

    // FileStream class
    const file_stream_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_FILE_STREAM] = Value.fromObject(file_stream_class);

    // DeafObject class
    const deaf_object_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_DEAF_OBJECT] = Value.fromObject(deaf_object_class);

    // TranscriptShell class
    const transcript_shell_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_TRANSCRIPT_SHELL] = Value.fromObject(transcript_shell_class);

    // Association class
    const association_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_ASSOCIATION] = Value.fromObject(association_class);

    // PoolDictionary class
    const pool_dictionary_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_POOL_DICTIONARY] = Value.fromObject(pool_dictionary_class);

    // Magnitude class (abstract class for ordered objects)
    const magnitude_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_MAGNITUDE] = Value.fromObject(magnitude_class);

    // Number class (abstract class for numeric objects)
    const number_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_NUMBER] = Value.fromObject(number_class);

    // Integer class (abstract class for integer numbers)
    const integer_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_INTEGER] = Value.fromObject(integer_class);

    // Boolean class (abstract class for True/False)
    const boolean_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_BOOLEAN] = Value.fromObject(boolean_class);

    // Collection class (abstract class for collections)
    const collection_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_COLLECTION] = Value.fromObject(collection_class);

    // SequenceableCollection class
    const sequenceable_collection_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_SEQUENCEABLE_COLLECTION] = Value.fromObject(sequenceable_collection_class);

    // ArrayedCollection class
    const arrayed_collection_class = try createClassObject(heap, Heap.CLASS_CLASS);
    heap.class_table.items[Heap.CLASS_ARRAYED_COLLECTION] = Value.fromObject(arrayed_collection_class);

    // Phase 2: Set up superclass chain

    // Object has no superclass (nil)
    setClassSuperclass(object_class, Value.nil);

    // Behavior -> Object
    setClassSuperclass(behavior_class, Value.fromObject(object_class));

    // ClassDescription -> Behavior
    setClassSuperclass(class_desc_class, Value.fromObject(behavior_class));

    // Class -> ClassDescription
    setClassSuperclass(class_class, Value.fromObject(class_desc_class));

    // Metaclass -> ClassDescription
    setClassSuperclass(metaclass_class, Value.fromObject(class_desc_class));

    // Magnitude -> Object
    setClassSuperclass(magnitude_class, Value.fromObject(object_class));

    // Number -> Magnitude
    setClassSuperclass(number_class, Value.fromObject(magnitude_class));

    // Integer -> Number
    setClassSuperclass(integer_class, Value.fromObject(number_class));

    // SmallInteger -> Integer
    setClassSuperclass(small_int_class, Value.fromObject(integer_class));

    // Collection -> Object
    setClassSuperclass(collection_class, Value.fromObject(object_class));

    // SequenceableCollection -> Collection
    setClassSuperclass(sequenceable_collection_class, Value.fromObject(collection_class));

    // ArrayedCollection -> SequenceableCollection
    setClassSuperclass(arrayed_collection_class, Value.fromObject(sequenceable_collection_class));

    // String -> ArrayedCollection
    setClassSuperclass(string_class, Value.fromObject(arrayed_collection_class));

    // Symbol -> String
    setClassSuperclass(symbol_class, Value.fromObject(string_class));

    // Array -> ArrayedCollection
    setClassSuperclass(array_class, Value.fromObject(arrayed_collection_class));

    // ByteArray -> ArrayedCollection
    setClassSuperclass(byte_array_class, Value.fromObject(arrayed_collection_class));

    // CompiledMethod -> Object
    setClassSuperclass(compiled_method_class, Value.fromObject(object_class));

    // BlockClosure -> Object
    setClassSuperclass(block_closure_class, Value.fromObject(object_class));

    // UndefinedObject -> Object
    setClassSuperclass(undefined_object_class, Value.fromObject(object_class));

    // Boolean -> Object
    setClassSuperclass(boolean_class, Value.fromObject(object_class));

    // True -> Boolean
    setClassSuperclass(true_class, Value.fromObject(boolean_class));

    // False -> Boolean
    setClassSuperclass(false_class, Value.fromObject(boolean_class));

    // Character -> Magnitude
    setClassSuperclass(character_class, Value.fromObject(magnitude_class));

    // Interval -> Object (in a full system: SequenceableCollection -> ...)
    setClassSuperclass(interval_class, Value.fromObject(object_class));

    // Float -> Number
    setClassSuperclass(float_class, Value.fromObject(number_class));

    // Exception -> Object
    setClassSuperclass(exception_class, Value.fromObject(object_class));

    // Error -> Exception
    setClassSuperclass(error_class, Value.fromObject(exception_class));

    // Message -> Object
    setClassSuperclass(message_class, Value.fromObject(object_class));

    // MessageNotUnderstood -> Error
    setClassSuperclass(mnu_class, Value.fromObject(error_class));

    // Dictionary -> Object (in full system: HashedCollection -> Collection -> Object)
    setClassSuperclass(dictionary_class, Value.fromObject(object_class));

    // Set -> Object (in full system: HashedCollection -> Collection -> Object)
    setClassSuperclass(set_class, Value.fromObject(object_class));

    // OrderedCollection -> Object (in full system: SequenceableCollection -> Collection -> Object)
    setClassSuperclass(ordered_collection_class, Value.fromObject(object_class));

    // ReadStream -> Object (in full system: PositionableStream -> Stream -> Object)
    setClassSuperclass(read_stream_class, Value.fromObject(object_class));

    // WriteStream -> Object (in full system: PositionableStream -> Stream -> Object)
    setClassSuperclass(write_stream_class, Value.fromObject(object_class));

    // FileStream -> WriteStream
    setClassSuperclass(file_stream_class, Value.fromObject(write_stream_class));

    // Association -> Magnitude
    setClassSuperclass(association_class, Value.fromObject(magnitude_class));

    // PoolDictionary -> Dictionary (pool dictionaries are specialized dictionaries)
    setClassSuperclass(pool_dictionary_class, Value.fromObject(dictionary_class));

    // Phase 3: Set class names

    try setClassName(heap, object_class, "Object");
    try setClassName(heap, class_class, "Class");
    try setClassName(heap, metaclass_class, "Metaclass");
    try setClassName(heap, behavior_class, "Behavior");
    try setClassName(heap, class_desc_class, "ClassDescription");
    try setClassName(heap, small_int_class, "SmallInteger");
    try setClassName(heap, string_class, "String");
    try setClassName(heap, symbol_class, "Symbol");
    try setClassName(heap, array_class, "Array");
    try setClassName(heap, byte_array_class, "ByteArray");
    try setClassName(heap, compiled_method_class, "CompiledMethod");
    try setClassName(heap, block_closure_class, "BlockClosure");
    try setClassName(heap, undefined_object_class, "UndefinedObject");
    try setClassName(heap, true_class, "True");
    try setClassName(heap, false_class, "False");
    try setClassName(heap, character_class, "Character");
    try setClassName(heap, interval_class, "Interval");
    try setClassName(heap, float_class, "Float");
    try setClassName(heap, exception_class, "Exception");
    try setClassName(heap, error_class, "Error");
    try setClassName(heap, message_class, "Message");
    try setClassName(heap, mnu_class, "MessageNotUnderstood");
    try setClassName(heap, dictionary_class, "Dictionary");
    try setClassName(heap, set_class, "Set");
    try setClassName(heap, ordered_collection_class, "OrderedCollection");
    try setClassName(heap, read_stream_class, "ReadStream");
    try setClassName(heap, write_stream_class, "WriteStream");
    try setClassName(heap, file_stream_class, "FileStream");
    try setClassName(heap, deaf_object_class, "DeafObject");
    try setClassName(heap, transcript_shell_class, "TranscriptShell");
    try setClassName(heap, association_class, "Association");
    try setClassName(heap, pool_dictionary_class, "PoolDictionary");
    try setClassName(heap, magnitude_class, "Magnitude");
    try setClassName(heap, number_class, "Number");
    try setClassName(heap, integer_class, "Integer");
    try setClassName(heap, boolean_class, "Boolean");
    try setClassName(heap, collection_class, "Collection");
    try setClassName(heap, sequenceable_collection_class, "SequenceableCollection");
    try setClassName(heap, arrayed_collection_class, "ArrayedCollection");

    // Phase 3b: Set class categories (packages)
    // All built-in classes go into "Kernel" package
    try setClassCategory(heap, object_class, "Kernel");
    try setClassCategory(heap, class_class, "Kernel");
    try setClassCategory(heap, metaclass_class, "Kernel");
    try setClassCategory(heap, behavior_class, "Kernel");
    try setClassCategory(heap, class_desc_class, "Kernel");
    try setClassCategory(heap, small_int_class, "Kernel");
    try setClassCategory(heap, string_class, "Kernel");
    try setClassCategory(heap, symbol_class, "Kernel");
    try setClassCategory(heap, array_class, "Collections");
    try setClassCategory(heap, byte_array_class, "Collections");
    try setClassCategory(heap, compiled_method_class, "Kernel");
    try setClassCategory(heap, block_closure_class, "Kernel");
    try setClassCategory(heap, undefined_object_class, "Kernel");
    try setClassCategory(heap, true_class, "Kernel");
    try setClassCategory(heap, false_class, "Kernel");
    try setClassCategory(heap, character_class, "Kernel");
    try setClassCategory(heap, interval_class, "Collections");
    try setClassCategory(heap, float_class, "Kernel");
    try setClassCategory(heap, exception_class, "Exceptions");
    try setClassCategory(heap, error_class, "Exceptions");
    try setClassCategory(heap, message_class, "Kernel");
    try setClassCategory(heap, dictionary_class, "Collections");
    try setClassCategory(heap, set_class, "Collections");
    try setClassCategory(heap, ordered_collection_class, "Collections");
    try setClassCategory(heap, read_stream_class, "Streams");
    try setClassCategory(heap, write_stream_class, "Streams");
    try setClassCategory(heap, file_stream_class, "Streams");
    try setClassCategory(heap, deaf_object_class, "Kernel");
    try setClassCategory(heap, transcript_shell_class, "Kernel");
    try setClassCategory(heap, association_class, "Collections");
    try setClassCategory(heap, pool_dictionary_class, "Kernel");
    try setClassCategory(heap, magnitude_class, "Kernel");
    try setClassCategory(heap, number_class, "Kernel");
    try setClassCategory(heap, integer_class, "Kernel");
    try setClassCategory(heap, boolean_class, "Kernel");
    try setClassCategory(heap, collection_class, "Collections");
    try setClassCategory(heap, sequenceable_collection_class, "Collections");
    try setClassCategory(heap, arrayed_collection_class, "Collections");

    // Phase 4: Set class formats (number of instance variables, format type)

    // Object has no instance variables
    setClassFormat(object_class, 0, .normal);

    // Class has 5 fields: superclass, methodDict, format, instanceVariables, name
    setClassFormat(class_class, 5, .normal);

    // Metaclass has 6 fields: same as Class + thisClass
    setClassFormat(metaclass_class, 6, .normal);

    // SmallInteger is immediate (no fields)
    setClassFormat(small_int_class, 0, .normal);

    // String is bytes
    setClassFormat(string_class, 0, .bytes);

    // Symbol is bytes
    setClassFormat(symbol_class, 0, .bytes);

    // Array is variable
    setClassFormat(array_class, 0, .variable);

    // ByteArray is bytes
    setClassFormat(byte_array_class, 0, .bytes);

    // CompiledMethod is special
    setClassFormat(compiled_method_class, 0, .compiled_method);

    // BlockClosure has fields: outerContext, startPC, numArgs
    setClassFormat(block_closure_class, 3, .normal);

    // UndefinedObject has no fields
    setClassFormat(undefined_object_class, 0, .normal);

    // True, False have no fields
    setClassFormat(true_class, 0, .normal);
    setClassFormat(false_class, 0, .normal);

    // Character is immediate
    setClassFormat(character_class, 0, .normal);

    // Interval has 3 fields: start, stop, step
    setClassFormat(interval_class, 3, .normal);

    // Float is byte-format to store 8 bytes for f64
    setClassFormat(float_class, 8, .bytes);

    // Exception has 2 instance variables: messageText, tag
    setClassFormat(exception_class, 2, .normal);

    // Error inherits from Exception
    setClassFormat(error_class, 2, .normal);

    // Message has 3 instance variables: selector, arguments, lookupClass
    setClassFormat(message_class, 3, .normal);

    // Dictionary has 2 instance variables: tally, array (for hash table)
    setClassFormat(dictionary_class, 2, .normal);

    // Set has 2 instance variables: tally, array (for hash table)
    setClassFormat(set_class, 2, .normal);

    // OrderedCollection has 3 instance variables: array, firstIndex, lastIndex
    setClassFormat(ordered_collection_class, 3, .normal);

    // ReadStream has 3 instance variables: collection, position, readLimit
    setClassFormat(read_stream_class, 3, .normal);

    // WriteStream has 4 instance variables: collection, position, readLimit, writeLimit
    setClassFormat(write_stream_class, 4, .normal);

    // FileStream has 5 instance variables: collection, position, readLimit, writeLimit, fileHandle
    setClassFormat(file_stream_class, 5, .normal);

    // DeafObject has no instance variables
    setClassFormat(deaf_object_class, 0, .normal);

    // TranscriptShell has 1 instance variable: outputStream
    setClassFormat(transcript_shell_class, 1, .normal);

    // Association has 2 instance variables: key, value
    setClassFormat(association_class, 2, .normal);

    // PoolDictionary inherits from Dictionary (2 inst vars: tally, array)
    setClassFormat(pool_dictionary_class, 2, .normal);

    // Magnitude has no instance variables (abstract class)
    setClassFormat(magnitude_class, 0, .normal);

    // Number has no instance variables (abstract class)
    setClassFormat(number_class, 0, .normal);

    // Integer has no instance variables (abstract class)
    setClassFormat(integer_class, 0, .normal);

    // Boolean has no instance variables (abstract class)
    setClassFormat(boolean_class, 0, .normal);

    // Collection has no instance variables (abstract class)
    setClassFormat(collection_class, 0, .normal);

    // SequenceableCollection has no instance variables (abstract class)
    setClassFormat(sequenceable_collection_class, 0, .normal);

    // ArrayedCollection has no instance variables (abstract class)
    setClassFormat(arrayed_collection_class, 0, .normal);

    // Phase 4b: Create metaclasses for each class
    // The metaclass hierarchy mirrors the class hierarchy:
    // Object class -> Class
    // Array class -> Object class -> Class
    // etc.

    const object_metaclass = try createMetaclassFor(heap, object_class, "Object");
    const class_metaclass = try createMetaclassFor(heap, class_class, "Class");
    const metaclass_metaclass = try createMetaclassFor(heap, metaclass_class, "Metaclass");
    const behavior_metaclass = try createMetaclassFor(heap, behavior_class, "Behavior");
    const class_desc_metaclass = try createMetaclassFor(heap, class_desc_class, "ClassDescription");
    const small_int_metaclass = try createMetaclassFor(heap, small_int_class, "SmallInteger");
    const string_metaclass = try createMetaclassFor(heap, string_class, "String");
    const symbol_metaclass = try createMetaclassFor(heap, symbol_class, "Symbol");
    const array_metaclass = try createMetaclassFor(heap, array_class, "Array");
    const byte_array_metaclass = try createMetaclassFor(heap, byte_array_class, "ByteArray");
    const compiled_method_metaclass = try createMetaclassFor(heap, compiled_method_class, "CompiledMethod");
    const block_closure_metaclass = try createMetaclassFor(heap, block_closure_class, "BlockClosure");
    const undefined_object_metaclass = try createMetaclassFor(heap, undefined_object_class, "UndefinedObject");
    const true_metaclass = try createMetaclassFor(heap, true_class, "True");
    const false_metaclass = try createMetaclassFor(heap, false_class, "False");
    const character_metaclass = try createMetaclassFor(heap, character_class, "Character");
    const interval_metaclass = try createMetaclassFor(heap, interval_class, "Interval");
    const float_metaclass = try createMetaclassFor(heap, float_class, "Float");
    const exception_metaclass = try createMetaclassFor(heap, exception_class, "Exception");
    const error_metaclass = try createMetaclassFor(heap, error_class, "Error");
    const message_metaclass = try createMetaclassFor(heap, message_class, "Message");
    const dictionary_metaclass = try createMetaclassFor(heap, dictionary_class, "Dictionary");
    const set_metaclass = try createMetaclassFor(heap, set_class, "Set");
    const ordered_collection_metaclass = try createMetaclassFor(heap, ordered_collection_class, "OrderedCollection");
    const read_stream_metaclass = try createMetaclassFor(heap, read_stream_class, "ReadStream");
    const write_stream_metaclass = try createMetaclassFor(heap, write_stream_class, "WriteStream");
    const file_stream_metaclass = try createMetaclassFor(heap, file_stream_class, "FileStream");
    const deaf_object_metaclass = try createMetaclassFor(heap, deaf_object_class, "DeafObject");
    const transcript_shell_metaclass = try createMetaclassFor(heap, transcript_shell_class, "TranscriptShell");
    const association_metaclass = try createMetaclassFor(heap, association_class, "Association");
    const pool_dictionary_metaclass = try createMetaclassFor(heap, pool_dictionary_class, "PoolDictionary");
    const magnitude_metaclass = try createMetaclassFor(heap, magnitude_class, "Magnitude");
    const number_metaclass = try createMetaclassFor(heap, number_class, "Number");
    const integer_metaclass = try createMetaclassFor(heap, integer_class, "Integer");
    const boolean_metaclass = try createMetaclassFor(heap, boolean_class, "Boolean");
    const collection_metaclass = try createMetaclassFor(heap, collection_class, "Collection");
    const sequenceable_collection_metaclass = try createMetaclassFor(heap, sequenceable_collection_class, "SequenceableCollection");
    const arrayed_collection_metaclass = try createMetaclassFor(heap, arrayed_collection_class, "ArrayedCollection");

    // Set up metaclass superclass chain
    // Object class's superclass is Class (the root)
    setMetaclassSuperclass(object_metaclass, Value.fromObject(class_class));

    // Other metaclasses' superclasses follow the class hierarchy
    // Magnitude class -> Object class
    setMetaclassSuperclass(magnitude_metaclass, Value.fromObject(object_metaclass));
    // Number class -> Magnitude class
    setMetaclassSuperclass(number_metaclass, Value.fromObject(magnitude_metaclass));
    // Integer class -> Number class
    setMetaclassSuperclass(integer_metaclass, Value.fromObject(number_metaclass));
    // SmallInteger class -> Integer class
    setMetaclassSuperclass(small_int_metaclass, Value.fromObject(integer_metaclass));
    // Collection class -> Object class
    setMetaclassSuperclass(collection_metaclass, Value.fromObject(object_metaclass));
    // SequenceableCollection class -> Collection class
    setMetaclassSuperclass(sequenceable_collection_metaclass, Value.fromObject(collection_metaclass));
    // ArrayedCollection class -> SequenceableCollection class
    setMetaclassSuperclass(arrayed_collection_metaclass, Value.fromObject(sequenceable_collection_metaclass));
    // String class -> ArrayedCollection class
    setMetaclassSuperclass(string_metaclass, Value.fromObject(arrayed_collection_metaclass));
    setMetaclassSuperclass(symbol_metaclass, Value.fromObject(string_metaclass));
    // Array class -> ArrayedCollection class
    setMetaclassSuperclass(array_metaclass, Value.fromObject(arrayed_collection_metaclass));
    // ByteArray class -> ArrayedCollection class
    setMetaclassSuperclass(byte_array_metaclass, Value.fromObject(arrayed_collection_metaclass));
    setMetaclassSuperclass(compiled_method_metaclass, Value.fromObject(object_metaclass));
    setMetaclassSuperclass(block_closure_metaclass, Value.fromObject(object_metaclass));
    setMetaclassSuperclass(undefined_object_metaclass, Value.fromObject(object_metaclass));
    // Boolean class -> Object class
    setMetaclassSuperclass(boolean_metaclass, Value.fromObject(object_metaclass));
    // True class -> Boolean class
    setMetaclassSuperclass(true_metaclass, Value.fromObject(boolean_metaclass));
    // False class -> Boolean class
    setMetaclassSuperclass(false_metaclass, Value.fromObject(boolean_metaclass));
    setMetaclassSuperclass(character_metaclass, Value.fromObject(magnitude_metaclass));
    setMetaclassSuperclass(interval_metaclass, Value.fromObject(object_metaclass));
    setMetaclassSuperclass(float_metaclass, Value.fromObject(number_metaclass));

    // Exception class -> Object class
    setMetaclassSuperclass(exception_metaclass, Value.fromObject(object_metaclass));
    // Error class -> Exception class
    setMetaclassSuperclass(error_metaclass, Value.fromObject(exception_metaclass));
    // Message class -> Object class
    setMetaclassSuperclass(message_metaclass, Value.fromObject(object_metaclass));
    // Dictionary class -> Object class
    setMetaclassSuperclass(dictionary_metaclass, Value.fromObject(object_metaclass));
    // Set class -> Object class
    setMetaclassSuperclass(set_metaclass, Value.fromObject(object_metaclass));
    // OrderedCollection class -> Object class
    setMetaclassSuperclass(ordered_collection_metaclass, Value.fromObject(object_metaclass));
    // ReadStream class -> Object class
    setMetaclassSuperclass(read_stream_metaclass, Value.fromObject(object_metaclass));
    // WriteStream class -> Object class
    setMetaclassSuperclass(write_stream_metaclass, Value.fromObject(object_metaclass));
    // FileStream class -> WriteStream class
    setMetaclassSuperclass(file_stream_metaclass, Value.fromObject(write_stream_metaclass));
    // Association class -> Magnitude class
    setMetaclassSuperclass(association_metaclass, Value.fromObject(magnitude_metaclass));
    // PoolDictionary class -> Dictionary class
    setMetaclassSuperclass(pool_dictionary_metaclass, Value.fromObject(dictionary_metaclass));
    // DeafObject class -> Object class
    setMetaclassSuperclass(deaf_object_metaclass, Value.fromObject(object_metaclass));
    // TranscriptShell class -> Object class
    setMetaclassSuperclass(transcript_shell_metaclass, Value.fromObject(object_metaclass));

    // Behavior class -> Object class
    setMetaclassSuperclass(behavior_metaclass, Value.fromObject(object_metaclass));
    // ClassDescription class -> Behavior class
    setMetaclassSuperclass(class_desc_metaclass, Value.fromObject(behavior_metaclass));
    // Class class -> ClassDescription class
    setMetaclassSuperclass(class_metaclass, Value.fromObject(class_desc_metaclass));
    // Metaclass class -> ClassDescription class
    setMetaclassSuperclass(metaclass_metaclass, Value.fromObject(class_desc_metaclass));

    // Phase 5: Register globals

    try heap.setGlobal("Object", Value.fromObject(object_class));
    try heap.setGlobal("Class", Value.fromObject(class_class));
    try heap.setGlobal("Metaclass", Value.fromObject(metaclass_class));
    try heap.setGlobal("Behavior", Value.fromObject(behavior_class));
    try heap.setGlobal("ClassDescription", Value.fromObject(class_desc_class));
    try heap.setGlobal("SmallInteger", Value.fromObject(small_int_class));
    try heap.setGlobal("String", Value.fromObject(string_class));
    try heap.setGlobal("Symbol", Value.fromObject(symbol_class));
    try heap.setGlobal("Array", Value.fromObject(array_class));
    try heap.setGlobal("ByteArray", Value.fromObject(byte_array_class));
    try heap.setGlobal("CompiledMethod", Value.fromObject(compiled_method_class));
    try heap.setGlobal("BlockClosure", Value.fromObject(block_closure_class));
    try heap.setGlobal("UndefinedObject", Value.fromObject(undefined_object_class));
    try heap.setGlobal("True", Value.fromObject(true_class));
    try heap.setGlobal("False", Value.fromObject(false_class));
    try heap.setGlobal("Character", Value.fromObject(character_class));
    try heap.setGlobal("Interval", Value.fromObject(interval_class));
    try heap.setGlobal("Float", Value.fromObject(float_class));
    try heap.setGlobal("Exception", Value.fromObject(exception_class));
    try heap.setGlobal("Error", Value.fromObject(error_class));
    try heap.setGlobal("Message", Value.fromObject(message_class));
    try heap.setGlobal("MessageNotUnderstood", Value.fromObject(mnu_class));
    try heap.setGlobal("Dictionary", Value.fromObject(dictionary_class));
    try heap.setGlobal("Set", Value.fromObject(set_class));
    try heap.setGlobal("OrderedCollection", Value.fromObject(ordered_collection_class));
    try heap.setGlobal("ReadStream", Value.fromObject(read_stream_class));
    try heap.setGlobal("WriteStream", Value.fromObject(write_stream_class));
    try heap.setGlobal("FileStream", Value.fromObject(file_stream_class));
    try heap.setGlobal("DeafObject", Value.fromObject(deaf_object_class));
    try heap.setGlobal("TranscriptShell", Value.fromObject(transcript_shell_class));

    // Create DeafObject singleton instance and set Transcript global
    const deaf_object_instance = try heap.allocateObject(Heap.CLASS_DEAF_OBJECT, 0, .normal);
    try heap.setGlobal("Transcript", Value.fromObject(deaf_object_instance));

    // Create Smalltalk dictionary instance (system dictionary for globals)
    const smalltalk_dict = try heap.allocateObject(Heap.CLASS_DICTIONARY, 2, .normal);
    smalltalk_dict.setField(0, Value.fromSmallInt(0), 2); // tally = 0
    smalltalk_dict.setField(1, Value.nil, 2); // array = nil (will be initialized on first put)
    try heap.setGlobal("Smalltalk", Value.fromObject(smalltalk_dict));

    try heap.setGlobal("Association", Value.fromObject(association_class));
    try heap.setGlobal("PoolDictionary", Value.fromObject(pool_dictionary_class));
    try heap.setGlobal("Magnitude", Value.fromObject(magnitude_class));
    try heap.setGlobal("Number", Value.fromObject(number_class));
    try heap.setGlobal("Integer", Value.fromObject(integer_class));
    try heap.setGlobal("Boolean", Value.fromObject(boolean_class));
    try heap.setGlobal("Collection", Value.fromObject(collection_class));
    try heap.setGlobal("SequenceableCollection", Value.fromObject(sequenceable_collection_class));
    try heap.setGlobal("ArrayedCollection", Value.fromObject(arrayed_collection_class));

    // Phase 6: Install core methods
    try installCoreMethods(heap);

}

fn createClassObject(heap: *Heap, class_index: u32) !*Object {
    // A class has 7 fields: superclass, methodDict, format, instanceVariables, name, classVars, metaclass
    return try heap.allocateObject(class_index, Heap.CLASS_NUM_FIELDS, .normal);
}

/// Create a metaclass object for a class
fn createMetaclassFor(heap: *Heap, class: *Object, name: []const u8) !*Object {
    // Metaclass has 8 fields: same as Class + thisClass
    const metaclass = try heap.allocateObject(Heap.CLASS_METACLASS, Heap.METACLASS_NUM_FIELDS, .normal);

    // Set metaclass's thisClass to point to this class
    metaclass.setField(Heap.METACLASS_FIELD_THIS_CLASS, Value.fromObject(class), Heap.METACLASS_NUM_FIELDS);

    // Metaclass name is "ClassName class"
    var meta_name_buf: [128]u8 = undefined;
    const meta_name = std.fmt.bufPrint(&meta_name_buf, "{s} class", .{name}) catch name;
    const meta_name_sym = try heap.internSymbol(meta_name);
    metaclass.setField(Heap.CLASS_FIELD_NAME, meta_name_sym, Heap.METACLASS_NUM_FIELDS);

    // Describe the layout of class objects (instances of this metaclass)
    // Note: Metaclasses have METACLASS_NUM_FIELDS (11), not CLASS_NUM_FIELDS (10)
    // because they have the extra METACLASS_FIELD_THIS_CLASS field
    setMetaclassFormat(metaclass, @as(i61, Heap.METACLASS_NUM_FIELDS), .normal);

    // Link the class to its metaclass
    class.setField(Heap.CLASS_FIELD_METACLASS, Value.fromObject(metaclass), Heap.CLASS_NUM_FIELDS);

    return metaclass;
}

fn setClassSuperclass(class: *Object, superclass: Value) void {
    class.setField(Heap.CLASS_FIELD_SUPERCLASS, superclass, Heap.CLASS_NUM_FIELDS);
}

fn setMetaclassSuperclass(metaclass: *Object, superclass: Value) void {
    metaclass.setField(Heap.CLASS_FIELD_SUPERCLASS, superclass, Heap.METACLASS_NUM_FIELDS);
}

fn setClassName(heap: *Heap, class: *Object, name: []const u8) !void {
    const sym = try heap.internSymbol(name);
    class.setField(Heap.CLASS_FIELD_NAME, sym, Heap.CLASS_NUM_FIELDS);
}

fn setClassCategory(heap: *Heap, class: *Object, category: []const u8) !void {
    const sym = try heap.internSymbol(category);
    class.setField(Heap.CLASS_FIELD_CATEGORY, sym, Heap.CLASS_NUM_FIELDS);
}

fn setClassFormat(class: *Object, num_inst_vars: i61, format: ClassFormat) void {
    const format_val = Heap.encodeInstanceSpec(@intCast(num_inst_vars), format);
    class.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(format_val), Heap.CLASS_NUM_FIELDS);
}

fn setMetaclassFormat(metaclass: *Object, num_inst_vars: i61, format: ClassFormat) void {
    const format_val = Heap.encodeInstanceSpec(@intCast(num_inst_vars), format);
    metaclass.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(format_val), Heap.METACLASS_NUM_FIELDS);
}

/// Create a method dictionary (simple array of [selector, method] pairs)
fn createMethodDict(heap: *Heap, capacity: usize) !*Object {
    // Allocate an array with capacity * 2 slots (selector, method pairs)
    return try heap.allocateObject(Heap.CLASS_ARRAY, capacity * 2, .variable);
}

/// Hash-based method dictionary insertion
/// Uses linear probing with the same hash function as lookup
fn insertIntoMethodDict(dict_obj: *Object, selector_sym: Value, method_val: Value) bool {
    const dict_size = dict_obj.header.size;
    // Dictionary must have at least 2 slots and be even (selector, method pairs)
    if (dict_size < 2 or (dict_size & 1) != 0) return false;

    const num_slots = dict_size / 2;
    if (num_slots == 0) return false;

    const fields = dict_obj.fields(dict_size);

    // Hash from selector bits - must match lookup!
    const hash = selector_sym.bits *% 2654435761; // Knuth's multiplicative hash
    var index = @as(usize, @intCast(hash)) % num_slots;

    // Linear probe to find empty slot or existing key
    var probes: usize = 0;
    while (probes < num_slots) : (probes += 1) {
        const slot_base = index * 2;

        // Empty slot - insert here
        if (fields[slot_base].isNil()) {
            fields[slot_base] = selector_sym;
            fields[slot_base + 1] = method_val;
            return true;
        }

        // Existing key - update
        if (fields[slot_base].bits == selector_sym.bits) {
            fields[slot_base + 1] = method_val;
            return true;
        }

        // Linear probe
        index = (index + 1) % num_slots;
    }

    return false; // Dictionary full
}

/// Install a method in a class's method dictionary
pub fn installMethod(heap: *Heap, class: *Object, selector: []const u8, method: *object.CompiledMethod) !void {
    // Get or create method dictionary
    var method_dict = class.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);

    if (method_dict.isNil()) {
        // Create a new method dictionary (must be larger than needed for hash efficiency)
        const dict = try createMethodDict(heap, 128); // ~50% load factor for 64 methods
        method_dict = Value.fromObject(dict);
        class.setField(Heap.CLASS_FIELD_METHOD_DICT, method_dict, Heap.CLASS_NUM_FIELDS);
    }

    const dict_obj = method_dict.asObject();
    const selector_sym = try heap.internSymbol(selector);
    const method_val = Value.fromObject(@ptrCast(@alignCast(method)));

    if (!insertIntoMethodDict(dict_obj, selector_sym, method_val)) {
        // Dictionary full - shouldn't happen with reasonable capacity
        return error.MethodDictFull;
    }
}

/// Install a class-side method in a metaclass's method dictionary
pub fn installClassMethod(heap: *Heap, class: *Object, selector: []const u8, method: *object.CompiledMethod) !void {
    // Get the metaclass from the class
    const metaclass_val = class.getField(Heap.CLASS_FIELD_METACLASS, Heap.CLASS_NUM_FIELDS);
    if (metaclass_val.isNil() or !metaclass_val.isObject()) {
        return error.NoMetaclass;
    }

    const metaclass = metaclass_val.asObject();

    // Get or create method dictionary for the metaclass
    var method_dict = metaclass.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.METACLASS_NUM_FIELDS);

    if (method_dict.isNil()) {
        // Create a new method dictionary (larger for hash efficiency)
        const dict = try createMethodDict(heap, 64); // ~50% load factor for 32 methods
        method_dict = Value.fromObject(dict);
        metaclass.setField(Heap.CLASS_FIELD_METHOD_DICT, method_dict, Heap.METACLASS_NUM_FIELDS);
    }

    const dict_obj = method_dict.asObject();
    const selector_sym = try heap.internSymbol(selector);
    const method_val = Value.fromObject(@ptrCast(@alignCast(method)));

    if (!insertIntoMethodDict(dict_obj, selector_sym, method_val)) {
        return error.MethodDictFull;
    }
}

/// Create a method that just returns self (for Object>>yourself)
pub fn createYourselfMethod(heap: *Heap) !*object.CompiledMethod {
    _ = heap;
    const bytecode_size: usize = 1;
    const num_literals: usize = 0;

    const header_size = @sizeOf(object.CompiledMethod.MethodHeader);
    const literals_size = num_literals * @sizeOf(Value);
    const total_size = header_size + literals_size + bytecode_size;

    const mem = try std.heap.page_allocator.alignedAlloc(u8, std.mem.Alignment.of(object.CompiledMethod), total_size);
    const method: *object.CompiledMethod = @ptrCast(mem.ptr);

    method.header = .{
        .num_args = 0,
        .num_temps = 0,
        .num_literals = 0,
        .primitive_index = 0,
        .flags = .{},
        .bytecode_size = @intCast(bytecode_size),
    };

    // Bytecode: return self (receiver)
    const bytecodes_ptr: [*]u8 = @ptrFromInt(@intFromPtr(method) + header_size + literals_size);
    bytecodes_ptr[0] = 0xA0; // return_receiver

    return method;
}

/// Create a primitive method (method that just calls a primitive)
pub fn createPrimitiveMethod(heap: *Heap, num_args: u8, primitive_index: u16) !*object.CompiledMethod {
    // A primitive method has:
    // - Method header with primitive index
    // - Single bytecode: return_top (in case primitive fails, though we don't handle that yet)

    const bytecode_size: usize = 1;
    const num_literals: usize = 0;

    const header_size = @sizeOf(object.CompiledMethod.MethodHeader);
    const literals_size = num_literals * @sizeOf(Value);
    const total_size = header_size + literals_size + bytecode_size;

    _ = heap;

    const mem = try std.heap.page_allocator.alignedAlloc(u8, std.mem.Alignment.of(object.CompiledMethod), total_size);
    const method: *object.CompiledMethod = @ptrCast(mem.ptr);

    method.header = .{
        .num_args = num_args,
        .num_temps = num_args,
        .num_literals = 0,
        .primitive_index = primitive_index,
        .flags = .{},
        .bytecode_size = @intCast(bytecode_size),
    };

    // Bytecode: return top of stack
    const bytecodes_ptr: [*]u8 = @ptrFromInt(@intFromPtr(method) + header_size + literals_size);
    bytecodes_ptr[0] = 0xA4; // return_top

    return method;
}

/// Create a method that just returns self (for methods like Integer>>truncated)
pub fn createReturnSelfMethod(num_args: u8) !*object.CompiledMethod {
    const bytecode_size: usize = 1;
    const num_literals: usize = 0;

    const header_size = @sizeOf(object.CompiledMethod.MethodHeader);
    const literals_size = num_literals * @sizeOf(Value);
    const total_size = header_size + literals_size + bytecode_size;

    const mem = try std.heap.page_allocator.alignedAlloc(u8, std.mem.Alignment.of(object.CompiledMethod), total_size);
    const method: *object.CompiledMethod = @ptrCast(mem.ptr);

    method.header = .{
        .num_args = num_args,
        .num_temps = num_args,
        .num_literals = 0,
        .primitive_index = 0,
        .flags = .{},
        .bytecode_size = @intCast(bytecode_size),
    };

    // Bytecode: return self
    const bytecodes_ptr: [*]u8 = @ptrFromInt(@intFromPtr(method) + header_size + literals_size);
    bytecodes_ptr[0] = 0xA0; // return_receiver

    return method;
}

/// Install core methods in bootstrap classes
/// Uses Dolphin-compatible primitive numbers for SmallInteger, Object, etc.
pub fn installCoreMethods(heap: *Heap) !void {
    // Get class objects with safety checks
    const small_int_val = heap.getClass(Heap.CLASS_SMALL_INTEGER);
    const string_val = heap.getClass(Heap.CLASS_STRING);
    const object_val = heap.getClass(Heap.CLASS_OBJECT);
    const true_val = heap.getClass(Heap.CLASS_TRUE);
    const false_val = heap.getClass(Heap.CLASS_FALSE);

    // Validate that all required classes are objects
    if (!small_int_val.isObject() or !string_val.isObject() or !object_val.isObject() or
        !true_val.isObject() or !false_val.isObject())
    {
        return error.ClassNotInitialized;
    }

    const small_int_class = small_int_val.asObject();
    const string_class = string_val.asObject();
    const object_class = object_val.asObject();
    const true_class = true_val.asObject();
    const false_class = false_val.asObject();

    // SmallInteger methods - Dolphin compatible primitive numbers
    try installMethod(heap, small_int_class, "+", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.add))); // 15
    try installMethod(heap, small_int_class, "-", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.subtract))); // 14
    try installMethod(heap, small_int_class, "*", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.multiply))); // 9
    try installMethod(heap, small_int_class, "/", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.divide))); // 10
    try installMethod(heap, small_int_class, "//", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.integer_divide))); // 12
    try installMethod(heap, small_int_class, "\\\\", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.mod))); // 11
    try installMethod(heap, small_int_class, "quo:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.quo))); // 13
    try installMethod(heap, small_int_class, "<", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.less_than))); // 18
    try installMethod(heap, small_int_class, ">", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.greater_than))); // 19
    try installMethod(heap, small_int_class, "<=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.less_or_equal))); // 20
    try installMethod(heap, small_int_class, ">=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.greater_or_equal))); // 17
    try installMethod(heap, small_int_class, "=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.equal))); // 16
    try installMethod(heap, small_int_class, "~=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.not_equal))); // 550
    try installMethod(heap, small_int_class, "bitAnd:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.bit_and))); // 40
    try installMethod(heap, small_int_class, "bitOr:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.bit_or))); // 41
    try installMethod(heap, small_int_class, "bitXor:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.bit_xor))); // 42
    try installMethod(heap, small_int_class, "bitShift:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.bit_shift))); // 43
    try installMethod(heap, small_int_class, "hash", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.hash))); // 109
    try installMethod(heap, small_int_class, "identityHash", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.hash))); // 109

    // Our extended Integer methods (using high primitive numbers)
    try installMethod(heap, small_int_class, "negated", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.negate)));
    try installMethod(heap, small_int_class, "abs", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.abs)));
    try installMethod(heap, small_int_class, "even", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.even)));
    try installMethod(heap, small_int_class, "odd", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.odd)));
    try installMethod(heap, small_int_class, "positive", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.positive)));
    try installMethod(heap, small_int_class, "negative", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.negative)));
    try installMethod(heap, small_int_class, "sign", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.sign)));
    try installMethod(heap, small_int_class, "max:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.max)));
    try installMethod(heap, small_int_class, "min:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.min)));
    try installMethod(heap, small_int_class, "between:and:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.between_and)));
    try installMethod(heap, small_int_class, "timesRepeat:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.times_repeat)));
    try installMethod(heap, small_int_class, "to:do:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.to_do)));
    try installMethod(heap, small_int_class, "to:by:do:", try createPrimitiveMethod(heap, 3, @intFromEnum(Primitive.to_by_do)));
    try installMethod(heap, small_int_class, "factorial", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.factorial)));
    try installMethod(heap, small_int_class, "printString", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.small_int_print_string))); // 44

    // String methods - Dolphin compatible
    try installMethod(heap, string_class, "size", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, string_class, "basicSize", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, string_class, "at:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, string_class, "basicAt:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, string_class, "at:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61
    try installMethod(heap, string_class, "basicAt:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61
    try installMethod(heap, string_class, ",", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_concat))); // 522
    try installMethod(heap, string_class, "compare:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_compare))); // 106
    try installMethod(heap, string_class, "<", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_less_than)));
    try installMethod(heap, string_class, ">", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_greater_than)));
    try installMethod(heap, string_class, "<=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_less_or_equal)));
    try installMethod(heap, string_class, ">=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_greater_or_equal)));
    try installMethod(heap, string_class, "=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.string_equal)));
    try installMethod(heap, string_class, "copyFrom:to:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.string_copy_from_to)));
    try installMethod(heap, string_class, "ffiCall:with:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.ffi_call_with_struct))); // 792

    // Symbol methods - Symbol is used as receiver in FFI calls (e.g., #Raylib ffiCall:with:)
    const symbol_val = heap.getClass(Heap.CLASS_SYMBOL);
    if (!symbol_val.isObject()) return error.ClassNotInitialized;
    const symbol_class = symbol_val.asObject();
    try installMethod(heap, symbol_class, "ffiCall:with:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.ffi_call_with_struct))); // 792

    // Array methods - Dolphin compatible
    const array_val = heap.getClass(Heap.CLASS_ARRAY);
    if (!array_val.isObject()) return error.ClassNotInitialized;
    const array_class = array_val.asObject();
    try installMethod(heap, array_class, "size", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, array_class, "basicSize", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, array_class, "at:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, array_class, "basicAt:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, array_class, "at:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61
    try installMethod(heap, array_class, "basicAt:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61

    // Collection operations (our extensions)
    try installMethod(heap, array_class, "do:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_do)));
    try installMethod(heap, array_class, "collect:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_collect)));
    try installMethod(heap, array_class, "select:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_select)));
    try installMethod(heap, array_class, "reject:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_reject)));
    try installMethod(heap, array_class, "detect:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_detect)));
    try installMethod(heap, array_class, "detect:ifNone:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.array_detect_if_none)));
    try installMethod(heap, array_class, "inject:into:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.array_inject_into)));
    try installMethod(heap, array_class, "includes:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_includes)));
    try installMethod(heap, array_class, "indexOf:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.array_index_of)));
    try installMethod(heap, array_class, "indexOf:ifAbsent:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.array_index_of_if_absent)));
    try installMethod(heap, array_class, "reversed", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.array_reversed)));

    // Object methods - Dolphin compatible
    try installMethod(heap, object_class, "class", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class))); // 111
    try installMethod(heap, object_class, "basicClass", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class))); // 111
    try installMethod(heap, object_class, "hash", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.hash))); // 109
    try installMethod(heap, object_class, "identityHash", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.identity_hash))); // 147
    try installMethod(heap, object_class, "basicIdentityHash", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_identity_hash))); // 75
    try installMethod(heap, object_class, "==", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.identical))); // 110
    try installMethod(heap, object_class, "=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.identical))); // 110 (default)
    try installMethod(heap, object_class, "~=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.not_equal)));
    try installMethod(heap, object_class, "basicSize", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, object_class, "at:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, object_class, "at:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61
    try installMethod(heap, object_class, "basicAt:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, object_class, "basicAt:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61
    try installMethod(heap, object_class, "printString", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.print_string)));
    try installMethod(heap, object_class, "yourself", try createYourselfMethod(heap)); // Returns self

    // Error handling methods
    try installMethod(heap, object_class, "doesNotUnderstand:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.does_not_understand)));
    try installMethod(heap, object_class, "error:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.error_message)));
    try installMethod(heap, object_class, "halt", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.halt)));
    try installMethod(heap, object_class, "respondsTo:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.responds_to)));

    // Reflection methods
    try installMethod(heap, object_class, "perform:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.perform)));
    try installMethod(heap, object_class, "perform:withArguments:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.perform_with_args)));
    try installMethod(heap, object_class, "instVarAt:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.inst_var_at)));
    try installMethod(heap, object_class, "instVarAt:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.inst_var_at_put)));

    // Class reflection methods (on Object class which is a metaclass)
    const behavior_class = heap.getClass(Heap.CLASS_CLASS).asObject(); // Behavior is approximated by Class
    try installMethod(heap, behavior_class, "selectors", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class_selectors)));
    try installMethod(heap, behavior_class, "allSelectors", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class_all_selectors)));
    try installMethod(heap, behavior_class, "superclass", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class_superclass)));
    try installMethod(heap, behavior_class, "name", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class_name)));
    try installMethod(heap, behavior_class, "instSize", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class_inst_size)));

    // Exception methods
    const exception_class = heap.getClass(Heap.CLASS_EXCEPTION).asObject();
    try installMethod(heap, exception_class, "signal", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.exception_signal)));
    try installMethod(heap, exception_class, "signal:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.exception_signal_with)));

    // BlockClosure methods - Use our extended primitives for different arg counts
    const block_closure_class = heap.getClass(Heap.CLASS_BLOCK_CLOSURE).asObject();
    try installMethod(heap, block_closure_class, "value", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.block_value))); // 81 (0 args)
    try installMethod(heap, block_closure_class, "value:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.block_value_1))); // 490 (1 arg)
    try installMethod(heap, block_closure_class, "value:value:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.block_value_2))); // 491 (2 args)
    try installMethod(heap, block_closure_class, "value:value:value:", try createPrimitiveMethod(heap, 3, @intFromEnum(Primitive.block_value_3))); // 492 (3 args)
    try installMethod(heap, block_closure_class, "value:value:value:value:", try createPrimitiveMethod(heap, 4, @intFromEnum(Primitive.block_value_4))); // 493 (4 args)
    try installMethod(heap, block_closure_class, "valueWithArguments:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.block_value_with_args))); // 82
    try installMethod(heap, block_closure_class, "whileTrue:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.while_true)));
    try installMethod(heap, block_closure_class, "whileFalse:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.while_false)));
    try installMethod(heap, block_closure_class, "on:do:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.on_do)));
    try installMethod(heap, block_closure_class, "ensure:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.ensure)));
    try installMethod(heap, block_closure_class, "ifCurtailed:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.if_curtailed)));

    // True methods (our extensions)
    try installMethod(heap, true_class, "ifTrue:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_if_true)));
    try installMethod(heap, true_class, "ifFalse:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_if_false)));
    try installMethod(heap, true_class, "ifTrue:ifFalse:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.true_if_true_if_false)));
    try installMethod(heap, true_class, "ifFalse:ifTrue:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.true_if_false_if_true)));
    try installMethod(heap, true_class, "not", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.boolean_not)));
    try installMethod(heap, true_class, "and:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_and)));
    try installMethod(heap, true_class, "or:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_or)));
    try installMethod(heap, true_class, "&", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_amp)));
    try installMethod(heap, true_class, "|", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_pipe)));

    // False methods (our extensions)
    try installMethod(heap, false_class, "ifTrue:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_if_true)));
    try installMethod(heap, false_class, "ifFalse:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_if_false)));
    try installMethod(heap, false_class, "ifTrue:ifFalse:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.false_if_true_if_false)));
    try installMethod(heap, false_class, "ifFalse:ifTrue:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.false_if_false_if_true)));
    try installMethod(heap, false_class, "not", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.boolean_not)));
    try installMethod(heap, false_class, "and:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_and)));
    try installMethod(heap, false_class, "or:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_or)));
    try installMethod(heap, false_class, "&", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_amp)));
    try installMethod(heap, false_class, "|", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_pipe)));

    // Float methods - Dolphin compatible primitive numbers
    const float_class = heap.getClass(Heap.CLASS_FLOAT).asObject();
    try installMethod(heap, float_class, "+", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_add))); // 160
    try installMethod(heap, float_class, "-", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_subtract))); // 161
    try installMethod(heap, float_class, "*", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_multiply))); // 164
    try installMethod(heap, float_class, "/", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_divide))); // 165
    try installMethod(heap, float_class, "<", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_less_than))); // 162
    try installMethod(heap, float_class, ">", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_greater_than))); // 45
    try installMethod(heap, float_class, "<=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_less_or_equal))); // 214
    try installMethod(heap, float_class, ">=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_greater_or_equal))); // 46
    try installMethod(heap, float_class, "=", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.float_equal))); // 47
    try installMethod(heap, float_class, "truncated", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_truncated))); // 166
    try installMethod(heap, float_class, "abs", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_abs))); // 205
    try installMethod(heap, float_class, "negated", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_negate))); // our extension
    try installMethod(heap, float_class, "printString", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_print_string))); // 169

    // Float math functions
    try installMethod(heap, float_class, "sqrt", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_sqrt)));
    try installMethod(heap, float_class, "sin", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_sin)));
    try installMethod(heap, float_class, "cos", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_cos)));
    try installMethod(heap, float_class, "tan", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_tan)));
    try installMethod(heap, float_class, "exp", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_exp)));
    try installMethod(heap, float_class, "ln", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_ln)));
    try installMethod(heap, float_class, "log10", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_log10)));
    try installMethod(heap, float_class, "floor", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_floor)));
    try installMethod(heap, float_class, "ceiling", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_ceiling)));
    try installMethod(heap, float_class, "rounded", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_rounded)));
    try installMethod(heap, float_class, "arcSin", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_arcsin)));
    try installMethod(heap, float_class, "arcCos", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_arccos)));
    try installMethod(heap, float_class, "arcTan", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.float_arctan)));

    // SmallInteger >> asFloat
    try installMethod(heap, small_int_class, "asFloat", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.small_as_float))); // 168

    // SmallInteger >> truncated, rounded, floor, ceiling - integers return self
    try installMethod(heap, small_int_class, "truncated", try createReturnSelfMethod(0));
    try installMethod(heap, small_int_class, "rounded", try createReturnSelfMethod(0));
    try installMethod(heap, small_int_class, "floor", try createReturnSelfMethod(0));
    try installMethod(heap, small_int_class, "ceiling", try createReturnSelfMethod(0));

    // SmallInteger >> to: and to:by: (create Interval objects)
    try installMethod(heap, small_int_class, "to:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.to_interval)));
    try installMethod(heap, small_int_class, "to:by:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.to_by_interval)));

    // Interval methods
    const interval_class = heap.getClass(Heap.CLASS_INTERVAL).asObject();
    try installMethod(heap, interval_class, "size", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.interval_size)));
    try installMethod(heap, interval_class, "at:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.interval_at)));
    try installMethod(heap, interval_class, "do:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.interval_do)));

    // Character methods
    const character_class = heap.getClass(Heap.CLASS_CHARACTER).asObject();
    try installMethod(heap, character_class, "asciiValue", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.char_code)));
    try installMethod(heap, character_class, "asInteger", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.char_code)));
    try installMethod(heap, character_class, "codePoint", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.char_code)));

    // UndefinedObject methods (nil)
    const undefined_object_class = heap.getClass(Heap.CLASS_UNDEFINED_OBJECT).asObject();
    try installMethod(heap, undefined_object_class, "isNil", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.is_nil)));
    try installMethod(heap, undefined_object_class, "notNil", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.not_nil)));

    // Object >> isNil and notNil (for non-nil objects)
    try installMethod(heap, object_class, "isNil", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.is_nil_false)));
    try installMethod(heap, object_class, "notNil", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.not_nil)));

    // Dictionary methods - keyed access for hash tables
    const dictionary_class = heap.getClass(Heap.CLASS_DICTIONARY).asObject();
    try installMethod(heap, dictionary_class, "at:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.dict_at)));
    try installMethod(heap, dictionary_class, "at:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.dict_at_put)));
    try installMethod(heap, dictionary_class, "at:ifAbsent:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.dict_at_if_absent)));
    try installMethod(heap, dictionary_class, "keys", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.dict_keys)));
    try installMethod(heap, dictionary_class, "allClasses", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.all_classes)));

    // Object reflection methods
    try installMethod(heap, object_class, "isKindOf:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.is_kind_of)));

    // ========================================================================
    // Class-side methods (installed on metaclasses)
    // ========================================================================

    // Object class methods
    try installClassMethod(heap, object_class, "new", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, object_class, "basicNew", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, object_class, "new:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));
    try installClassMethod(heap, object_class, "basicNew:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));

    // Array class methods
    try installClassMethod(heap, array_class, "new", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, array_class, "basicNew", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, array_class, "new:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));
    try installClassMethod(heap, array_class, "basicNew:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));

    // String class methods
    try installClassMethod(heap, string_class, "new", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, string_class, "basicNew", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, string_class, "new:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));
    try installClassMethod(heap, string_class, "basicNew:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));

    // ByteArray class methods
    const byte_array_class = heap.getClass(Heap.CLASS_BYTE_ARRAY).asObject();
    try installClassMethod(heap, byte_array_class, "new", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, byte_array_class, "basicNew", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.basic_new)));
    try installClassMethod(heap, byte_array_class, "new:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));
    try installClassMethod(heap, byte_array_class, "basicNew:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.basic_new_size)));

    // Character class methods
    try installClassMethod(heap, character_class, "value:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.char_from_code)));
    try installClassMethod(heap, character_class, "codePoint:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.char_from_code)));

    // ========================================================================
    // Process/Timing primitives (Dolphin-compatible primitive numbers)
    // ========================================================================

    // SmallInteger class methods for timing
    try installClassMethod(heap, small_int_class, "microsecondClockValue", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.microsecond_clock_value)));
    try installClassMethod(heap, small_int_class, "millisecondClockValue", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.millisecond_clock_value)));

    // Object class method for yield (temporary - for testing until ProcessorScheduler is set up)
    try installClassMethod(heap, object_class, "yield", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.yield)));
}

/// Ensure critical primitive methods are present (used after loading snapshots)
pub fn ensureCorePrimitives(heap: *Heap) !void {
    var behavior_obj: ?*Object = null;
    const behavior_val = heap.getClass(Heap.CLASS_BEHAVIOR);
    if (behavior_val.isObject()) {
        behavior_obj = behavior_val.asObject();
    }
    if (behavior_obj == null) {
        // Fallback for older snapshots: scan class table for Behavior by name
        for (heap.class_table.items) |val| {
            if (!val.isObject()) continue;
            const obj = val.asObject();
            const name_val = obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    const name_bytes = name_obj.bytes(name_obj.header.size);
                    if (std.mem.eql(u8, name_bytes, "Behavior")) {
                        behavior_obj = obj;
                        break;
                    }
                }
            }
        }
    }

    if (behavior_obj == null) return;

    // Always install the instSize primitive to avoid MethodNotUnderstood during copies
    try installMethod(heap, behavior_obj.?, "instSize", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.class_inst_size)));
}

/// Bootstrap FFI library classes with auto-generated methods
/// Creates a Smalltalk class for each configured FFI library and installs
/// class methods for each C function that wrap the ffiCall:with: primitive
pub fn bootstrapFFILibraries(heap: *Heap) !void {
    if (!ffi_enabled) return;

    // Get Object class as superclass for FFI libraries
    const object_class_val = heap.getClass(Heap.CLASS_OBJECT);
    if (!object_class_val.isObject()) return;
    const object_class = object_class_val.asObject();

    // Iterate through all configured libraries
    for (ffi_generated.library_names) |lib_name| {
        // Skip LibC and LibMath - they're defined in ffi.st with optimized primitives
        if (std.mem.eql(u8, lib_name, "LibC") or std.mem.eql(u8, lib_name, "LibMath")) {
            continue;
        }

        // Check if class already exists in globals
        if (heap.globals.get(lib_name) != null) {
            continue;
        }

        // Create the class (this also pre-allocates the method dictionary)
        _ = try createFFILibraryClass(heap, object_class, lib_name);

        // Get functions for this library and install methods
        if (ffi_generated.getLibraryFunctions(lib_name)) |functions| {
            var installed_count: usize = 0;
            for (functions) |func| {
                // IMPORTANT: Order matters for GC safety!
                // 1. First intern the selector (may trigger GC)
                const selector_str = buildFFISelector(func.name, func.arg_count);
                const selector = try heap.internSymbol(selector_str);

                // 2. Re-fetch class/metaclass (GC may have moved them)
                const lib_class_val = heap.globals.get(lib_name) orelse continue;
                if (!lib_class_val.isObject()) continue;
                const lib_class = lib_class_val.asObject();
                const metaclass_val = lib_class.getField(Heap.CLASS_FIELD_METACLASS, Heap.CLASS_NUM_FIELDS);
                if (!metaclass_val.isObject()) continue;
                const metaclass = metaclass_val.asObject();

                // 3. Create method (allocates literals, then method off-heap)
                //    After this, NO MORE HEAP ALLOCATIONS until method is installed!
                const method = try createFFIMethod(heap, lib_name, func.name, func.arg_count) orelse continue;

                // 4. Install method (method dict is pre-allocated, no GC here)
                try installMethodWithSelector(heap, metaclass, selector, method);
                installed_count += 1;
            }
            std.debug.print("FFI: Installed {d} methods for {s}\n", .{ installed_count, lib_name });
        }
    }
}

/// Build a Smalltalk selector from a C function name and arg count
/// e.g., "sin" with 1 arg -> "sin:"
///       "pow" with 2 args -> "pow:with:"
///       "InitWindow" with 3 args -> "InitWindow:with:and:"
fn buildFFISelector(func_name: []const u8, arg_count: usize) []const u8 {
    // Use static buffer for selector construction
    const Static = struct {
        var buf: [256]u8 = undefined;
    };

    var pos: usize = 0;

    // Copy function name
    for (func_name) |c| {
        if (pos < Static.buf.len - 1) {
            Static.buf[pos] = c;
            pos += 1;
        }
    }

    if (arg_count == 0) {
        return Static.buf[0..pos];
    }

    // Add first colon
    if (pos < Static.buf.len - 1) {
        Static.buf[pos] = ':';
        pos += 1;
    }

    // Add additional keyword args
    var i: usize = 1;
    while (i < arg_count) : (i += 1) {
        const keyword = switch (i) {
            1 => "with:",
            2 => "and:",
            3 => "also:",
            4 => "plus:",
            else => "arg:",
        };
        for (keyword) |c| {
            if (pos < Static.buf.len - 1) {
                Static.buf[pos] = c;
                pos += 1;
            }
        }
    }

    return Static.buf[0..pos];
}

/// Create an FFI library class as a subclass of Object
fn createFFILibraryClass(heap: *Heap, superclass: *Object, name: []const u8) !*Object {
    // Allocate class object
    const class = try heap.allocateObject(Heap.CLASS_CLASS, Heap.CLASS_NUM_FIELDS, .normal);

    // Set up class fields
    const name_sym = try heap.internSymbol(name);
    class.setField(Heap.CLASS_FIELD_NAME, name_sym, Heap.CLASS_NUM_FIELDS);
    class.setField(Heap.CLASS_FIELD_SUPERCLASS, Value.fromObject(superclass), Heap.CLASS_NUM_FIELDS);
    class.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.nil, Heap.CLASS_NUM_FIELDS);
    class.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(@intFromEnum(ClassFormat.normal)), Heap.CLASS_NUM_FIELDS);
    class.setField(Heap.CLASS_FIELD_INST_VARS, Value.nil, Heap.CLASS_NUM_FIELDS);

    // Create and link metaclass (metaclass has one extra field: thisClass)
    const metaclass = try heap.allocateObject(Heap.CLASS_METACLASS, Heap.METACLASS_NUM_FIELDS, .normal);
    metaclass.setField(Heap.CLASS_FIELD_NAME, name_sym, Heap.METACLASS_NUM_FIELDS);
    // Metaclass's superclass should be the superclass's metaclass (e.g., Object class)
    const super_metaclass = superclass.getField(Heap.CLASS_FIELD_METACLASS, Heap.CLASS_NUM_FIELDS);
    metaclass.setField(Heap.CLASS_FIELD_SUPERCLASS, super_metaclass, Heap.METACLASS_NUM_FIELDS);
    metaclass.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(@intFromEnum(ClassFormat.normal)), Heap.METACLASS_NUM_FIELDS);
    metaclass.setField(Heap.METACLASS_FIELD_THIS_CLASS, Value.fromObject(class), Heap.METACLASS_NUM_FIELDS);
    class.setField(Heap.CLASS_FIELD_METACLASS, Value.fromObject(metaclass), Heap.CLASS_NUM_FIELDS);

    // Register class in globals FIRST (so we can re-fetch it after GC)
    // Use setGlobal which properly dupes the key string
    try heap.setGlobal(name, Value.fromObject(class));

    // Pre-allocate method dictionary for metaclass (large enough for ~600 FFI methods)
    // This prevents GC during method installation
    const method_dict_size: usize = 1200; // 600 methods * 2 slots each
    const method_dict = try heap.allocateObject(Heap.CLASS_ARRAY, method_dict_size, .variable);

    // Initialize all slots to nil
    const dict_fields = method_dict.fields(method_dict_size);
    for (0..method_dict_size) |i| {
        dict_fields[i] = Value.nil;
    }

    // Re-fetch class and metaclass (GC may have moved them during method_dict allocation)
    const class_val = heap.globals.get(name) orelse return error.OutOfMemory;
    const fresh_class = class_val.asObject();
    const fresh_metaclass = fresh_class.getField(Heap.CLASS_FIELD_METACLASS, Heap.CLASS_NUM_FIELDS).asObject();
    fresh_metaclass.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(method_dict), Heap.METACLASS_NUM_FIELDS);

    return fresh_class;
}

/// Create an FFI wrapper method that calls 'LibName' ffiCall: #funcName with: { args }
/// The method sends ffiCall:with: to the library name symbol (symbols work as receivers too)
fn createFFIMethod(heap: *Heap, lib_name: []const u8, func_name: []const u8, arg_count: usize) !?*object.CompiledMethod {
    // Build bytecodes for the method:
    // 1. Push library name symbol literal (receiver for ffiCall:with:)
    // 2. Push function name symbol literal (first arg)
    // 3. Build array from method arguments (second arg)
    // 4. Send ffiCall:with: message (2 args)
    // 5. Return result

    const num_literals: usize = 3; // lib_name symbol, func_name symbol, #ffiCall:with: selector
    const header_size = @sizeOf(object.CompiledMethod.MethodHeader);
    const literals_size = num_literals * @sizeOf(Value);

    var bytecode_buf: [64]u8 = undefined;
    var bc_len: usize = 0;

    // Push library name symbol (literal 0) - this is the receiver for ffiCall:with:
    bytecode_buf[bc_len] = 0x20; // push_literal_constant
    bc_len += 1;
    bytecode_buf[bc_len] = 0; // literal index 0
    bc_len += 1;

    // Push function name symbol (literal 1) - first argument to ffiCall:with:
    bytecode_buf[bc_len] = 0x20; // push_literal_constant
    bc_len += 1;
    bytecode_buf[bc_len] = 1; // literal index 1
    bc_len += 1;

    // Build array from method arguments
    if (arg_count == 0) {
        // Create empty array
        bytecode_buf[bc_len] = 0xC6; // make_array
        bc_len += 1;
        bytecode_buf[bc_len] = 0; // 0 elements
        bc_len += 1;
    } else {
        // Push all method arguments (temp vars 0..arg_count-1)
        // Use single-byte push_temporary_N opcodes (0x10-0x1F for temps 0-15)
        // or extended push_temporary (0x2A + index) for temps >= 16
        var i: usize = 0;
        while (i < arg_count) : (i += 1) {
            if (i < 16) {
                // Single-byte opcode: push_temporary_0 (0x10) through push_temporary_15 (0x1F)
                bytecode_buf[bc_len] = @intCast(0x10 + i);
                bc_len += 1;
            } else {
                // Extended: push_temporary (0x2A) + index byte
                bytecode_buf[bc_len] = 0x2A;
                bc_len += 1;
                bytecode_buf[bc_len] = @intCast(i);
                bc_len += 1;
            }
        }
        // Create array from stack elements
        bytecode_buf[bc_len] = 0xC6; // make_array
        bc_len += 1;
        bytecode_buf[bc_len] = @intCast(arg_count);
        bc_len += 1;
    }

    // Send ffiCall:with: (2 arguments) - selector is literal 2
    bytecode_buf[bc_len] = 0x80; // send
    bc_len += 1;
    bytecode_buf[bc_len] = 2; // literal index 2 (selector)
    bc_len += 1;
    bytecode_buf[bc_len] = 2; // num args
    bc_len += 1;

    // Return the result
    bytecode_buf[bc_len] = 0xA4; // return_top
    bc_len += 1;

    // GC-SAFE ALLOCATION STRATEGY:
    // The problem: Each heap allocation can trigger GC, which moves objects.
    // Local variables holding heap pointers become STALE after GC.
    //
    // Solution:
    // 1. Pre-intern all symbols (they go into symbol_table which IS traced by GC)
    // 2. Allocate the method OFF-HEAP (CompiledMethod has different layout than Object)
    // 3. Re-fetch FRESH pointers from symbol_table (GC updated them)
    // 4. Store fresh pointers - no more allocations after this point

    // Step 1: Pre-intern all symbols (may trigger GC, but symbols survive in symbol_table)
    _ = try heap.internSymbol(lib_name);
    _ = try heap.internSymbol(func_name);
    _ = try heap.internSymbol("ffiCall:with:");

    // Step 2: Allocate method OFF-HEAP (CompiledMethod doesn't use ObjectHeader)
    const total_size = header_size + literals_size + bc_len;
    const mem = try std.heap.page_allocator.alignedAlloc(u8, std.mem.Alignment.of(object.CompiledMethod), total_size);
    const method: *object.CompiledMethod = @ptrCast(mem.ptr);

    // Step 3: Re-fetch FRESH symbol pointers from symbol_table
    // These are guaranteed to be valid because symbol_table is traced by GC
    const lib_sym = heap.symbol_table.get(lib_name) orelse return null;
    const func_sym = heap.symbol_table.get(func_name) orelse return null;
    const sel_sym = heap.symbol_table.get("ffiCall:with:") orelse return null;

    // Step 4: Set up method - NO MORE HEAP ALLOCATIONS from here
    method.header = .{
        .num_args = @intCast(arg_count),
        .num_temps = @intCast(arg_count),
        .num_literals = @intCast(num_literals),
        .primitive_index = 0,
        .flags = .{},
        .bytecode_size = @intCast(bc_len),
    };

    // Store fresh literals
    const literals_ptr: [*]Value = @ptrFromInt(@intFromPtr(method) + header_size);
    literals_ptr[0] = lib_sym;
    literals_ptr[1] = func_sym;
    literals_ptr[2] = sel_sym;

    // Copy bytecodes
    const bc_ptr: [*]u8 = @ptrFromInt(@intFromPtr(method) + header_size + literals_size);
    @memcpy(bc_ptr[0..bc_len], bytecode_buf[0..bc_len]);

    return method;
}

/// Install a method with an already-created selector symbol
fn installMethodWithSelector(heap: *Heap, class: *Object, selector: Value, method: *object.CompiledMethod) !void {
    // Determine the number of fields - metaclasses have one extra field
    const num_fields = if (class.header.class_index == Heap.CLASS_METACLASS)
        Heap.METACLASS_NUM_FIELDS
    else
        Heap.CLASS_NUM_FIELDS;

    // Get or create method dictionary
    var dict_val = class.getField(Heap.CLASS_FIELD_METHOD_DICT, num_fields);
    var dict: *Object = undefined;

    if (dict_val.isNil()) {
        // Create new method dictionary - large enough for Raylib's ~550 functions
        // Each entry takes 2 slots (selector + method), so 1200 slots = 600 methods
        const initial_size: usize = 1200;
        dict = try heap.allocateObject(Heap.CLASS_ARRAY, initial_size, .variable);
        // Initialize all slots to nil
        const fields = dict.fields(initial_size);
        for (0..initial_size) |i| {
            fields[i] = Value.nil;
        }
        class.setField(Heap.CLASS_FIELD_METHOD_DICT, Value.fromObject(dict), num_fields);
    } else {
        dict = dict_val.asObject();
    }

    // Use hash-based insertion to match hash-based lookup in interpreter
    const method_val = Value.fromObject(@ptrCast(@alignCast(method)));
    if (!insertIntoMethodDict(dict, selector, method_val)) {
        std.debug.print("WARNING: Method dictionary full for selector\n", .{});
    }
}

test "bootstrap creates core classes" {
    const allocator = std.testing.allocator;
    const heap = try Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    try bootstrap(heap);

    // Verify Object class exists
    const object_class = heap.getClass(Heap.CLASS_OBJECT);
    try std.testing.expect(!object_class.isNil());

    // Verify SmallInteger class exists
    const small_int_class = heap.getClass(Heap.CLASS_SMALL_INTEGER);
    try std.testing.expect(!small_int_class.isNil());
}
