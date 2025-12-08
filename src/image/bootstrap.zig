const std = @import("std");
const memory = @import("../vm/memory.zig");
const object = @import("../vm/object.zig");
const bytecodes = @import("../vm/bytecodes.zig");

const Heap = memory.Heap;
const Value = object.Value;
const Object = object.Object;
const ClassFormat = object.ClassFormat;
const Primitive = bytecodes.Primitive;

/// Bootstrap the core Smalltalk classes
/// This creates the minimal class hierarchy needed to start the system
pub fn bootstrap(heap: *Heap) !void {
    // The "tying the knot" problem: Class is an instance of Metaclass,
    // but Metaclass is a subclass of Class. We solve this by:
    // 1. Creating stub class objects first
    // 2. Fixing up the class pointers afterwards

    std.debug.print("Bootstrapping Zig Smalltalk...\n", .{});

    // Phase 1: Create all class objects with placeholder class references

    // Pre-allocate space in class table
    var i: usize = 0;
    while (i < 25) : (i += 1) {
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

    // SmallInteger -> Object (in a full system: Integer -> Number -> Magnitude -> Object)
    setClassSuperclass(small_int_class, Value.fromObject(object_class));

    // String -> Object (in a full system: ArrayedCollection -> ...)
    setClassSuperclass(string_class, Value.fromObject(object_class));

    // Symbol -> String
    setClassSuperclass(symbol_class, Value.fromObject(string_class));

    // Array -> Object
    setClassSuperclass(array_class, Value.fromObject(object_class));

    // ByteArray -> Object
    setClassSuperclass(byte_array_class, Value.fromObject(object_class));

    // CompiledMethod -> Object
    setClassSuperclass(compiled_method_class, Value.fromObject(object_class));

    // BlockClosure -> Object
    setClassSuperclass(block_closure_class, Value.fromObject(object_class));

    // UndefinedObject -> Object
    setClassSuperclass(undefined_object_class, Value.fromObject(object_class));

    // True -> Object (in a full system: Boolean -> Object)
    setClassSuperclass(true_class, Value.fromObject(object_class));

    // False -> Object
    setClassSuperclass(false_class, Value.fromObject(object_class));

    // Character -> Object (in a full system: Magnitude -> Object)
    setClassSuperclass(character_class, Value.fromObject(object_class));

    // Interval -> Object (in a full system: SequenceableCollection -> ...)
    setClassSuperclass(interval_class, Value.fromObject(object_class));

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

    // Phase 6: Install core methods
    try installCoreMethods(heap);

    std.debug.print("Bootstrap complete. {} classes created.\n", .{heap.class_table.items.len});
}

fn createClassObject(heap: *Heap, class_index: u32) !*Object {
    // A class has 5 fields: superclass, methodDict, format, instanceVariables, name
    return try heap.allocateObject(class_index, Heap.CLASS_NUM_FIELDS, .normal);
}

fn setClassSuperclass(class: *Object, superclass: Value) void {
    class.setField(Heap.CLASS_FIELD_SUPERCLASS, superclass, Heap.CLASS_NUM_FIELDS);
}

fn setClassName(heap: *Heap, class: *Object, name: []const u8) !void {
    const sym = try heap.internSymbol(name);
    class.setField(Heap.CLASS_FIELD_NAME, sym, Heap.CLASS_NUM_FIELDS);
}

fn setClassFormat(class: *Object, num_inst_vars: i61, format: ClassFormat) void {
    // Encode format: low byte is ClassFormat, rest is number of instance variables
    const format_val = (@as(i61, @intFromEnum(format)) << 8) | num_inst_vars;
    class.setField(Heap.CLASS_FIELD_FORMAT, Value.fromSmallInt(format_val), Heap.CLASS_NUM_FIELDS);
}

/// Create a method dictionary (simple array of [selector, method] pairs)
fn createMethodDict(heap: *Heap, capacity: usize) !*Object {
    // Allocate an array with capacity * 2 slots (selector, method pairs)
    return try heap.allocateObject(Heap.CLASS_ARRAY, capacity * 2, .variable);
}

/// Install a method in a class's method dictionary
pub fn installMethod(heap: *Heap, class: *Object, selector: []const u8, method: *object.CompiledMethod) !void {
    // Get or create method dictionary
    var method_dict = class.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);

    if (method_dict.isNil()) {
        // Create a new method dictionary
        const dict = try createMethodDict(heap, 32);
        method_dict = Value.fromObject(dict);
        class.setField(Heap.CLASS_FIELD_METHOD_DICT, method_dict, Heap.CLASS_NUM_FIELDS);
    }

    const dict_obj = method_dict.asObject();
    const selector_sym = try heap.internSymbol(selector);

    // Find first empty slot
    const max_entries: usize = 64;
    const fields = dict_obj.fields(max_entries);

    var i: usize = 0;
    while (i < max_entries) : (i += 2) {
        if (fields[i].isNil()) {
            // Found empty slot
            fields[i] = selector_sym;
            fields[i + 1] = Value.fromObject(@ptrCast(@alignCast(method)));
            return;
        }
        // Check if selector already exists (update)
        if (fields[i].eql(selector_sym)) {
            fields[i + 1] = Value.fromObject(@ptrCast(@alignCast(method)));
            return;
        }
    }

    // Dictionary full - shouldn't happen with reasonable capacity
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

    const mem = try heap.allocator.alignedAlloc(u8, .@"8", total_size);

    const method: *object.CompiledMethod = @ptrCast(@alignCast(mem.ptr));

    method.header = .{
        .num_args = num_args,
        .num_temps = num_args,
        .num_literals = 0,
        .primitive_index = primitive_index,
        .flags = .{},
        .bytecode_size = @intCast(bytecode_size),
    };

    // Bytecode: return top of stack
    const bytecodes_ptr: [*]u8 = @ptrCast(mem.ptr + header_size + literals_size);
    bytecodes_ptr[0] = 0xA4; // return_top

    return method;
}

/// Install core methods in bootstrap classes
/// Uses Dolphin-compatible primitive numbers for SmallInteger, Object, etc.
pub fn installCoreMethods(heap: *Heap) !void {
    // Get class objects
    const small_int_class = heap.getClass(Heap.CLASS_SMALL_INTEGER).asObject();
    const string_class = heap.getClass(Heap.CLASS_STRING).asObject();
    const object_class = heap.getClass(Heap.CLASS_OBJECT).asObject();
    const true_class = heap.getClass(Heap.CLASS_TRUE).asObject();
    const false_class = heap.getClass(Heap.CLASS_FALSE).asObject();

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

    // String methods - Dolphin compatible
    try installMethod(heap, string_class, "size", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, string_class, "basicSize", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.size))); // 62
    try installMethod(heap, string_class, "at:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, string_class, "basicAt:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.at))); // 60
    try installMethod(heap, string_class, "at:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61
    try installMethod(heap, string_class, "basicAt:put:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.at_put))); // 61

    // Array methods - Dolphin compatible
    const array_class = heap.getClass(Heap.CLASS_ARRAY).asObject();
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

    // True methods (our extensions)
    try installMethod(heap, true_class, "ifTrue:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_if_true)));
    try installMethod(heap, true_class, "ifFalse:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.true_if_false)));
    try installMethod(heap, true_class, "ifTrue:ifFalse:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.true_if_true_if_false)));
    try installMethod(heap, true_class, "ifFalse:ifTrue:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.true_if_false_if_true)));
    try installMethod(heap, true_class, "not", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.boolean_not)));

    // False methods (our extensions)
    try installMethod(heap, false_class, "ifTrue:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_if_true)));
    try installMethod(heap, false_class, "ifFalse:", try createPrimitiveMethod(heap, 1, @intFromEnum(Primitive.false_if_false)));
    try installMethod(heap, false_class, "ifTrue:ifFalse:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.false_if_true_if_false)));
    try installMethod(heap, false_class, "ifFalse:ifTrue:", try createPrimitiveMethod(heap, 2, @intFromEnum(Primitive.false_if_false_if_true)));
    try installMethod(heap, false_class, "not", try createPrimitiveMethod(heap, 0, @intFromEnum(Primitive.boolean_not)));
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
