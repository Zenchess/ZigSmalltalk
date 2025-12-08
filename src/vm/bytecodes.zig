/// Bytecode definitions for Zig Smalltalk VM
/// Simple stack-based bytecode, similar to Squeak/Pharo but simplified

pub const Opcode = enum(u8) {
    // PUSH OPERATIONS (0x00-0x0F): Push receiver instance variable
    push_receiver_variable_0 = 0x00,
    push_receiver_variable_1 = 0x01,
    push_receiver_variable_2 = 0x02,
    push_receiver_variable_3 = 0x03,
    push_receiver_variable_4 = 0x04,
    push_receiver_variable_5 = 0x05,
    push_receiver_variable_6 = 0x06,
    push_receiver_variable_7 = 0x07,
    push_receiver_variable_8 = 0x08,
    push_receiver_variable_9 = 0x09,
    push_receiver_variable_10 = 0x0A,
    push_receiver_variable_11 = 0x0B,
    push_receiver_variable_12 = 0x0C,
    push_receiver_variable_13 = 0x0D,
    push_receiver_variable_14 = 0x0E,
    push_receiver_variable_15 = 0x0F,

    // PUSH TEMPORARY (0x10-0x1F): Push temporary/argument
    push_temporary_0 = 0x10,
    push_temporary_1 = 0x11,
    push_temporary_2 = 0x12,
    push_temporary_3 = 0x13,
    push_temporary_4 = 0x14,
    push_temporary_5 = 0x15,
    push_temporary_6 = 0x16,
    push_temporary_7 = 0x17,
    push_temporary_8 = 0x18,
    push_temporary_9 = 0x19,
    push_temporary_10 = 0x1A,
    push_temporary_11 = 0x1B,
    push_temporary_12 = 0x1C,
    push_temporary_13 = 0x1D,
    push_temporary_14 = 0x1E,
    push_temporary_15 = 0x1F,

    // PUSH EXTENDED (0x20-0x3F)
    push_literal = 0x20, // Next byte is literal index
    push_literal_variable = 0x21, // Push global/class var (next byte is literal index)
    push_receiver = 0x22, // Push self
    push_nil = 0x23,
    push_true = 0x24,
    push_false = 0x25,
    push_integer = 0x26, // Next byte is signed 8-bit integer
    push_integer_16 = 0x27, // Next two bytes are signed 16-bit integer
    push_context = 0x28, // Push thisContext
    push_receiver_variable = 0x29, // Next byte is inst var index (extended)
    push_temporary = 0x2A, // Next byte is temp index (extended)

    // STORE OPERATIONS (0x40-0x4F): Store into receiver inst var
    store_receiver_variable_0 = 0x40,
    store_receiver_variable_1 = 0x41,
    store_receiver_variable_2 = 0x42,
    store_receiver_variable_3 = 0x43,
    store_receiver_variable_4 = 0x44,
    store_receiver_variable_5 = 0x45,
    store_receiver_variable_6 = 0x46,
    store_receiver_variable_7 = 0x47,
    store_receiver_variable_8 = 0x48,
    store_receiver_variable_9 = 0x49,
    store_receiver_variable_10 = 0x4A,
    store_receiver_variable_11 = 0x4B,
    store_receiver_variable_12 = 0x4C,
    store_receiver_variable_13 = 0x4D,
    store_receiver_variable_14 = 0x4E,
    store_receiver_variable_15 = 0x4F,

    // STORE TEMPORARY (0x50-0x5F)
    store_temporary_0 = 0x50,
    store_temporary_1 = 0x51,
    store_temporary_2 = 0x52,
    store_temporary_3 = 0x53,
    store_temporary_4 = 0x54,
    store_temporary_5 = 0x55,
    store_temporary_6 = 0x56,
    store_temporary_7 = 0x57,
    store_temporary_8 = 0x58,
    store_temporary_9 = 0x59,
    store_temporary_10 = 0x5A,
    store_temporary_11 = 0x5B,
    store_temporary_12 = 0x5C,
    store_temporary_13 = 0x5D,
    store_temporary_14 = 0x5E,
    store_temporary_15 = 0x5F,

    // POP AND STORE (0x60-0x7F)
    pop_store_receiver_variable_0 = 0x60,
    pop_store_receiver_variable_1 = 0x61,
    pop_store_receiver_variable_2 = 0x62,
    pop_store_receiver_variable_3 = 0x63,
    pop_store_receiver_variable_4 = 0x64,
    pop_store_receiver_variable_5 = 0x65,
    pop_store_receiver_variable_6 = 0x66,
    pop_store_receiver_variable_7 = 0x67,
    pop_store_temporary_0 = 0x70,
    pop_store_temporary_1 = 0x71,
    pop_store_temporary_2 = 0x72,
    pop_store_temporary_3 = 0x73,
    pop_store_temporary_4 = 0x74,
    pop_store_temporary_5 = 0x75,
    pop_store_temporary_6 = 0x76,
    pop_store_temporary_7 = 0x77,

    // SENDS (0x80-0x9F)
    send = 0x80, // Next bytes: literal index, num args
    super_send = 0x81, // Send to super
    send_plus = 0x82, // Optimized +
    send_minus = 0x83, // Optimized -
    send_times = 0x84, // Optimized *
    send_divide = 0x85, // Optimized /
    send_mod = 0x86, // Optimized \\
    send_less_than = 0x87, // Optimized <
    send_greater_than = 0x88, // Optimized >
    send_less_or_equal = 0x89, // Optimized <=
    send_greater_or_equal = 0x8A, // Optimized >=
    send_equal = 0x8B, // Optimized =
    send_not_equal = 0x8C, // Optimized ~=
    send_at = 0x8D, // Optimized at:
    send_at_put = 0x8E, // Optimized at:put:
    send_size = 0x8F, // Optimized size
    send_class = 0x90, // Optimized class
    send_identical = 0x91, // Optimized ==
    send_not_identical = 0x92, // Optimized ~~
    send_value = 0x93, // Optimized value (for blocks)
    send_value_1 = 0x94, // Optimized value:
    send_new = 0x95, // Optimized new
    send_new_size = 0x96, // Optimized new:

    // RETURNS (0xA0-0xAF)
    return_receiver = 0xA0, // Return self
    return_true = 0xA1,
    return_false = 0xA2,
    return_nil = 0xA3,
    return_top = 0xA4, // Return top of stack
    block_return = 0xA5, // Return from enclosing method (non-local return)

    // JUMPS (0xB0-0xBF)
    jump = 0xB0, // Next 2 bytes: signed 16-bit offset
    jump_if_true = 0xB1,
    jump_if_false = 0xB2,
    jump_if_nil = 0xB3,
    jump_if_not_nil = 0xB4,
    // Short jumps (offset encoded in opcode)
    short_jump_0 = 0xB8,
    short_jump_1 = 0xB9,
    short_jump_2 = 0xBA,
    short_jump_3 = 0xBB,
    short_jump_4 = 0xBC,
    short_jump_5 = 0xBD,
    short_jump_6 = 0xBE,
    short_jump_7 = 0xBF,

    // MISC (0xC0-0xCF)
    pop = 0xC0, // Pop top of stack
    dup = 0xC1, // Duplicate top of stack
    push_outer_temp = 0xC2, // Push from outer context (closure), next 2 bytes: level, index
    store_outer_temp = 0xC3, // Store to outer context
    push_closure = 0xC4, // Create block closure, next bytes: num copied, num args, bytecode size
    primitive = 0xC5, // Execute primitive, next 2 bytes: primitive index
    make_array = 0xC6, // Create array from top N stack values, next byte: count

    // Extended operations (0xD0-0xFF)
    extended_push = 0xD0, // Extended push with type byte
    extended_store = 0xD1, // Extended store with type byte
    extended_send = 0xD2, // Extended send with more args
    nop = 0xFF, // No operation

    // Helper function to check if opcode is a short push receiver variable
    pub fn isPushReceiverVariable(byte: u8) bool {
        return byte <= 0x0F;
    }

    pub fn isPushTemporary(byte: u8) bool {
        return byte >= 0x10 and byte <= 0x1F;
    }

    pub fn isStoreReceiverVariable(byte: u8) bool {
        return byte >= 0x40 and byte <= 0x4F;
    }

    pub fn isStoreTemporary(byte: u8) bool {
        return byte >= 0x50 and byte <= 0x5F;
    }

    pub fn isPopStoreReceiverVariable(byte: u8) bool {
        return byte >= 0x60 and byte <= 0x6F;
    }

    pub fn isPopStoreTemporary(byte: u8) bool {
        return byte >= 0x70 and byte <= 0x7F;
    }

    pub fn isShortJump(byte: u8) bool {
        return byte >= 0xB8 and byte <= 0xBF;
    }

    /// Get the embedded index from a short-form opcode
    pub fn getEmbeddedIndex(byte: u8) u8 {
        return byte & 0x0F;
    }

    /// Get the short jump offset (0-7)
    pub fn getShortJumpOffset(byte: u8) u8 {
        return byte - 0xB8;
    }
};

/// Special selectors table - for optimized sends
pub const SpecialSelectors = struct {
    pub const plus = "+";
    pub const minus = "-";
    pub const times = "*";
    pub const divide = "/";
    pub const mod = "\\\\";
    pub const less_than = "<";
    pub const greater_than = ">";
    pub const less_or_equal = "<=";
    pub const greater_or_equal = ">=";
    pub const equal = "=";
    pub const not_equal = "~=";
    pub const at = "at:";
    pub const at_put = "at:put:";
    pub const size = "size";
    pub const class_selector = "class";
    pub const identical = "==";
    pub const not_identical = "~~";
    pub const value = "value";
    pub const value_1 = "value:";
    pub const new = "new";
    pub const new_size = "new:";
};

/// Primitive indices - Dolphin Smalltalk compatible numbering
/// See Dolphin/Core/Object Arts/Dolphin/Base/*.cls for reference
pub const Primitive = enum(u16) {
    // ========================================================================
    // SmallInteger arithmetic (Dolphin compatible: 9-20, 40-43)
    // ========================================================================
    multiply = 9, // SmallInteger >> *
    divide = 10, // SmallInteger >> /  (exact division)
    mod = 11, // SmallInteger >> \\  (modulo)
    integer_divide = 12, // SmallInteger >> //  (integer division)
    quo = 13, // SmallInteger >> quo:  (quotient toward zero)
    subtract = 14, // SmallInteger >> -
    add = 15, // SmallInteger >> +
    equal = 16, // SmallInteger >> =
    greater_or_equal = 17, // SmallInteger >> >=
    less_than = 18, // SmallInteger >> <
    greater_than = 19, // SmallInteger >> >
    less_or_equal = 20, // SmallInteger >> <=

    // LargeInteger arithmetic (Dolphin: 21-39)
    large_add = 21,
    large_multiply = 22,
    large_less_than = 23,
    large_equal = 24,
    large_greater_than = 25,
    large_greater_or_equal = 26,
    large_less_or_equal = 27,
    large_negate = 28,
    large_subtract = 29,
    large_divide = 30,
    large_mod = 31,
    large_quo = 32,
    large_normalize = 33,
    large_bit_and = 34,
    large_bit_or = 35,
    large_bit_xor = 36,
    large_bit_invert = 37,
    large_bit_shift = 38,
    large_high_bit = 39,

    // Bit operations (Dolphin: 40-43)
    bit_and = 40, // SmallInteger >> bitAnd:
    bit_or = 41, // SmallInteger >> bitOr:
    bit_xor = 42, // SmallInteger >> bitXor:
    bit_shift = 43, // SmallInteger >> bitShift:

    // Float (Dolphin: 45-47, 160-165, 205, 214)
    float_greater_than = 45, // Float >> >
    float_greater_or_equal = 46, // Float >> >=
    float_equal = 47, // Float >> =
    high_bit = 54, // Integer >> highBit

    // ========================================================================
    // Object/Array primitives (Dolphin: 60-77)
    // ========================================================================
    at = 60, // Object >> at:, basicAt:
    at_put = 61, // Object >> at:put:, basicAt:put:
    size = 62, // Object >> basicSize
    string_at = 63, // String >> basicAt: (returns Character)
    string_at_put = 64, // String >> basicAt:put:
    string_next_index_of = 65, // String search
    string_next_put_all = 66, // String >> nextPutAll:

    basic_new = 70, // Behavior >> basicNew
    basic_new_size = 71, // Behavior >> basicNew:

    inst_var_at = 73, // Object >> instVarAt:
    inst_var_at_put = 74, // Object >> instVarAt:put:
    basic_identity_hash = 75, // Object >> basicIdentityHash
    basic_new_pinned = 76, // Behavior >> basicNewPinned
    basic_new_pinned_size = 77, // Behavior >> basicNewPinned:

    // ========================================================================
    // Block/Control flow (Dolphin: 79-100)
    // ========================================================================
    perform_with_args_at = 79, // perform:withArgumentsAt:descriptor:
    block_value = 81, // BlockClosure >> value (all variants)
    block_value_with_args = 82, // BlockClosure >> valueWithArguments:
    perform = 83, // Object >> perform:
    perform_with_args = 84, // Object >> perform:withArguments:
    become = 85, // Object >> become:
    snapshot = 86, // System snapshot
    perform_method = 87, // perform method
    one_way_become = 88, // Object >> oneWayBecome:
    compile = 89, // Compiler >> compile:

    // Extended block value primitives (our extension, to handle different arg counts)
    block_value_1 = 490, // BlockClosure >> value: (1 arg)
    block_value_2 = 491, // BlockClosure >> value:value: (2 args)
    block_value_3 = 492, // BlockClosure >> value:value:value: (3 args)
    block_value_4 = 493, // BlockClosure >> value:value:value:value: (4 args)

    // Boolean control flow (our extension, use high numbers to avoid conflicts)
    true_if_true = 500, // True >> ifTrue:
    true_if_false = 501, // True >> ifFalse:
    true_if_true_if_false = 502, // True >> ifTrue:ifFalse:
    true_if_false_if_true = 503, // True >> ifFalse:ifTrue:
    false_if_true = 504, // False >> ifTrue:
    false_if_false = 505, // False >> ifFalse:
    false_if_true_if_false = 506, // False >> ifTrue:ifFalse:
    false_if_false_if_true = 507, // False >> ifFalse:ifTrue:
    boolean_not = 508, // Boolean >> not
    while_true = 509, // [cond] whileTrue: [body]
    while_false = 510, // [cond] whileFalse: [body]

    // ========================================================================
    // Object system (Dolphin: 100-119)
    // ========================================================================
    basic_resize = 101, // Object >> basicResize:
    replace_bytes = 102, // replaceFrom:to:with:startingAt: for bytes
    replace_elements = 105, // replaceFrom:to:with:startingAt: for elements
    string_compare = 106, // String comparison
    hash = 109, // SmallInteger >> hash, identityHash
    identical = 110, // Object >> ==, =
    class = 111, // Object >> class, basicClass
    core_left = 112, // Memory >> coreLeft
    garbage_collect = 114, // Memory >> garbageCollect
    compact = 115, // Memory >> compact
    external_call = 116, // External library call
    callback_return = 117, // Callback return

    // ========================================================================
    // More object operations (Dolphin: 145-159)
    // ========================================================================
    any_mask = 145, // Integer >> anyMask:
    all_mask = 146, // Integer >> allMask:
    identity_hash = 147, // Object >> identityHash
    behavior_is_bits = 148, // Behavior >> isBits
    string_copy_to_heap = 149, // String >> copyToHeap

    low_bit = 152, // Integer >> lowBit
    all_references = 153, // Object >> allReferences
    shallow_copy = 155, // Object >> basicShallowCopy
    species = 156, // Object >> species
    object_with_at_put = 157, // with:at:put:
    byte_at = 158, // Integer >> byteAt:

    // Float operations (Dolphin: 160-168, 205, 214)
    float_add = 160, // Float >> +
    float_subtract = 161, // Float >> -
    float_less_than = 162, // Float >> <
    float_multiply = 164, // Float >> *
    float_divide = 165, // Float >> /
    float_truncated = 166, // Float >> truncated
    large_as_float = 167, // LargeInteger >> asFloat
    small_as_float = 168, // SmallInteger >> asFloat
    float_abs = 205, // Float >> abs
    float_less_or_equal = 214, // Float >> <=

    // ========================================================================
    // I/O and System (Dolphin compatible where possible)
    // ========================================================================
    quit = 113, // System >> quit (Dolphin uses 113 for exit)
    delay_milliseconds = 174, // Delay >> milliseconds
    time_milliseconds = 189, // Time >> millisecondClockValue

    // Character/String (our extensions, high numbers)
    char_code = 520, // Character >> asciiValue
    char_from_code = 521, // Character class >> value:
    string_concat = 522, // String >> ,
    as_string = 523, // Object >> asString
    as_symbol = 524, // String >> asSymbol
    print_string = 525, // Primitive print
    print_char = 526,
    read_line = 527,

    // Collection operations (our extensions, high numbers)
    array_do = 530,
    array_collect = 531,
    array_select = 532,
    array_reject = 533,
    array_detect = 534,
    array_detect_if_none = 535,
    array_inject_into = 536,
    to_do = 537, // start to: stop do: [:i | ...]
    to_by_do = 538, // start to: stop by: step do: [:i | ...]
    times_repeat = 539, // Integer >> timesRepeat:

    // Additional Integer operations (our extensions)
    negate = 540, // Integer >> negated
    abs = 541, // Integer >> abs
    even = 542, // Integer >> even
    odd = 543, // Integer >> odd
    positive = 544, // Number >> positive
    negative = 545, // Number >> negative
    sign = 546, // Number >> sign
    max = 547, // Magnitude >> max:
    min = 548, // Magnitude >> min:
    between_and = 549, // Magnitude >> between:and:
    not_equal = 550, // Object >> ~=

    // File I/O (use high numbers to avoid conflicts)
    file_open = 600,
    file_close = 601,
    file_read = 602,
    file_write = 603,
    file_position = 604,
    file_set_position = 605,
    file_size_prim = 606,

    // FFI (Dolphin uses 200+ for VM-internal, we use 700+)
    ffi_call = 700,
    ffi_callback = 701,
    ffi_malloc = 702,
    ffi_free = 703,
    ffi_read = 704,
    ffi_write = 705,

    _,
};
