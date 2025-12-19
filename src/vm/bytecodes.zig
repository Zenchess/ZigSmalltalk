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
    thread = 0x3D, // Thread execution (for blocks/continuations)

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

    // SmallInteger printing (Dolphin: 44)
    small_int_print_string = 44, // SmallInteger >> printString

    // Array operations (Dolphin: 50-51)
    replace_from_to_with = 50, // replaceFrom:to:with:startingAt:
    string_cmp_ordinal = 51, // String comparison ordinal

    // String search (Dolphin: 52)
    string_next_index_of_dolphin = 52, // String >> nextIndexOf:from:to:
    high_bit = 54, // Integer >> highBit

    // Type checking (Dolphin: 57-58)
    is_kind_of = 57, // Object >> isKindOf:
    inherits_from = 58, // Behavior >> inheritsFrom:

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
    become_dolphin = 72, // Object >> become: (Dolphin primitive number)

    inst_var_at = 73, // Object >> instVarAt:
    inst_var_at_put = 74, // Object >> instVarAt:put:
    basic_identity_hash = 75, // Object >> basicIdentityHash
    basic_new_pinned = 76, // Behavior >> basicNewPinned
    basic_new_pinned_size = 77, // Behavior >> basicNewPinned:

    // ========================================================================
    // Block/Control flow (Dolphin: 79-84)
    // ========================================================================
    perform_with_args_at = 79, // perform:withArgumentsAt:descriptor:
    block_value = 81, // BlockClosure >> value (all variants)
    block_value_with_args = 82, // BlockClosure >> valueWithArguments:
    perform = 83, // Object >> perform:
    perform_with_args = 84, // Object >> perform:withArguments:

    // ========================================================================
    // Process/Semaphore primitives (Dolphin: 85-100)
    // ========================================================================
    semaphore_signal = 85, // Semaphore >> signal
    semaphore_wait = 86, // Semaphore >> wait:ret:
    process_resume = 87, // Process >> resume
    process_suspend = 88, // Process >> suspend
    compile = 89, // Compiler >> compile:
    process_terminate = 91, // Process >> primTerminate
    process_set_priority = 92, // Process >> priority:
    enable_async_events = 95, // Processor >> enableAsyncEvents:
    process_queue_interrupt = 98, // Process >> queueInterrupt:with:
    semaphore_set_signals = 99, // Semaphore >> primSetSignals:
    signal_timer_after = 100, // Delay class >> signalTimerAfter:

    // Object swapping (moved to avoid conflict with process primitives)
    become = 180, // Object >> become:
    one_way_become = 181, // Object >> oneWayBecome:

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
    true_and = 511, // True >> and: aBlock
    true_or = 512, // True >> or: aBlock
    false_and = 513, // False >> and: aBlock
    false_or = 514, // False >> or: aBlock

    // Error handling
    does_not_understand = 800, // Object >> doesNotUnderstand:
    error_message = 801, // Object >> error:
    halt = 802, // Object >> halt

    // Exception handling (810+)
    exception_signal = 810, // Exception >> signal
    exception_signal_with = 811, // Exception >> signal:
    on_do = 812, // Block >> on:do:
    ensure = 813, // Block >> ensure:
    if_curtailed = 814, // Block >> ifCurtailed:

    // Dictionary primitives (820+)
    dict_at = 820, // Dictionary >> at:
    dict_at_put = 821, // Dictionary >> at:put:
    dict_at_if_absent = 822, // Dictionary >> at:ifAbsent:
    dict_includes_key = 823, // Dictionary >> includesKey:
    dict_remove_key = 824, // Dictionary >> removeKey:
    dict_keys = 825, // Dictionary >> keys
    dict_values = 826, // Dictionary >> values
    dict_size = 827, // Dictionary >> size

    // Set primitives (830+)
    set_add = 830, // Set >> add:
    set_includes = 831, // Set >> includes:
    set_remove = 832, // Set >> remove:
    set_size = 833, // Set >> size

    // OrderedCollection primitives (840+)
    oc_add = 840, // OrderedCollection >> add:
    oc_add_first = 841, // OrderedCollection >> addFirst:
    oc_add_last = 842, // OrderedCollection >> addLast:
    oc_remove_first = 843, // OrderedCollection >> removeFirst
    oc_remove_last = 844, // OrderedCollection >> removeLast
    oc_at = 845, // OrderedCollection >> at:
    oc_at_put = 846, // OrderedCollection >> at:put:
    oc_size = 847, // OrderedCollection >> size

    // Stream primitives (850+)
    stream_next = 850, // Stream >> next
    stream_next_put = 851, // Stream >> nextPut:
    stream_next_put_all = 852, // Stream >> nextPutAll:
    stream_peek = 853, // Stream >> peek
    stream_at_end = 854, // Stream >> atEnd
    stream_position = 855, // Stream >> position
    stream_set_position = 856, // Stream >> position:
    stream_reset = 857, // Stream >> reset
    stream_contents = 858, // Stream >> contents

    // String additional operations (860+)
    string_index_of = 860, // String >> indexOf:
    string_index_of_starting_at = 861, // String >> indexOf:startingAt:
    string_as_upper = 862, // String >> asUppercase
    string_as_lower = 863, // String >> asLowercase
    string_trim = 864, // String >> trim
    string_includes = 865, // String >> includes:
    string_split = 866, // String >> subStrings:
    as_number = 867, // String >> asNumber

    // ========================================================================
    // Dynamic library loading (870-875)
    // ========================================================================
    dll_load = 870, // ExternalLibrary >> load: 'library.dll' - returns handle
    dll_get_proc = 871, // ExternalLibrary >> getProcAddress: 'funcName' from: handle
    dll_free = 872, // ExternalLibrary >> free: handle
    dll_call_ptr = 873, // Call function pointer with signature and args

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
    lookup_method = 148, // Behavior >> lookupMethod:
    string_copy_to_heap = 149, // String >> copyToHeap

    low_bit = 152, // Integer >> lowBit
    all_references = 153, // Object >> allReferences
    shallow_copy = 155, // Object >> basicShallowCopy
    yield = 156, // Processor >> yield (Dolphin process primitive)
    object_with_at_put = 157, // with:at:put:
    byte_at = 158, // Integer >> byteAt:
    species = 182, // Object >> species (moved to avoid conflict)

    // Float operations (Dolphin: 160-168, 205, 214)
    float_add = 160, // Float >> +
    float_subtract = 161, // Float >> -
    float_less_than = 162, // Float >> <
    float_multiply = 164, // Float >> *
    float_divide = 165, // Float >> /
    float_truncated = 166, // Float >> truncated
    large_as_float = 167, // LargeInteger >> asFloat
    small_as_float = 168, // SmallInteger >> asFloat
    float_print_string = 169, // Float >> printString
    float_abs = 205, // Float >> abs
    float_less_or_equal = 214, // Float >> <=
    float_negate = 215, // Float >> negated (our extension)

    // String primitives (Dolphin: 216-222)
    as_utf8_string = 216, // String >> asUtf8String
    as_ansi_string = 217, // String >> asAnsiString
    string_append = 218, // String >> ,
    dolphin_string_equal = 219, // String >> = (Dolphin primitive)
    string_cmp_ordinal_ignoring_case = 220, // String comparison ignoring case
    file_read_buffer = 221, // FileStream >> read:
    file_write_buffer = 222, // FileStream >> write:

    // ========================================================================
    // I/O and System (Dolphin compatible where possible)
    // ========================================================================
    quit = 113, // System >> quit (Dolphin uses 113 for exit)
    dolphin_stream_next_put_all = 173, // Stream >> nextPutAll: (Dolphin primitive)
    millisecond_clock_value = 174, // Delay class >> millisecondClockValue
    microsecond_clock_value = 189, // Delay class >> microsecondClockValue

    // Character/String (our extensions, high numbers)
    char_code = 520, // Character >> asciiValue
    char_from_code = 521, // Character class >> value:
    string_concat = 522, // String >> ,
    string_less_than = 560, // String >> <
    string_greater_than = 561, // String >> >
    string_less_or_equal = 562, // String >> <=
    string_greater_or_equal = 563, // String >> >=
    string_equal = 564, // String >> =
    string_copy_from_to = 565, // String >> copyFrom:to:
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
    factorial = 551, // Integer >> factorial (optimized)

    // Interval creation (our extensions)
    to_interval = 552, // SmallInteger >> to: (creates Interval)
    to_by_interval = 553, // SmallInteger >> to:by: (creates Interval with step)
    interval_size = 554, // Interval >> size
    interval_at = 555, // Interval >> at:
    interval_do = 556, // Interval >> do:

    // Nil checking (our extensions)
    is_nil = 557, // UndefinedObject >> isNil (returns true)
    not_nil = 558, // Object >> notNil
    is_nil_false = 559, // Object >> isNil (returns false for non-nil)

    // File I/O (use high numbers to avoid conflicts)
    file_open = 600,
    file_close = 601,
    file_read = 602,
    file_write = 603,
    file_position = 604,
    file_set_position = 605,
    file_size_prim = 606,
    file_flush = 607,
    file_at_end = 608,
    file_delete = 609,
    file_exists = 610,
    file_rename = 611,
    stdout_write = 612,
    stdin_read = 613,
    stderr_write = 614,
    transcript_show = 650,
    transcript_cr = 651,
    transcript_next_put_all = 652,
    transcript_flush = 653,

    // FFI (Dolphin uses 200+ for VM-internal, we use 700+)
    ffi_call = 700, // Generic FFI call with sub-primitive
    ffi_callback = 701,
    ffi_malloc = 702,
    ffi_free = 703,
    ffi_read = 704,
    ffi_write = 705,

    // LibC string functions (710-719)
    ffi_strlen = 710,
    ffi_puts = 711,

    // LibC math functions (720-749)
    ffi_sin = 720,
    ffi_cos = 721,
    ffi_sqrt = 722,
    ffi_pow = 723,
    ffi_exp = 724,
    ffi_log = 725,
    ffi_floor = 726,
    ffi_ceil = 727,
    ffi_fabs = 728,
    ffi_atan2 = 729,
    ffi_tan = 730,
    ffi_asin = 731,
    ffi_acos = 732,
    ffi_atan = 733,

    // Memory operations (750-759)
    ffi_memset = 750,
    ffi_memcpy = 751,
    ffi_read_int8 = 752,
    ffi_read_int16 = 753,
    ffi_read_int32 = 754,
    ffi_read_int64 = 755,
    ffi_read_float64 = 756,
    ffi_write_int8 = 757,
    ffi_write_int32 = 758,
    ffi_write_float64 = 759,

    // FFI introspection
    ffi_libraries = 760, // FFI class >> libraries - returns array of library names
    ffi_functions = 761, // 'LibMath' ffiFunctions - returns array of function names

    // ByteArray/ExternalStructure field access (770-789)
    // Read operations (receiver is ByteArray, arg is offset)
    bytes_uint8_at = 770, // ByteArray >> uint8At:
    bytes_uint16_at = 771, // ByteArray >> uint16At:
    bytes_uint32_at = 772, // ByteArray >> uint32At:
    bytes_int8_at = 773, // ByteArray >> int8At:
    bytes_int16_at = 774, // ByteArray >> int16At:
    bytes_int32_at = 775, // ByteArray >> int32At:
    bytes_float32_at = 776, // ByteArray >> float32At:
    bytes_float64_at = 777, // ByteArray >> float64At:
    // Write operations (receiver is ByteArray, args are offset and value)
    bytes_uint8_at_put = 780, // ByteArray >> uint8At:put:
    bytes_uint16_at_put = 781, // ByteArray >> uint16At:put:
    bytes_uint32_at_put = 782, // ByteArray >> uint32At:put:
    bytes_int8_at_put = 783, // ByteArray >> int8At:put:
    bytes_int16_at_put = 784, // ByteArray >> int16At:put:
    bytes_int32_at_put = 785, // ByteArray >> int32At:put:
    bytes_float32_at_put = 786, // ByteArray >> float32At:put:
    bytes_float64_at_put = 787, // ByteArray >> float64At:put:
    bytes_address = 788, // ByteArray >> address - returns pointer to data

    // FFI struct introspection (790-799)
    ffi_struct_names = 790, // 'Raylib' ffiStructNames - returns array of struct names
    ffi_struct_info = 791, // 'Raylib' ffiStructInfo: #Color - returns struct metadata
    ffi_call_with_struct = 792, // FFI call that handles struct args/returns
    ffi_runtime_call = 793, // Runtime FFI call via libffi - for function pointers
    ffi_generate_method = 794, // Generate Smalltalk method source for FFI function
    ffi_function_info = 795, // Get FFI function info (arg count, arg types, return type)
    subclass_create = 796, // Class >> subclass: #Name - create a new subclass dynamically
    compile_method = 797, // Class >> compile: 'source' - compile and install method
    load_obj_file = 798, // OBJLoader >> load: 'path' - load OBJ file, returns {vertexData. indexData. vertexCount. indexCount}
    ffi_create_struct_class = 799, // FFILibrary createStructClass: #Name for: 'LibName' - create struct subclass with accessor methods

    // Reflection primitives (900+)
    class_selectors = 900, // Behavior >> selectors
    class_all_selectors = 901, // Behavior >> allSelectors
    class_inst_var_names = 902, // Behavior >> instVarNames
    class_inst_size = 903, // Behavior >> instSize
    class_class_var_names = 904, // Behavior >> classVarNames
    class_superclass = 905, // Behavior >> superclass
    class_name = 906, // Class >> name

    // Global lookup
    global_at = 910, // Smalltalk >> at:
    global_at_ifAbsent = 911, // Smalltalk >> at:ifAbsent:

    // UI Process primitives
    ui_process_iteration = 920, // UIProcess >> processOneIteration
    ui_is_running = 921, // UIProcess >> isRunning

    // Image/Session primitives (Dolphin compatibility)
    image_path = 930, // SessionManager >> imagePath - full path without extension
    image_file_name = 931, // SessionManager >> imageFileName - full path with extension
    image_directory = 932, // SessionManager >> imageDirectory - directory only
    image_path_set = 933, // SessionManager >> imagePath: aString - set the image path

    // ========================================================================
    // Terminal primitives for Smalltalk TUI (940-970)
    // ========================================================================
    terminal_init = 940, // Terminal >> initialize - enter raw/alternate screen mode
    terminal_deinit = 941, // Terminal >> shutdown - restore normal terminal mode
    terminal_write = 942, // Terminal >> write: aString - write string at cursor
    terminal_clear = 943, // Terminal >> clear - clear entire screen
    terminal_set_cursor = 944, // Terminal >> setCursorRow: row col: col - move cursor
    terminal_get_cursor = 945, // Terminal >> cursorPosition - returns Point
    terminal_set_fg_color = 946, // Terminal >> foregroundColor: r:g:b: - set fg RGB
    terminal_set_bg_color = 947, // Terminal >> backgroundColor: r:g:b: - set bg RGB
    terminal_reset_style = 948, // Terminal >> resetStyle - reset colors/attributes
    terminal_poll_key = 949, // Terminal >> pollKey - non-blocking key check, nil if none
    terminal_read_key = 950, // Terminal >> readKey - blocking key read
    terminal_get_size = 951, // Terminal >> size - returns Point (cols @ rows)
    terminal_flush = 952, // Terminal >> flush - flush output buffer
    terminal_set_bold = 953, // Terminal >> bold: aBoolean
    terminal_set_italic = 954, // Terminal >> italic: aBoolean
    terminal_set_underline = 955, // Terminal >> underline: aBoolean
    terminal_hide_cursor = 956, // Terminal >> hideCursor
    terminal_show_cursor = 957, // Terminal >> showCursor
    terminal_clear_line = 958, // Terminal >> clearLine - clear current line
    terminal_clear_to_eol = 959, // Terminal >> clearToEndOfLine
    terminal_set_fg_indexed = 960, // Terminal >> foregroundIndex: anInteger (0-255)
    terminal_set_bg_indexed = 961, // Terminal >> backgroundIndex: anInteger (0-255)
    terminal_draw_box = 962, // Terminal >> drawBoxAt: origin extent: size - draw box chars
    terminal_fill_rect = 963, // Terminal >> fillRectAt: origin extent: size char: char

    // System introspection
    all_classes = 964, // Smalltalk >> allClasses - return array of all classes

    _,
};
