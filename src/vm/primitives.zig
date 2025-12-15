const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");
const bytecodes = @import("bytecodes.zig");
const interpreter_mod = @import("interpreter.zig");
const build_options = @import("build_options");
const ffi = if (build_options.ffi_enabled) @import("ffi.zig") else undefined;
const ffi_autogen = if (build_options.ffi_enabled) @import("ffi_autogen.zig") else undefined;
const ffi_runtime = if (build_options.ffi_enabled) @import("ffi_runtime.zig") else undefined;
const ffi_generated = if (build_options.ffi_enabled) @import("ffi_generated.zig") else undefined;
const filein = @import("../image/filein.zig");
const app_mod = @import("../tui/app.zig");
const obj_loader = @import("obj_loader.zig");

const Value = object.Value;
const Object = object.Object;
const CompiledMethod = object.CompiledMethod;
const Heap = memory.Heap;
const Interpreter = interpreter_mod.Interpreter;
const InterpreterError = interpreter_mod.InterpreterError;
const Primitive = bytecodes.Primitive;

// Debug flag - set to false to disable verbose debug output
const DEBUG_VERBOSE = false;


/// Execute a primitive by index
pub fn executePrimitive(interp: *Interpreter, prim_index: u16) InterpreterError!Value {
    const prim: Primitive = @enumFromInt(prim_index);

    return switch (prim) {
        // ====================================================================
        // SmallInteger arithmetic (Dolphin: 9-20)
        // ====================================================================
        .multiply => primMultiply(interp),
        .divide => primDivide(interp),
        .mod => primMod(interp),
        .integer_divide => primIntegerDivide(interp),
        .quo => primQuo(interp),
        .subtract => primSubtract(interp),
        .add => primAdd(interp),
        .equal => primEqual(interp),
        .greater_or_equal => primGreaterOrEqual(interp),
        .less_than => primLessThan(interp),
        .greater_than => primGreaterThan(interp),
        .less_or_equal => primLessOrEqual(interp),

        // Bit operations (Dolphin: 40-43)
        .bit_and => primBitAnd(interp),
        .bit_or => primBitOr(interp),
        .bit_xor => primBitXor(interp),
        .bit_shift => primBitShift(interp),

        // ====================================================================
        // Object/Array primitives (Dolphin: 60-77)
        // ====================================================================
        .at => primAt(interp),
        .at_put => primAtPut(interp),
        .size => primSize(interp),
        .string_at => primStringAt(interp),
        .string_at_put => primStringAtPut(interp),
        .basic_new => primBasicNew(interp),
        .basic_new_size => primBasicNewSize(interp),
        .become_dolphin => primBecome(interp), // Dolphin uses primitive 72 for become:
        .basic_identity_hash => primHash(interp),
        .shallow_copy => primShallowCopy(interp),
        .basic_resize => primBasicResize(interp),

        // ====================================================================
        // Block/Control flow (Dolphin: 81-84)
        // ====================================================================
        .block_value => primBlockValue(interp),
        .block_value_with_args => primBlockValueWithArgs(interp),
        .perform => primPerform(interp),
        .perform_with_args => primPerformWithArgs(interp),
        .become => primBecome(interp),
        .one_way_become => primOneWayBecome(interp),

        // Extended block value primitives (490-493)
        .block_value_1 => primBlockValue1(interp),
        .block_value_2 => primBlockValue2(interp),
        .block_value_3 => primBlockValue3(interp),
        .block_value_4 => primBlockValue4(interp),

        // ====================================================================
        // Process/Semaphore primitives (Dolphin: 85-100, 156, 189)
        // ====================================================================
        .semaphore_signal => primSemaphoreSignal(interp),
        .semaphore_wait => primSemaphoreWait(interp),
        .process_resume => primProcessResume(interp),
        .process_suspend => primProcessSuspend(interp),
        .process_terminate => primProcessTerminate(interp),
        .process_set_priority => primProcessSetPriority(interp),
        .enable_async_events => primEnableAsyncEvents(interp),
        .process_queue_interrupt => primProcessQueueInterrupt(interp),
        .semaphore_set_signals => primSemaphoreSetSignals(interp),
        .signal_timer_after => primSignalTimerAfter(interp),
        .yield => primYield(interp),
        .microsecond_clock_value => primMicrosecondClockValue(interp),
        .millisecond_clock_value => primMillisecondClockValue(interp),

        // ====================================================================
        // Object system (Dolphin: 109-114)
        // ====================================================================
        .hash => primHash(interp),
        .identical => primIdentical(interp),
        .class => primClass(interp),
        .quit => primQuit(interp),
        .garbage_collect => primGarbageCollect(interp),

        // ====================================================================
        // Boolean control flow (our extensions: 500+)
        // ====================================================================
        .true_if_true => primTrueIfTrue(interp),
        .true_if_false => primTrueIfFalse(interp),
        .true_if_true_if_false => primTrueIfTrueIfFalse(interp),
        .true_if_false_if_true => primTrueIfFalseIfTrue(interp),
        .false_if_true => primFalseIfTrue(interp),
        .false_if_false => primFalseIfFalse(interp),
        .false_if_true_if_false => primFalseIfTrueIfFalse(interp),
        .false_if_false_if_true => primFalseIfFalseIfTrue(interp),
        .boolean_not => primBooleanNot(interp),
        .while_true => primWhileTrue(interp),
        .while_false => primWhileFalse(interp),
        .true_and => primTrueAnd(interp),
        .true_or => primTrueOr(interp),
        .false_and => primFalseAnd(interp),
        .false_or => primFalseOr(interp),

        // Error handling
        .does_not_understand => primDoesNotUnderstand(interp),
        .error_message => primError(interp),
        .halt => primHalt(interp),

        // ====================================================================
        // Character/String (our extensions: 520+)
        // ====================================================================
        .char_code => primCharCode(interp),
        .char_from_code => primCharFromCode(interp),
        .print_string => primPrintString(interp),
        .print_char => primPrintChar(interp),

        // ====================================================================
        // Collection operations (our extensions: 530+)
        // ====================================================================
        .array_do => primArrayDo(interp),
        .array_collect => primArrayCollect(interp),
        .array_select => primArraySelect(interp),
        .array_reject => primArrayReject(interp),
        .array_detect => primArrayDetect(interp),
        .array_detect_if_none => primArrayDetectIfNone(interp),
        .array_inject_into => primArrayInjectInto(interp),
        .to_do => primToDo(interp),
        .to_by_do => primToByDo(interp),
        .times_repeat => primTimesRepeat(interp),

        // ====================================================================
        // Additional Integer operations (our extensions: 540+)
        // ====================================================================
        .negate => primNegate(interp),
        .abs => primAbs(interp),
        .even => primEven(interp),
        .odd => primOdd(interp),
        .positive => primPositive(interp),
        .negative => primNegative(interp),
        .sign => primSign(interp),
        .max => primMax(interp),
        .min => primMin(interp),
        .between_and => primBetweenAnd(interp),
        .not_equal => primNotEqual(interp),
        .factorial => primFactorial(interp),

        // Interval creation
        .to_interval => primToInterval(interp),
        .to_by_interval => primToByInterval(interp),
        .interval_size => primIntervalSize(interp),
        .interval_at => primIntervalAt(interp),
        .interval_do => primIntervalDo(interp),

        // Nil checking
        .is_nil => primIsNilTrue(interp), // UndefinedObject >> isNil returns true
        .is_nil_false => primIsNilFalse(interp), // Object >> isNil returns false
        .not_nil => primNotNil(interp), // Object >> notNil

        // ====================================================================
        // Float arithmetic (Dolphin: 45-47, 160-168, 205, 214-215)
        // ====================================================================
        .float_add => primFloatAdd(interp),
        .float_subtract => primFloatSubtract(interp),
        .float_multiply => primFloatMultiply(interp),
        .float_divide => primFloatDivide(interp),
        .float_less_than => primFloatLessThan(interp),
        .float_greater_than => primFloatGreaterThan(interp),
        .float_less_or_equal => primFloatLessOrEqual(interp),
        .float_greater_or_equal => primFloatGreaterOrEqual(interp),
        .float_equal => primFloatEqual(interp),
        .float_truncated => primFloatTruncated(interp),
        .float_abs => primFloatAbs(interp),
        .float_negate => primFloatNegate(interp),
        .float_print_string => primFloatPrintString(interp),
        .small_as_float => primSmallAsFloat(interp),

        // ====================================================================
        // String operations
        // ====================================================================
        .string_concat => primStringConcat(interp),
        .string_append => primStringConcat(interp), // Dolphin/ANSI primitive 218
        .string_compare => primStringCompare(interp),
        .string_less_than => primStringLessThan(interp),
        .string_greater_than => primStringGreaterThan(interp),
        .string_less_or_equal => primStringLessOrEqual(interp),
        .string_greater_or_equal => primStringGreaterOrEqual(interp),
        .string_equal => primStringEqual(interp),
        .dolphin_string_equal => primStringEqual(interp), // Dolphin primitive 219
        .string_copy_from_to => primStringCopyFromTo(interp),
        .string_next_index_of_dolphin => primStringNextIndexOf(interp), // Dolphin primitive 52

        // Exception handling
        .exception_signal => primExceptionSignal(interp),
        .exception_signal_with => primExceptionSignalWith(interp),
        .on_do => primOnDo(interp),
        .ensure => primEnsure(interp),
        .if_curtailed => primIfCurtailed(interp),

        // Dictionary primitives
        .dict_at => primDictAt(interp),
        .dict_at_put => primDictAtPut(interp),
        .dict_at_if_absent => primDictAtIfAbsent(interp),
        .dict_includes_key => primDictIncludesKey(interp),
        .dict_remove_key => primDictRemoveKey(interp),
        .dict_keys => primDictKeys(interp),
        .dict_values => primDictValues(interp),
        .dict_size => primDictSize(interp),

        // Set primitives
        .set_add => primSetAdd(interp),
        .set_includes => primSetIncludes(interp),
        .set_remove => primSetRemove(interp),
        .set_size => primSetSize(interp),

        // OrderedCollection primitives
        .oc_add => primOcAdd(interp),
        .oc_add_first => primOcAddFirst(interp),
        .oc_add_last => primOcAddLast(interp),
        .oc_remove_first => primOcRemoveFirst(interp),
        .oc_remove_last => primOcRemoveLast(interp),
        .oc_at => primOcAt(interp),
        .oc_at_put => primOcAtPut(interp),
        .oc_size => primOcSize(interp),

        // Stream primitives
        .stream_next => primStreamNext(interp),
        .stream_next_put => primStreamNextPut(interp),
        .stream_next_put_all => primStreamNextPutAll(interp),
        .dolphin_stream_next_put_all => primStreamNextPutAll(interp), // Dolphin primitive 173
        .stream_peek => primStreamPeek(interp),
        .stream_at_end => primStreamAtEnd(interp),
        .stream_position => primStreamPosition(interp),
        .stream_set_position => primStreamSetPosition(interp),
        .stream_reset => primStreamReset(interp),
        .stream_contents => primStreamContents(interp),

        // String additional primitives
        .string_index_of => primStringIndexOf(interp),
        .string_index_of_starting_at => primStringIndexOfStartingAt(interp),
        .string_as_upper => primStringAsUpper(interp),
        .string_as_lower => primStringAsLower(interp),
        .string_trim => primStringTrim(interp),
        .string_includes => primStringIncludes(interp),
        .string_split => primStringSplit(interp),
        .as_number => primAsNumber(interp),

        // File I/O primitives
        .file_open => primFileOpen(interp),
        .file_close => primFileClose(interp),
        .file_read => primFileRead(interp),
        .file_write => primFileWrite(interp),
        .file_flush => primFileFlush(interp),
        .file_size_prim => primFileSize(interp),
        .file_position => primFilePosition(interp),
        .file_set_position => primFileSetPosition(interp),
        .file_at_end => primFileAtEnd(interp),
        .file_delete => primFileDelete(interp),
        .file_exists => primFileExists(interp),
        .file_rename => primFileRename(interp),
        .stdout_write => primStdoutWrite(interp),
        .stdin_read => primStdinRead(interp),
        .stderr_write => primStderrWrite(interp),
        .transcript_show => primTranscriptShow(interp),
        .transcript_cr => primTranscriptCr(interp),
        .transcript_next_put_all => primTranscriptNextPutAll(interp),
        .transcript_flush => primTranscriptFlush(interp),

        // Reflection primitives
        .class_selectors => primClassSelectors(interp),
        .class_all_selectors => primClassAllSelectors(interp),
        .class_superclass => primClassSuperclass(interp),
        .class_name => primClassName(interp),
        .class_inst_size => primClassInstSize(interp),

        // Critical Dolphin primitives
        .small_int_print_string => primSmallIntPrintString(interp),
        .replace_from_to_with => primReplaceFromToWith(interp),
        .is_kind_of => primIsKindOf(interp),
        .inherits_from => primInheritsFrom(interp),
        .lookup_method => primLookupMethod(interp),
        .as_utf8_string => primAsUtf8String(interp),
        .identity_hash => primIdentityHash(interp),

        // Global lookup primitives
        .global_at => primGlobalAt(interp),
        .global_at_ifAbsent => primGlobalAtIfAbsent(interp),

        // FFI primitives
        .ffi_malloc => primFFIMalloc(interp),
        .ffi_free => primFFIFree(interp),
        .ffi_strlen => primFFIStrlen(interp),
        .ffi_puts => primFFIPuts(interp),
        .ffi_sin => primFFISin(interp),
        .ffi_cos => primFFICos(interp),
        .ffi_sqrt => primFFISqrt(interp),
        .ffi_pow => primFFIPow(interp),
        .ffi_exp => primFFIExp(interp),
        .ffi_log => primFFILog(interp),
        .ffi_floor => primFFIFloor(interp),
        .ffi_ceil => primFFICeil(interp),
        .ffi_fabs => primFFIFabs(interp),
        .ffi_atan2 => primFFIAtan2(interp),
        .ffi_tan => primFFITan(interp),
        .ffi_asin => primFFIAsin(interp),
        .ffi_acos => primFFIAcos(interp),
        .ffi_atan => primFFIAtan(interp),
        .ffi_memset => primFFIMemset(interp),
        .ffi_memcpy => primFFIMemcpy(interp),
        .ffi_read_int8 => primFFIReadInt8(interp),
        .ffi_read_int16 => primFFIReadInt16(interp),
        .ffi_read_int32 => primFFIReadInt32(interp),
        .ffi_read_int64 => primFFIReadInt64(interp),
        .ffi_read_float64 => primFFIReadFloat64(interp),
        .ffi_write_int8 => primFFIWriteInt8(interp),
        .ffi_write_int32 => primFFIWriteInt32(interp),
        .ffi_write_float64 => primFFIWriteFloat64(interp),

        // Generic auto-generated FFI call
        .ffi_call => primFFIGenericCall(interp),

        // FFI introspection
        .ffi_libraries => primFFILibraries(interp),
        .ffi_functions => primFFIFunctions(interp),

        // ByteArray/ExternalStructure field access primitives
        .bytes_uint8_at => primBytesUint8At(interp),
        .bytes_uint16_at => primBytesUint16At(interp),
        .bytes_uint32_at => primBytesUint32At(interp),
        .bytes_int8_at => primBytesInt8At(interp),
        .bytes_int16_at => primBytesInt16At(interp),
        .bytes_int32_at => primBytesInt32At(interp),
        .bytes_float32_at => primBytesFloat32At(interp),
        .bytes_float64_at => primBytesFloat64At(interp),
        .bytes_uint8_at_put => primBytesUint8AtPut(interp),
        .bytes_uint16_at_put => primBytesUint16AtPut(interp),
        .bytes_uint32_at_put => primBytesUint32AtPut(interp),
        .bytes_int8_at_put => primBytesInt8AtPut(interp),
        .bytes_int16_at_put => primBytesInt16AtPut(interp),
        .bytes_int32_at_put => primBytesInt32AtPut(interp),
        .bytes_float32_at_put => primBytesFloat32AtPut(interp),
        .bytes_float64_at_put => primBytesFloat64AtPut(interp),
        .bytes_address => primBytesAddress(interp),

        // FFI struct introspection
        .ffi_struct_names => primFFIStructNames(interp),
        .ffi_struct_info => primFFIStructInfo(interp),
        .ffi_call_with_struct => primFFICallWithStruct(interp),
        .ffi_runtime_call => primFFIRuntimeCall(interp),
        .ffi_generate_method => primFFIGenerateMethod(interp),
        .ffi_function_info => primFFIFunctionInfo(interp),

        // Dynamic class and method creation
        .subclass_create => primSubclassCreate(interp),
        .compile_method => primCompileMethod(interp),
        .load_obj_file => primLoadOBJFile(interp),

        // UI Process primitives (920-921)
        .ui_process_iteration => primUIProcessIteration(interp),
        .ui_is_running => primUIIsRunning(interp),

        else => InterpreterError.PrimitiveFailed,
    };
}

// ============================================================================
// Arithmetic Primitives
// ============================================================================

fn primAdd(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const result = a.asSmallInt() +% b.asSmallInt();
        // Check for overflow - SmallInteger fits in 62 bits
        const max: i61 = std.math.maxInt(i61);
        const min: i61 = std.math.minInt(i61);
        if (result >= min and result <= max) {
            return Value.fromSmallInt(@intCast(result));
        }
    }

    // Restore stack and fail
    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primSubtract(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const result = a.asSmallInt() -% b.asSmallInt();
        return Value.fromSmallInt(@intCast(result));
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primMultiply(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const av: i64 = a.asSmallInt();
        const bv: i64 = b.asSmallInt();
        const result = av * bv;
        // Check if result fits in SmallInteger
        const max: i64 = std.math.maxInt(i61);
        const min: i64 = std.math.minInt(i61);
        if (result >= min and result <= max) {
            return Value.fromSmallInt(@intCast(result));
        }
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primDivide(interp: *Interpreter) InterpreterError!Value {
    // Division primitive - returns integer if exact, Float otherwise
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const bv = b.asSmallInt();
        if (bv == 0) {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        }
        const av = a.asSmallInt();
        // Check if division is exact
        if (@mod(av, bv) == 0) {
            const result = @divTrunc(av, bv);
            return Value.fromSmallInt(result);
        }
        // Not exact division - return Float
        // (in full Smalltalk this would return a Fraction, but we don't have that yet)
        const float_result = @as(f64, @floatFromInt(av)) / @as(f64, @floatFromInt(bv));
        const float_obj = interp.heap.allocateFloat(float_result) catch {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        };
        return float_obj;
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primIntegerDivide(interp: *Interpreter) InterpreterError!Value {
    // Dolphin's // primitive - integer division toward negative infinity
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const bv = b.asSmallInt();
        if (bv == 0) {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        }
        const av = a.asSmallInt();
        const result = @divFloor(av, bv);
        return Value.fromSmallInt(result);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primQuo(interp: *Interpreter) InterpreterError!Value {
    // Dolphin's quo: primitive - integer quotient toward zero
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const bv = b.asSmallInt();
        if (bv == 0) {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        }
        const av = a.asSmallInt();
        const result = @divTrunc(av, bv);
        return Value.fromSmallInt(result);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primMod(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const bv = b.asSmallInt();
        if (bv == 0) {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        }
        const av = a.asSmallInt();
        const result = @mod(av, bv);
        return Value.fromSmallInt(result);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primLessThan(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() < b.asSmallInt());
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primGreaterThan(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() > b.asSmallInt());
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primLessOrEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() <= b.asSmallInt());
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primGreaterOrEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() >= b.asSmallInt());
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() == b.asSmallInt());
    }

    // For non-SmallIntegers, fail so that class-specific = method is invoked
    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primNotEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() != b.asSmallInt());
    }

    return Value.fromBool(!a.eql(b));
}

fn primFactorial(interp: *Interpreter) InterpreterError!Value {
    const n = try interp.pop();

    if (!n.isSmallInt()) {
        try interp.push(n);
        return InterpreterError.PrimitiveFailed;
    }

    const val = n.asSmallInt();
    if (val < 0) {
        try interp.push(n);
        return InterpreterError.PrimitiveFailed;
    }

    // Compute factorial iteratively
    var result: i64 = 1;
    var i: i64 = 2;
    while (i <= val) : (i += 1) {
        result = result * i;
        // Check for overflow - SmallInt max is about 2^60
        if (result > 0x0FFFFFFFFFFFFFFF or result < 0) {
            try interp.push(n);
            return InterpreterError.PrimitiveFailed;
        }
    }

    return Value.fromSmallInt(@intCast(result));
}

// ============================================================================
// Interval primitives
// ============================================================================

/// SmallInteger >> to: creates an Interval object
/// Interval has 3 instance variables: start, stop, step
fn primToInterval(interp: *Interpreter) InterpreterError!Value {
    const stop = try interp.pop();
    const start = try interp.pop();

    if (!start.isSmallInt() or !stop.isSmallInt()) {
        try interp.push(start);
        try interp.push(stop);
        return InterpreterError.PrimitiveFailed;
    }

    // Allocate Interval object with 3 fields: start, stop, step
    const interval = interp.heap.allocateObject(Heap.CLASS_INTERVAL, 3, .normal) catch {
        try interp.push(start);
        try interp.push(stop);
        return InterpreterError.PrimitiveFailed;
    };

    // Set fields
    interval.setField(0, start, 3); // start
    interval.setField(1, stop, 3); // stop
    interval.setField(2, Value.fromSmallInt(1), 3); // step = 1

    return Value.fromObject(interval);
}

/// SmallInteger >> to:by: creates an Interval object with custom step
fn primToByInterval(interp: *Interpreter) InterpreterError!Value {
    const step = try interp.pop();
    const stop = try interp.pop();
    const start = try interp.pop();

    if (!start.isSmallInt() or !stop.isSmallInt() or !step.isSmallInt()) {
        try interp.push(start);
        try interp.push(stop);
        try interp.push(step);
        return InterpreterError.PrimitiveFailed;
    }

    // step must not be zero
    if (step.asSmallInt() == 0) {
        try interp.push(start);
        try interp.push(stop);
        try interp.push(step);
        return InterpreterError.PrimitiveFailed;
    }

    // Allocate Interval object with 3 fields: start, stop, step
    const interval = interp.heap.allocateObject(Heap.CLASS_INTERVAL, 3, .normal) catch {
        try interp.push(start);
        try interp.push(stop);
        try interp.push(step);
        return InterpreterError.PrimitiveFailed;
    };

    // Set fields
    interval.setField(0, start, 3); // start
    interval.setField(1, stop, 3); // stop
    interval.setField(2, step, 3); // step

    return Value.fromObject(interval);
}

/// Interval >> size
fn primIntervalSize(interp: *Interpreter) InterpreterError!Value {
    const recv = try interp.pop();

    if (!recv.isObject()) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    if (obj.header.class_index != Heap.CLASS_INTERVAL) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    const start_val = obj.getField(0, 3);
    const stop_val = obj.getField(1, 3);
    const step_val = obj.getField(2, 3);

    if (!start_val.isSmallInt() or !stop_val.isSmallInt() or !step_val.isSmallInt()) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    const start = start_val.asSmallInt();
    const stop = stop_val.asSmallInt();
    const step = step_val.asSmallInt();

    if (step == 0) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    // Calculate size
    var size: i64 = 0;
    if (step > 0) {
        if (stop >= start) {
            size = @divFloor(stop - start, step) + 1;
        }
    } else {
        if (start >= stop) {
            size = @divFloor(start - stop, -step) + 1;
        }
    }

    return Value.fromSmallInt(@intCast(size));
}

/// Interval >> at:
fn primIntervalAt(interp: *Interpreter) InterpreterError!Value {
    const index_val = try interp.pop();
    const recv = try interp.pop();

    if (!recv.isObject() or !index_val.isSmallInt()) {
        try interp.push(recv);
        try interp.push(index_val);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    if (obj.header.class_index != Heap.CLASS_INTERVAL) {
        try interp.push(recv);
        try interp.push(index_val);
        return InterpreterError.PrimitiveFailed;
    }

    const index = index_val.asSmallInt();
    if (index < 1) {
        try interp.push(recv);
        try interp.push(index_val);
        return InterpreterError.PrimitiveFailed;
    }

    const start_val = obj.getField(0, 3);
    const stop_val = obj.getField(1, 3);
    const step_val = obj.getField(2, 3);

    if (!start_val.isSmallInt() or !stop_val.isSmallInt() or !step_val.isSmallInt()) {
        try interp.push(recv);
        try interp.push(index_val);
        return InterpreterError.PrimitiveFailed;
    }

    const start = start_val.asSmallInt();
    const stop = stop_val.asSmallInt();
    const step = step_val.asSmallInt();

    // Calculate the value at index (1-based)
    const value = start + (index - 1) * step;

    // Check bounds
    if (step > 0) {
        if (value > stop) {
            try interp.push(recv);
            try interp.push(index_val);
            return InterpreterError.PrimitiveFailed;
        }
    } else {
        if (value < stop) {
            try interp.push(recv);
            try interp.push(index_val);
            return InterpreterError.PrimitiveFailed;
        }
    }

    return Value.fromSmallInt(@intCast(value));
}

/// Interval >> do: aBlock
fn primIntervalDo(interp: *Interpreter) InterpreterError!Value {
    const block_val = try interp.pop();
    const recv = try interp.pop();

    if (!recv.isObject() or !block_val.isObject()) {
        try interp.push(recv);
        try interp.push(block_val);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    if (obj.header.class_index != Heap.CLASS_INTERVAL) {
        try interp.push(recv);
        try interp.push(block_val);
        return InterpreterError.PrimitiveFailed;
    }

    const start_val = obj.getField(0, 3);
    const stop_val = obj.getField(1, 3);
    const step_val = obj.getField(2, 3);

    if (!start_val.isSmallInt() or !stop_val.isSmallInt() or !step_val.isSmallInt()) {
        try interp.push(recv);
        try interp.push(block_val);
        return InterpreterError.PrimitiveFailed;
    }

    const start = start_val.asSmallInt();
    const stop = stop_val.asSmallInt();
    const step = step_val.asSmallInt();

    if (step == 0) {
        try interp.push(recv);
        try interp.push(block_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Iterate through the interval
    var i: i64 = start;
    if (step > 0) {
        while (i <= stop) : (i += step) {
            try interp.push(block_val);
            try interp.push(Value.fromSmallInt(@intCast(i)));
            _ = try primBlockValue1(interp);
        }
    } else {
        while (i >= stop) : (i += step) {
            try interp.push(block_val);
            try interp.push(Value.fromSmallInt(@intCast(i)));
            _ = try primBlockValue1(interp);
        }
    }

    return recv; // Return the interval itself
}

// ============================================================================
// Nil checking primitives
// ============================================================================

/// UndefinedObject >> isNil - always returns true
fn primIsNilTrue(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // discard receiver (nil)
    return Value.fromBool(true);
}

/// Object >> isNil - always returns false for non-nil objects
fn primIsNilFalse(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // discard receiver
    return Value.fromBool(false);
}

/// Object >> notNil - returns true if receiver is not nil
fn primNotNil(interp: *Interpreter) InterpreterError!Value {
    const recv = try interp.pop();
    return Value.fromBool(!recv.isNil());
}

fn primBitAnd(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const result = @as(u61, @bitCast(a.asSmallInt())) & @as(u61, @bitCast(b.asSmallInt()));
        return Value.fromSmallInt(@bitCast(result));
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primBitOr(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const result = @as(u61, @bitCast(a.asSmallInt())) | @as(u61, @bitCast(b.asSmallInt()));
        return Value.fromSmallInt(@bitCast(result));
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primBitXor(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const result = @as(u61, @bitCast(a.asSmallInt())) ^ @as(u61, @bitCast(b.asSmallInt()));
        return Value.fromSmallInt(@bitCast(result));
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primBitShift(interp: *Interpreter) InterpreterError!Value {
    const shift = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and shift.isSmallInt()) {
        const av = a.asSmallInt();
        const sv = shift.asSmallInt();

        if (sv >= 0) {
            // Left shift
            if (sv < 61) {
                const shifted = @as(u61, @bitCast(av)) << @intCast(sv);
                return Value.fromSmallInt(@bitCast(shifted));
            }
        } else {
            // Right shift
            const neg_sv: u6 = @intCast(-sv);
            if (neg_sv < 61) {
                const shifted = av >> neg_sv;
                return Value.fromSmallInt(shifted);
            }
        }
    }

    try interp.push(a);
    try interp.push(shift);
    return InterpreterError.PrimitiveFailed;
}

fn primNegate(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        return Value.fromSmallInt(-a.asSmallInt());
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primAbs(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        const val = a.asSmallInt();
        return Value.fromSmallInt(if (val < 0) -val else val);
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primEven(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        return Value.fromBool((a.asSmallInt() & 1) == 0);
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primOdd(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        return Value.fromBool((a.asSmallInt() & 1) != 0);
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primPositive(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() > 0);
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primNegative(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() < 0);
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primSign(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        const val = a.asSmallInt();
        const result: i61 = if (val > 0) 1 else if (val < 0) -1 else 0;
        return Value.fromSmallInt(result);
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primMax(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const av = a.asSmallInt();
        const bv = b.asSmallInt();
        return Value.fromSmallInt(if (av > bv) av else bv);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primMin(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        const av = a.asSmallInt();
        const bv = b.asSmallInt();
        return Value.fromSmallInt(if (av < bv) av else bv);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primBetweenAnd(interp: *Interpreter) InterpreterError!Value {
    const max = try interp.pop();
    const min = try interp.pop();
    const val = try interp.pop();

    if (val.isSmallInt() and min.isSmallInt() and max.isSmallInt()) {
        const v = val.asSmallInt();
        const mn = min.asSmallInt();
        const mx = max.asSmallInt();
        return Value.fromBool(v >= mn and v <= mx);
    }

    try interp.push(val);
    try interp.push(min);
    try interp.push(max);
    return InterpreterError.PrimitiveFailed;
}

fn primTimesRepeat(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const count = try interp.pop();

    if (!count.isSmallInt() or !block.isObject()) {
        try interp.push(count);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(count);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const n = count.asSmallInt();
    if (n <= 0) {
        return Value.nil;
    }

    // Get block data: [outerTempBase, startPC, numArgs, method, receiver]
    const start_pc = block_obj.getField(1, 6);
    const num_args_val = block_obj.getField(2, 6);
    const method_val = block_obj.getField(3, 6);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(count);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 0) {
        // timesRepeat: block should take no arguments
        try interp.push(count);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Loop n times
    var i: i61 = 0;
    while (i < n) : (i += 1) {
        const saved_ip = interp.ip;
        const saved_method = interp.method;
        const saved_context_ptr = interp.context_ptr;
        
        interp.method = @ptrCast(@alignCast(method_val.asObject()));
        interp.ip = @intCast(start_pc.asSmallInt());
        if (interp.primitive_block_depth >= interp.primitive_block_bases.len) {
            return InterpreterError.StackOverflow;
        }
        interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
        interp.primitive_block_depth += 1;

        // Execute the block (result is discarded)
        _ = interp.interpretLoop() catch |err| {
            interp.primitive_block_depth -= 1;
                        interp.ip = saved_ip;
            interp.method = saved_method;
            interp.context_ptr = saved_context_ptr;
            return err;
        };

        interp.primitive_block_depth -= 1;
                interp.ip = saved_ip;
        interp.method = saved_method;
        interp.context_ptr = saved_context_ptr;
    }

    return Value.nil;
}

// ============================================================================
// Object Primitives
// ============================================================================

fn primClass(interp: *Interpreter) InterpreterError!Value {
    const obj = try interp.pop();
    return interp.heap.classOf(obj);
}

fn primHash(interp: *Interpreter) InterpreterError!Value {
    const obj = try interp.pop();

    if (obj.isSmallInt()) {
        // Hash of SmallInteger is itself
        return obj;
    } else if (obj.isCharacter()) {
        return Value.fromSmallInt(obj.asCharacter());
    } else if (obj.isObject()) {
        return Value.fromSmallInt(obj.asObject().header.hash);
    } else if (obj.isNil()) {
        return Value.fromSmallInt(0);
    } else if (obj.isTrue()) {
        return Value.fromSmallInt(1);
    } else if (obj.isFalse()) {
        return Value.fromSmallInt(2);
    }

    return Value.fromSmallInt(0);
}

fn primIdentical(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();
    return Value.fromBool(a.eql(b));
}

fn primBasicNew(interp: *Interpreter) InterpreterError!Value {
    const class = try interp.pop();

    if (!class.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    // Extract instance variables count and format from the encoded instanceSpec
    var num_fields: usize = 0;
    var format: object.ClassFormat = .normal;
    if (format_val.isSmallInt()) {
        const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
        num_fields = info.inst_size;
        format = info.format;
    }

    // Find the class index by looking through the class table
    const class_index = findClassIndex(interp.heap, class_obj) orelse {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    };

    const obj = interp.heap.allocateObject(class_index, num_fields, format) catch {
        try interp.push(class);
        return InterpreterError.OutOfMemory;
    };

    return Value.fromObject(obj);
}

/// Find the class index for a class object by searching the class table
fn findClassIndex(heap: *Heap, class_obj: *Object) ?u32 {
    for (heap.class_table.items, 0..) |item, i| {
        if (item.isObject() and item.asObject() == class_obj) {
            return @intCast(i);
        }
    }
    return null;
}

fn primBasicNewSize(interp: *Interpreter) InterpreterError!Value {
    const size = try interp.pop();
    const class = try interp.pop();

    if (!class.isObject() or !size.isSmallInt()) {
        try interp.push(class);
        try interp.push(size);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class.asObject();
    const num_indexed: usize = @intCast(size.asSmallInt());

    // Find the class index by looking through the class table
    const class_index = findClassIndex(interp.heap, class_obj) orelse {
        try interp.push(class);
        try interp.push(size);
        return InterpreterError.PrimitiveFailed;
    };

    // Get the format from the class (instanceSpec)
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
    var format: object.ClassFormat = .variable;
    var num_fixed: usize = 0;
    if (format_val.isSmallInt()) {
        const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
        format = info.format;
        num_fixed = info.inst_size;
    }

    // Total fields = fixed instance variables + indexed slots
    const total_fields = num_fixed + num_indexed;
    const obj = interp.heap.allocateObject(class_index, total_fields, format) catch {
        try interp.push(class);
        try interp.push(size);
        return InterpreterError.OutOfMemory;
    };

    return Value.fromObject(obj);
}

fn primBasicResize(interp: *Interpreter) InterpreterError!Value {
    // Primitive 101: basicResize: newSize
    // Resize an indexable object to a new size
    // Stack: receiver, newSize -> resizedObject
    const new_size = try interp.pop();
    const recv = try interp.pop();

    if (!recv.isObject() or !new_size.isSmallInt()) {
        try interp.push(recv);
        try interp.push(new_size);
        return InterpreterError.PrimitiveFailed;
    }

    const new_size_int = new_size.asSmallInt();
    if (new_size_int < 0) {
        try interp.push(recv);
        try interp.push(new_size);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    const format = obj.header.getFormat();

    // Only variable/indexable objects can be resized
    if (format != .variable and format != .bytes and format != .words) {
        try interp.push(recv);
        try interp.push(new_size);
        return InterpreterError.PrimitiveFailed;
    }

    const new_indexed_size: usize = @intCast(new_size_int);
    const old_size = obj.header.size;

    // Get instance variable count from class for variable objects
    var inst_size: usize = 0;
    if (format == .variable) {
        const class_val = interp.heap.getClass(obj.header.class_index);
        if (class_val.isObject()) {
            const class_obj = class_val.asObject();
            const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
            if (format_val.isSmallInt()) {
                const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
                inst_size = info.inst_size;
            }
        }
    }

    // Allocate new object with named instance vars + new indexed size
    const total_new_size = if (format == .variable) inst_size + new_indexed_size else new_indexed_size;
    const new_obj = interp.heap.allocateObject(obj.header.class_index, total_new_size, format) catch {
        try interp.push(recv);
        try interp.push(new_size);
        return InterpreterError.OutOfMemory;
    };

    // Copy data from old object to new
    if (format == .variable) {
        // Copy named instance variables first
        const old_fields = obj.fields(old_size);
        const new_fields = new_obj.fields(total_new_size);
        for (0..inst_size) |i| {
            if (i < old_fields.len) {
                new_fields[i] = old_fields[i];
            }
        }
        // Copy indexed elements (up to min of old and new indexed sizes)
        const old_indexed_size = if (old_size > inst_size) old_size - inst_size else 0;
        const copy_count = @min(old_indexed_size, new_indexed_size);
        for (0..copy_count) |i| {
            new_fields[inst_size + i] = old_fields[inst_size + i];
        }
    } else if (format == .bytes) {
        // Copy byte data
        const old_bytes = obj.bytes(old_size);
        const new_bytes = new_obj.bytes(new_indexed_size);
        const copy_count = @min(old_size, new_indexed_size);
        @memcpy(new_bytes[0..copy_count], old_bytes[0..copy_count]);
    } else if (format == .words) {
        // Copy word data (as bytes)
        const old_word_bytes = old_size * 4;
        const new_word_bytes = new_indexed_size * 4;
        const src_ptr: [*]u8 = @ptrFromInt(@intFromPtr(obj) + @sizeOf(object.ObjectHeader));
        const dst_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_obj) + @sizeOf(object.ObjectHeader));
        const copy_count = @min(old_word_bytes, new_word_bytes);
        @memcpy(dst_ptr[0..copy_count], src_ptr[0..copy_count]);
    }

    return Value.fromObject(new_obj);
}

fn primShallowCopy(interp: *Interpreter) InterpreterError!Value {
    const recv = try interp.pop();

    if (!recv.isObject()) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    const format = obj.header.getFormat();
    const field_size = switch (format) {
        .normal, .variable, .weak => obj.header.size * @sizeOf(Value),
        .bytes => obj.header.size,
        .words => obj.header.size * 4,
        .compiled_method => obj.header.size,
    };

    const copy_obj = try interp.heap.allocateObject(obj.header.class_index, obj.header.size, format);

    // Copy payload bytes (fields or raw data), leave header (hash/class) as allocated
    const src_ptr: [*]u8 = @ptrFromInt(@intFromPtr(obj) + @sizeOf(object.ObjectHeader));
    const dst_ptr: [*]u8 = @ptrFromInt(@intFromPtr(copy_obj) + @sizeOf(object.ObjectHeader));
    @memcpy(dst_ptr[0..field_size], src_ptr[0..field_size]);

    return Value.fromObject(copy_obj);
}

/// Primitive 85: become: - Two-way identity swap
/// All references to receiver become references to argument and vice versa.
/// This is implemented by swapping object contents (header + data).
fn primBecome(interp: *Interpreter) InterpreterError!Value {
    const other = try interp.pop();
    const recv = try interp.pop();

    // Both must be objects (can't become immediates)
    if (!recv.isObject() or !other.isObject()) {
        try interp.push(recv);
        try interp.push(other);
        return InterpreterError.PrimitiveFailed;
    }

    const recv_obj = recv.asObject();
    const other_obj = other.asObject();

    // Can't become yourself
    if (recv_obj == other_obj) {
        return recv;
    }

    // Get the total sizes of both objects
    const recv_format = recv_obj.header.getFormat();
    const other_format = other_obj.header.getFormat();

    const recv_data_size = switch (recv_format) {
        .normal, .variable, .weak => recv_obj.header.size * @sizeOf(Value),
        .bytes => recv_obj.header.size,
        .words => recv_obj.header.size * 4,
        .compiled_method => recv_obj.header.size,
    };

    const other_data_size = switch (other_format) {
        .normal, .variable, .weak => other_obj.header.size * @sizeOf(Value),
        .bytes => other_obj.header.size,
        .words => other_obj.header.size * 4,
        .compiled_method => other_obj.header.size,
    };

    // For objects of different sizes, we need to do a full heap scan
    // For same size, we can just swap contents
    if (recv_data_size == other_data_size) {
        // Swap headers
        const temp_header = recv_obj.header;
        recv_obj.header = other_obj.header;
        other_obj.header = temp_header;

        // Swap data
        const recv_ptr: [*]u8 = @ptrFromInt(@intFromPtr(recv_obj) + @sizeOf(object.ObjectHeader));
        const other_ptr: [*]u8 = @ptrFromInt(@intFromPtr(other_obj) + @sizeOf(object.ObjectHeader));

        // Use a temp buffer for swapping
        var i: usize = 0;
        while (i < recv_data_size) : (i += 1) {
            const temp = recv_ptr[i];
            recv_ptr[i] = other_ptr[i];
            other_ptr[i] = temp;
        }
    } else {
        // Different sizes - need full heap scan to update all references
        // This is the expensive path
        scanHeapAndSwapReferences(interp.heap, recv, other);
    }

    return recv;
}

/// Primitive 88: oneWayBecome: - One-way identity replacement
/// All references to receiver are replaced with references to argument.
/// The receiver is effectively "replaced" by the argument.
fn primOneWayBecome(interp: *Interpreter) InterpreterError!Value {
    const other = try interp.pop();
    const recv = try interp.pop();

    // Both must be objects (can't become immediates)
    if (!recv.isObject() or !other.isObject()) {
        try interp.push(recv);
        try interp.push(other);
        return InterpreterError.PrimitiveFailed;
    }

    const recv_obj = recv.asObject();
    const other_obj = other.asObject();

    // Can't become yourself
    if (recv_obj == other_obj) {
        return recv;
    }

    // One-way become: scan heap and replace all references to recv with other
    scanHeapAndReplaceReferences(interp.heap, recv, other);

    // Also update the interpreter's stack and receiver if needed
    updateInterpreterReferences(interp, recv, other);

    return other;
}

/// Scan the heap and swap all references between two objects (two-way become)
fn scanHeapAndSwapReferences(heap: *Heap, obj_a: Value, obj_b: Value) void {
    // Scan all allocated objects in the heap (from_space is the active space)
    const heap_start = @intFromPtr(heap.from_space.ptr);
    const heap_end = heap_start + heap.alloc_ptr;

    var ptr = heap_start;
    while (ptr < heap_end) {
        const obj: *Object = @ptrFromInt(ptr);
        const format = obj.header.getFormat();

        // Calculate object size
        const data_size = switch (format) {
            .normal, .variable, .weak => obj.header.size * @sizeOf(Value),
            .bytes => obj.header.size,
            .words => obj.header.size * 4,
            .compiled_method => obj.header.size,
        };
        const total_size = @sizeOf(object.ObjectHeader) + data_size;
        const aligned_size = (total_size + 7) & ~@as(usize, 7); // 8-byte alignment

        // Only scan pointer-containing objects
        if (format == .normal or format == .variable or format == .weak) {
            const fields = obj.fields(obj.header.size);
            for (fields) |*field| {
                if (field.eql(obj_a)) {
                    field.* = obj_b;
                } else if (field.eql(obj_b)) {
                    field.* = obj_a;
                }
            }
        }

        ptr += aligned_size;
    }

    // Also swap references in the class table
    for (heap.class_table.items) |*class_entry| {
        if (class_entry.eql(obj_a)) {
            class_entry.* = obj_b;
        } else if (class_entry.eql(obj_b)) {
            class_entry.* = obj_a;
        }
    }

    // And in the global dictionary values
    var global_it = heap.globals.valueIterator();
    while (global_it.next()) |val_ptr| {
        if (val_ptr.eql(obj_a)) {
            val_ptr.* = obj_b;
        } else if (val_ptr.eql(obj_b)) {
            val_ptr.* = obj_a;
        }
    }
}

/// Scan the heap and replace all references to old_obj with new_obj (one-way become)
fn scanHeapAndReplaceReferences(heap: *Heap, old_obj: Value, new_obj: Value) void {
    // Scan all allocated objects in the heap (from_space is the active space)
    const heap_start = @intFromPtr(heap.from_space.ptr);
    const heap_end = heap_start + heap.alloc_ptr;

    var ptr = heap_start;
    while (ptr < heap_end) {
        const obj: *Object = @ptrFromInt(ptr);
        const format = obj.header.getFormat();

        // Calculate object size
        const data_size = switch (format) {
            .normal, .variable, .weak => obj.header.size * @sizeOf(Value),
            .bytes => obj.header.size,
            .words => obj.header.size * 4,
            .compiled_method => obj.header.size,
        };
        const total_size = @sizeOf(object.ObjectHeader) + data_size;
        const aligned_size = (total_size + 7) & ~@as(usize, 7); // 8-byte alignment

        // Only scan pointer-containing objects
        if (format == .normal or format == .variable or format == .weak) {
            const fields = obj.fields(obj.header.size);
            for (fields) |*field| {
                if (field.eql(old_obj)) {
                    field.* = new_obj;
                }
            }
        }

        ptr += aligned_size;
    }

    // Also update references in the class table
    for (heap.class_table.items) |*class_entry| {
        if (class_entry.eql(old_obj)) {
            class_entry.* = new_obj;
        }
    }

    // And in the global dictionary values
    var global_it = heap.globals.valueIterator();
    while (global_it.next()) |val_ptr| {
        if (val_ptr.eql(old_obj)) {
            val_ptr.* = new_obj;
        }
    }
}

/// Update references in the interpreter's stack and context
fn updateInterpreterReferences(interp: *Interpreter, old_obj: Value, new_obj: Value) void {
    // Update stack
    for (interp.stack[0..interp.sp]) |*slot| {
        if (slot.eql(old_obj)) {
            slot.* = new_obj;
        }
    }

    // Update receiver if it matches
    if (interp.receiver.eql(old_obj)) {
        interp.receiver = new_obj;
    }

    // Update context receivers
    for (interp.contexts[0..interp.context_ptr]) |*ctx| {
        if (ctx.receiver.eql(old_obj)) {
            ctx.receiver = new_obj;
        }
    }
}

// ============================================================================
// Array Primitives
// ============================================================================

fn primAt(interp: *Interpreter) InterpreterError!Value {
    const index = try interp.pop();
    const recv = try interp.pop();

    if (!recv.isObject() or !index.isSmallInt()) {
        try interp.push(recv);
        try interp.push(index);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    const idx_val = index.asSmallInt();
    if (idx_val < 1) {
        try interp.push(recv);
        try interp.push(index);
        return InterpreterError.PrimitiveFailed;
    }
    const base_idx: usize = @intCast(idx_val - 1); // Smalltalk is 1-indexed

    // Check if this is a string/symbol - return Character
    if (obj.header.class_index == Heap.CLASS_STRING or obj.header.class_index == Heap.CLASS_SYMBOL) {
        const bytes_data = obj.bytes(obj.header.size);
        if (base_idx >= bytes_data.len) {
            try interp.push(recv);
            try interp.push(index);
            return InterpreterError.PrimitiveFailed;
        }
        return Value.fromCharacter(bytes_data[base_idx]);
    }

    // Get size (this is simplified - real impl needs proper size tracking)
    const format = obj.header.getFormat();

    // Handle byte objects (ByteArray, etc.) - return SmallInteger for each byte
    if (format == .bytes) {
        const bytes_data = obj.bytes(obj.header.size);
        if (base_idx >= bytes_data.len) {
            try interp.push(recv);
            try interp.push(index);
            return InterpreterError.PrimitiveFailed;
        }
        return Value.fromSmallInt(@intCast(bytes_data[base_idx]));
    }

    if (format == .variable or format == .normal) {
        // For variable subclasses (like OrderedCollection), basicAt: indexes into
        // the indexed part AFTER the named instance variables
        var idx = base_idx;
        if (format == .variable) {
            // Get number of named instance variables from the class
            const class_val = interp.heap.getClass(obj.header.class_index);
            if (class_val.isObject()) {
                const class_obj = class_val.asObject();
                const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
                if (format_val.isSmallInt()) {
                    const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
                    idx = base_idx + info.inst_size; // Skip past named inst vars
                }
            }
        }
        const obj_size = obj.header.size;
        if (idx >= obj_size) {
            try interp.push(recv);
            try interp.push(index);
            return InterpreterError.PrimitiveFailed;
        }
        const val = obj.getField(idx, obj_size);
        return val;
    }

    try interp.push(recv);
    try interp.push(index);
    return InterpreterError.PrimitiveFailed;
}

fn primAtPut(interp: *Interpreter) InterpreterError!Value {
    const val = try interp.pop();
    const index = try interp.pop();
    const recv = try interp.pop();

    if (!recv.isObject() or !index.isSmallInt()) {
        try interp.push(recv);
        try interp.push(index);
        try interp.push(val);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    const base_idx: usize = @intCast(index.asSmallInt() - 1);
    const format = obj.header.getFormat();

    // For bytes format objects (Strings, ByteArrays), store a single byte
    if (format == .bytes) {
        const bytes_data = obj.bytes(obj.header.size);
        if (base_idx >= bytes_data.len) {
            try interp.push(recv);
            try interp.push(index);
            try interp.push(val);
            return InterpreterError.PrimitiveFailed;
        }
        // Value can be a Character or SmallInt representing a byte
        var byte_val: u8 = 0;
        if (val.isCharacter()) {
            byte_val = @truncate(val.asCharacter());
        } else if (val.isSmallInt()) {
            byte_val = @truncate(@as(u64, @intCast(val.asSmallInt())));
        } else {
            try interp.push(recv);
            try interp.push(index);
            try interp.push(val);
            return InterpreterError.PrimitiveFailed;
        }
        bytes_data[base_idx] = byte_val;
        return val;
    }

    // For variable subclasses (like OrderedCollection), basicAt:put: indexes into
    // the indexed part AFTER the named instance variables
    var idx = base_idx;
    if (format == .variable) {
        // Get number of named instance variables from the class
        const class_val = interp.heap.getClass(obj.header.class_index);
        if (class_val.isObject()) {
            const class_obj = class_val.asObject();
            const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
            if (format_val.isSmallInt()) {
                const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
                idx = base_idx + info.inst_size; // Skip past named inst vars
            }
        }
    }

    const obj_size = obj.header.size;
    if (idx >= obj_size) {
        try interp.push(recv);
        try interp.push(index);
        try interp.push(val);
        return InterpreterError.PrimitiveFailed;
    }
    obj.setField(idx, val, obj_size);
    return val;
}

fn primSize(interp: *Interpreter) InterpreterError!Value {
    const recv = try interp.pop();

    if (!recv.isObject()) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();
    const format = obj.header.getFormat();

    // For variable objects, basicSize returns only the indexed slots (not named instance vars)
    if (format == .variable) {
        // Get the number of named instance variables from the class
        const class_val = interp.heap.getClass(obj.header.class_index);
        if (class_val.isObject()) {
            const class_obj = class_val.asObject();
            const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
            if (format_val.isSmallInt()) {
                const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
                const total_size = obj.header.size;
                const indexed_size = if (total_size > info.inst_size) total_size - info.inst_size else 0;
                return Value.fromSmallInt(@intCast(indexed_size));
            }
        }
    }

    // For other objects (Arrays, Strings, ByteArrays, etc.), return total size
    return Value.fromSmallInt(@intCast(obj.header.size));
}

// ============================================================================
// I/O Primitives
// ============================================================================

fn primPrintString(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (str.isObject()) {
        const obj = str.asObject();
        if (obj.header.getFormat() == .bytes) {
            // Get the class to determine size
            const class = interp.heap.getClass(obj.header.class_index);
            if (class.isObject()) {
                const class_obj = class.asObject();
                const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
                if (format_val.isSmallInt()) {
                    const size: usize = @intCast(format_val.asSmallInt());
                    const bytes_slice = obj.bytes(size);
                    const stdout = std.fs.File.stdout();
                    _ = stdout.write(bytes_slice) catch {};
                    return str;
                }
            }
        }
    }

    // For SmallInteger, print the number
    if (str.isSmallInt()) {
        const stdout = std.fs.File.stdout();
        var buf: [32]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf, "{d}", .{str.asSmallInt()}) catch "?";
        _ = stdout.write(formatted) catch {};
        return str;
    }

    try interp.push(str);
    return InterpreterError.PrimitiveFailed;
}

fn primPrintChar(interp: *Interpreter) InterpreterError!Value {
    const ch = try interp.pop();

    if (ch.isCharacter()) {
        const stdout = std.fs.File.stdout();
        const cp = ch.asCharacter();
        var buf: [4]u8 = undefined;
        const len = std.unicode.utf8Encode(cp, &buf) catch 0;
        _ = stdout.write(buf[0..len]) catch {};
        return ch;
    }

    try interp.push(ch);
    return InterpreterError.PrimitiveFailed;
}

// ============================================================================
// System Primitives
// ============================================================================

fn primQuit(interp: *Interpreter) InterpreterError!Value {
    _ = interp;
    std.process.exit(0);
}

fn primGarbageCollect(interp: *Interpreter) InterpreterError!Value {
    interp.heap.collectGarbage() catch {
        return InterpreterError.OutOfMemory;
    };
    return Value.nil;
}

// ============================================================================
// Character Primitives
// ============================================================================

fn primCharCode(interp: *Interpreter) InterpreterError!Value {
    const ch = try interp.pop();

    if (ch.isCharacter()) {
        return Value.fromSmallInt(ch.asCharacter());
    }

    try interp.push(ch);
    return InterpreterError.PrimitiveFailed;
}

fn primCharFromCode(interp: *Interpreter) InterpreterError!Value {
    const code = try interp.pop();
    _ = try interp.pop(); // Pop receiver (Character class)

    if (code.isSmallInt()) {
        const cp = code.asSmallInt();
        if (cp >= 0 and cp <= 0x10FFFF) {
            return Value.fromCharacter(@intCast(cp));
        }
    }

    return InterpreterError.PrimitiveFailed;
}

// ============================================================================
// Block Evaluation Primitives
// ============================================================================

pub fn primBlockValue(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();

    if (!block.isObject()) {
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
    // Field 0/5 can be either heap context (Object) or stack index (SmallInt) for backwards compatibility
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 0) {
        // Wrong number of arguments
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Save interpreter state
    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver; // Restore the receiver from when the block was created

    // Push a placeholder for the receiver slot (like primBlockValue1 does implicitly)
    // This ensures temp_base + 1 + index correctly addresses temps
    try interp.push(block_receiver);
    interp.temp_base = interp.sp - 1; // temp_base points to receiver slot

    // Restore heap contexts from block for cross-process variable access
    // Check if block has heap contexts (new format) or stack indices (old format)
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        // New format: heap contexts
        interp.heap_context = outer_context_val;
        interp.home_heap_context = home_context_val;
        // Stack indices aren't valid for heap context blocks - use current frame
        interp.outer_temp_base = interp.temp_base;
        interp.home_temp_base = interp.temp_base;
    } else if (outer_context_val.isSmallInt()) {
        // Old format: stack indices (for backwards compatibility)
        interp.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interp.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else interp.temp_base;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    } else {
        // Neither format - use defaults
        interp.outer_temp_base = interp.temp_base;
        interp.home_temp_base = interp.temp_base;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    }

    // Allocate space for block temporaries by pushing nil values
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var i: usize = 0;
    while (i < num_temps) : (i += 1) {
        try interp.push(Value.nil);
    }

    // If the block has temps, create a new heap context for them.
    // This is critical for nested blocks to access this block's temps via push_outer_temp.
    if (num_temps > 0) {
        // Save outer context for home access
        const outer_ctx = interp.heap_context;
        // Create new context for this block's temps
        const heap_ctx = try interp.createHeapContext(num_temps);
        interp.heap_context = Value.fromObject(heap_ctx);
        // Keep home context pointing to outer for level >= 2 access
        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = outer_ctx;
        }
    }

    // Check for recursion limit
    if (interp.primitive_block_depth >= interp.primitive_block_bases.len) {
        return InterpreterError.StackOverflow;
    }
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    // Execute until we hit a return
    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

fn primBlockValue1(interp: *Interpreter) InterpreterError!Value {
    const arg = try interp.pop();
    const block = try interp.pop();

    if (!block.isObject()) {
        try interp.push(block);
        try interp.push(arg);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        try interp.push(arg);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(block);
        try interp.push(arg);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 1) {
        try interp.push(block);
        try interp.push(arg);
        return InterpreterError.PrimitiveFailed;
    }

    // Save interpreter state including sp before we push args
    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver; // Restore the receiver from when the block was created

    // Handle heap contexts or stack indices
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        interp.heap_context = outer_context_val;
        interp.home_heap_context = home_context_val;
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
    } else if (outer_context_val.isSmallInt()) {
        interp.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interp.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    } else {
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    }

    // Set temp_base so that temp_base + 1 + 0 points to the first argument
    // Arguments will be pushed starting at current sp
    interp.temp_base = interp.sp - 1;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    // Push the argument
    try interp.push(arg);

    // Allocate space for block temporaries by pushing nil values
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var ti: usize = 0;
    while (ti < num_temps) : (ti += 1) {
        try interp.push(Value.nil);
    }

    // If the block has temps, create a new heap context for them.
    // The context needs slots for BOTH args and temps since bytecode indices are absolute
    if (num_temps > 0) {
        const outer_ctx = interp.heap_context;
        const heap_ctx = try interp.createHeapContext(1 + num_temps); // 1 arg + temps
        interp.heap_context = Value.fromObject(heap_ctx);
        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = outer_ctx;
        }
    }

    if (DEBUG_VERBOSE) std.debug.print("DEBUG primBlockValue1: saved_temp_base={} interp.home_temp_base={}\n", .{ saved_temp_base, interp.home_temp_base });

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

fn primBlockValue2(interp: *Interpreter) InterpreterError!Value {
    const arg2 = try interp.pop();
    const arg1 = try interp.pop();
    const block = try interp.pop();

    if (!block.isObject()) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 2) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver; // Restore the receiver from when the block was created

    // Handle heap contexts or stack indices
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        interp.heap_context = outer_context_val;
        interp.home_heap_context = home_context_val;
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
    } else if (outer_context_val.isSmallInt()) {
        interp.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interp.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    } else {
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    }

    // Set temp_base so that temp_base + 1 + 0 points to the first argument
    interp.temp_base = interp.sp - 1;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    // Push arguments
    try interp.push(arg1);
    try interp.push(arg2);

    // Allocate space for block temporaries
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var ti: usize = 0;
    while (ti < num_temps) : (ti += 1) {
        try interp.push(Value.nil);
    }

    // If the block has temps, create a new heap context for them.
    // The context needs slots for BOTH args and temps since bytecode indices are absolute
    if (num_temps > 0) {
        const outer_ctx = interp.heap_context;
        const heap_ctx = try interp.createHeapContext(2 + num_temps); // 2 args + temps
        interp.heap_context = Value.fromObject(heap_ctx);
        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = outer_ctx;
        }
    }

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

fn primBlockValue3(interp: *Interpreter) InterpreterError!Value {
    const arg3 = try interp.pop();
    const arg2 = try interp.pop();
    const arg1 = try interp.pop();
    const block = try interp.pop();

    if (!block.isObject()) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 3) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver; // Restore the receiver from when the block was created

    // Handle heap contexts or stack indices
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        interp.heap_context = outer_context_val;
        interp.home_heap_context = home_context_val;
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
    } else if (outer_context_val.isSmallInt()) {
        interp.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interp.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    } else {
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    }

    // Set temp_base so that temp_base + 1 + 0 points to the first argument
    interp.temp_base = interp.sp - 1;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    try interp.push(arg1);
    try interp.push(arg2);
    try interp.push(arg3);

    // Allocate space for block temporaries
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var ti: usize = 0;
    while (ti < num_temps) : (ti += 1) {
        try interp.push(Value.nil);
    }

    // If the block has temps, create a new heap context for them.
    // The context needs slots for BOTH args and temps since bytecode indices are absolute
    if (num_temps > 0) {
        const outer_ctx = interp.heap_context;
        const heap_ctx = try interp.createHeapContext(3 + num_temps); // 3 args + temps
        interp.heap_context = Value.fromObject(heap_ctx);
        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = outer_ctx;
        }
    }

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

fn primBlockValue4(interp: *Interpreter) InterpreterError!Value {
    const arg4 = try interp.pop();
    const arg3 = try interp.pop();
    const arg2 = try interp.pop();
    const arg1 = try interp.pop();
    const block = try interp.pop();

    if (!block.isObject()) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        try interp.push(arg4);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        try interp.push(arg4);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        try interp.push(arg4);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 4) {
        try interp.push(block);
        try interp.push(arg1);
        try interp.push(arg2);
        try interp.push(arg3);
        try interp.push(arg4);
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver; // Restore the receiver from when the block was created

    // Handle heap contexts or stack indices
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        interp.heap_context = outer_context_val;
        interp.home_heap_context = home_context_val;
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
    } else if (outer_context_val.isSmallInt()) {
        interp.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interp.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    } else {
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    }

    // Set temp_base so that temp_base + 1 + 0 points to the first argument
    interp.temp_base = interp.sp - 1;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    try interp.push(arg1);
    try interp.push(arg2);
    try interp.push(arg3);
    try interp.push(arg4);

    // Allocate space for block temporaries
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var ti: usize = 0;
    while (ti < num_temps) : (ti += 1) {
        try interp.push(Value.nil);
    }

    // If the block has temps, create a new heap context for them.
    // The context needs slots for BOTH args and temps since bytecode indices are absolute
    if (num_temps > 0) {
        const outer_ctx = interp.heap_context;
        const heap_ctx = try interp.createHeapContext(4 + num_temps); // 4 args + temps
        interp.heap_context = Value.fromObject(heap_ctx);
        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = outer_ctx;
        }
    }

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

// BlockClosure >> valueWithArguments: (Dolphin primitive 82)
fn primBlockValueWithArgs(interp: *Interpreter) InterpreterError!Value {
    const args = try interp.pop();
    const block = try interp.pop();

    if (!block.isObject() or !args.isObject()) {
        try interp.push(block);
        try interp.push(args);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        try interp.push(args);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method, receiver, homeContext, numTemps]
    const outer_context_val = block_obj.getField(Heap.BLOCK_FIELD_OUTER_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const start_pc = block_obj.getField(Heap.BLOCK_FIELD_START_PC, Heap.BLOCK_NUM_FIELDS);
    const num_args_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_ARGS, Heap.BLOCK_NUM_FIELDS);
    const method_val = block_obj.getField(Heap.BLOCK_FIELD_METHOD, Heap.BLOCK_NUM_FIELDS);
    const block_receiver = block_obj.getField(Heap.BLOCK_FIELD_RECEIVER, Heap.BLOCK_NUM_FIELDS);
    const home_context_val = block_obj.getField(Heap.BLOCK_FIELD_HOME_CONTEXT, Heap.BLOCK_NUM_FIELDS);
    const num_temps_val = block_obj.getField(Heap.BLOCK_FIELD_NUM_TEMPS, Heap.BLOCK_NUM_FIELDS);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(block);
        try interp.push(args);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    const args_obj = args.asObject();

    // Get array size from header
    const array_size: usize = args_obj.header.size;
    if (array_size != @as(usize, @intCast(expected_args))) {
        try interp.push(block);
        try interp.push(args);
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver; // Restore the receiver from when the block was created

    // Handle heap contexts or stack indices
    if (outer_context_val.isObject() and outer_context_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT) {
        interp.heap_context = outer_context_val;
        interp.home_heap_context = home_context_val;
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
    } else if (outer_context_val.isSmallInt()) {
        interp.outer_temp_base = @intCast(outer_context_val.asSmallInt());
        interp.home_temp_base = if (home_context_val.isSmallInt()) @intCast(home_context_val.asSmallInt()) else interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    } else {
        interp.outer_temp_base = interp.sp - 1;
        interp.home_temp_base = interp.sp - 1;
        interp.heap_context = Value.nil;
        interp.home_heap_context = Value.nil;
    }

    // Set temp_base so that temp_base + 1 + 0 points to the first argument
    interp.temp_base = interp.sp - 1;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    // Push all arguments from array
    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const arg = args_obj.getField(i, array_size);
        try interp.push(arg);
    }

    // Allocate space for block temporaries
    const num_temps: usize = if (num_temps_val.isSmallInt() and num_temps_val.asSmallInt() > 0)
        @intCast(num_temps_val.asSmallInt())
    else
        0;
    var ti: usize = 0;
    while (ti < num_temps) : (ti += 1) {
        try interp.push(Value.nil);
    }

    // If the block has temps, create a new heap context for them.
    // The context needs slots for BOTH args and temps since bytecode indices are absolute
    if (num_temps > 0) {
        const outer_ctx = interp.heap_context;
        const heap_ctx = try interp.createHeapContext(array_size + num_temps); // args + temps
        interp.heap_context = Value.fromObject(heap_ctx);
        if (interp.home_heap_context.isNil()) {
            interp.home_heap_context = outer_ctx;
        }
    }

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

// Object >> perform: (Dolphin primitive 83)
fn primPerform(interp: *Interpreter) InterpreterError!Value {
    const selector = try interp.pop();
    const receiver = try interp.pop();

    // Selector must be a symbol
    if (!selector.isObject()) {
        try interp.push(receiver);
        try interp.push(selector);
        return InterpreterError.PrimitiveFailed;
    }

    const sel_obj = selector.asObject();
    if (sel_obj.header.class_index != Heap.CLASS_SYMBOL) {
        try interp.push(receiver);
        try interp.push(selector);
        return InterpreterError.PrimitiveFailed;
    }

    const sel_name = sel_obj.bytes(sel_obj.header.size);
    if (DEBUG_VERBOSE) std.debug.print("DEBUG perform: selector={s} context_ptr={}\n", .{ sel_name, interp.context_ptr });

    // Look up the method
    const class = interp.heap.classOf(receiver);
    const method_opt = interp.lookupMethod(class, selector);

    if (method_opt) |method| {
        // Execute the method
        if (method.header.primitive_index != 0) {
            // Push receiver for primitive
            try interp.push(receiver);
            if (executePrimitive(interp, method.header.primitive_index)) |result| {
                return result;
            } else |err| {
                // If primitive set up context, propagate that
                if (err == InterpreterError.ContinueExecution) {
                    return err;
                }
                // Primitive failed - pop receiver and fall through to bytecode
                _ = try interp.pop();
            }
        }

        // Save current context
        if (interp.context_ptr >= interp.contexts.len - 1) {
            return InterpreterError.StackOverflow;
        }

        interp.contexts[interp.context_ptr] = .{
            .method = interp.method,
            .method_class = interp.method_class,
            .ip = interp.ip,
            .receiver = interp.receiver,
            .temp_base = interp.temp_base,
            .outer_temp_base = interp.outer_temp_base,
            .home_temp_base = interp.home_temp_base,
            .num_args = 0,
            .num_temps = 0,
            .outer_context = null,
            .closure = null,
            .heap_context = interp.heap_context,
            .home_heap_context = interp.home_heap_context,
        };
        interp.context_ptr += 1;

        interp.method = method;
        interp.ip = 0;
        interp.receiver = receiver;
        interp.temp_base = interp.sp;
        interp.outer_temp_base = interp.temp_base;
        interp.home_temp_base = interp.temp_base;

        // Push receiver
        try interp.push(receiver);

        // Allocate temps (no args for perform:)
        const local_temps = method.header.num_temps;
        var k: usize = 0;
        while (k < local_temps) : (k += 1) {
            try interp.push(Value.nil);
        }

        // Tell caller to continue executing - we've set up the new context
        return InterpreterError.ContinueExecution;
    }

    // Method not found
    try interp.push(receiver);
    try interp.push(selector);
    if (selector.isObject()) {
        const sel_obj2 = selector.asObject();
        if (sel_obj2.header.class_index == Heap.CLASS_SYMBOL) {
            interp.last_mnu_selector = sel_obj2.bytes(sel_obj2.header.size);
        }
    }
    interp.last_mnu_receiver = receiver;
    interp.last_mnu_method = interp.currentMethodSource();
    return InterpreterError.MessageNotUnderstood;
}

// Object >> perform:withArguments: (Dolphin primitive 84)
fn primPerformWithArgs(interp: *Interpreter) InterpreterError!Value {
    const args_array = try interp.pop();
    const selector = try interp.pop();
    const receiver = try interp.pop();

    // Selector must be a symbol
    if (!selector.isObject()) {
        try interp.push(receiver);
        try interp.push(selector);
        try interp.push(args_array);
        return InterpreterError.PrimitiveFailed;
    }

    const sel_obj = selector.asObject();
    if (sel_obj.header.class_index != Heap.CLASS_SYMBOL) {
        try interp.push(receiver);
        try interp.push(selector);
        try interp.push(args_array);
        return InterpreterError.PrimitiveFailed;
    }

    // Args must be an array
    if (!args_array.isObject()) {
        try interp.push(receiver);
        try interp.push(selector);
        try interp.push(args_array);
        return InterpreterError.PrimitiveFailed;
    }

    const args_obj = args_array.asObject();
    if (args_obj.header.class_index != Heap.CLASS_ARRAY) {
        try interp.push(receiver);
        try interp.push(selector);
        try interp.push(args_array);
        return InterpreterError.PrimitiveFailed;
    }

    const num_args = args_obj.header.size;

    // Look up the method
    const class = interp.heap.classOf(receiver);
    const method_opt = interp.lookupMethod(class, selector);

    if (method_opt) |method| {
        // Execute the method
        if (method.header.primitive_index != 0) {
            // Push receiver and args for primitive
            try interp.push(receiver);
            var i: usize = 0;
            while (i < num_args) : (i += 1) {
                try interp.push(args_obj.getField(i, num_args));
            }
            if (executePrimitive(interp, method.header.primitive_index)) |result| {
                return result;
            } else |err| {
                // If primitive set up context, propagate that
                if (err == InterpreterError.ContinueExecution) {
                    return err;
                }
                // Primitive failed - pop args and receiver
                var j: usize = 0;
                while (j < num_args + 1) : (j += 1) {
                    _ = try interp.pop();
                }
            }
        }

        // Save current context
        if (interp.context_ptr >= interp.contexts.len - 1) {
            return InterpreterError.StackOverflow;
        }

        interp.contexts[interp.context_ptr] = .{
            .method = interp.method,
            .method_class = interp.method_class,
            .ip = interp.ip,
            .receiver = interp.receiver,
            .temp_base = interp.temp_base,
            .outer_temp_base = interp.outer_temp_base,
            .home_temp_base = interp.home_temp_base,
            .num_args = 0,
            .num_temps = 0,
            .outer_context = null,
            .closure = null,
            .heap_context = interp.heap_context,
            .home_heap_context = interp.home_heap_context,
        };
        interp.context_ptr += 1;

        interp.method = method;
        interp.ip = 0;
        interp.receiver = receiver;
        interp.temp_base = interp.sp;
        interp.outer_temp_base = interp.temp_base;
        interp.home_temp_base = interp.temp_base;

        // Push receiver
        try interp.push(receiver);

        // Push arguments from array
        var i: usize = 0;
        while (i < num_args) : (i += 1) {
            try interp.push(args_obj.getField(i, num_args));
        }

        // Allocate local temps (beyond args)
        const total_temps = method.header.num_temps;
        const local_temps = if (total_temps > num_args) total_temps - @as(u8, @intCast(num_args)) else 0;
        var k: usize = 0;
        while (k < local_temps) : (k += 1) {
            try interp.push(Value.nil);
        }

        // Tell caller to continue executing - we've set up the new context
        return InterpreterError.ContinueExecution;
    }

    // Method not found
    try interp.push(receiver);
    try interp.push(selector);
    try interp.push(args_array);
    if (selector.isObject()) {
        const sel_obj2 = selector.asObject();
        if (sel_obj2.header.class_index == Heap.CLASS_SYMBOL) {
            interp.last_mnu_selector = sel_obj2.bytes(sel_obj2.header.size);
        }
    }
    interp.last_mnu_receiver = receiver;
    interp.last_mnu_method = interp.currentMethodSource();
    return InterpreterError.MessageNotUnderstood;
}

// ============================================================================
// Boolean Control Flow Primitives
// ============================================================================

/// Helper to evaluate a block (shared by control flow primitives)
fn evaluateBlock(interp: *Interpreter, block: Value) InterpreterError!Value {
    if (!block.isObject()) {
        std.debug.print("DEBUG evaluateBlock: block is not object\n", .{});
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        std.debug.print("DEBUG evaluateBlock: block class_idx={} not BlockClosure\n", .{block_obj.header.class_index});
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerTempBase, startPC, numArgs, method, receiver, homeTempBase]
    const outer_temp_base_val = block_obj.getField(0, 6);
    const start_pc = block_obj.getField(1, 6);
    const num_args_val = block_obj.getField(2, 6);
    const method_val = block_obj.getField(3, 6);
    const block_receiver = block_obj.getField(4, 6);
    const home_temp_base_val = block_obj.getField(5, 6);

    // Check if outer_temp_base is a heap context (MethodContext object) or stack index (SmallInt)
    const uses_heap_context = outer_temp_base_val.isObject() and
        outer_temp_base_val.asObject().header.class_index == Heap.CLASS_METHOD_CONTEXT;

    // Validate required fields
    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }
    // outer_temp_base must be either SmallInt or MethodContext
    if (!outer_temp_base_val.isSmallInt() and !uses_heap_context) {
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 0) {
        // Control flow blocks should take no arguments
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    const saved_heap_context = interp.heap_context;
    const saved_home_heap_context = interp.home_heap_context;

    const block_method_ptr: *CompiledMethod = @ptrCast(@alignCast(method_val.asObject()));
    interp.method = block_method_ptr;
    interp.ip = @intCast(start_pc.asSmallInt());

    // Set up context based on format (heap context vs stack-based)
    if (uses_heap_context) {
        // Heap context format - for closure variable capture
        interp.heap_context = outer_temp_base_val;
        interp.home_heap_context = home_temp_base_val;
        // Stack indices aren't used with heap contexts
        interp.outer_temp_base = interp.sp;
        interp.home_temp_base = interp.sp;
    } else {
        // Stack-based format
        interp.outer_temp_base = @intCast(outer_temp_base_val.asSmallInt());
        interp.home_temp_base = if (home_temp_base_val.isSmallInt()) @intCast(home_temp_base_val.asSmallInt()) else interp.temp_base;
    }
    interp.receiver = block_receiver;
    interp.temp_base = interp.sp;
    // Push receiver and allocate locals (no args for these control-flow helpers)
    try interp.push(block_receiver);
    const total_temps = interp.method.header.num_temps;
    const local_temps = total_temps;
    var k: usize = 0;
    while (k < local_temps) : (k += 1) {
        try interp.push(Value.nil);
    }
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        interp.heap_context = saved_heap_context;
        interp.home_heap_context = saved_home_heap_context;
        return err;
    };

    interp.primitive_block_depth -= 1;
    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;
    interp.heap_context = saved_heap_context;
    interp.home_heap_context = saved_home_heap_context;

    return result;
}

// True >> ifTrue: aBlock - evaluate block and return result
fn primTrueIfTrue(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const recv = try interp.pop();
    _ = recv; // Should be true, but we don't check since method is installed on True

    return evaluateBlock(interp, block);
}

// True >> ifFalse: aBlock - return nil (don't evaluate)
fn primTrueIfFalse(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // block (not evaluated)
    _ = try interp.pop(); // receiver (true)
    return Value.nil;
}

// True >> ifTrue: trueBlock ifFalse: falseBlock - evaluate trueBlock
fn primTrueIfTrueIfFalse(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // falseBlock (not evaluated)
    const true_block = try interp.pop();
    const recv = try interp.pop();
    _ = recv;

    return evaluateBlock(interp, true_block);
}

// True >> ifFalse: falseBlock ifTrue: trueBlock - evaluate trueBlock
fn primTrueIfFalseIfTrue(interp: *Interpreter) InterpreterError!Value {
    const true_block = try interp.pop();
    _ = try interp.pop(); // falseBlock (not evaluated)
    const recv = try interp.pop();
    _ = recv;

    return evaluateBlock(interp, true_block);
}

// False >> ifTrue: aBlock - return nil (don't evaluate)
fn primFalseIfTrue(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // block (not evaluated)
    _ = try interp.pop(); // receiver (false)
    return Value.nil;
}

// False >> ifFalse: aBlock - evaluate block and return result
fn primFalseIfFalse(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const recv = try interp.pop();
    _ = recv;

    return evaluateBlock(interp, block);
}

// False >> ifTrue: trueBlock ifFalse: falseBlock - evaluate falseBlock
fn primFalseIfTrueIfFalse(interp: *Interpreter) InterpreterError!Value {
    const false_block = try interp.pop();
    _ = try interp.pop(); // trueBlock (not evaluated)
    const recv = try interp.pop();
    _ = recv;

    return evaluateBlock(interp, false_block);
}

// False >> ifFalse: falseBlock ifTrue: trueBlock - evaluate falseBlock
fn primFalseIfFalseIfTrue(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // trueBlock (not evaluated)
    const false_block = try interp.pop();
    const recv = try interp.pop();
    _ = recv;

    return evaluateBlock(interp, false_block);
}

// Boolean >> not
fn primBooleanNot(interp: *Interpreter) InterpreterError!Value {
    const recv = try interp.pop();

    if (recv.isTrue()) {
        return Value.@"false";
    } else if (recv.isFalse()) {
        return Value.@"true";
    }

    try interp.push(recv);
    return InterpreterError.PrimitiveFailed;
}

// True >> and: aBlock - evaluate block and return result
fn primTrueAnd(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const recv = try interp.pop();
    _ = recv; // Should be true

    return evaluateBlock(interp, block);
}

// True >> or: aBlock - return true without evaluating block
fn primTrueOr(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // block (not evaluated)
    _ = try interp.pop(); // recv (true)

    return Value.@"true";
}

// False >> and: aBlock - return false without evaluating block
fn primFalseAnd(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // block (not evaluated)
    _ = try interp.pop(); // recv (false)

    return Value.@"false";
}

// False >> or: aBlock - evaluate block and return result
fn primFalseOr(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const recv = try interp.pop();
    _ = recv; // Should be false

    return evaluateBlock(interp, block);
}

// ============================================================================
// Loop Control Flow Primitives
// ============================================================================

// [condition] whileTrue: [body] - evaluates body while condition returns true
fn primWhileTrue(interp: *Interpreter) InterpreterError!Value {
    const body_block = try interp.pop();
    const cond_block = try interp.pop();

    // Validate both are blocks
    if (!cond_block.isObject() or !body_block.isObject()) {
        try interp.push(cond_block);
        try interp.push(body_block);
        return InterpreterError.PrimitiveFailed;
    }

    const cond_obj = cond_block.asObject();
    const body_obj = body_block.asObject();

    if (cond_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE or
        body_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE)
    {
        try interp.push(cond_block);
        try interp.push(body_block);
        return InterpreterError.PrimitiveFailed;
    }

    // Loop: evaluate condition, if true evaluate body, repeat
    var iteration_count: usize = 0;
    const max_iterations: usize = 1000000; // Safety limit

    while (iteration_count < max_iterations) : (iteration_count += 1) {
        // Evaluate condition
        const cond_result = evaluateBlock(interp, cond_block) catch |err| {
            return err;
        };

        // Check if condition is true
        if (!cond_result.isTrue()) {
            break;
        }

        // Evaluate body (result is discarded)
        _ = evaluateBlock(interp, body_block) catch |err| {
            return err;
        };
    }

    return Value.nil;
}

// [condition] whileFalse: [body] - evaluates body while condition returns false
fn primWhileFalse(interp: *Interpreter) InterpreterError!Value {
    const body_block = try interp.pop();
    const cond_block = try interp.pop();

    // Validate both are blocks
    if (!cond_block.isObject() or !body_block.isObject()) {
        try interp.push(cond_block);
        try interp.push(body_block);
        return InterpreterError.PrimitiveFailed;
    }

    const cond_obj = cond_block.asObject();
    const body_obj = body_block.asObject();

    if (cond_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE or
        body_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE)
    {
        try interp.push(cond_block);
        try interp.push(body_block);
        return InterpreterError.PrimitiveFailed;
    }

    // Loop: evaluate condition, if false evaluate body, repeat
    var iteration_count: usize = 0;
    const max_iterations: usize = 1000000; // Safety limit

    while (iteration_count < max_iterations) : (iteration_count += 1) {
        // Evaluate condition
        const cond_result = evaluateBlock(interp, cond_block) catch |err| {
            return err;
        };

        // Check if condition is false
        if (!cond_result.isFalse()) {
            break;
        }

        // Evaluate body (result is discarded)
        _ = evaluateBlock(interp, body_block) catch |err| {
            return err;
        };
    }

    return Value.nil;
}

// ============================================================================
// Interval/Loop Primitives
// ============================================================================

// Integer >> to:do: - iterate from receiver to limit, calling block with each value
fn primToDo(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const limit = try interp.pop();
    const start = try interp.pop();

    // Validate inputs
    if (!start.isSmallInt() or !limit.isSmallInt() or !block.isObject()) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerTempBase, startPC, numArgs, method, receiver]
    const outer_temp_base_val = block_obj.getField(0, 6);
    const start_pc = block_obj.getField(1, 6);
    const num_args_val = block_obj.getField(2, 6);
    const method_val = block_obj.getField(3, 6);
    const block_receiver = block_obj.getField(4, 6);

    if (!outer_temp_base_val.isSmallInt() or !start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 1) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Get home_temp_base from block
    const home_temp_base_val = block_obj.getField(5, 6);

    // Loop from start to limit with step 1
    const start_val = start.asSmallInt();
    const limit_val = limit.asSmallInt();

    var i = start_val;
    while (i <= limit_val) : (i += 1) {
        // Save interpreter state
        const saved_ip = interp.ip;
        const saved_method = interp.method;
        const saved_sp = interp.sp;
        const saved_temp_base = interp.temp_base;
        const saved_outer_temp_base = interp.outer_temp_base;
        const saved_home_temp_base = interp.home_temp_base;
        const saved_receiver = interp.receiver;
        const saved_context_ptr = interp.context_ptr;

        interp.method = @ptrCast(@alignCast(method_val.asObject()));
        interp.ip = @intCast(start_pc.asSmallInt());
        interp.receiver = block_receiver;
        interp.outer_temp_base = @intCast(outer_temp_base_val.asSmallInt());
        interp.home_temp_base = if (home_temp_base_val.isSmallInt()) @intCast(home_temp_base_val.asSmallInt()) else interp.temp_base;
        interp.temp_base = interp.sp - 1;
        interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
        interp.primitive_block_depth += 1;

        // Push the loop variable as argument
        try interp.push(Value.fromSmallInt(i));

        // Execute the block
        _ = interp.interpretLoop() catch |err| {
            interp.primitive_block_depth -= 1;
                        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
            interp.ip = saved_ip;
            interp.method = saved_method;
            if (err != InterpreterError.BlockNonLocalReturn) {
                interp.sp = saved_sp;
            }
            interp.temp_base = saved_temp_base;
            interp.outer_temp_base = saved_outer_temp_base;
            interp.home_temp_base = saved_home_temp_base;
            interp.receiver = saved_receiver;
            interp.context_ptr = saved_context_ptr;
            return err;
        };

        interp.primitive_block_depth -= 1;
                // Restore interpreter state
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.sp = saved_sp;
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
    }

    return Value.nil;
}

// Integer >> to:by:do: - iterate from receiver to limit by step, calling block with each value
fn primToByDo(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const step = try interp.pop();
    const limit = try interp.pop();
    const start = try interp.pop();

    // Validate inputs
    if (!start.isSmallInt() or !limit.isSmallInt() or !step.isSmallInt() or !block.isObject()) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(step);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(step);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerTempBase, startPC, numArgs, method, receiver]
    const outer_temp_base_val = block_obj.getField(0, 6);
    const start_pc = block_obj.getField(1, 6);
    const num_args_val = block_obj.getField(2, 6);
    const method_val = block_obj.getField(3, 6);
    const block_receiver = block_obj.getField(4, 6);

    if (!outer_temp_base_val.isSmallInt() or !start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(step);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 1) {
        try interp.push(start);
        try interp.push(limit);
        try interp.push(step);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Loop from start to limit with given step
    const start_val = start.asSmallInt();
    const limit_val = limit.asSmallInt();
    const step_val = step.asSmallInt();

    if (step_val == 0) {
        // Invalid step
        try interp.push(start);
        try interp.push(limit);
        try interp.push(step);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    // Handle both positive and negative steps
    var i = start_val;
    if (step_val > 0) {
        while (i <= limit_val) : (i += step_val) {
            // Save interpreter state
            const saved_ip = interp.ip;
            const saved_method = interp.method;
            const saved_sp = interp.sp;
            const saved_temp_base = interp.temp_base;
            const saved_outer_temp_base = interp.outer_temp_base;
            const saved_receiver = interp.receiver;
            const saved_context_ptr = interp.context_ptr;
            
            interp.method = @ptrCast(@alignCast(method_val.asObject()));
            interp.ip = @intCast(start_pc.asSmallInt());
            interp.receiver = block_receiver;
            interp.outer_temp_base = @intCast(outer_temp_base_val.asSmallInt());
            interp.temp_base = interp.sp - 1;
            interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
            interp.primitive_block_depth += 1;

            // Push the loop variable as argument
            try interp.push(Value.fromSmallInt(i));

            // Execute the block
            _ = interp.interpretLoop() catch |err| {
                interp.primitive_block_depth -= 1;
                                if (err == InterpreterError.BlockNonLocalReturn) {
                    const result = try interp.pop();
                    interp.ip = saved_ip;
                    interp.method = saved_method;
                    interp.sp = saved_sp;
                    interp.temp_base = saved_temp_base;
                    interp.outer_temp_base = saved_outer_temp_base;
                    interp.receiver = saved_receiver;
                    interp.context_ptr = saved_context_ptr;
                    try interp.push(result);
                    return err;
                }
                interp.ip = saved_ip;
                interp.method = saved_method;
                interp.sp = saved_sp;
                interp.temp_base = saved_temp_base;
                interp.outer_temp_base = saved_outer_temp_base;
                interp.receiver = saved_receiver;
                interp.context_ptr = saved_context_ptr;
                return err;
            };

            interp.primitive_block_depth -= 1;
                        // Restore interpreter state
            interp.ip = saved_ip;
            interp.method = saved_method;
            interp.sp = saved_sp;
            interp.temp_base = saved_temp_base;
            interp.outer_temp_base = saved_outer_temp_base;
            interp.receiver = saved_receiver;
            interp.context_ptr = saved_context_ptr;
        }
    } else {
        while (i >= limit_val) : (i += step_val) {
            // Save interpreter state
            const saved_ip = interp.ip;
            const saved_method = interp.method;
            const saved_sp = interp.sp;
            const saved_temp_base = interp.temp_base;
            const saved_outer_temp_base = interp.outer_temp_base;
            const saved_receiver = interp.receiver;
            const saved_context_ptr = interp.context_ptr;
            
            interp.method = @ptrCast(@alignCast(method_val.asObject()));
            interp.ip = @intCast(start_pc.asSmallInt());
            interp.receiver = block_receiver;
            interp.outer_temp_base = @intCast(outer_temp_base_val.asSmallInt());
            interp.temp_base = interp.sp - 1;
            interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
            interp.primitive_block_depth += 1;

            // Push the loop variable as argument
            try interp.push(Value.fromSmallInt(i));

            // Execute the block
            _ = interp.interpretLoop() catch |err| {
                interp.primitive_block_depth -= 1;
                                if (err == InterpreterError.BlockNonLocalReturn) {
                    const result = try interp.pop();
                    interp.ip = saved_ip;
                    interp.method = saved_method;
                    interp.sp = saved_sp;
                    interp.temp_base = saved_temp_base;
                    interp.outer_temp_base = saved_outer_temp_base;
                    interp.receiver = saved_receiver;
                    interp.context_ptr = saved_context_ptr;
                    try interp.push(result);
                    return err;
                }
                interp.ip = saved_ip;
                interp.method = saved_method;
                interp.sp = saved_sp;
                interp.temp_base = saved_temp_base;
                interp.outer_temp_base = saved_outer_temp_base;
                interp.receiver = saved_receiver;
                interp.context_ptr = saved_context_ptr;
                return err;
            };

            interp.primitive_block_depth -= 1;
                        // Restore interpreter state
            interp.ip = saved_ip;
            interp.method = saved_method;
            interp.sp = saved_sp;
            interp.temp_base = saved_temp_base;
            interp.outer_temp_base = saved_outer_temp_base;
            interp.receiver = saved_receiver;
            interp.context_ptr = saved_context_ptr;
        }
    }

    return Value.nil;
}

// ============================================================================
// Collection Primitives
// ============================================================================

/// Helper to evaluate a block with one argument (used by collection operations)
fn evaluateBlockWith1(interp: *Interpreter, block: Value, arg: Value) InterpreterError!Value {
    if (!block.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerTempBase, startPC, numArgs, method, receiver, homeTempBase]
    const outer_temp_base_val = block_obj.getField(0, 6);
    const start_pc = block_obj.getField(1, 6);
    const num_args_val = block_obj.getField(2, 6);
    const method_val = block_obj.getField(3, 6);
    const block_receiver = block_obj.getField(4, 6);
    const home_temp_base_val = block_obj.getField(5, 6);

    if (!outer_temp_base_val.isSmallInt() or !start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 1) {
        return InterpreterError.PrimitiveFailed;
    }

    const outer_base: usize = @intCast(outer_temp_base_val.asSmallInt());

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_home_temp_base = interp.home_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    
    const block_method_ptr: *CompiledMethod = @ptrCast(@alignCast(method_val.asObject()));
    interp.method = block_method_ptr;
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver;
    interp.outer_temp_base = @intCast(outer_temp_base_val.asSmallInt());
    interp.home_temp_base = if (home_temp_base_val.isSmallInt()) @intCast(home_temp_base_val.asSmallInt()) else interp.temp_base;
    interp.temp_base = interp.sp;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    const home_from_block = if (home_temp_base_val.isSmallInt()) home_temp_base_val.asSmallInt() else -999;
    if (DEBUG_VERBOSE) std.debug.print("DEBUG evaluateBlockWith1: saved_temp_base={} block_stored_home={} interp.home_temp_base={} depth={}\n", .{ saved_temp_base, home_from_block, interp.home_temp_base, interp.primitive_block_depth });

    // Push receiver and argument
    try interp.push(block_receiver);
    try interp.push(arg);
    const total_temps = interp.method.header.num_temps;
    const local_temps = if (total_temps > 1) total_temps - 1 else 0;
    var k: usize = 0;
    while (k < local_temps) : (k += 1) {
        try interp.push(Value.nil);
    }

    // Targeted debug for captured temps in problematic methods
    const block_lits_dbg = block_method_ptr.getLiterals();
    if (block_lits_dbg.len > 0 and block_lits_dbg[block_lits_dbg.len - 1].isObject()) {
        const src_obj = block_lits_dbg[block_lits_dbg.len - 1].asObject();
        if (src_obj.header.class_index == Heap.CLASS_STRING) {
            const src_bytes = src_obj.bytes(src_obj.header.size);
            if (std.mem.indexOf(u8, src_bytes, "basicBeginsWith") != null and outer_base + 2 < interp.stack.len) {
                const slot0 = interp.stack[outer_base + 1];
                const slot1 = interp.stack[outer_base + 2];
                const name0 = blk: {
                    const cls = interp.heap.classOf(slot0);
                    if (cls.isObject()) {
                        const name_val = cls.asObject().getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                        if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                            break :blk name_val.asObject().bytes(name_val.asObject().header.size);
                        }
                    }
                    if (slot0.isSmallInt()) break :blk "SmallInteger";
                    if (slot0.isNil()) break :blk "nil";
                    break :blk "<?>";
                };
                const name1 = blk: {
                    const cls = interp.heap.classOf(slot1);
                    if (cls.isObject()) {
                        const name_val = cls.asObject().getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                        if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                            break :blk name_val.asObject().bytes(name_val.asObject().header.size);
                        }
                    }
                    if (slot1.isSmallInt()) break :blk "SmallInteger";
                    if (slot1.isNil()) break :blk "nil";
                    break :blk "<?>";
                };
                if (DEBUG_VERBOSE) std.debug.print("DEBUG block capture outer_base={} slot0={s} slot1={s} sp={}\n", .{
                    outer_base, name0, name1, interp.sp,
                });
            }
        }
    }

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
                interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.home_temp_base = saved_home_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        return err;
    };

    interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.home_temp_base = saved_home_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;

    return result;
}

/// Helper to evaluate a block with two arguments
fn evaluateBlockWith2(interp: *Interpreter, block: Value, arg1: Value, arg2: Value) InterpreterError!Value {
    if (!block.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerTempBase, startPC, numArgs, method, receiver]
    const outer_temp_base_val = block_obj.getField(0, 6);
    const start_pc = block_obj.getField(1, 6);
    const num_args_val = block_obj.getField(2, 6);
    const method_val = block_obj.getField(3, 6);
    const block_receiver = block_obj.getField(4, 6);

    if (!outer_temp_base_val.isSmallInt() or !start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 2) {
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_sp = interp.sp;
    const saved_temp_base = interp.temp_base;
    const saved_outer_temp_base = interp.outer_temp_base;
    const saved_receiver = interp.receiver;
    const saved_context_ptr = interp.context_ptr;
    
    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.receiver = block_receiver;
    interp.outer_temp_base = @intCast(outer_temp_base_val.asSmallInt());
    interp.temp_base = interp.sp;
    interp.primitive_block_bases[interp.primitive_block_depth] = interp.context_ptr;
    interp.primitive_block_depth += 1;

    // Push receiver and arguments
    try interp.push(block_receiver);
    try interp.push(arg1);
    try interp.push(arg2);
    const total_temps = interp.method.header.num_temps;
    const local_temps = if (total_temps > 2) total_temps - 2 else 0;
    var k: usize = 0;
    while (k < local_temps) : (k += 1) {
        try interp.push(Value.nil);
    }

    const result = interp.interpretLoop() catch |err| {
        interp.primitive_block_depth -= 1;
                interp.ip = saved_ip;
        interp.method = saved_method;

        // For BlockNonLocalReturn, always propagate - let the .send bytecode handler intercept it
        if (err != InterpreterError.BlockNonLocalReturn) {
            interp.sp = saved_sp;
        }
        interp.temp_base = saved_temp_base;
        interp.outer_temp_base = saved_outer_temp_base;
        interp.receiver = saved_receiver;
        interp.context_ptr = saved_context_ptr;
        return err;
    };

    interp.primitive_block_depth -= 1;
        interp.ip = saved_ip;
    interp.method = saved_method;
    interp.sp = saved_sp;
    interp.temp_base = saved_temp_base;
    interp.outer_temp_base = saved_outer_temp_base;
    interp.receiver = saved_receiver;
    interp.context_ptr = saved_context_ptr;

    return result;
}

// Array >> do: aBlock
// Iterate over elements: #(1 2 3) do: [:x | x printString]
fn primArrayDo(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const array_obj = array.asObject();

    if (std.debug.runtime_safety) {
        const block_obj = block.asObject();
        if (block_obj.header.class_index == Heap.CLASS_BLOCK_CLOSURE) {
            const outer_base_val = block_obj.getField(0, 6);
            if (outer_base_val.isSmallInt()) {
                const outer_base: usize = @intCast(outer_base_val.asSmallInt());
                if (outer_base + 2 < interp.stack.len) {
                    const slot0 = interp.stack[outer_base + 1];
                    const slot1 = interp.stack[outer_base + 2];
                    const name0 = blk: {
                        const cls = interp.heap.classOf(slot0);
                        if (cls.isObject()) {
                            const name_val = cls.asObject().getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                            if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                break :blk name_val.asObject().bytes(name_val.asObject().header.size);
                            }
                        }
                        if (slot0.isSmallInt()) break :blk "SmallInteger";
                        if (slot0.isNil()) break :blk "nil";
                        break :blk "<?>";            
                    };
                    const name1 = blk: {
                        const cls = interp.heap.classOf(slot1);
                        if (cls.isObject()) {
                            const name_val = cls.asObject().getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                            if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                break :blk name_val.asObject().bytes(name_val.asObject().header.size);
                            }
                        }
                        if (slot1.isSmallInt()) break :blk "SmallInteger";
                        if (slot1.isNil()) break :blk "nil";
                        break :blk "<?>";            
                    };
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG arrayDo block outer_base={} slot0={s} slot1={s}\n", .{ outer_base, name0, name1 });
                }
            }
        }
    }

    // Get array size from object header (not class format - that's for fixed fields)
    const size: usize = array_obj.header.size;

    // Iterate over each element
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        _ = try evaluateBlockWith1(interp, block, elem);
    }

    return array; // Return the array itself
}

// Array >> collect: aBlock
// Transform elements: #(1 2 3) collect: [:x | x * 2] => #(2 4 6)
fn primArrayCollect(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const array_obj = array.asObject();

    // Get array size from object header
    const size: usize = array_obj.header.size;

    // Create result array
    const result = try interp.heap.allocateObject(Heap.CLASS_ARRAY, size, .variable);

    // Collect transformed elements
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const transformed = try evaluateBlockWith1(interp, block, elem);
        result.setField(i, transformed, size);
    }

    return Value.fromObject(result);
}

// Array >> select: aBlock
// Filter elements: #(1 2 3 4 5) select: [:x | x > 2] => #(3 4 5)
fn primArraySelect(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    if (std.debug.runtime_safety) {
        const block_obj = block.asObject();
        if (block_obj.header.class_index == Heap.CLASS_BLOCK_CLOSURE) {
            const outer_base_val = block_obj.getField(0, 6);
            if (outer_base_val.isSmallInt()) {
                const outer_base: usize = @intCast(outer_base_val.asSmallInt());
                if (outer_base + 2 < interp.stack.len) {
                    const slot0 = interp.stack[outer_base + 1];
                    const slot1 = interp.stack[outer_base + 2];
                    const name0 = blk: {
                        const cls = interp.heap.classOf(slot0);
                        if (cls.isObject()) {
                            const name_val = cls.asObject().getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                            if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                break :blk name_val.asObject().bytes(name_val.asObject().header.size);
                            }
                        }
                        if (slot0.isSmallInt()) break :blk "SmallInteger";
                        if (slot0.isNil()) break :blk "nil";
                        break :blk "<?>";            
                    };
                    const name1 = blk: {
                        const cls = interp.heap.classOf(slot1);
                        if (cls.isObject()) {
                            const name_val = cls.asObject().getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
                            if (name_val.isObject() and name_val.asObject().header.class_index == Heap.CLASS_SYMBOL) {
                                break :blk name_val.asObject().bytes(name_val.asObject().header.size);
                            }
                        }
                        if (slot1.isSmallInt()) break :blk "SmallInteger";
                        if (slot1.isNil()) break :blk "nil";
                        break :blk "<?>";            
                    };
                    if (DEBUG_VERBOSE) std.debug.print("DEBUG select block outer_base={} slot0={s} slot1={s}\n", .{ outer_base, name0, name1 });
                }
            }
        }
    }

    const array_obj = array.asObject();

    // Get array size from object header
    const size: usize = array_obj.header.size;

    // First pass: count matching elements
    var count: usize = 0;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const matches = try evaluateBlockWith1(interp, block, elem);
        if (matches.isTrue()) {
            count += 1;
        }
    }

    // Create result array
    const result = try interp.heap.allocateObject(Heap.CLASS_ARRAY, count, .variable);

    // Second pass: collect matching elements
    var j: usize = 0;
    i = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const matches = try evaluateBlockWith1(interp, block, elem);
        if (matches.isTrue()) {
            result.setField(j, elem, count);
            j += 1;
        }
    }

    return Value.fromObject(result);
}

// Array >> reject: aBlock
// Opposite of select
fn primArrayReject(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const array_obj = array.asObject();

    // Get array size from object header
    const size: usize = array_obj.header.size;

    // First pass: count non-matching elements
    var count: usize = 0;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const matches = try evaluateBlockWith1(interp, block, elem);
        if (!matches.isTrue()) {
            count += 1;
        }
    }

    // Create result array
    const result = try interp.heap.allocateObject(Heap.CLASS_ARRAY, count, .variable);

    // Second pass: collect non-matching elements
    var j: usize = 0;
    i = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const matches = try evaluateBlockWith1(interp, block, elem);
        if (!matches.isTrue()) {
            result.setField(j, elem, count);
            j += 1;
        }
    }

    return Value.fromObject(result);
}

// Array >> detect: aBlock
// Find first matching element, error if not found
fn primArrayDetect(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const array_obj = array.asObject();

    // Get array size from object header
    const size: usize = array_obj.header.size;

    // Find first matching element
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const matches = try evaluateBlockWith1(interp, block, elem);
        if (matches.isTrue()) {
            return elem;
        }
    }

    // Not found - fail the primitive so Smalltalk code can handle it
    try interp.push(array);
    try interp.push(block);
    return InterpreterError.PrimitiveFailed;
}

// Array >> detect: aBlock ifNone: exceptionBlock
// Find first matching element, evaluate exceptionBlock if not found
fn primArrayDetectIfNone(interp: *Interpreter) InterpreterError!Value {
    const exception_block = try interp.pop();
    const detect_block = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(detect_block);
        try interp.push(exception_block);
        return InterpreterError.PrimitiveFailed;
    }

    const array_obj = array.asObject();

    // Get array size from object header
    const size: usize = array_obj.header.size;

    // Find first matching element
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        const matches = try evaluateBlockWith1(interp, detect_block, elem);
        if (matches.isTrue()) {
            return elem;
        }
    }

    // Not found - evaluate exception block
    return evaluateBlock(interp, exception_block);
}

// Array >> inject: initialValue into: aBlock
// Fold/reduce: #(1 2 3 4) inject: 0 into: [:sum :x | sum + x] => 10
fn primArrayInjectInto(interp: *Interpreter) InterpreterError!Value {
    const block = try interp.pop();
    const initial = try interp.pop();
    const array = try interp.pop();

    if (!array.isObject()) {
        try interp.push(array);
        try interp.push(initial);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const array_obj = array.asObject();

    // Get array size from object header
    const size: usize = array_obj.header.size;

    // Fold over elements
    var accumulator = initial;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        accumulator = try evaluateBlockWith2(interp, block, accumulator, elem);
    }

    return accumulator;
}

// ============================================================================
// Process/Semaphore Primitives (Dolphin compatible: 85-100, 156, 189)
// ============================================================================

const scheduler = @import("scheduler.zig");
const Scheduler = scheduler.Scheduler;
const Process = scheduler.Process;
const ProcessState = scheduler.ProcessState;
const SemaphoreFields = scheduler.SemaphoreFields;
const ProcessFields = scheduler.ProcessFields;

/// Primitive 85: Semaphore >> signal
/// Signal the semaphore - wake one waiting process or increment excess signals
fn primSemaphoreSignal(interp: *Interpreter) InterpreterError!Value {
    const sem = try interp.pop();

    if (!sem.isObject()) {
        try interp.push(sem);
        return InterpreterError.PrimitiveFailed;
    }

    const sem_obj = sem.asObject();

    // Get current excess signals count
    const signals_val = sem_obj.getField(SemaphoreFields.signals, 3);
    if (!signals_val.isSmallInt()) {
        try interp.push(sem);
        return InterpreterError.PrimitiveFailed;
    }

    const current_signals = signals_val.asSmallInt();

    // Check if there are waiting processes (first link of LinkedList)
    const first_link = sem_obj.getField(SemaphoreFields.firstLink, 3);

    if (first_link.isNil()) {
        // No waiters - increment excess signals
        sem_obj.setField(SemaphoreFields.signals, Value.fromSmallInt(current_signals + 1), 3);
    } else {
        // Wake up the first waiter
        // For now, we just decrement signals and let the process continue
        // Full implementation would need to remove from wait queue and add to ready queue
        // This is a simplified version - the Smalltalk code handles most of the logic
        if (current_signals > 0) {
            sem_obj.setField(SemaphoreFields.signals, Value.fromSmallInt(current_signals - 1), 3);
        }
    }

    return sem; // Answer the receiver
}

/// Primitive 86: Semaphore >> wait:ret:
/// Wait on semaphore with timeout. Answer WAIT_OBJECT_0 (0) on success, WAIT_TIMEOUT (258) on timeout
fn primSemaphoreWait(interp: *Interpreter) InterpreterError!Value {
    // Arguments: semaphore (receiver), timeout, returnValueHolder
    const ret_holder = try interp.pop();
    const timeout = try interp.pop();
    const sem = try interp.pop();

    if (!sem.isObject()) {
        try interp.push(sem);
        try interp.push(timeout);
        try interp.push(ret_holder);
        return InterpreterError.PrimitiveFailed;
    }

    const sem_obj = sem.asObject();

    // Get current excess signals count
    const signals_val = sem_obj.getField(SemaphoreFields.signals, 3);
    if (!signals_val.isSmallInt()) {
        try interp.push(sem);
        try interp.push(timeout);
        try interp.push(ret_holder);
        return InterpreterError.PrimitiveFailed;
    }

    const current_signals = signals_val.asSmallInt();

    // WAIT_OBJECT_0 = 0, WAIT_TIMEOUT = 258
    const WAIT_OBJECT_0: i61 = 0;
    const WAIT_TIMEOUT: i61 = 258;

    if (current_signals > 0) {
        // Semaphore has excess signals - consume one and return immediately
        sem_obj.setField(SemaphoreFields.signals, Value.fromSmallInt(current_signals - 1), 3);

        // Store result in return value holder
        if (ret_holder.isObject()) {
            const holder_obj = ret_holder.asObject();
            holder_obj.setField(0, Value.fromSmallInt(WAIT_OBJECT_0), holder_obj.header.size);
        }

        return ret_holder;
    }

    // No excess signals - check timeout
    if (timeout.isSmallInt()) {
        const timeout_ms = timeout.asSmallInt();
        if (timeout_ms == 0) {
            // Zero timeout means "don't wait, poll"
            if (ret_holder.isObject()) {
                const holder_obj = ret_holder.asObject();
                holder_obj.setField(0, Value.fromSmallInt(WAIT_TIMEOUT), holder_obj.header.size);
            }
            return ret_holder;
        }
        // For INFINITE (-1) or positive timeout, we would block
        // For now, simplified: just return success after consuming signal (if any)
    }

    // Full blocking wait would require process switching
    // For now, return timeout for no signals case
    if (ret_holder.isObject()) {
        const holder_obj = ret_holder.asObject();
        holder_obj.setField(0, Value.fromSmallInt(WAIT_TIMEOUT), holder_obj.header.size);
    }

    return ret_holder;
}

/// Primitive 87: Process >> resume
/// Move a suspended process to the ready queue and potentially switch to it
fn primProcessResume(interp: *Interpreter) InterpreterError!Value {
    const process_val = try interp.pop();

    if (!process_val.isObject()) {
        try interp.push(process_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Find or create the VM-side Process
    var vm_process = interp.process_scheduler.findProcess(process_val);
    if (vm_process == null) {
        // Create a new VM process for this Smalltalk Process object
        const process_obj = process_val.asObject();
        const priority_val = process_obj.getField(ProcessFields.priority, 6);
        const priority: u8 = if (priority_val.isSmallInt())
            @intCast(@max(1, @min(10, priority_val.asSmallInt())))
        else
            5;
        vm_process = interp.process_scheduler.createProcess(process_val, priority) catch {
            try interp.push(process_val);
            return InterpreterError.PrimitiveFailed;
        };
    }

    const proc = vm_process.?;

    // If process is already running or terminated, just return it
    if (proc.state == .running or proc.state == .terminated) {
        return process_val;
    }

    // Add to ready queue
    interp.process_scheduler.makeReady(proc);

    // Just add to ready queue - don't switch immediately
    // The process will run when the current process yields, blocks, or terminates
    // This is the simpler and more predictable behavior
    // (Immediate preemption can be added later if needed)

    return process_val;
}

/// Primitive 88: Process >> suspend
/// Remove the process from ready queue and suspend it
fn primProcessSuspend(interp: *Interpreter) InterpreterError!Value {
    const process_val = try interp.pop();

    if (!process_val.isObject()) {
        try interp.push(process_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Find the VM-side Process
    const vm_process = interp.process_scheduler.findProcess(process_val);
    if (vm_process == null) {
        // Process not known to scheduler, just return it
        return process_val;
    }

    const proc = vm_process.?;

    // If this is the active process, we need to switch to another
    if (proc.state == .running) {
        proc.state = .suspended;

        // Save current state
        interp.saveContextToProcess(proc);

        // Find next process to run
        const next = interp.process_scheduler.findHighestPriorityReady();
        if (next) |next_proc| {
            interp.process_scheduler.removeFromReadyQueue(next_proc);
            interp.restoreContextFromProcess(next_proc);
            next_proc.state = .running;
            interp.process_scheduler.active_process = next_proc;
        } else {
            // No other process to run - this is a problem in real system
            // For now, just keep running
            proc.state = .running;
        }
    } else if (proc.state == .ready) {
        // Remove from ready queue
        interp.process_scheduler.removeFromReadyQueue(proc);
        proc.state = .suspended;
    }

    return process_val;
}

/// Primitive 91: Process >> primTerminate
/// Terminate a process
fn primProcessTerminate(interp: *Interpreter) InterpreterError!Value {
    const process_val = try interp.pop();

    if (!process_val.isObject()) {
        try interp.push(process_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Mark Smalltalk process as terminated
    const process_obj = process_val.asObject();
    process_obj.setField(ProcessFields.suspendedFrame, Value.nil, 6);

    // Find the VM-side Process
    const vm_process = interp.process_scheduler.findProcess(process_val);
    if (vm_process) |proc| {
        // If this is the active process, switch to another
        if (proc.state == .running) {
            proc.state = .terminated;

            const next = interp.process_scheduler.findHighestPriorityReady();
            if (next) |next_proc| {
                interp.process_scheduler.removeFromReadyQueue(next_proc);
                interp.restoreContextFromProcess(next_proc);
                next_proc.state = .running;
                interp.process_scheduler.active_process = next_proc;
            } else {
                interp.process_scheduler.active_process = null;
            }
        } else if (proc.state == .ready) {
            interp.process_scheduler.removeFromReadyQueue(proc);
            proc.state = .terminated;
        } else {
            proc.state = .terminated;
        }
    }

    return Value.@"true";
}

/// Primitive 92: Process >> priority:
/// Set process priority, answer previous priority
fn primProcessSetPriority(interp: *Interpreter) InterpreterError!Value {
    const new_priority = try interp.pop();
    const process = try interp.pop();

    if (!process.isObject() or !new_priority.isSmallInt()) {
        try interp.push(process);
        try interp.push(new_priority);
        return InterpreterError.PrimitiveFailed;
    }

    const priority = new_priority.asSmallInt();
    if (priority < 1 or priority > 10) {
        try interp.push(process);
        try interp.push(new_priority);
        return InterpreterError.PrimitiveFailed;
    }

    const process_obj = process.asObject();

    // Get old priority
    const old_priority = process_obj.getField(ProcessFields.priority, 16);

    // Set new priority
    process_obj.setField(ProcessFields.priority, new_priority, 16);

    return old_priority;
}

/// Primitive 95: Processor >> enableAsyncEvents:
/// Enable or disable async events (process switching)
fn primEnableAsyncEvents(interp: *Interpreter) InterpreterError!Value {
    const enable = try interp.pop();
    _ = try interp.pop(); // processor (receiver)

    const was_enabled = interp.process_scheduler.async_events_enabled;
    interp.process_scheduler.async_events_enabled = enable.isTrue();

    return Value.fromBool(was_enabled);
}

/// Primitive 98: Process >> queueInterrupt:with:
/// Queue an interrupt for a process
fn primProcessQueueInterrupt(interp: *Interpreter) InterpreterError!Value {
    const arg = try interp.pop();
    const interrupt_num = try interp.pop();
    const process = try interp.pop();

    // For now, just return the process
    // Full implementation would queue the interrupt for later delivery
    _ = arg;
    _ = interrupt_num;

    return process;
}

/// Primitive 99: Semaphore >> primSetSignals:
/// Set excess signal count (for pulse/set operations)
fn primSemaphoreSetSignals(interp: *Interpreter) InterpreterError!Value {
    const pulse = try interp.pop();
    const sem = try interp.pop();

    if (!sem.isObject() or !pulse.isSmallInt()) {
        try interp.push(sem);
        try interp.push(pulse);
        return InterpreterError.PrimitiveFailed;
    }

    const sem_obj = sem.asObject();
    sem_obj.setField(SemaphoreFields.signals, pulse, 3);

    return sem;
}

/// Primitive 100: Delay class >> signalTimerAfter:
/// Request that the timing semaphore be signaled after N milliseconds
fn primSignalTimerAfter(interp: *Interpreter) InterpreterError!Value {
    const milliseconds = try interp.pop();
    const receiver = try interp.pop(); // Delay class or timing semaphore

    if (!milliseconds.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(milliseconds);
        return InterpreterError.PrimitiveFailed;
    }

    const ms = milliseconds.asSmallInt();

    // Get the timing semaphore from the scheduler
    const timing_sem = interp.process_scheduler.timing_semaphore;

    if (ms < 0) {
        // Cancel timer
        interp.process_scheduler.setTimer(-1, Value.nil);
    } else {
        // Set timer
        interp.process_scheduler.setTimer(@intCast(ms), timing_sem);
    }

    return receiver;
}

/// Primitive 156: Processor >> yield
/// Yield to other processes at same or higher priority
fn primYield(interp: *Interpreter) InterpreterError!Value {
    const processor = try interp.pop();

    // Check for timer expiration
    if (interp.process_scheduler.checkTimer()) |sem| {
        interp.signalSemaphore(sem);
    }

    // Try to yield to another process
    _ = interp.yieldToNextProcess();

    return processor;
}

/// Primitive 189: Delay class >> microsecondClockValue
/// Get high-resolution microsecond clock value
fn primMicrosecondClockValue(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // receiver

    const us = interp.process_scheduler.microsecondClockValue();

    // Return as SmallInteger (will overflow after ~292 years from VM start)
    return Value.fromSmallInt(@intCast(us));
}

/// Primitive 174: Delay class >> millisecondClockValue
/// Get millisecond clock value
fn primMillisecondClockValue(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // receiver

    const us = interp.process_scheduler.microsecondClockValue();
    const ms = @divFloor(us, 1000);

    return Value.fromSmallInt(@intCast(ms));
}

test "Primitives - arithmetic" {
    const allocator = std.testing.allocator;
    var heap = try memory.Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var interp = Interpreter.init(&heap, allocator);
    defer interp.deinit();

    // Test addition
    try interp.push(Value.fromSmallInt(3));
    try interp.push(Value.fromSmallInt(4));
    const sum = try executePrimitive(&interp, @intFromEnum(Primitive.add));
    try std.testing.expectEqual(@as(i61, 7), sum.asSmallInt());

    // Test subtraction
    try interp.push(Value.fromSmallInt(10));
    try interp.push(Value.fromSmallInt(3));
    const diff = try executePrimitive(&interp, @intFromEnum(Primitive.subtract));
    try std.testing.expectEqual(@as(i61, 7), diff.asSmallInt());

    // Test multiplication
    try interp.push(Value.fromSmallInt(6));
    try interp.push(Value.fromSmallInt(7));
    const prod = try executePrimitive(&interp, @intFromEnum(Primitive.multiply));
    try std.testing.expectEqual(@as(i61, 42), prod.asSmallInt());
}

// ============================================================================
// Float Primitives
// ============================================================================

/// Helper: get f64 from a Value (Float object or SmallInteger)
fn getFloatOrSmallInt(interp: *Interpreter, value: Value) ?f64 {
    if (value.isSmallInt()) {
        return @floatFromInt(value.asSmallInt());
    }
    return interp.heap.getFloatValue(value);
}

fn primFloatAdd(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return interp.heap.allocateFloat(av.? + bv.?) catch {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatSubtract(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return interp.heap.allocateFloat(av.? - bv.?) catch {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatMultiply(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return interp.heap.allocateFloat(av.? * bv.?) catch {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatDivide(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        if (bv.? == 0.0) {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        }
        return interp.heap.allocateFloat(av.? / bv.?) catch {
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatLessThan(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return Value.fromBool(av.? < bv.?);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatGreaterThan(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return Value.fromBool(av.? > bv.?);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatLessOrEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return Value.fromBool(av.? <= bv.?);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatGreaterOrEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return Value.fromBool(av.? >= bv.?);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    const av = getFloatOrSmallInt(interp, a);
    const bv = getFloatOrSmallInt(interp, b);

    if (av != null and bv != null) {
        return Value.fromBool(av.? == bv.?);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatTruncated(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    const av = interp.heap.getFloatValue(a);
    if (av != null) {
        const truncated = @trunc(av.?);
        const max: f64 = @floatFromInt(std.math.maxInt(i61));
        const min: f64 = @floatFromInt(std.math.minInt(i61));
        if (truncated >= min and truncated <= max) {
            return Value.fromSmallInt(@intFromFloat(truncated));
        }
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatAbs(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    const av = interp.heap.getFloatValue(a);
    if (av != null) {
        return interp.heap.allocateFloat(@abs(av.?)) catch {
            try interp.push(a);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

fn primFloatNegate(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    const av = interp.heap.getFloatValue(a);
    if (av != null) {
        return interp.heap.allocateFloat(-av.?) catch {
            try interp.push(a);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

/// Primitive 169: Float >> printString
/// Convert Float to its decimal string representation
fn primFloatPrintString(interp: *Interpreter) InterpreterError!Value {
    const receiver = try interp.pop();

    const fv = interp.heap.getFloatValue(receiver);
    if (fv != null) {
        // Format the float value
        var buf: [64]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "{d}", .{fv.?}) catch {
            try interp.push(receiver);
            return InterpreterError.PrimitiveFailed;
        };

        // Create a String object
        return interp.heap.allocateString(str) catch {
            try interp.push(receiver);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(receiver);
    return InterpreterError.PrimitiveFailed;
}

fn primSmallAsFloat(interp: *Interpreter) InterpreterError!Value {
    const a = try interp.pop();

    if (a.isSmallInt()) {
        const f: f64 = @floatFromInt(a.asSmallInt());
        return interp.heap.allocateFloat(f) catch {
            try interp.push(a);
            return InterpreterError.OutOfMemory;
        };
    }

    try interp.push(a);
    return InterpreterError.PrimitiveFailed;
}

test "Primitives - comparison" {
    const allocator = std.testing.allocator;
    var heap = try memory.Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var interp = Interpreter.init(&heap, allocator);
    defer interp.deinit();

    // Test less than
    try interp.push(Value.fromSmallInt(3));
    try interp.push(Value.fromSmallInt(5));
    const lt = try executePrimitive(&interp, @intFromEnum(Primitive.less_than));
    try std.testing.expect(lt.isTrue());

    try interp.push(Value.fromSmallInt(5));
    try interp.push(Value.fromSmallInt(3));
    const gt = try executePrimitive(&interp, @intFromEnum(Primitive.greater_than));
    try std.testing.expect(gt.isTrue());
}

// ============================================================================
// String Primitives
// ============================================================================

fn primStringConcat(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    // Both must be strings
    if (!a.isObject() or !b.isObject()) {
        try interp.push(a);
        try interp.push(b);
        return InterpreterError.PrimitiveFailed;
    }

    const a_obj = a.asObject();
    const b_obj = b.asObject();

    if (a_obj.header.class_index != memory.Heap.CLASS_STRING or
        b_obj.header.class_index != memory.Heap.CLASS_STRING)
    {
        try interp.push(a);
        try interp.push(b);
        return InterpreterError.PrimitiveFailed;
    }

    const a_bytes = a_obj.bytes(a_obj.header.size);
    const b_bytes = b_obj.bytes(b_obj.header.size);

    const total_len = a_bytes.len + b_bytes.len;

    // Allocate new string
    const new_obj = interp.heap.allocateObject(
        memory.Heap.CLASS_STRING,
        total_len,
        .bytes,
    ) catch {
        try interp.push(a);
        try interp.push(b);
        return InterpreterError.OutOfMemory;
    };

    // Copy bytes
    const new_bytes = new_obj.bytes(total_len);
    @memcpy(new_bytes[0..a_bytes.len], a_bytes);
    @memcpy(new_bytes[a_bytes.len..], b_bytes);

    return Value.fromObject(new_obj);
}

fn primStringCompare(interp: *Interpreter) InterpreterError!Value {
    // String compare returns -1, 0, or 1
    const b = try interp.pop();
    const a = try interp.pop();

    // Both must be strings
    if (!a.isObject() or !b.isObject()) {
        try interp.push(a);
        try interp.push(b);
        return InterpreterError.PrimitiveFailed;
    }

    const a_obj = a.asObject();
    const b_obj = b.asObject();

    if (a_obj.header.class_index != memory.Heap.CLASS_STRING or
        b_obj.header.class_index != memory.Heap.CLASS_STRING)
    {
        try interp.push(a);
        try interp.push(b);
        return InterpreterError.PrimitiveFailed;
    }

    const a_bytes = a_obj.bytes(a_obj.header.size);
    const b_bytes = b_obj.bytes(b_obj.header.size);

    // Compare lexicographically
    const min_len = @min(a_bytes.len, b_bytes.len);
    for (a_bytes[0..min_len], b_bytes[0..min_len]) |ac, bc| {
        if (ac < bc) return Value.fromSmallInt(-1);
        if (ac > bc) return Value.fromSmallInt(1);
    }

    // Prefixes are equal, compare lengths
    if (a_bytes.len < b_bytes.len) return Value.fromSmallInt(-1);
    if (a_bytes.len > b_bytes.len) return Value.fromSmallInt(1);
    return Value.fromSmallInt(0);
}

/// Helper function to compare two strings
fn stringCompareHelper(a: Value, b: Value) ?i2 {
    if (!a.isObject() or !b.isObject()) {
        return null;
    }

    const a_obj = a.asObject();
    const b_obj = b.asObject();

    if (a_obj.header.class_index != memory.Heap.CLASS_STRING or
        b_obj.header.class_index != memory.Heap.CLASS_STRING)
    {
        return null;
    }

    const a_bytes = a_obj.bytes(a_obj.header.size);
    const b_bytes = b_obj.bytes(b_obj.header.size);

    // Compare lexicographically
    const min_len = @min(a_bytes.len, b_bytes.len);
    for (a_bytes[0..min_len], b_bytes[0..min_len]) |ac, bc| {
        if (ac < bc) return -1;
        if (ac > bc) return 1;
    }

    // Prefixes are equal, compare lengths
    if (a_bytes.len < b_bytes.len) return -1;
    if (a_bytes.len > b_bytes.len) return 1;
    return 0;
}

fn primStringLessThan(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (stringCompareHelper(a, b)) |cmp| {
        return Value.fromBool(cmp < 0);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primStringGreaterThan(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (stringCompareHelper(a, b)) |cmp| {
        return Value.fromBool(cmp > 0);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primStringLessOrEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (stringCompareHelper(a, b)) |cmp| {
        return Value.fromBool(cmp <= 0);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primStringGreaterOrEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (stringCompareHelper(a, b)) |cmp| {
        return Value.fromBool(cmp >= 0);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primStringEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (stringCompareHelper(a, b)) |cmp| {
        return Value.fromBool(cmp == 0);
    }

    try interp.push(a);
    try interp.push(b);
    return InterpreterError.PrimitiveFailed;
}

fn primStringCopyFromTo(interp: *Interpreter) InterpreterError!Value {
    // String >> copyFrom: start to: end
    // Returns substring from start to end (1-indexed, inclusive)
    const end_val = try interp.pop();
    const start_val = try interp.pop();
    const string = try interp.pop();

    if (!string.isObject() or !start_val.isSmallInt() or !end_val.isSmallInt()) {
        try interp.push(string);
        try interp.push(start_val);
        try interp.push(end_val);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = string.asObject();
    if (str_obj.header.class_index != memory.Heap.CLASS_STRING) {
        try interp.push(string);
        try interp.push(start_val);
        try interp.push(end_val);
        return InterpreterError.PrimitiveFailed;
    }

    const start_1indexed = start_val.asSmallInt();
    const end_1indexed = end_val.asSmallInt();

    // Convert to 0-indexed
    if (start_1indexed < 1 or end_1indexed < start_1indexed) {
        try interp.push(string);
        try interp.push(start_val);
        try interp.push(end_val);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const start: usize = @intCast(start_1indexed - 1);
    const end: usize = @intCast(end_1indexed);

    if (end > str_bytes.len) {
        try interp.push(string);
        try interp.push(start_val);
        try interp.push(end_val);
        return InterpreterError.PrimitiveFailed;
    }

    const new_len = end - start;

    // Allocate new string
    const new_obj = interp.heap.allocateObject(
        memory.Heap.CLASS_STRING,
        new_len,
        .bytes,
    ) catch {
        try interp.push(string);
        try interp.push(start_val);
        try interp.push(end_val);
        return InterpreterError.OutOfMemory;
    };

    // Copy bytes
    const new_bytes = new_obj.bytes(new_len);
    @memcpy(new_bytes, str_bytes[start..end]);

    return Value.fromObject(new_obj);
}

// ============================================================================
// Error Handling Primitives
// ============================================================================

fn primDoesNotUnderstand(interp: *Interpreter) InterpreterError!Value {
    const message = try interp.pop();
    const recv = try interp.pop();
    interp.last_mnu_receiver = recv;
    interp.last_mnu_method = interp.currentMethodSource();

    // Print error message
    std.debug.print("MessageNotUnderstood: ", .{});

    // Try to print receiver class name
    if (recv.isObject()) {
        const obj = recv.asObject();
        const class = interp.heap.getClass(obj.header.class_index);
        if (class.isObject()) {
            const class_obj = class.asObject();
            const name_val = class_obj.getField(memory.Heap.CLASS_FIELD_NAME, memory.Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                const name_bytes = name_obj.bytes(name_obj.header.size);
                std.debug.print("{s}", .{name_bytes});
            }
        }
    } else if (recv.isSmallInt()) {
        std.debug.print("SmallInteger({d})", .{recv.asSmallInt()});
    } else if (recv.isNil()) {
        std.debug.print("nil", .{});
    } else if (recv.isTrue()) {
        std.debug.print("true", .{});
    } else if (recv.isFalse()) {
        std.debug.print("false", .{});
    }

    std.debug.print(" >> ", .{});

    // Print selector from Message object
    if (message.isObject()) {
        const msg_obj = message.asObject();
        const selector = msg_obj.getField(0, 3);
        if (selector.isObject()) {
            const sel_obj = selector.asObject();
            const sel_bytes = sel_obj.bytes(sel_obj.header.size);
            std.debug.print("{s}", .{sel_bytes});
            interp.last_mnu_selector = sel_bytes;
        }
    }

    std.debug.print("\n", .{});

    return InterpreterError.MessageNotUnderstood;
}

fn primError(interp: *Interpreter) InterpreterError!Value {
    const msg = try interp.pop();
    _ = try interp.pop(); // receiver

    std.debug.print("Error: ", .{});

    // Print error message
    if (msg.isObject()) {
        const msg_obj = msg.asObject();
        if (msg_obj.header.class_index == memory.Heap.CLASS_STRING) {
            const str_bytes = msg_obj.bytes(msg_obj.header.size);
            std.debug.print("{s}", .{str_bytes});
        }
    }
    std.debug.print("\n", .{});

    return InterpreterError.PrimitiveFailed;
}

fn primHalt(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // receiver
    std.debug.print("Halt!\n", .{});
    return Value.nil;
}

// ============================================================================
// Exception Handling Primitives
// ============================================================================

fn primExceptionSignal(interp: *Interpreter) InterpreterError!Value {
    const exception = try interp.pop();

    // Look for a handler
    if (interp.findExceptionHandler(exception)) |handler_idx| {
        const handler = interp.exception_handlers[handler_idx];

        // Unwind the stack to the handler's context
        interp.context_ptr = handler.context_ptr;
        interp.sp = handler.sp;
        interp.temp_base = handler.temp_base;
        interp.method = handler.method;
        interp.ip = handler.ip;
        interp.receiver = handler.receiver;

        // Remove handlers installed after this one
        interp.handler_ptr = handler_idx;

        // Store exception for handler block to access
        interp.current_exception = exception;

        // Execute the handler block with the exception as argument
        try interp.push(handler.handler_block);
        try interp.push(exception);
        return primBlockValue1(interp);
    }

    // No handler found - return the exception for Zig-level handling
    try interp.push(exception);
    return InterpreterError.SmalltalkException;
}

fn primExceptionSignalWith(interp: *Interpreter) InterpreterError!Value {
    const msg = try interp.pop();
    const exception = try interp.pop();

    // Store message in the exception if it's an object with a messageText field
    if (exception.isObject()) {
        const exc_obj = exception.asObject();
        // Try to set messageText (field 0 for most exceptions)
        exc_obj.setField(0, msg, 1);
    }

    // Now signal the exception
    try interp.push(exception);
    return primExceptionSignal(interp);
}

fn primOnDo(interp: *Interpreter) InterpreterError!Value {
    const handler_block = try interp.pop();
    const exception_class = try interp.pop();
    const block = try interp.pop();

    // Verify both are blocks/classes
    if (!block.isObject()) {
        try interp.push(block);
        try interp.push(exception_class);
        try interp.push(handler_block);
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        try interp.push(block);
        try interp.push(exception_class);
        try interp.push(handler_block);
        return InterpreterError.PrimitiveFailed;
    }

    // Push the exception handler
    try interp.pushExceptionHandler(exception_class, handler_block);

    // Execute the protected block
    try interp.push(block);
    const result = primBlockValue(interp) catch |err| {
        // Pop the handler on error
        interp.popExceptionHandler();

        // If this is a Smalltalk exception, check if we should handle it
        if (err == InterpreterError.SmalltalkException) {
            // The exception was already handled by primExceptionSignal
            // or there was no matching handler
            return err;
        }

        return err;
    };

    // Pop the handler on normal completion
    interp.popExceptionHandler();

    return result;
}

fn primEnsure(interp: *Interpreter) InterpreterError!Value {
    const ensure_block = try interp.pop();
    const block = try interp.pop();

    // Evaluate the main block
    var result: Value = Value.nil;
    var saved_error: ?InterpreterError = null;

    if (block.isObject()) {
        const block_obj = block.asObject();
        if (block_obj.header.class_index == Heap.CLASS_BLOCK_CLOSURE) {
            try interp.push(block);
            result = primBlockValue(interp) catch |err| blk: {
                saved_error = err;
                break :blk Value.nil;
            };
        }
    }

    // Always evaluate the ensure block
    if (ensure_block.isObject()) {
        const ensure_obj = ensure_block.asObject();
        if (ensure_obj.header.class_index == Heap.CLASS_BLOCK_CLOSURE) {
            try interp.push(ensure_block);
            _ = primBlockValue(interp) catch {};
        }
    }

    // Re-throw the original error if there was one
    if (saved_error) |err| {
        return err;
    }

    return result;
}

fn primIfCurtailed(interp: *Interpreter) InterpreterError!Value {
    const curtailed_block = try interp.pop();
    const block = try interp.pop();

    // Evaluate the main block
    if (block.isObject()) {
        const block_obj = block.asObject();
        if (block_obj.header.class_index == Heap.CLASS_BLOCK_CLOSURE) {
            try interp.push(block);
            const result = primBlockValue(interp) catch |err| {
                // On error, run the curtailed block
                if (curtailed_block.isObject()) {
                    const curtailed_obj = curtailed_block.asObject();
                    if (curtailed_obj.header.class_index == Heap.CLASS_BLOCK_CLOSURE) {
                        try interp.push(curtailed_block);
                        _ = primBlockValue(interp) catch {};
                    }
                }
                return err;
            };
            return result;
        }
    }

    try interp.push(block);
    try interp.push(curtailed_block);
    return InterpreterError.PrimitiveFailed;
}

// ============================================================================
// Dictionary Primitives
// Dictionary has 2 inst vars: tally (count), array (hash table of Associations)
// ============================================================================

fn primDictAt(interp: *Interpreter) InterpreterError!Value {
    const key = try interp.pop();
    const dict = try interp.pop();

    if (!dict.isObject()) {
        try interp.push(dict);
        try interp.push(key);
        return InterpreterError.PrimitiveFailed;
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2); // array is field 1

    if (!array_val.isObject()) {
        try interp.push(dict);
        try interp.push(key);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    // Linear search for now (should use hash for performance)
    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const assoc_val = array.getField(i, array_size);
        if (assoc_val.isObject()) {
            const assoc = assoc_val.asObject();
            if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                const assoc_key = assoc.getField(0, 2);
                if (valuesEqual(assoc_key, key)) {
                    return assoc.getField(1, 2); // Return value
                }
            }
        }
    }

    // Key not found
    try interp.push(dict);
    try interp.push(key);
    return InterpreterError.PrimitiveFailed;
}

fn primDictAtPut(interp: *Interpreter) InterpreterError!Value {
    const val = try interp.pop();
    const key = try interp.pop();
    const dict = try interp.pop();

    if (!dict.isObject()) {
        try interp.push(dict);
        try interp.push(key);
        try interp.push(val);
        return InterpreterError.PrimitiveFailed;
    }

    // If the key is a symbol, also update heap globals so compiled code can find it
    // This makes Smalltalk at: #SomeGlobal put: value accessible to push_literal_variable
    if (key.isObject()) {
        const key_obj = key.asObject();
        if (key_obj.header.class_index == Heap.CLASS_SYMBOL) {
            const key_name = key_obj.bytes(key_obj.header.size);
            interp.heap.setGlobal(key_name, val) catch {};
        }
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2);

    if (!array_val.isObject()) {
        // Need to initialize the array first
        const new_array = interp.heap.allocateObject(Heap.CLASS_ARRAY, 16, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        dict_obj.setField(1, Value.fromObject(new_array), 2);

        // Create association
        const assoc = interp.heap.allocateObject(Heap.CLASS_ASSOCIATION, 2, .normal) catch {
            return InterpreterError.OutOfMemory;
        };
        assoc.setField(0, key, 2);
        assoc.setField(1, val, 2);
        new_array.setField(0, Value.fromObject(assoc), 16);
        dict_obj.setField(0, Value.fromSmallInt(1), 2); // tally = 1
        return val;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    // Look for existing key or empty slot
    var empty_slot: ?usize = null;
    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const assoc_val = array.getField(i, array_size);
        if (assoc_val.isNil()) {
            if (empty_slot == null) {
                empty_slot = i;
            }
        } else if (assoc_val.isObject()) {
            const assoc = assoc_val.asObject();
            if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                const assoc_key = assoc.getField(0, 2);
                if (valuesEqual(assoc_key, key)) {
                    // Update existing entry
                    assoc.setField(1, val, 2);
                    return val;
                }
            }
        }
    }

    // Add new entry
    if (empty_slot) |slot| {
        const assoc = interp.heap.allocateObject(Heap.CLASS_ASSOCIATION, 2, .normal) catch {
            return InterpreterError.OutOfMemory;
        };
        assoc.setField(0, key, 2);
        assoc.setField(1, val, 2);
        array.setField(slot, Value.fromObject(assoc), array_size);

        // Increment tally
        const tally = dict_obj.getField(0, 2);
        if (tally.isSmallInt()) {
            dict_obj.setField(0, Value.fromSmallInt(tally.asSmallInt() + 1), 2);
        }
        return val;
    }

    // Array is full - would need to grow
    try interp.push(dict);
    try interp.push(key);
    try interp.push(val);
    return InterpreterError.PrimitiveFailed;
}

fn primDictAtIfAbsent(interp: *Interpreter) InterpreterError!Value {
    const absent_block = try interp.pop();
    const key = try interp.pop();
    const dict = try interp.pop();

    if (!dict.isObject()) {
        // Return absent block value
        try interp.push(absent_block);
        return primBlockValue(interp);
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2);

    if (!array_val.isObject()) {
        try interp.push(absent_block);
        return primBlockValue(interp);
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    // Search for key
    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const assoc_val = array.getField(i, array_size);
        if (assoc_val.isObject()) {
            const assoc = assoc_val.asObject();
            if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                const assoc_key = assoc.getField(0, 2);
                if (valuesEqual(assoc_key, key)) {
                    return assoc.getField(1, 2);
                }
            }
        }
    }

    // Not found - evaluate absent block
    try interp.push(absent_block);
    return primBlockValue(interp);
}

fn primDictIncludesKey(interp: *Interpreter) InterpreterError!Value {
    const key = try interp.pop();
    const dict = try interp.pop();

    if (!dict.isObject()) {
        return Value.@"false";
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2);

    if (!array_val.isObject()) {
        return Value.@"false";
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const assoc_val = array.getField(i, array_size);
        if (assoc_val.isObject()) {
            const assoc = assoc_val.asObject();
            if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                const assoc_key = assoc.getField(0, 2);
                if (valuesEqual(assoc_key, key)) {
                    return Value.@"true";
                }
            }
        }
    }

    return Value.@"false";
}

fn primDictRemoveKey(interp: *Interpreter) InterpreterError!Value {
    const key = try interp.pop();
    const dict = try interp.pop();

    if (!dict.isObject()) {
        try interp.push(dict);
        try interp.push(key);
        return InterpreterError.PrimitiveFailed;
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2);

    if (!array_val.isObject()) {
        try interp.push(dict);
        try interp.push(key);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const assoc_val = array.getField(i, array_size);
        if (assoc_val.isObject()) {
            const assoc = assoc_val.asObject();
            if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                const assoc_key = assoc.getField(0, 2);
                if (valuesEqual(assoc_key, key)) {
                    const removed_val = assoc.getField(1, 2);
                    array.setField(i, Value.nil, array_size);

                    // Decrement tally
                    const tally = dict_obj.getField(0, 2);
                    if (tally.isSmallInt()) {
                        dict_obj.setField(0, Value.fromSmallInt(tally.asSmallInt() - 1), 2);
                    }
                    return removed_val;
                }
            }
        }
    }

    try interp.push(dict);
    try interp.push(key);
    return InterpreterError.PrimitiveFailed;
}

fn primDictKeys(interp: *Interpreter) InterpreterError!Value {
    const dict = try interp.pop();

    if (!dict.isObject()) {
        try interp.push(dict);
        return InterpreterError.PrimitiveFailed;
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2);
    const tally_val = dict_obj.getField(0, 2);

    const tally: usize = if (tally_val.isSmallInt()) @intCast(tally_val.asSmallInt()) else 0;

    const result = interp.heap.allocateObject(Heap.CLASS_ARRAY, tally, .variable) catch {
        return InterpreterError.OutOfMemory;
    };

    if (array_val.isObject()) {
        const array = array_val.asObject();
        const array_size = array.header.size;
        var result_idx: usize = 0;

        var i: usize = 0;
        while (i < array_size and result_idx < tally) : (i += 1) {
            const assoc_val = array.getField(i, array_size);
            if (assoc_val.isObject()) {
                const assoc = assoc_val.asObject();
                if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                    result.setField(result_idx, assoc.getField(0, 2), tally);
                    result_idx += 1;
                }
            }
        }
    }

    return Value.fromObject(result);
}

fn primDictValues(interp: *Interpreter) InterpreterError!Value {
    const dict = try interp.pop();

    if (!dict.isObject()) {
        try interp.push(dict);
        return InterpreterError.PrimitiveFailed;
    }

    const dict_obj = dict.asObject();
    const array_val = dict_obj.getField(1, 2);
    const tally_val = dict_obj.getField(0, 2);

    const tally: usize = if (tally_val.isSmallInt()) @intCast(tally_val.asSmallInt()) else 0;

    const result = interp.heap.allocateObject(Heap.CLASS_ARRAY, tally, .variable) catch {
        return InterpreterError.OutOfMemory;
    };

    if (array_val.isObject()) {
        const array = array_val.asObject();
        const array_size = array.header.size;
        var result_idx: usize = 0;

        var i: usize = 0;
        while (i < array_size and result_idx < tally) : (i += 1) {
            const assoc_val = array.getField(i, array_size);
            if (assoc_val.isObject()) {
                const assoc = assoc_val.asObject();
                if (assoc.header.class_index == Heap.CLASS_ASSOCIATION) {
                    result.setField(result_idx, assoc.getField(1, 2), tally);
                    result_idx += 1;
                }
            }
        }
    }

    return Value.fromObject(result);
}

fn primDictSize(interp: *Interpreter) InterpreterError!Value {
    const dict = try interp.pop();

    if (!dict.isObject()) {
        return Value.fromSmallInt(0);
    }

    const dict_obj = dict.asObject();
    const tally = dict_obj.getField(0, 2);

    if (tally.isSmallInt()) {
        return tally;
    }

    return Value.fromSmallInt(0);
}

// ============================================================================
// Set Primitives
// Set has 2 inst vars: tally (count), array (hash table of elements)
// ============================================================================

fn primSetAdd(interp: *Interpreter) InterpreterError!Value {
    const element = try interp.pop();
    const set = try interp.pop();

    if (!set.isObject()) {
        try interp.push(set);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const set_obj = set.asObject();
    var array_val = set_obj.getField(1, 2);

    if (!array_val.isObject()) {
        // Initialize array
        const new_array = interp.heap.allocateObject(Heap.CLASS_ARRAY, 16, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        set_obj.setField(1, Value.fromObject(new_array), 2);
        array_val = Value.fromObject(new_array);
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    // Check if already present
    var empty_slot: ?usize = null;
    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const existing = array.getField(i, array_size);
        if (existing.isNil()) {
            if (empty_slot == null) {
                empty_slot = i;
            }
        } else if (valuesEqual(existing, element)) {
            return element; // Already present
        }
    }

    // Add new element
    if (empty_slot) |slot| {
        array.setField(slot, element, array_size);
        const tally = set_obj.getField(0, 2);
        if (tally.isSmallInt()) {
            set_obj.setField(0, Value.fromSmallInt(tally.asSmallInt() + 1), 2);
        } else {
            set_obj.setField(0, Value.fromSmallInt(1), 2);
        }
        return element;
    }

    // Array is full
    try interp.push(set);
    try interp.push(element);
    return InterpreterError.PrimitiveFailed;
}

fn primSetIncludes(interp: *Interpreter) InterpreterError!Value {
    const element = try interp.pop();
    const set = try interp.pop();

    if (!set.isObject()) {
        return Value.@"false";
    }

    const set_obj = set.asObject();
    const array_val = set_obj.getField(1, 2);

    if (!array_val.isObject()) {
        return Value.@"false";
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const existing = array.getField(i, array_size);
        if (valuesEqual(existing, element)) {
            return Value.@"true";
        }
    }

    return Value.@"false";
}

fn primSetRemove(interp: *Interpreter) InterpreterError!Value {
    const element = try interp.pop();
    const set = try interp.pop();

    if (!set.isObject()) {
        try interp.push(set);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const set_obj = set.asObject();
    const array_val = set_obj.getField(1, 2);

    if (!array_val.isObject()) {
        try interp.push(set);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const existing = array.getField(i, array_size);
        if (valuesEqual(existing, element)) {
            array.setField(i, Value.nil, array_size);
            const tally = set_obj.getField(0, 2);
            if (tally.isSmallInt()) {
                set_obj.setField(0, Value.fromSmallInt(tally.asSmallInt() - 1), 2);
            }
            return element;
        }
    }

    try interp.push(set);
    try interp.push(element);
    return InterpreterError.PrimitiveFailed;
}

fn primSetSize(interp: *Interpreter) InterpreterError!Value {
    const set = try interp.pop();

    if (!set.isObject()) {
        return Value.fromSmallInt(0);
    }

    const set_obj = set.asObject();
    const tally = set_obj.getField(0, 2);

    if (tally.isSmallInt()) {
        return tally;
    }

    return Value.fromSmallInt(0);
}

// ============================================================================
// OrderedCollection Primitives
// OrderedCollection has 3 inst vars: array, firstIndex, lastIndex
// ============================================================================

fn primOcAdd(interp: *Interpreter) InterpreterError!Value {
    return primOcAddLast(interp);
}

fn primOcAddFirst(interp: *Interpreter) InterpreterError!Value {
    const element = try interp.pop();
    const oc = try interp.pop();

    if (!oc.isObject()) {
        try interp.push(oc);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const oc_obj = oc.asObject();
    var array_val = oc_obj.getField(0, 3);
    var first_idx = oc_obj.getField(1, 3);

    if (!array_val.isObject()) {
        // Initialize
        const new_array = interp.heap.allocateObject(Heap.CLASS_ARRAY, 16, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        oc_obj.setField(0, Value.fromObject(new_array), 3);
        oc_obj.setField(1, Value.fromSmallInt(8), 3); // first = 8
        oc_obj.setField(2, Value.fromSmallInt(7), 3); // last = 7 (empty)
        array_val = Value.fromObject(new_array);
        first_idx = Value.fromSmallInt(8);
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    if (!first_idx.isSmallInt()) {
        try interp.push(oc);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const new_first = first_idx.asSmallInt() - 1;
    if (new_first < 0) {
        // Would need to grow - for now, fail
        try interp.push(oc);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    array.setField(@intCast(new_first), element, array_size);
    oc_obj.setField(1, Value.fromSmallInt(new_first), 3);

    return element;
}

fn primOcAddLast(interp: *Interpreter) InterpreterError!Value {
    const element = try interp.pop();
    const oc = try interp.pop();

    if (!oc.isObject()) {
        try interp.push(oc);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const oc_obj = oc.asObject();
    var array_val = oc_obj.getField(0, 3);
    var last_idx = oc_obj.getField(2, 3);

    if (!array_val.isObject()) {
        // Initialize
        const new_array = interp.heap.allocateObject(Heap.CLASS_ARRAY, 16, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        oc_obj.setField(0, Value.fromObject(new_array), 3);
        oc_obj.setField(1, Value.fromSmallInt(0), 3); // first = 0
        oc_obj.setField(2, Value.fromSmallInt(-1), 3); // last = -1 (empty)
        array_val = Value.fromObject(new_array);
        last_idx = Value.fromSmallInt(-1);
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    if (!last_idx.isSmallInt()) {
        try interp.push(oc);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    const new_last = last_idx.asSmallInt() + 1;
    if (new_last >= @as(i61, @intCast(array_size))) {
        // Would need to grow - for now, fail
        try interp.push(oc);
        try interp.push(element);
        return InterpreterError.PrimitiveFailed;
    }

    array.setField(@intCast(new_last), element, array_size);
    oc_obj.setField(2, Value.fromSmallInt(new_last), 3);

    return element;
}

fn primOcRemoveFirst(interp: *Interpreter) InterpreterError!Value {
    const oc = try interp.pop();

    if (!oc.isObject()) {
        try interp.push(oc);
        return InterpreterError.PrimitiveFailed;
    }

    const oc_obj = oc.asObject();
    const array_val = oc_obj.getField(0, 3);
    const first_idx = oc_obj.getField(1, 3);
    const last_idx = oc_obj.getField(2, 3);

    if (!array_val.isObject() or !first_idx.isSmallInt() or !last_idx.isSmallInt()) {
        try interp.push(oc);
        return InterpreterError.PrimitiveFailed;
    }

    const first = first_idx.asSmallInt();
    const last = last_idx.asSmallInt();

    if (first > last) {
        // Empty
        try interp.push(oc);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    const element = array.getField(@intCast(first), array_size);
    array.setField(@intCast(first), Value.nil, array_size);
    oc_obj.setField(1, Value.fromSmallInt(first + 1), 3);

    return element;
}

fn primOcRemoveLast(interp: *Interpreter) InterpreterError!Value {
    const oc = try interp.pop();

    if (!oc.isObject()) {
        try interp.push(oc);
        return InterpreterError.PrimitiveFailed;
    }

    const oc_obj = oc.asObject();
    const array_val = oc_obj.getField(0, 3);
    const first_idx = oc_obj.getField(1, 3);
    const last_idx = oc_obj.getField(2, 3);

    if (!array_val.isObject() or !first_idx.isSmallInt() or !last_idx.isSmallInt()) {
        try interp.push(oc);
        return InterpreterError.PrimitiveFailed;
    }

    const first = first_idx.asSmallInt();
    const last = last_idx.asSmallInt();

    if (first > last) {
        // Empty
        try interp.push(oc);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;

    const element = array.getField(@intCast(last), array_size);
    array.setField(@intCast(last), Value.nil, array_size);
    oc_obj.setField(2, Value.fromSmallInt(last - 1), 3);

    return element;
}

fn primOcAt(interp: *Interpreter) InterpreterError!Value {
    const index = try interp.pop();
    const oc = try interp.pop();

    if (!oc.isObject() or !index.isSmallInt()) {
        try interp.push(oc);
        try interp.push(index);
        return InterpreterError.PrimitiveFailed;
    }

    const oc_obj = oc.asObject();
    const array_val = oc_obj.getField(0, 3);
    const first_idx = oc_obj.getField(1, 3);

    if (!array_val.isObject() or !first_idx.isSmallInt()) {
        try interp.push(oc);
        try interp.push(index);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;
    const first = first_idx.asSmallInt();
    const idx = index.asSmallInt() - 1; // 1-based

    const actual_idx = first + idx;
    if (actual_idx < 0 or actual_idx >= @as(i61, @intCast(array_size))) {
        try interp.push(oc);
        try interp.push(index);
        return InterpreterError.PrimitiveFailed;
    }

    return array.getField(@intCast(actual_idx), array_size);
}

fn primOcAtPut(interp: *Interpreter) InterpreterError!Value {
    const val = try interp.pop();
    const index = try interp.pop();
    const oc = try interp.pop();

    if (!oc.isObject() or !index.isSmallInt()) {
        try interp.push(oc);
        try interp.push(index);
        try interp.push(val);
        return InterpreterError.PrimitiveFailed;
    }

    const oc_obj = oc.asObject();
    const array_val = oc_obj.getField(0, 3);
    const first_idx = oc_obj.getField(1, 3);

    if (!array_val.isObject() or !first_idx.isSmallInt()) {
        try interp.push(oc);
        try interp.push(index);
        try interp.push(val);
        return InterpreterError.PrimitiveFailed;
    }

    const array = array_val.asObject();
    const array_size = array.header.size;
    const first = first_idx.asSmallInt();
    const idx = index.asSmallInt() - 1; // 1-based

    const actual_idx = first + idx;
    if (actual_idx < 0 or actual_idx >= @as(i61, @intCast(array_size))) {
        try interp.push(oc);
        try interp.push(index);
        try interp.push(val);
        return InterpreterError.PrimitiveFailed;
    }

    array.setField(@intCast(actual_idx), val, array_size);
    return val;
}

fn primOcSize(interp: *Interpreter) InterpreterError!Value {
    const oc = try interp.pop();

    if (!oc.isObject()) {
        return Value.fromSmallInt(0);
    }

    const oc_obj = oc.asObject();
    const first_idx = oc_obj.getField(1, 3);
    const last_idx = oc_obj.getField(2, 3);

    if (!first_idx.isSmallInt() or !last_idx.isSmallInt()) {
        return Value.fromSmallInt(0);
    }

    const first = first_idx.asSmallInt();
    const last = last_idx.asSmallInt();

    if (last < first) {
        return Value.fromSmallInt(0);
    }

    return Value.fromSmallInt(last - first + 1);
}

// ============================================================================
// Stream Primitives
// ReadStream has 3 inst vars: collection, position, readLimit
// WriteStream has 4 inst vars: collection, position, readLimit, writeLimit
// ============================================================================

fn primStreamNext(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const collection = stream_obj.getField(0, 4);
    const position = stream_obj.getField(1, 4);
    const readLimit = stream_obj.getField(2, 4);

    if (!collection.isObject() or !position.isSmallInt() or !readLimit.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const pos = position.asSmallInt();
    const limit = readLimit.asSmallInt();

    if (pos >= limit) {
        // At end
        return Value.nil;
    }

    const coll_obj = collection.asObject();
    const coll_size = coll_obj.header.size;

    if (pos < 0 or pos >= @as(i61, @intCast(coll_size))) {
        return Value.nil;
    }

    var result: Value = undefined;
    if (coll_obj.header.getFormat() == .bytes) {
        const bytes_slice = coll_obj.bytes(coll_size);
        result = Value.fromCharacter(bytes_slice[@intCast(pos)]);
    } else {
        result = coll_obj.getField(@intCast(pos), coll_size);
    }

    stream_obj.setField(1, Value.fromSmallInt(pos + 1), 4);
    return result;
}

fn primStreamNextPut(interp: *Interpreter) InterpreterError!Value {
    const obj = try interp.pop();
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        try interp.push(obj);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const collection = stream_obj.getField(0, 4);
    const position = stream_obj.getField(1, 4);

    if (!collection.isObject() or !position.isSmallInt()) {
        try interp.push(stream);
        try interp.push(obj);
        return InterpreterError.PrimitiveFailed;
    }

    const pos = position.asSmallInt();
    const coll_obj = collection.asObject();
    const coll_size = coll_obj.header.size;

    if (pos < 0 or pos >= @as(i61, @intCast(coll_size))) {
        try interp.push(stream);
        try interp.push(obj);
        return InterpreterError.PrimitiveFailed;
    }

    if (coll_obj.header.getFormat() == .bytes) {
        var bytes_slice = coll_obj.bytes(coll_size);
        if (obj.isCharacter()) {
            bytes_slice[@intCast(pos)] = @intCast(obj.asCharacter());
        } else if (obj.isSmallInt()) {
            bytes_slice[@intCast(pos)] = @intCast(obj.asSmallInt());
        }
    } else {
        coll_obj.setField(@intCast(pos), obj, coll_size);
    }

    stream_obj.setField(1, Value.fromSmallInt(pos + 1), 4);

    // Update writeLimit if needed
    const writeLimit = stream_obj.getField(3, 4);
    if (writeLimit.isSmallInt() and pos + 1 > writeLimit.asSmallInt()) {
        stream_obj.setField(3, Value.fromSmallInt(pos + 1), 4);
    }

    return obj;
}

fn primStreamNextPutAll(interp: *Interpreter) InterpreterError!Value {
    const coll = try interp.pop();
    const stream = try interp.pop();

    if (!stream.isObject() or !coll.isObject()) {
        try interp.push(stream);
        try interp.push(coll);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const collection = stream_obj.getField(0, 4);
    var position = stream_obj.getField(1, 4);

    if (!collection.isObject() or !position.isSmallInt()) {
        try interp.push(stream);
        try interp.push(coll);
        return InterpreterError.PrimitiveFailed;
    }

    const coll_obj = coll.asObject();
    const coll_size = coll_obj.header.size;
    const dest_obj = collection.asObject();
    const dest_size = dest_obj.header.size;

    var pos = position.asSmallInt();

    // Copy elements
    if (coll_obj.header.getFormat() == .bytes and dest_obj.header.getFormat() == .bytes) {
        const src_bytes = coll_obj.bytes(coll_size);
        var dest_bytes = dest_obj.bytes(dest_size);
        for (src_bytes) |b| {
            if (pos >= @as(i61, @intCast(dest_size))) break;
            dest_bytes[@intCast(pos)] = b;
            pos += 1;
        }
    } else {
        var i: usize = 0;
        while (i < coll_size and pos < @as(i61, @intCast(dest_size))) : (i += 1) {
            dest_obj.setField(@intCast(pos), coll_obj.getField(i, coll_size), dest_size);
            pos += 1;
        }
    }

    stream_obj.setField(1, Value.fromSmallInt(pos), 4);

    // Update writeLimit if needed
    const writeLimit = stream_obj.getField(3, 4);
    if (writeLimit.isSmallInt() and pos > writeLimit.asSmallInt()) {
        stream_obj.setField(3, Value.fromSmallInt(pos), 4);
    }

    return coll;
}

fn primStreamPeek(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const collection = stream_obj.getField(0, 4);
    const position = stream_obj.getField(1, 4);
    const readLimit = stream_obj.getField(2, 4);

    if (!collection.isObject() or !position.isSmallInt() or !readLimit.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const pos = position.asSmallInt();
    const limit = readLimit.asSmallInt();

    if (pos >= limit) {
        return Value.nil;
    }

    const coll_obj = collection.asObject();
    const coll_size = coll_obj.header.size;

    if (pos < 0 or pos >= @as(i61, @intCast(coll_size))) {
        return Value.nil;
    }

    if (coll_obj.header.getFormat() == .bytes) {
        const bytes_slice = coll_obj.bytes(coll_size);
        return Value.fromCharacter(bytes_slice[@intCast(pos)]);
    } else {
        return coll_obj.getField(@intCast(pos), coll_size);
    }
}

fn primStreamAtEnd(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        return Value.@"true";
    }

    const stream_obj = stream.asObject();
    const position = stream_obj.getField(1, 4);
    const readLimit = stream_obj.getField(2, 4);

    if (!position.isSmallInt() or !readLimit.isSmallInt()) {
        return Value.@"true";
    }

    if (position.asSmallInt() >= readLimit.asSmallInt()) {
        return Value.@"true";
    }

    return Value.@"false";
}

fn primStreamPosition(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        return Value.fromSmallInt(0);
    }

    const stream_obj = stream.asObject();
    const position = stream_obj.getField(1, 4);

    if (position.isSmallInt()) {
        return position;
    }

    return Value.fromSmallInt(0);
}

fn primStreamSetPosition(interp: *Interpreter) InterpreterError!Value {
    const new_pos = try interp.pop();
    const stream = try interp.pop();

    if (!stream.isObject() or !new_pos.isSmallInt()) {
        try interp.push(stream);
        try interp.push(new_pos);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    stream_obj.setField(1, new_pos, 4);

    return new_pos;
}

fn primStreamReset(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    stream_obj.setField(1, Value.fromSmallInt(0), 4);

    return stream;
}

fn primStreamContents(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const collection = stream_obj.getField(0, 4);
    const writeLimit = stream_obj.getField(3, 4);

    if (!collection.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const coll_obj = collection.asObject();
    const limit: usize = if (writeLimit.isSmallInt()) @intCast(writeLimit.asSmallInt()) else coll_obj.header.size;

    // Create a copy of the used portion
    if (coll_obj.header.getFormat() == .bytes) {
        const new_str = interp.heap.allocateObject(Heap.CLASS_STRING, limit, .bytes) catch {
            return InterpreterError.OutOfMemory;
        };
        const src_bytes = coll_obj.bytes(coll_obj.header.size);
        var dest_bytes = new_str.bytes(limit);
        @memcpy(dest_bytes[0..limit], src_bytes[0..limit]);
        return Value.fromObject(new_str);
    } else {
        const new_array = interp.heap.allocateObject(Heap.CLASS_ARRAY, limit, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        var i: usize = 0;
        while (i < limit) : (i += 1) {
            new_array.setField(i, coll_obj.getField(i, coll_obj.header.size), limit);
        }
        return Value.fromObject(new_array);
    }
}

// ============================================================================
// Additional String Primitives
// ============================================================================

fn primStringIndexOf(interp: *Interpreter) InterpreterError!Value {
    const char = try interp.pop();
    const str = try interp.pop();

    if (!str.isObject()) {
        return Value.fromSmallInt(0);
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        return Value.fromSmallInt(0);
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);

    var search_char: u8 = 0;
    if (char.isCharacter()) {
        const cp = char.asCharacter();
        if (cp < 256) {
            search_char = @intCast(cp);
        }
    } else if (char.isSmallInt()) {
        const n = char.asSmallInt();
        if (n >= 0 and n < 256) {
            search_char = @intCast(n);
        }
    }

    for (str_bytes, 0..) |b, i| {
        if (b == search_char) {
            return Value.fromSmallInt(@intCast(i + 1)); // 1-based
        }
    }

    return Value.fromSmallInt(0);
}

fn primStringIndexOfStartingAt(interp: *Interpreter) InterpreterError!Value {
    const start_pos = try interp.pop();
    const char = try interp.pop();
    const str = try interp.pop();

    if (!str.isObject() or !start_pos.isSmallInt()) {
        return Value.fromSmallInt(0);
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        return Value.fromSmallInt(0);
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const start: usize = @intCast(@max(0, start_pos.asSmallInt() - 1));

    var search_char: u8 = 0;
    if (char.isCharacter()) {
        const cp = char.asCharacter();
        if (cp < 256) {
            search_char = @intCast(cp);
        }
    } else if (char.isSmallInt()) {
        const n = char.asSmallInt();
        if (n >= 0 and n < 256) {
            search_char = @intCast(n);
        }
    }

    var i: usize = start;
    while (i < str_bytes.len) : (i += 1) {
        if (str_bytes[i] == search_char) {
            return Value.fromSmallInt(@intCast(i + 1)); // 1-based
        }
    }

    return Value.fromSmallInt(0);
}

/// Dolphin primitive 52: String >> nextIndexOf:from:to:
/// Returns the 1-based index of anObject in the receiver between start and stop (inclusive),
/// or 0 if not found.
fn primStringNextIndexOf(interp: *Interpreter) InterpreterError!Value {
    // Arguments: receiver (string), anObject (char), startIndex, stopIndex
    const stop_idx = try interp.pop();
    const start_idx = try interp.pop();
    const search_obj = try interp.pop();
    const str = try interp.pop();

    if (!str.isObject() or !start_idx.isSmallInt() or !stop_idx.isSmallInt()) {
        // Primitive failed, push args back and signal failure
        try interp.push(str);
        try interp.push(search_obj);
        try interp.push(start_idx);
        try interp.push(stop_idx);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        try interp.push(search_obj);
        try interp.push(start_idx);
        try interp.push(stop_idx);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const start_int = start_idx.asSmallInt();
    const stop_int = stop_idx.asSmallInt();

    // Bounds check
    if (start_int < 1 or stop_int < 1) {
        return Value.fromSmallInt(0);
    }

    const start: usize = @intCast(start_int - 1); // Convert to 0-based
    const stop: usize = @intCast(@min(stop_int, @as(i64, @intCast(str_bytes.len))));

    if (start >= str_bytes.len or start >= stop) {
        return Value.fromSmallInt(0);
    }

    // Get the character to search for
    var search_char: u8 = 0;
    if (search_obj.isCharacter()) {
        const cp = search_obj.asCharacter();
        if (cp < 256) {
            search_char = @intCast(cp);
        } else {
            return Value.fromSmallInt(0); // Can't find non-ASCII char in byte string
        }
    } else if (search_obj.isSmallInt()) {
        const n = search_obj.asSmallInt();
        if (n >= 0 and n < 256) {
            search_char = @intCast(n);
        } else {
            return Value.fromSmallInt(0);
        }
    } else {
        // Not a character - strings can only contain characters
        return Value.fromSmallInt(0);
    }

    // Search for the character
    var i: usize = start;
    while (i < stop) : (i += 1) {
        if (str_bytes[i] == search_char) {
            return Value.fromSmallInt(@intCast(i + 1)); // Return 1-based index
        }
    }

    return Value.fromSmallInt(0);
}

fn primStringAsUpper(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const new_str = interp.heap.allocateObject(Heap.CLASS_STRING, str_bytes.len, .bytes) catch {
        return InterpreterError.OutOfMemory;
    };
    var new_bytes = new_str.bytes(str_bytes.len);

    for (str_bytes, 0..) |b, i| {
        new_bytes[i] = std.ascii.toUpper(b);
    }

    return Value.fromObject(new_str);
}

fn primStringAsLower(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const new_str = interp.heap.allocateObject(Heap.CLASS_STRING, str_bytes.len, .bytes) catch {
        return InterpreterError.OutOfMemory;
    };
    var new_bytes = new_str.bytes(str_bytes.len);

    for (str_bytes, 0..) |b, i| {
        new_bytes[i] = std.ascii.toLower(b);
    }

    return Value.fromObject(new_str);
}

fn primStringTrim(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const trimmed = std.mem.trim(u8, str_bytes, " \t\n\r");

    const new_str = interp.heap.allocateObject(Heap.CLASS_STRING, trimmed.len, .bytes) catch {
        return InterpreterError.OutOfMemory;
    };
    const new_bytes = new_str.bytes(trimmed.len);
    @memcpy(new_bytes, trimmed);

    return Value.fromObject(new_str);
}

fn primStringIncludes(interp: *Interpreter) InterpreterError!Value {
    const char = try interp.pop();
    const str = try interp.pop();

    if (!str.isObject()) {
        return Value.@"false";
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        return Value.@"false";
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);

    var search_char: u8 = 0;
    if (char.isCharacter()) {
        const cp = char.asCharacter();
        if (cp < 256) {
            search_char = @intCast(cp);
        }
    } else if (char.isSmallInt()) {
        const n = char.asSmallInt();
        if (n >= 0 and n < 256) {
            search_char = @intCast(n);
        }
    }

    for (str_bytes) |b| {
        if (b == search_char) {
            return Value.@"true";
        }
    }

    return Value.@"false";
}

fn primStringSplit(interp: *Interpreter) InterpreterError!Value {
    const delim = try interp.pop();
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        try interp.push(delim);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        try interp.push(delim);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);

    var sep: u8 = ' ';
    if (delim.isCharacter()) {
        const cp = delim.asCharacter();
        if (cp < 256) {
            sep = @intCast(cp);
        }
    } else if (delim.isSmallInt()) {
        const n = delim.asSmallInt();
        if (n >= 0 and n < 256) {
            sep = @intCast(n);
        }
    }

    // Count parts first
    var count: usize = 1;
    for (str_bytes) |b| {
        if (b == sep) {
            count += 1;
        }
    }

    // Allocate result array
    const result = interp.heap.allocateObject(Heap.CLASS_ARRAY, count, .variable) catch {
        return InterpreterError.OutOfMemory;
    };

    // Split and create strings
    var part_idx: usize = 0;
    var start: usize = 0;
    for (str_bytes, 0..) |b, i| {
        if (b == sep) {
            const part_len = i - start;
            const part_str = interp.heap.allocateObject(Heap.CLASS_STRING, part_len, .bytes) catch {
                return InterpreterError.OutOfMemory;
            };
            const part_bytes = part_str.bytes(part_len);
            @memcpy(part_bytes, str_bytes[start..i]);
            result.setField(part_idx, Value.fromObject(part_str), count);
            part_idx += 1;
            start = i + 1;
        }
    }

    // Last part
    const part_len = str_bytes.len - start;
    const part_str = interp.heap.allocateObject(Heap.CLASS_STRING, part_len, .bytes) catch {
        return InterpreterError.OutOfMemory;
    };
    const part_bytes = part_str.bytes(part_len);
    @memcpy(part_bytes, str_bytes[start..]);
    result.setField(part_idx, Value.fromObject(part_str), count);

    return Value.fromObject(result);
}

fn primAsNumber(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);
    const trimmed = std.mem.trim(u8, str_bytes, " \t\n\r");

    // Try to parse as integer first
    const int_val = std.fmt.parseInt(i61, trimmed, 10) catch {
        // Try as float
        const float_val = std.fmt.parseFloat(f64, trimmed) catch {
            try interp.push(str);
            return InterpreterError.PrimitiveFailed;
        };
        return interp.heap.allocateFloat(float_val) catch {
            return InterpreterError.OutOfMemory;
        };
    };

    return Value.fromSmallInt(int_val);
}

// ============================================================================
// Helper Functions
// ============================================================================

fn valuesEqual(a: Value, b: Value) bool {
    if (a.eql(b)) return true;

    // For objects, check if they're symbols or strings with same content
    if (a.isObject() and b.isObject()) {
        const obj_a = a.asObject();
        const obj_b = b.asObject();

        if (obj_a.header.getFormat() == .bytes and obj_b.header.getFormat() == .bytes) {
            const bytes_a = obj_a.bytes(obj_a.header.size);
            const bytes_b = obj_b.bytes(obj_b.header.size);
            return std.mem.eql(u8, bytes_a, bytes_b);
        }
    }

    return false;
}

// ============================================================================
// File I/O Primitives
// ============================================================================

// File handle storage - simple approach using the fileHandle instance variable
// We store file handles as small integers representing indices into a handle table

var file_handles: [256]?std.fs.File = [_]?std.fs.File{null} ** 256;
var next_handle: usize = 0;

fn allocFileHandle(file: std.fs.File) ?usize {
    var i: usize = 0;
    while (i < 256) : (i += 1) {
        const idx = (next_handle + i) % 256;
        if (file_handles[idx] == null) {
            file_handles[idx] = file;
            next_handle = (idx + 1) % 256;
            return idx;
        }
    }
    return null;
}

fn freeFileHandle(handle: usize) void {
    if (handle < 256) {
        if (file_handles[handle]) |file| {
            file.close();
        }
        file_handles[handle] = null;
    }
}

fn getFileHandle(handle: usize) ?std.fs.File {
    if (handle < 256) {
        return file_handles[handle];
    }
    return null;
}

fn primFileOpen(interp: *Interpreter) InterpreterError!Value {
    // Stack: mode, filename, receiver (FileStream class)
    const mode = try interp.pop();
    const filename = try interp.pop();
    const receiver = try interp.pop();

    if (!filename.isObject()) {
        try interp.push(receiver);
        try interp.push(filename);
        try interp.push(mode);
        return InterpreterError.PrimitiveFailed;
    }

    const filename_obj = filename.asObject();
    if (filename_obj.header.getFormat() != .bytes) {
        try interp.push(receiver);
        try interp.push(filename);
        try interp.push(mode);
        return InterpreterError.PrimitiveFailed;
    }

    const filename_bytes = filename_obj.bytes(filename_obj.header.size);

    // Parse mode string to determine open flags
    var mode_str: []const u8 = "r";
    if (mode.isObject()) {
        const mode_obj = mode.asObject();
        if (mode_obj.header.getFormat() == .bytes) {
            mode_str = mode_obj.bytes(mode_obj.header.size);
        }
    }

    const create_file = std.mem.indexOf(u8, mode_str, "w") != null or std.mem.indexOf(u8, mode_str, "a") != null;
    const read_flag = std.mem.indexOf(u8, mode_str, "r") != null or std.mem.indexOf(u8, mode_str, "+") != null;
    const write_flag = std.mem.indexOf(u8, mode_str, "w") != null or std.mem.indexOf(u8, mode_str, "a") != null or std.mem.indexOf(u8, mode_str, "+") != null;
    const append_flag = std.mem.indexOf(u8, mode_str, "a") != null;
    const truncate_flag = std.mem.indexOf(u8, mode_str, "w") != null and std.mem.indexOf(u8, mode_str, "a") == null;

    const flags: std.fs.File.OpenFlags = .{
        .mode = if (read_flag and write_flag) .read_write else if (write_flag) .write_only else .read_only,
    };

    // Try to open/create the file
    const file = blk: {
        if (create_file) {
            break :blk std.fs.cwd().createFile(filename_bytes, .{
                .read = read_flag,
                .truncate = truncate_flag,
            }) catch {
                try interp.push(receiver);
                try interp.push(filename);
                try interp.push(mode);
                return InterpreterError.PrimitiveFailed;
            };
        } else {
            break :blk std.fs.cwd().openFile(filename_bytes, flags) catch {
                try interp.push(receiver);
                try interp.push(filename);
                try interp.push(mode);
                return InterpreterError.PrimitiveFailed;
            };
        }
    };

    // If append mode, seek to end
    if (append_flag) {
        file.seekFromEnd(0) catch {};
    }

    // Allocate a handle
    const handle = allocFileHandle(file) orelse {
        file.close();
        try interp.push(receiver);
        try interp.push(filename);
        try interp.push(mode);
        return InterpreterError.PrimitiveFailed;
    };

    // Create a FileStream object
    const stream = interp.heap.allocateObject(Heap.CLASS_FILE_STREAM, 5, .normal) catch {
        freeFileHandle(handle);
        return InterpreterError.OutOfMemory;
    };

    // Initialize FileStream fields: collection, position, readLimit, writeLimit, fileHandle
    stream.setField(0, Value.nil, 5); // collection (buffer) - nil for now
    stream.setField(1, Value.fromSmallInt(0), 5); // position
    stream.setField(2, Value.fromSmallInt(0), 5); // readLimit
    stream.setField(3, Value.fromSmallInt(0), 5); // writeLimit
    stream.setField(4, Value.fromSmallInt(@intCast(handle)), 5); // fileHandle

    return Value.fromObject(stream);
}

fn primFileClose(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    if (stream_obj.header.class_index != Heap.CLASS_FILE_STREAM) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    freeFileHandle(handle);

    // Mark handle as closed
    stream_obj.setField(4, Value.fromSmallInt(-1), 5);

    return stream;
}

fn primFileRead(interp: *Interpreter) InterpreterError!Value {
    // Stack order for readInto:count:startingAt: is receiver, buffer, count, startPos
    // So we pop in reverse: startPos, count, buffer, receiver
    const starting_at = try interp.pop();
    const count = try interp.pop();
    const buffer = try interp.pop();
    const stream = try interp.pop();

    if (!stream.isObject() or !buffer.isObject()) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const buffer_obj = buffer.asObject();

    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    };

    const count_val: usize = if (count.isSmallInt()) @intCast(@max(0, count.asSmallInt())) else 0;
    const start: usize = if (starting_at.isSmallInt()) @intCast(@max(0, starting_at.asSmallInt() - 1)) else 0;

    if (buffer_obj.header.getFormat() != .bytes) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const buf_bytes = buffer_obj.bytes(buffer_obj.header.size);
    if (start + count_val > buf_bytes.len) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const bytes_read = file.read(buf_bytes[start .. start + count_val]) catch {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromSmallInt(@intCast(bytes_read));
}

fn primFileWrite(interp: *Interpreter) InterpreterError!Value {
    // Stack order for writeFrom:count:startingAt: is receiver, buffer, count, startPos
    // So we pop in reverse: startPos, count, buffer, receiver
    const starting_at = try interp.pop();
    const count = try interp.pop();
    const buffer = try interp.pop();
    const stream = try interp.pop();

    if (!stream.isObject() or !buffer.isObject()) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const buffer_obj = buffer.asObject();

    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    };

    const count_val: usize = if (count.isSmallInt()) @intCast(@max(0, count.asSmallInt())) else 0;
    const start: usize = if (starting_at.isSmallInt()) @intCast(@max(0, starting_at.asSmallInt() - 1)) else 0;

    if (buffer_obj.header.getFormat() != .bytes) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const buf_bytes = buffer_obj.bytes(buffer_obj.header.size);
    if (start + count_val > buf_bytes.len) {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    }

    const bytes_written = file.write(buf_bytes[start .. start + count_val]) catch {
        try interp.push(stream);
        try interp.push(buffer);
        try interp.push(count);
        try interp.push(starting_at);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromSmallInt(@intCast(bytes_written));
}

fn primFileFlush(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    file.sync() catch {};

    return stream;
}

fn primFileSize(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    const stat = file.stat() catch {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromSmallInt(@intCast(stat.size));
}

fn primFilePosition(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    const pos = file.getPos() catch {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromSmallInt(@intCast(pos));
}

fn primFileSetPosition(interp: *Interpreter) InterpreterError!Value {
    const pos = try interp.pop();
    const stream = try interp.pop();

    if (!stream.isObject() or !pos.isSmallInt()) {
        try interp.push(stream);
        try interp.push(pos);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        try interp.push(pos);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        try interp.push(pos);
        return InterpreterError.PrimitiveFailed;
    };

    const new_pos: u64 = @intCast(@max(0, pos.asSmallInt()));
    file.seekTo(new_pos) catch {
        try interp.push(stream);
        try interp.push(pos);
        return InterpreterError.PrimitiveFailed;
    };

    return stream;
}

fn primFileAtEnd(interp: *Interpreter) InterpreterError!Value {
    const stream = try interp.pop();

    if (!stream.isObject()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const stream_obj = stream.asObject();
    const handle_val = stream_obj.getField(4, 5);
    if (!handle_val.isSmallInt()) {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    }

    const handle: usize = @intCast(handle_val.asSmallInt());
    const file = getFileHandle(handle) orelse {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    const pos = file.getPos() catch {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    const stat = file.stat() catch {
        try interp.push(stream);
        return InterpreterError.PrimitiveFailed;
    };

    return if (pos >= stat.size) Value.@"true" else Value.@"false";
}

fn primFileDelete(interp: *Interpreter) InterpreterError!Value {
    const filename = try interp.pop();
    const receiver = try interp.pop();

    if (!filename.isObject()) {
        try interp.push(receiver);
        try interp.push(filename);
        return InterpreterError.PrimitiveFailed;
    }

    const filename_obj = filename.asObject();
    if (filename_obj.header.getFormat() != .bytes) {
        try interp.push(receiver);
        try interp.push(filename);
        return InterpreterError.PrimitiveFailed;
    }

    const filename_bytes = filename_obj.bytes(filename_obj.header.size);

    std.fs.cwd().deleteFile(filename_bytes) catch {
        try interp.push(receiver);
        try interp.push(filename);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.@"true";
}

fn primFileExists(interp: *Interpreter) InterpreterError!Value {
    const filename = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!filename.isObject()) {
        return Value.@"false";
    }

    const filename_obj = filename.asObject();
    if (filename_obj.header.getFormat() != .bytes) {
        return Value.@"false";
    }

    const filename_bytes = filename_obj.bytes(filename_obj.header.size);

    const stat = std.fs.cwd().statFile(filename_bytes) catch {
        return Value.@"false";
    };
    _ = stat;

    return Value.@"true";
}

fn primFileRename(interp: *Interpreter) InterpreterError!Value {
    const new_name = try interp.pop();
    const old_name = try interp.pop();
    const receiver = try interp.pop();

    if (!old_name.isObject() or !new_name.isObject()) {
        try interp.push(receiver);
        try interp.push(old_name);
        try interp.push(new_name);
        return InterpreterError.PrimitiveFailed;
    }

    const old_obj = old_name.asObject();
    const new_obj = new_name.asObject();

    if (old_obj.header.getFormat() != .bytes or new_obj.header.getFormat() != .bytes) {
        try interp.push(receiver);
        try interp.push(old_name);
        try interp.push(new_name);
        return InterpreterError.PrimitiveFailed;
    }

    const old_bytes = old_obj.bytes(old_obj.header.size);
    const new_bytes = new_obj.bytes(new_obj.header.size);

    std.fs.cwd().rename(old_bytes, new_bytes) catch {
        try interp.push(receiver);
        try interp.push(old_name);
        try interp.push(new_name);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.@"true";
}

fn primStdoutWrite(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);

    const stdout = std.fs.File.stdout();
    const written = stdout.write(str_bytes) catch {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromSmallInt(@intCast(written));
}

fn primStdinRead(interp: *Interpreter) InterpreterError!Value {
    const count = try interp.pop();

    if (!count.isSmallInt()) {
        try interp.push(count);
        return InterpreterError.PrimitiveFailed;
    }

    const count_val: usize = @intCast(@max(0, count.asSmallInt()));
    if (count_val == 0) return interp.heap.allocateString("") catch return InterpreterError.OutOfMemory;

    const buffer = interp.heap.allocator.alloc(u8, count_val) catch {
        return InterpreterError.OutOfMemory;
    };
    defer interp.heap.allocator.free(buffer);

    const stdin = std.fs.File.stdin();
    const bytes_read = stdin.read(buffer) catch {
        try interp.push(count);
        return InterpreterError.PrimitiveFailed;
    };

    return interp.heap.allocateString(buffer[0..bytes_read]) catch return InterpreterError.OutOfMemory;
}

fn primStderrWrite(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();

    if (!str.isObject()) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_obj = str.asObject();
    if (str_obj.header.getFormat() != .bytes) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    const str_bytes = str_obj.bytes(str_obj.header.size);

    const stderr = std.fs.File.stderr();
    const written = stderr.write(str_bytes) catch {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromSmallInt(@intCast(written));
}

// ============================================================================
// Reflection Primitives
// ============================================================================

fn primClassSelectors(interp: *Interpreter) InterpreterError!Value {
    const class = try interp.pop();

    if (!class.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class.asObject();

    // Get method dictionary
    const method_dict = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);
    if (method_dict.isNil()) {
        // No methods - return empty array
        return try interp.heap.allocateArray(0);
    }

    if (!method_dict.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    const dict = method_dict.asObject();
    const dict_size = dict.header.size;

    // Count non-nil entries (selectors)
    var count: usize = 0;
    var i: usize = 0;
    while (i < dict_size) : (i += 2) {
        const key = dict.getField(i, dict_size);
        if (!key.isNil()) {
            count += 1;
        }
    }

    // Create result array
    const result_val = try interp.heap.allocateArray(count);
    const result = result_val.asObject();

    // Fill with selectors
    i = 0;
    var idx: usize = 0;
    while (i < dict_size) : (i += 2) {
        const key = dict.getField(i, dict_size);
        if (!key.isNil()) {
            result.setField(idx, key, count);
            idx += 1;
        }
    }

    return result_val;
}

fn primClassAllSelectors(interp: *Interpreter) InterpreterError!Value {
    const class = try interp.pop();

    if (!class.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    // For simplicity, just return selectors from this class
    // A full implementation would walk the superclass chain
    // and collect all selectors

    // First count all selectors in the class hierarchy
    var total_count: usize = 0;
    var current = class;

    while (!current.isNil() and current.isObject()) {
        const curr_obj = current.asObject();
        const method_dict = curr_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);

        if (method_dict.isObject()) {
            const dict = method_dict.asObject();
            const dict_size = dict.header.size;
            var i: usize = 0;
            while (i < dict_size) : (i += 2) {
                const key = dict.getField(i, dict_size);
                if (!key.isNil()) {
                    total_count += 1;
                }
            }
        }

        // Move to superclass
        current = curr_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
    }

    // Create result array
    const result_val = try interp.heap.allocateArray(total_count);
    const result = result_val.asObject();

    // Fill with selectors from all classes
    var idx: usize = 0;
    current = class;

    while (!current.isNil() and current.isObject()) {
        const curr_obj = current.asObject();
        const method_dict = curr_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);

        if (method_dict.isObject()) {
            const dict = method_dict.asObject();
            const dict_size = dict.header.size;
            var i: usize = 0;
            while (i < dict_size) : (i += 2) {
                const key = dict.getField(i, dict_size);
                if (!key.isNil()) {
                    result.setField(idx, key, total_count);
                    idx += 1;
                }
            }
        }

        // Move to superclass
        current = curr_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
    }

    return result_val;
}

fn primClassSuperclass(interp: *Interpreter) InterpreterError!Value {
    const class = try interp.pop();

    if (!class.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class.asObject();
    return class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
}

fn primClassName(interp: *Interpreter) InterpreterError!Value {
    const class = try interp.pop();

    if (!class.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class.asObject();
    return class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
}

fn primClassInstSize(interp: *Interpreter) InterpreterError!Value {
    const class_val = try interp.pop();

    if (!class_val.isObject()) {
        try interp.push(class_val);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);
    if (!format_val.isSmallInt()) {
        try interp.push(class_val);
        return InterpreterError.PrimitiveFailed;
    }

    const info = Heap.decodeInstanceSpec(format_val.asSmallInt());
    return Value.fromSmallInt(@intCast(info.inst_size));
}

// ============================================================================
// Critical Dolphin Primitives
// ============================================================================

fn primSmallIntPrintString(interp: *Interpreter) InterpreterError!Value {
    // Primitive 44: SmallInteger >> printString
    // Convert receiver (SmallInteger) to its decimal string representation
    const receiver = try interp.pop();

    if (!receiver.isSmallInt()) {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    }

    const val = receiver.asSmallInt();

    // Convert to string
    var buf: [24]u8 = undefined;
    const str = std.fmt.bufPrint(&buf, "{d}", .{val}) catch {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    };

    // Create a String object using allocateString
    return interp.heap.allocateString(str) catch {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    };
}

fn primReplaceFromToWith(interp: *Interpreter) InterpreterError!Value {
    // Primitive 50: replaceFrom:to:with:startingAt:
    // Replace elements in receiver from start to stop with elements from replacement starting at repStart
    const rep_start_val = try interp.pop();
    const replacement = try interp.pop();
    const stop_val = try interp.pop();
    const start_val = try interp.pop();
    const receiver = try interp.pop();

    if (!receiver.isObject() or !start_val.isSmallInt() or !stop_val.isSmallInt() or !rep_start_val.isSmallInt()) {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    }

    const start: usize = @intCast(start_val.asSmallInt());
    const stop: usize = @intCast(stop_val.asSmallInt());
    const rep_start: usize = @intCast(rep_start_val.asSmallInt());

    if (start < 1 or stop < start - 1) {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    }

    const recv_obj = receiver.asObject();
    const format = recv_obj.header.getFormat();

    if (format == .bytes) {
        // Byte replacement (strings)
        const recv_bytes = recv_obj.bytes(recv_obj.header.size);
        const count = stop - start + 1;

        if (stop > recv_bytes.len or count == 0) {
            return receiver; // Nothing to replace or bounds ok
        }

        if (replacement.isObject()) {
            const rep_obj = replacement.asObject();
            if (rep_obj.header.getFormat() == .bytes) {
                const rep_bytes = rep_obj.bytes(rep_obj.header.size);
                if (rep_start < 1 or rep_start + count - 1 > rep_bytes.len) {
                    try interp.push(receiver);
                    return InterpreterError.PrimitiveFailed;
                }
                // Use a loop for overlapping copies
                const src = rep_bytes[rep_start - 1 .. rep_start - 1 + count];
                const dst = recv_bytes[start - 1 .. start - 1 + count];
                for (0..count) |i| {
                    dst[i] = src[i];
                }
            }
        }
    } else if (format == .variable or format == .normal) {
        // Object array replacement (variable = indexed, normal = fixed fields)
        const count = stop - start + 1;
        if (stop > recv_obj.header.size) {
            try interp.push(receiver);
            return InterpreterError.PrimitiveFailed;
        }

        if (replacement.isObject()) {
            const rep_obj = replacement.asObject();
            if (rep_start < 1 or rep_start + count - 1 > rep_obj.header.size) {
                try interp.push(receiver);
                return InterpreterError.PrimitiveFailed;
            }
            for (0..count) |i| {
                const val = rep_obj.getField(rep_start - 1 + i, rep_obj.header.size);
                recv_obj.setField(start - 1 + i, val, recv_obj.header.size);
            }
        }
    }

    return receiver;
}

fn primIsKindOf(interp: *Interpreter) InterpreterError!Value {
    // Primitive 57: Object >> isKindOf:
    // Answer whether the receiver is an instance of aClass or one of its subclasses
    const a_class = try interp.pop();
    const receiver = try interp.pop();

    // Get receiver's class
    var current_class = interp.heap.classOf(receiver);

    // Walk up the inheritance chain
    while (!current_class.isNil()) {
        if (current_class.eql(a_class)) {
            return Value.@"true";
        }

        if (current_class.isObject()) {
            const class_obj = current_class.asObject();
            current_class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
        } else {
            break;
        }
    }

    return Value.@"false";
}

fn primInheritsFrom(interp: *Interpreter) InterpreterError!Value {
    // Primitive 58: Behavior >> inheritsFrom:
    // Answer whether the receiver inherits from aClass
    const a_class = try interp.pop();
    const receiver = try interp.pop();

    if (!receiver.isObject()) {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    }

    // Start from superclass, not receiver itself
    var current_class = receiver.asObject().getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);

    while (!current_class.isNil()) {
        if (current_class.eql(a_class)) {
            return Value.@"true";
        }

        if (current_class.isObject()) {
            const class_obj = current_class.asObject();
            current_class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
        } else {
            break;
        }
    }

    return Value.@"false";
}

fn primLookupMethod(interp: *Interpreter) InterpreterError!Value {
    // Primitive 148: Behavior >> lookupMethod:
    // Answer the CompiledMethod for the selector in the receiver's method dictionary or inherited
    const selector = try interp.pop();
    const class = try interp.pop();

    if (!class.isObject()) {
        try interp.push(class);
        return InterpreterError.PrimitiveFailed;
    }

    // Use interpreter's lookupMethod
    if (interp.lookupMethod(class, selector)) |method| {
        return Value.fromObject(@ptrCast(method));
    }

    return Value.nil;
}

fn primAsUtf8String(interp: *Interpreter) InterpreterError!Value {
    // Primitive 216: String >> asUtf8String
    // For Utf8String subclasses, just return self
    // For other strings, would need conversion (not implemented)
    const receiver = try interp.pop();

    if (!receiver.isObject()) {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = receiver.asObject();
    const class_idx = obj.header.class_index;

    // If it's already a string/symbol (UTF8 internally), return self
    if (class_idx == Heap.CLASS_STRING or class_idx == Heap.CLASS_SYMBOL) {
        return receiver;
    }

    // Check if it's a Utf8String subclass by walking the class chain
    var current_class = interp.heap.getClass(class_idx);
    while (!current_class.isNil()) {
        if (current_class.isObject()) {
            const class_obj = current_class.asObject();
            const name_val = class_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    const name_bytes = name_obj.bytes(name_obj.header.size);
                    if (std.mem.eql(u8, name_bytes, "Utf8String") or
                        std.mem.eql(u8, name_bytes, "Symbol") or
                        std.mem.eql(u8, name_bytes, "String"))
                    {
                        return receiver; // Already UTF8
                    }
                }
            }
            current_class = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
        } else {
            break;
        }
    }

    // For other string types, return self (our strings are always UTF8)
    if (obj.header.getFormat() == .bytes) {
        return receiver;
    }

    try interp.push(receiver);
    return InterpreterError.PrimitiveFailed;
}

fn primStringAt(interp: *Interpreter) InterpreterError!Value {
    // Primitive 63: String >> at: (returns Character)
    const index_val = try interp.pop();
    const receiver = try interp.pop();

    if (!index_val.isSmallInt() or !receiver.isObject()) {
        try interp.push(receiver);
        try interp.push(index_val);
        return InterpreterError.PrimitiveFailed;
    }

    const index = index_val.asSmallInt();
    if (index < 1) {
        try interp.push(receiver);
        try interp.push(index_val);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = receiver.asObject();
    const format = obj.header.getFormat();

    // For byte objects (strings), get the byte at the given 1-based index
    if (format == .bytes or format == .words) {
        const byte_size: usize = obj.header.size;
        const idx: usize = @intCast(index - 1);
        if (idx >= byte_size) {
            try interp.push(receiver);
            try interp.push(index_val);
            return InterpreterError.PrimitiveFailed;
        }
        const bytes = obj.bytes(byte_size);
        const ch: u21 = bytes[idx];
        return Value.fromCharacter(ch);
    }

    try interp.push(receiver);
    try interp.push(index_val);
    return InterpreterError.PrimitiveFailed;
}

fn primStringAtPut(interp: *Interpreter) InterpreterError!Value {
    // Primitive 64: String >> at:put:
    const value = try interp.pop();
    const index_val = try interp.pop();
    const receiver = try interp.pop();

    if (!index_val.isSmallInt() or !receiver.isObject()) {
        try interp.push(receiver);
        try interp.push(index_val);
        try interp.push(value);
        return InterpreterError.PrimitiveFailed;
    }

    const index = index_val.asSmallInt();
    if (index < 1) {
        try interp.push(receiver);
        try interp.push(index_val);
        try interp.push(value);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = receiver.asObject();
    const format = obj.header.getFormat();

    // For byte objects (strings), set the byte at the given 1-based index
    if (format == .bytes or format == .words) {
        const byte_size: usize = obj.header.size;
        const idx: usize = @intCast(index - 1);
        if (idx >= byte_size) {
            try interp.push(receiver);
            try interp.push(index_val);
            try interp.push(value);
            return InterpreterError.PrimitiveFailed;
        }

        // Get the character code to store
        var ch: u8 = 0;
        if (value.isCharacter()) {
            const code = value.asCharacter();
            if (code > 255) {
                try interp.push(receiver);
                try interp.push(index_val);
                try interp.push(value);
                return InterpreterError.PrimitiveFailed;
            }
            ch = @intCast(code);
        } else if (value.isSmallInt()) {
            const code = value.asSmallInt();
            if (code < 0 or code > 255) {
                try interp.push(receiver);
                try interp.push(index_val);
                try interp.push(value);
                return InterpreterError.PrimitiveFailed;
            }
            ch = @intCast(code);
        } else {
            try interp.push(receiver);
            try interp.push(index_val);
            try interp.push(value);
            return InterpreterError.PrimitiveFailed;
        }

        const bytes = obj.bytes(byte_size);
        bytes[idx] = ch;
        return value;
    }

    try interp.push(receiver);
    try interp.push(index_val);
    try interp.push(value);
    return InterpreterError.PrimitiveFailed;
}

fn primIdentityHash(interp: *Interpreter) InterpreterError!Value {
    // Primitive 147: Object >> identityHash
    // Answer a hash value based on object identity
    const receiver = try interp.pop();

    if (receiver.isSmallInt()) {
        // SmallIntegers hash to themselves
        return receiver;
    } else if (receiver.isCharacter()) {
        return Value.fromSmallInt(@intCast(receiver.asCharacter()));
    } else if (receiver.isNil()) {
        return Value.fromSmallInt(0);
    } else if (receiver.isTrue()) {
        return Value.fromSmallInt(1);
    } else if (receiver.isFalse()) {
        return Value.fromSmallInt(2);
    } else if (receiver.isObject()) {
        // Use object address as hash (shifted to fit in SmallInteger)
        const addr = @intFromPtr(receiver.asObject());
        const hash: i32 = @intCast((addr >> 3) & 0x3FFFFFFF);
        return Value.fromSmallInt(hash);
    }

    return Value.fromSmallInt(0);
}

// ============================================================================
// Transcript Primitives (650+)
// ============================================================================

fn primTranscriptShow(interp: *Interpreter) InterpreterError!Value {
    // Primitive 650: Transcript >> show:
    // Output the string representation of an object to transcript
    const obj = try interp.pop();
    const receiver = try interp.pop(); // Pop the receiver (Transcript)

    // Get string representation and output to transcript (via callback or stdout)
    const str_repr = try objectPrintString(interp, obj);
    interp.writeTranscript(str_repr);

    // Return self (the receiver) for chaining
    return receiver;
}

fn primTranscriptCr(interp: *Interpreter) InterpreterError!Value {
    // Primitive 651: Transcript >> cr
    // Output a newline to transcript
    const receiver = try interp.pop(); // Pop the receiver (Transcript)
    interp.writeTranscript("\n");
    return receiver; // Return self for chaining
}

fn primTranscriptNextPutAll(interp: *Interpreter) InterpreterError!Value {
    // Primitive 652: Transcript >> nextPutAll:
    // Output a string directly to transcript
    const str = try interp.pop();
    const receiver = try interp.pop(); // Pop the receiver (Transcript)

    if (str.isObject()) {
        const str_obj = str.asObject();
        const byte_size = @as(usize, @intCast(str_obj.header.size));
        const bytes = str_obj.bytes(byte_size);
        interp.writeTranscript(bytes[0..byte_size]);
    }

    return receiver; // Return self for chaining
}

fn primTranscriptFlush(interp: *Interpreter) InterpreterError!Value {
    // Primitive 653: Transcript >> flush
    // No-op for stdout since it's always flushed
    const receiver = try interp.pop(); // Pop the receiver (Transcript)
    return receiver; // Return self for chaining
}

fn objectPrintString(interp: *Interpreter, obj: Value) InterpreterError![]const u8 {
    // Helper function to get string representation of an object
    _ = interp; // unused
    if (obj.isSmallInt()) {
        const num = obj.asSmallInt();
        // Convert to string - return a static string for now
        if (num == 0) return "0";
        if (num == 1) return "1";
        if (num == 2) return "2";
        if (num == 3) return "3";
        if (num == 4) return "4";
        if (num == 5) return "5";
        if (num == 6) return "6";
        if (num == 7) return "7";
        if (num == 8) return "8";
        if (num == 9) return "9";
        // For other numbers, just return a placeholder
        return "<number>";
    } else if (obj.isCharacter()) {
        // For characters, just return a placeholder
        return "<char>";
    } else if (obj.isNil()) {
        return "nil";
    } else if (obj.isTrue()) {
        return "true";
    } else if (obj.isFalse()) {
        return "false";
    } else if (obj.isObject()) {
        const str_obj = obj.asObject();
        const byte_size = @as(usize, @intCast(str_obj.header.size));
        const bytes = str_obj.bytes(byte_size);
        return bytes[0..byte_size];
    }

    return "<unknown>";
}

// ============================================================================
// Global Lookup Primitives
// ============================================================================

fn primGlobalAt(interp: *Interpreter) InterpreterError!Value {
    const key = try interp.pop();
    _ = try interp.pop(); // Pop receiver (Smalltalk)

    // Get the symbol name
    if (!key.isObject()) {
        return Value.nil;
    }

    const key_obj = key.asObject();
    const key_class = key_obj.header.class_index;

    // Check if it's a Symbol (class ID 4)
    if (key_class != Heap.CLASS_SYMBOL) {
        return Value.nil;
    }

    const key_size = key_obj.header.size;
    const key_bytes = key_obj.bytes(@intCast(key_size));

    // Look up in globals
    if (interp.heap.getGlobal(key_bytes[0..@intCast(key_size)])) |val| {
        return val;
    }

    return Value.nil;
}

fn primGlobalAtIfAbsent(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // Pop absent block - we'll handle this in Smalltalk
    const key = try interp.pop();
    _ = try interp.pop(); // Pop receiver (Smalltalk)

    // Get the symbol name
    if (!key.isObject()) {
        // Return nil and let Smalltalk handle the absent block
        return InterpreterError.PrimitiveFailed;
    }

    const key_obj = key.asObject();
    const key_class = key_obj.header.class_index;

    // Check if it's a Symbol (class ID 4)
    if (key_class != Heap.CLASS_SYMBOL) {
        return InterpreterError.PrimitiveFailed;
    }

    const key_size = key_obj.header.size;
    const key_bytes = key_obj.bytes(@intCast(key_size));

    // Look up in globals
    if (interp.heap.getGlobal(key_bytes[0..@intCast(key_size)])) |val| {
        return val;
    }

    // Not found - primitive fails, Smalltalk fallback handles absent block
    return InterpreterError.PrimitiveFailed;
}

// ============================================================================
// FFI Primitives
// ============================================================================

// Helper constant for FFI availability
const ffi_enabled = build_options.ffi_enabled;

fn primFFIMalloc(interp: *Interpreter) InterpreterError!Value {
    const size = try interp.pop();
    _ = try interp.pop(); // receiver (LibC class)

    if (!ffi_enabled) {
        try interp.push(size);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{size};
    const result = ffi.libc_malloc(interp.heap, &args) catch {
        try interp.push(size);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIFree(interp: *Interpreter) InterpreterError!Value {
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ptr};
    _ = ffi.libc_free(interp.heap, &args) catch {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return Value.nil;
}

fn primFFIStrlen(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{str};
    const result = ffi.libc_strlen(interp.heap, &args) catch {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIPuts(interp: *Interpreter) InterpreterError!Value {
    const str = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{str};
    const result = ffi.libc_puts(interp.heap, &args, interp.heap.allocator) catch {
        try interp.push(str);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFISin(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_sin(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFICos(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_cos(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFISqrt(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_sqrt(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIPow(interp: *Interpreter) InterpreterError!Value {
    const y = try interp.pop();
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(y);
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ x, y };
    const result = ffi.libc_pow(interp.heap, &args) catch {
        try interp.push(y);
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIExp(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_exp(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFILog(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_log(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIFloor(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_floor(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFICeil(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_ceil(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIFabs(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_abs_float(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIAtan2(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    const y = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        try interp.push(y);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ y, x };
    const result = ffi.libc_atan2(interp.heap, &args) catch {
        try interp.push(x);
        try interp.push(y);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFITan(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_tan(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIAsin(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_asin(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIAcos(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_acos(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIAtan(interp: *Interpreter) InterpreterError!Value {
    const x = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{x};
    const result = ffi.libc_atan(interp.heap, &args) catch {
        try interp.push(x);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIMemset(interp: *Interpreter) InterpreterError!Value {
    const size = try interp.pop();
    const val = try interp.pop();
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(size);
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ ptr, val, size };
    const result = ffi.libc_memset(interp.heap, &args) catch {
        try interp.push(size);
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIMemcpy(interp: *Interpreter) InterpreterError!Value {
    const size = try interp.pop();
    const src = try interp.pop();
    const dest = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(size);
        try interp.push(src);
        try interp.push(dest);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ dest, src, size };
    const result = ffi.libc_memcpy(interp.heap, &args) catch {
        try interp.push(size);
        try interp.push(src);
        try interp.push(dest);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIReadInt8(interp: *Interpreter) InterpreterError!Value {
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ptr};
    const result = ffi.readInt8(interp.heap, &args) catch {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIReadInt16(interp: *Interpreter) InterpreterError!Value {
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ptr};
    const result = ffi.readInt16(interp.heap, &args) catch {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIReadInt32(interp: *Interpreter) InterpreterError!Value {
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ptr};
    const result = ffi.readInt32(interp.heap, &args) catch {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIReadInt64(interp: *Interpreter) InterpreterError!Value {
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ptr};
    const result = ffi.readInt64(interp.heap, &args) catch {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIReadFloat64(interp: *Interpreter) InterpreterError!Value {
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ptr};
    const result = ffi.readFloat64(interp.heap, &args) catch {
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return result;
}

fn primFFIWriteInt8(interp: *Interpreter) InterpreterError!Value {
    const val = try interp.pop();
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ ptr, val };
    _ = ffi.writeInt8(interp.heap, &args) catch {
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return Value.nil;
}

fn primFFIWriteInt32(interp: *Interpreter) InterpreterError!Value {
    const val = try interp.pop();
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ ptr, val };
    _ = ffi.writeInt32(interp.heap, &args) catch {
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return Value.nil;
}

fn primFFIWriteFloat64(interp: *Interpreter) InterpreterError!Value {
    const val = try interp.pop();
    const ptr = try interp.pop();
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    }

    var args = [_]Value{ ptr, val };
    _ = ffi.writeFloat64(interp.heap, &args) catch {
        try interp.push(val);
        try interp.push(ptr);
        return InterpreterError.PrimitiveFailed;
    };
    return Value.nil;
}

// ============================================================================
// Generic Auto-Generated FFI Call
// ============================================================================

/// Generic FFI call primitive
/// Stack: receiver (library name as String/Symbol) args (Array) funcName (String/Symbol)
/// Usage in Smalltalk: LibMath ffiCall: #sin with: { 1.0 }
fn primFFIGenericCall(interp: *Interpreter) InterpreterError!Value {
    // Stack for 'LibMath' ffiCall: #sin with: { 1.0 }
    // Pop in reverse order of how they were pushed
    const args_val = try interp.pop();      // { 1.0 } - second keyword arg
    const func_name_val = try interp.pop(); // #sin - first keyword arg
    const receiver = try interp.pop();      // 'LibMath' - receiver

    // Debug output
    std.debug.print("DEBUG primFFIGenericCall: receiver.bits=0x{x} func.bits=0x{x} args.bits=0x{x}\n", .{ receiver.bits, func_name_val.bits, args_val.bits });
    if (getStringFromValue(interp.heap, receiver)) |lib| {
        std.debug.print("  lib_name='{s}'\n", .{lib});
    } else {
        std.debug.print("  lib_name=<not a string>\n", .{});
    }
    if (getStringFromValue(interp.heap, func_name_val)) |func| {
        std.debug.print("  func_name='{s}'\n", .{func});
    } else {
        std.debug.print("  func_name=<not a string>\n", .{});
    }

    if (!ffi_enabled) {
        try interp.push(receiver);
        try interp.push(func_name_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Get library name from receiver (should be a class name like 'LibMath')
    const lib_name = getStringFromValue(interp.heap, receiver) orelse {
        // Restore stack in original order (receiver first, then args in order)
        try interp.push(receiver);
        try interp.push(func_name_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    };

    // Get function name
    const func_name = getStringFromValue(interp.heap, func_name_val) orelse {
        try interp.push(receiver);
        try interp.push(func_name_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    };

    // Extract arguments from array
    // Note: header.size is the number of slots, not bytes
    var args_slice: []const Value = &.{};
    if (args_val.isObject()) {
        const args_obj = args_val.asObject();
        if (args_obj.header.class_index == Heap.CLASS_ARRAY) {
            const arr_size = args_obj.header.size; // size IS the slot count
            args_slice = args_obj.fields(arr_size);
        }
    }

    // Call the FFI function
    const result = ffi_autogen.callFFI(lib_name, func_name, interp.heap, args_slice, interp.heap.allocator) catch |err| {
        std.debug.print("FFI FAIL: {s}::{s} err={}\n", .{ lib_name, func_name, err });
        try interp.push(receiver);
        try interp.push(func_name_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    };

    return result;
}

/// Helper to extract a string from a Value (String or Symbol)
fn getStringFromValue(_: *Heap, val: Value) ?[]const u8 {
    if (val.isObject()) {
        const obj = val.asObject();
        if (obj.header.class_index == Heap.CLASS_STRING or
            obj.header.class_index == Heap.CLASS_SYMBOL)
        {
            return obj.bytes(obj.header.size);
        }
    }
    // Also check for class objects - they might have a name
    if (val.isObject()) {
        const obj = val.asObject();
        // If it's a class, get its name
        if (obj.header.size >= 8 * @sizeOf(Value)) {
            const class_fields = obj.fields(8);
            const name_val = class_fields[Heap.CLASS_FIELD_NAME];
            if (name_val.isObject()) {
                const name_obj = name_val.asObject();
                if (name_obj.header.class_index == Heap.CLASS_STRING or
                    name_obj.header.class_index == Heap.CLASS_SYMBOL)
                {
                    return name_obj.bytes(name_obj.header.size);
                }
            }
        }
    }
    return null;
}

// ============================================================================
// FFI Introspection Primitives
// ============================================================================

/// Primitive 760: FFI libraries
/// Returns an Array of library name Strings
fn primFFILibraries(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // receiver

    if (!ffi_enabled) {
        // Return empty array when FFI is disabled
        const empty = interp.heap.allocateObject(Heap.CLASS_ARRAY, 0, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        return Value.fromObject(empty);
    }

    const lib_names = ffi_autogen.available_libraries;

    // Allocate result array
    const result = interp.heap.allocateObject(Heap.CLASS_ARRAY, lib_names.len, .variable) catch {
        return InterpreterError.OutOfMemory;
    };

    // Fill with library names as Strings
    for (lib_names, 0..) |name, i| {
        const str = interp.heap.allocateString(name) catch {
            return InterpreterError.OutOfMemory;
        };
        result.setField(i, str, lib_names.len);
    }

    return Value.fromObject(result);
}

/// Primitive 761: FFI functions for a library
/// Receiver is library name as String, returns Array of function name Strings
fn primFFIFunctions(interp: *Interpreter) InterpreterError!Value {
    const receiver = try interp.pop();

    if (!ffi_enabled) {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    }

    // Get library name from receiver
    const lib_name = getStringFromValue(interp.heap, receiver) orelse {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    };

    // Get function names for this library
    const func_names = ffi_autogen.getLibraryFunctionNames(lib_name) orelse {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    };

    // Allocate result array
    const result = interp.heap.allocateObject(Heap.CLASS_ARRAY, func_names.len, .variable) catch {
        return InterpreterError.OutOfMemory;
    };

    // Fill with function names as Strings
    for (func_names, 0..) |name, i| {
        const str = interp.heap.allocateString(name) catch {
            return InterpreterError.OutOfMemory;
        };
        result.setField(i, str, func_names.len);
    }

    return Value.fromObject(result);
}

// ============================================================================
// ByteArray/ExternalStructure Field Access Primitives
// ============================================================================
// These primitives allow reading and writing typed values at byte offsets
// within ByteArray objects, enabling C struct interop.

/// Helper to get byte data from a byte object (ByteArray, String, etc.)
fn getByteObjectData(val: Value) ?[]u8 {
    if (!val.isObject()) return null;
    const obj = val.asObject();

    // Check if it's a byte-type object
    const class_idx = obj.header.class_index;
    if (class_idx == Heap.CLASS_BYTE_ARRAY or
        class_idx == Heap.CLASS_STRING or
        class_idx == Heap.CLASS_SYMBOL)
    {
        return obj.bytes(obj.header.size);
    }
    return null;
}

/// Primitive 770: ByteArray >> uint8At: offset
/// Read unsigned 8-bit value at 0-based byte offset
fn primBytesUint8At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset >= bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    return Value.fromSmallInt(@intCast(bytes[offset]));
}

/// Primitive 771: ByteArray >> uint16At: offset
/// Read unsigned 16-bit value at 0-based byte offset (little-endian)
fn primBytesUint16At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 2 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val = std.mem.readInt(u16, bytes[offset..][0..2], .little);
    return Value.fromSmallInt(@intCast(val));
}

/// Primitive 772: ByteArray >> uint32At: offset
/// Read unsigned 32-bit value at 0-based byte offset (little-endian)
fn primBytesUint32At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 4 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val = std.mem.readInt(u32, bytes[offset..][0..4], .little);
    return Value.fromSmallInt(@intCast(val));
}

/// Primitive 773: ByteArray >> int8At: offset
/// Read signed 8-bit value at 0-based byte offset
fn primBytesInt8At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset >= bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: i8 = @bitCast(bytes[offset]);
    return Value.fromSmallInt(@intCast(val));
}

/// Primitive 774: ByteArray >> int16At: offset
/// Read signed 16-bit value at 0-based byte offset (little-endian)
fn primBytesInt16At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 2 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val = std.mem.readInt(i16, bytes[offset..][0..2], .little);
    return Value.fromSmallInt(@intCast(val));
}

/// Primitive 775: ByteArray >> int32At: offset
/// Read signed 32-bit value at 0-based byte offset (little-endian)
fn primBytesInt32At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 4 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val = std.mem.readInt(i32, bytes[offset..][0..4], .little);
    return Value.fromSmallInt(@intCast(val));
}

/// Primitive 776: ByteArray >> float32At: offset
/// Read 32-bit float value at 0-based byte offset
fn primBytesFloat32At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 4 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const bits = std.mem.readInt(u32, bytes[offset..][0..4], .little);
    const val: f32 = @bitCast(bits);
    const float_obj = interp.heap.allocateFloat(@floatCast(val)) catch {
        return InterpreterError.OutOfMemory;
    };
    return float_obj;
}

/// Primitive 777: ByteArray >> float64At: offset
/// Read 64-bit float value at 0-based byte offset
fn primBytesFloat64At(interp: *Interpreter) InterpreterError!Value {
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 8 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        return InterpreterError.PrimitiveFailed;
    }

    const bits = std.mem.readInt(u64, bytes[offset..][0..8], .little);
    const val: f64 = @bitCast(bits);
    const float_obj = interp.heap.allocateFloat(val) catch {
        return InterpreterError.OutOfMemory;
    };
    return float_obj;
}

/// Primitive 780: ByteArray >> uint8At: offset put: value
/// Write unsigned 8-bit value at 0-based byte offset
fn primBytesUint8AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt() or !value_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset >= bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: u8 = @truncate(@as(u64, @intCast(value_val.asSmallInt())));
    bytes[offset] = val;
    return receiver; // Return receiver for chaining
}

/// Primitive 781: ByteArray >> uint16At: offset put: value
/// Write unsigned 16-bit value at 0-based byte offset (little-endian)
fn primBytesUint16AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt() or !value_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 2 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: u16 = @truncate(@as(u64, @intCast(value_val.asSmallInt())));
    std.mem.writeInt(u16, bytes[offset..][0..2], val, .little);
    return receiver;
}

/// Primitive 782: ByteArray >> uint32At: offset put: value
/// Write unsigned 32-bit value at 0-based byte offset (little-endian)
fn primBytesUint32AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt() or !value_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 4 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: u32 = @truncate(@as(u64, @intCast(value_val.asSmallInt())));
    std.mem.writeInt(u32, bytes[offset..][0..4], val, .little);
    return receiver;
}

/// Primitive 783: ByteArray >> int8At: offset put: value
/// Write signed 8-bit value at 0-based byte offset
fn primBytesInt8AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt() or !value_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset >= bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: i8 = @truncate(value_val.asSmallInt());
    bytes[offset] = @bitCast(val);
    return receiver;
}

/// Primitive 784: ByteArray >> int16At: offset put: value
/// Write signed 16-bit value at 0-based byte offset (little-endian)
fn primBytesInt16AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt() or !value_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 2 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: i16 = @truncate(value_val.asSmallInt());
    std.mem.writeInt(i16, bytes[offset..][0..2], val, .little);
    return receiver;
}

/// Primitive 785: ByteArray >> int32At: offset put: value
/// Write signed 32-bit value at 0-based byte offset (little-endian)
fn primBytesInt32AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt() or !value_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 4 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const val: i32 = @truncate(value_val.asSmallInt());
    std.mem.writeInt(i32, bytes[offset..][0..4], val, .little);
    return receiver;
}

/// Primitive 786: ByteArray >> float32At: offset put: value
/// Write 32-bit float value at 0-based byte offset
fn primBytesFloat32AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 4 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Get float value from SmallInteger or Float object
    var float_val: f32 = 0;
    if (value_val.isSmallInt()) {
        float_val = @floatFromInt(value_val.asSmallInt());
    } else if (value_val.isObject()) {
        const obj = value_val.asObject();
        if (obj.header.class_index == Heap.CLASS_FLOAT) {
            const float_bytes = obj.bytes(8);
            const f64_val: f64 = @bitCast(float_bytes[0..8].*);
            float_val = @floatCast(f64_val);
        } else {
            try interp.push(receiver);
            try interp.push(offset_val);
            try interp.push(value_val);
            return InterpreterError.PrimitiveFailed;
        }
    } else {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const bits: u32 = @bitCast(float_val);
    std.mem.writeInt(u32, bytes[offset..][0..4], bits, .little);
    return receiver;
}

/// Primitive 787: ByteArray >> float64At: offset put: value
/// Write 64-bit float value at 0-based byte offset
fn primBytesFloat64AtPut(interp: *Interpreter) InterpreterError!Value {
    const value_val = try interp.pop();
    const offset_val = try interp.pop();
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    };

    if (!offset_val.isSmallInt()) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const offset: usize = @intCast(offset_val.asSmallInt());
    if (offset + 8 > bytes.len) {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Get float value from SmallInteger or Float object
    var float_val: f64 = 0;
    if (value_val.isSmallInt()) {
        float_val = @floatFromInt(value_val.asSmallInt());
    } else if (value_val.isObject()) {
        const obj = value_val.asObject();
        if (obj.header.class_index == Heap.CLASS_FLOAT) {
            const float_bytes = obj.bytes(8);
            float_val = @bitCast(float_bytes[0..8].*);
        } else {
            try interp.push(receiver);
            try interp.push(offset_val);
            try interp.push(value_val);
            return InterpreterError.PrimitiveFailed;
        }
    } else {
        try interp.push(receiver);
        try interp.push(offset_val);
        try interp.push(value_val);
        return InterpreterError.PrimitiveFailed;
    }

    const bits: u64 = @bitCast(float_val);
    std.mem.writeInt(u64, bytes[offset..][0..8], bits, .little);
    return receiver;
}

/// Primitive 788: ByteArray >> address
/// Return pointer to the byte data as an integer (for FFI)
fn primBytesAddress(interp: *Interpreter) InterpreterError!Value {
    const receiver = try interp.pop();

    const bytes = getByteObjectData(receiver) orelse {
        try interp.push(receiver);
        return InterpreterError.PrimitiveFailed;
    };

    const addr: i61 = @intCast(@intFromPtr(bytes.ptr));
    return Value.fromSmallInt(addr);
}

// ============================================================================
// FFI Struct Introspection Primitives (placeholders for now)
// ============================================================================

/// Primitive 790: FFILibrary structNamesFor: 'LibName'
/// Returns array of struct names available in the library
fn primFFIStructNames(interp: *Interpreter) InterpreterError!Value {
    // Stack order for structNamesFor: is: receiver, libraryName
    const lib_name_val = try interp.pop();  // libraryName (arg1)
    _ = try interp.pop();                    // receiver (FFILibrary class)

    if (!ffi_enabled) {
        // Return empty array when FFI is disabled
        const empty_arr = interp.heap.allocateObject(Heap.CLASS_ARRAY, 0, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        return Value.fromObject(empty_arr);
    }

    const lib_name = getStringFromValue(interp.heap, lib_name_val) orelse {
        return InterpreterError.PrimitiveFailed;
    };

    // Get struct names from ffi_generated
    const struct_names = ffi_generated.getLibraryStructNames(lib_name) orelse {
        // Library doesn't support structs or unknown library - return empty array
        const empty_arr = interp.heap.allocateObject(Heap.CLASS_ARRAY, 0, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        return Value.fromObject(empty_arr);
    };

    // Create array of symbols for struct names
    const result_obj = interp.heap.allocateObject(Heap.CLASS_ARRAY, struct_names.len, .variable) catch {
        return InterpreterError.OutOfMemory;
    };
    const slots = result_obj.fields(struct_names.len);

    for (struct_names, 0..) |name, i| {
        const symbol = interp.heap.internSymbol(name) catch {
            return InterpreterError.OutOfMemory;
        };
        slots[i] = symbol;
    }

    return Value.fromObject(result_obj);
}

/// Primitive 791: FFILibrary structInfo: #StructName for: 'LibName'
/// Returns struct metadata as an Array: #(size #(field1Name field1Offset field1Size field1Type) ...)
fn primFFIStructInfo(interp: *Interpreter) InterpreterError!Value {
    // Stack order for structInfo:for: is: receiver, structName, libraryName
    const lib_name_val = try interp.pop();      // libraryName (arg2)
    const struct_name_val = try interp.pop();   // structName (arg1)
    _ = try interp.pop();                        // receiver (FFILibrary class)

    if (!ffi_enabled) {
        return Value.nil;
    }

    const lib_name = getStringFromValue(interp.heap, lib_name_val) orelse {
        return InterpreterError.PrimitiveFailed;
    };

    const struct_name = getStringFromValue(interp.heap, struct_name_val) orelse {
        return InterpreterError.PrimitiveFailed;
    };

    // Get struct info from ffi_generated
    const info = ffi_generated.getStructInfo(lib_name, struct_name) orelse {
        // Struct not found
        return Value.nil;
    };

    // Create result array: #(size #(field1...) #(field2...) ...)
    // Each field is: #(name offset size accessorType)
    const result_obj = interp.heap.allocateObject(Heap.CLASS_ARRAY, 1 + info.fields.len, .variable) catch {
        return InterpreterError.OutOfMemory;
    };
    const slots = result_obj.fields(1 + info.fields.len);

    // First element is the struct size
    slots[0] = Value.fromSmallInt(@intCast(info.size));

    // Remaining elements are field info arrays
    for (info.fields, 0..) |field, i| {
        // Create field array: #(name offset size accessorType)
        const field_obj = interp.heap.allocateObject(Heap.CLASS_ARRAY, 4, .variable) catch {
            return InterpreterError.OutOfMemory;
        };
        const field_slots = field_obj.fields(4);

        // Field name as string (for easier concatenation in Smalltalk)
        const name_str = interp.heap.allocateString(field.name) catch {
            return InterpreterError.OutOfMemory;
        };
        field_slots[0] = name_str;

        // Field offset
        field_slots[1] = Value.fromSmallInt(@intCast(field.offset));

        // Field size
        field_slots[2] = Value.fromSmallInt(@intCast(field.size));

        // Accessor type as string (for easier concatenation in Smalltalk)
        const type_str = interp.heap.allocateString(field.accessor_type) catch {
            return InterpreterError.OutOfMemory;
        };
        field_slots[3] = type_str;

        slots[1 + i] = Value.fromObject(field_obj);
    }

    return Value.fromObject(result_obj);
}

/// Primitive 792: FFI call that handles struct arguments and returns
/// This wraps ffi_call with struct marshaling support
fn primFFICallWithStruct(interp: *Interpreter) InterpreterError!Value {
    // For now, delegate to regular FFI call
    // TODO: Add struct-aware marshaling
    return primFFIGenericCall(interp);
}

/// Primitive 793: Runtime FFI call via libffi
/// Stack: receiver (function pointer as Integer), signature (String), args (Array)
/// Usage: fnPtr ffiCallWithSignature: 'uint32(uint32,string)' args: { 35633. 'hello' }
/// Signature format: "returnType(argType1,argType2,...)"
/// Types: void, int, uint, int32, uint32, int64, uint64, float, double, pointer, string
fn primFFIRuntimeCall(interp: *Interpreter) InterpreterError!Value {
    const args_val = try interp.pop(); // args array
    const sig_val = try interp.pop(); // signature string
    const receiver = try interp.pop(); // function pointer

    if (!ffi_enabled) {
        try interp.push(receiver);
        try interp.push(sig_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Get function pointer from receiver (should be SmallInt with address)
    const fn_ptr: *const anyopaque = blk: {
        if (receiver.isSmallInt()) {
            const addr: usize = @intCast(receiver.asSmallInt());
            if (addr == 0) {
                try interp.push(receiver);
                try interp.push(sig_val);
                try interp.push(args_val);
                return InterpreterError.PrimitiveFailed;
            }
            break :blk @ptrFromInt(addr);
        }
        try interp.push(receiver);
        try interp.push(sig_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    };

    // Get signature string
    const signature = getStringFromValue(interp.heap, sig_val) orelse {
        try interp.push(receiver);
        try interp.push(sig_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    };

    // Extract arguments from array
    var args_slice: []const Value = &.{};
    if (args_val.isObject()) {
        const args_obj = args_val.asObject();
        if (args_obj.header.class_index == Heap.CLASS_ARRAY) {
            const arr_size = args_obj.header.size;
            args_slice = args_obj.fields(arr_size);
        }
    }

    // Call via runtime FFI
    const result = ffi_runtime.callWithSignature(
        fn_ptr,
        signature,
        args_slice,
        interp.heap,
        interp.heap.allocator,
    ) catch |err| {
        std.debug.print("Runtime FFI error: {}\n", .{err});
        try interp.push(receiver);
        try interp.push(sig_val);
        try interp.push(args_val);
        return InterpreterError.PrimitiveFailed;
    };

    return result;
}

/// Primitive 794: Generate Smalltalk method source for an FFI function
/// Stack: receiver (library name String), funcName (Symbol)
/// Returns: String with method source code, or nil if function not found
fn primFFIGenerateMethod(interp: *Interpreter) InterpreterError!Value {
    const func_name_val = try interp.pop();
    const lib_name_val = try interp.pop();

    if (!ffi_enabled) {
        return Value.nil;
    }

    const lib_name = getStringFromValue(interp.heap, lib_name_val) orelse {
        return Value.nil;
    };

    const func_name = getStringFromValue(interp.heap, func_name_val) orelse {
        return Value.nil;
    };

    // Get the FFI function info
    const functions = ffi_generated.getLibraryFunctions(lib_name) orelse {
        return Value.nil;
    };

    // Find the function
    var func_info: ?ffi_autogen.FFIFunction = null;
    for (functions) |f| {
        if (std.mem.eql(u8, f.name, func_name)) {
            func_info = f;
            break;
        }
    }

    const func = func_info orelse return Value.nil;

    // Generate Smalltalk method source
    var source_buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&source_buf);
    const writer = fbs.writer();

    // Build method selector with arg names
    // e.g., for sin(double) -> sin: x
    // for pow(double, double) -> pow: x y: y
    if (func.arg_count == 0) {
        // No args: just the function name
        writer.print("{s}\n", .{func.name}) catch return Value.nil;
    } else if (func.arg_count == 1) {
        // One arg: funcName: arg1
        writer.print("{s}: arg1\n", .{func.name}) catch return Value.nil;
    } else {
        // Multiple args: funcName: arg1 with: arg2 ...
        writer.print("{s}: arg1", .{func.name}) catch return Value.nil;
        var i: usize = 1;
        while (i < func.arg_count) : (i += 1) {
            const keyword = switch (i) {
                1 => "with",
                2 => "and",
                else => "arg",
            };
            writer.print(" {s}: arg{d}", .{ keyword, i + 1 }) catch return Value.nil;
        }
        writer.writeAll("\n") catch return Value.nil;
    }

    // Add comment with type information
    writer.writeAll("    \"FFI: ") catch return Value.nil;
    writer.print("{s}(", .{func.name}) catch return Value.nil;
    var i: usize = 0;
    while (i < func.arg_count) : (i += 1) {
        if (i > 0) writer.writeAll(", ") catch return Value.nil;
        writer.print("{s}", .{simplifyTypeName(func.arg_types[i])}) catch return Value.nil;
    }
    writer.print(") -> {s}\"\n", .{simplifyTypeName(func.return_type)}) catch return Value.nil;

    // Add primitive call
    writer.writeAll("    ^'") catch return Value.nil;
    writer.print("{s}", .{lib_name}) catch return Value.nil;
    writer.print("' ffiCall: #{s} with: {{", .{func.name}) catch return Value.nil;

    // Add arguments to array
    i = 0;
    while (i < func.arg_count) : (i += 1) {
        if (i > 0) writer.writeAll(". ") catch return Value.nil;
        writer.print(" arg{d}", .{i + 1}) catch return Value.nil;
    }
    writer.writeAll(" }") catch return Value.nil;

    // Allocate and return the string
    const source = fbs.getWritten();
    const str_val = interp.heap.allocateString(source) catch {
        return InterpreterError.OutOfMemory;
    };
    return str_val;
}

/// Primitive 795: Get FFI function info
/// Stack: receiver (library name String), funcName (Symbol)
/// Returns: Array #(argCount returnType arg1Type arg2Type ...) or nil
fn primFFIFunctionInfo(interp: *Interpreter) InterpreterError!Value {
    const func_name_val = try interp.pop();
    const lib_name_val = try interp.pop();

    if (!ffi_enabled) {
        return Value.nil;
    }

    const lib_name = getStringFromValue(interp.heap, lib_name_val) orelse {
        return Value.nil;
    };

    const func_name = getStringFromValue(interp.heap, func_name_val) orelse {
        return Value.nil;
    };

    // Get the FFI function info
    const functions = ffi_generated.getLibraryFunctions(lib_name) orelse {
        return Value.nil;
    };

    // Find the function
    var func_info: ?ffi_autogen.FFIFunction = null;
    for (functions) |f| {
        if (std.mem.eql(u8, f.name, func_name)) {
            func_info = f;
            break;
        }
    }

    const func = func_info orelse return Value.nil;

    // Create result array: #(argCount returnType arg1Type arg2Type ...)
    const array_size = 2 + func.arg_count;
    const result_obj = interp.heap.allocateObject(Heap.CLASS_ARRAY, array_size, .variable) catch {
        return InterpreterError.OutOfMemory;
    };
    const slots = result_obj.fields(array_size);

    // Slot 0: arg count
    slots[0] = Value.fromSmallInt(@intCast(func.arg_count));

    // Slot 1: return type as string
    const ret_type_val = interp.heap.allocateString(simplifyTypeName(func.return_type)) catch {
        return InterpreterError.OutOfMemory;
    };
    slots[1] = ret_type_val;

    // Remaining slots: argument types
    var i: usize = 0;
    while (i < func.arg_count) : (i += 1) {
        const arg_type_val = interp.heap.allocateString(simplifyTypeName(func.arg_types[i])) catch {
            return InterpreterError.OutOfMemory;
        };
        slots[2 + i] = arg_type_val;
    }

    return Value.fromObject(result_obj);
}

/// Simplify C type names for display
fn simplifyTypeName(full_name: []const u8) []const u8 {
    // Handle common patterns
    if (std.mem.eql(u8, full_name, "f64")) return "Float";
    if (std.mem.eql(u8, full_name, "f32")) return "Float";
    if (std.mem.eql(u8, full_name, "i32")) return "Integer";
    if (std.mem.eql(u8, full_name, "i64")) return "Integer";
    if (std.mem.eql(u8, full_name, "u32")) return "Integer";
    if (std.mem.eql(u8, full_name, "u64")) return "Integer";
    if (std.mem.eql(u8, full_name, "u8")) return "Integer";
    if (std.mem.eql(u8, full_name, "i8")) return "Integer";
    if (std.mem.eql(u8, full_name, "c_int")) return "Integer";
    if (std.mem.eql(u8, full_name, "c_uint")) return "Integer";
    if (std.mem.eql(u8, full_name, "c_long")) return "Integer";
    if (std.mem.eql(u8, full_name, "c_ulong")) return "Integer";
    if (std.mem.eql(u8, full_name, "c_float")) return "Float";
    if (std.mem.eql(u8, full_name, "c_double")) return "Float";
    if (std.mem.eql(u8, full_name, "void")) return "nil";
    if (std.mem.eql(u8, full_name, "bool")) return "Boolean";

    // Handle pointers - look for "[*c]" or "*"
    const is_pointer = std.mem.indexOf(u8, full_name, "*") != null or
        std.mem.indexOf(u8, full_name, "[*c]") != null;

    if (is_pointer) {
        // Check for string types (char* or u8*)
        if (std.mem.indexOf(u8, full_name, "u8") != null) return "String";
        if (std.mem.indexOf(u8, full_name, "char") != null) return "String";
        // Other pointers - could be structs or raw pointers
        return "Pointer/ByteArray";
    }

    // Check for struct types (often start with uppercase in C)
    if (full_name.len > 0 and full_name[0] >= 'A' and full_name[0] <= 'Z') {
        return "Struct (ByteArray)";
    }

    // Return as-is for unknown types
    return full_name;
}

// ============================================================================
// Dynamic Class and Method Creation Primitives
// ============================================================================

fn primSubclassCreate(interp: *Interpreter) InterpreterError!Value {
    // Primitive 796: Class >> subclass: #Name
    // Creates a new subclass of the receiver with the given name
    const name_val = try interp.pop();
    const superclass_val = try interp.pop();

    // Verify superclass is a class object
    if (!superclass_val.isObject()) {
        try interp.push(superclass_val);
        try interp.push(name_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Get class name from symbol
    const name_str = getStringFromValue(interp.heap, name_val) orelse {
        try interp.push(superclass_val);
        try interp.push(name_val);
        return InterpreterError.PrimitiveFailed;
    };

    const superclass_obj = superclass_val.asObject();

    // Create the new class using createDynamicClass
    const new_class = filein.createDynamicClass(
        interp.heap,
        name_str,
        superclass_obj,
        "", // inst_var_names
        "", // class_var_names
        "", // pool_dict_names
        "", // class_inst_var_names
        .normal, // class_format
        null, // existing_class
    ) catch {
        try interp.push(superclass_val);
        try interp.push(name_val);
        return InterpreterError.PrimitiveFailed;
    };

    return Value.fromObject(new_class);
}

fn primCompileMethod(interp: *Interpreter) InterpreterError!Value {
    // Primitive 797: Class >> compile: 'source'
    // Compiles and installs a method from source code
    const source_val = try interp.pop();
    const class_val = try interp.pop();

    // Verify class is a class object
    if (!class_val.isObject()) {
        std.debug.print("DEBUG primCompileMethod: class_val is not object\n", .{});
        try interp.push(class_val);
        try interp.push(source_val);
        return InterpreterError.PrimitiveFailed;
    }

    // Get source string
    const source_str = getStringFromValue(interp.heap, source_val) orelse {
        try interp.push(class_val);
        try interp.push(source_val);
        return InterpreterError.PrimitiveFailed;
    };

    const class_obj = class_val.asObject();

    // Use FileIn to compile the method
    filein.compileAndInstallMethod(interp.heap, class_obj, source_str) catch {
        try interp.push(class_val);
        try interp.push(source_val);
        return InterpreterError.PrimitiveFailed;
    };

    return class_val;
}

/// Primitive 798: OBJLoader >> load: 'path'
/// Loads an OBJ file and returns an Array: {vertexData. indexData. vertexCount. indexCount}
fn primLoadOBJFile(interp: *Interpreter) InterpreterError!Value {
    const path_val = try interp.pop();
    const receiver = try interp.pop();
    _ = receiver;

    // Get path string
    const path_str = getStringFromValue(interp.heap, path_val) orelse {
        try interp.push(path_val);
        return InterpreterError.PrimitiveFailed;
    };

    // Load OBJ file
    var mesh = obj_loader.loadOBJ(path_str, interp.heap.allocator) catch {
        std.debug.print("Failed to load OBJ: {s}\n", .{path_str});
        try interp.push(path_val);
        return InterpreterError.PrimitiveFailed;
    };
    defer mesh.deinit();

    // Convert to ByteArrays
    const arrays = obj_loader.meshToByteArrays(&mesh, interp.heap) catch {
        try interp.push(path_val);
        return InterpreterError.PrimitiveFailed;
    };

    // Create result array: {vertexData, indexData, vertexCount, indexCount}
    const result = interp.heap.allocateArray(4) catch {
        return InterpreterError.OutOfMemory;
    };
    const result_obj = result.asObject();
    const fields = result_obj.fields(4);
    fields[0] = arrays.vertices;
    fields[1] = arrays.indices;
    fields[2] = Value.fromSmallInt(@intCast(mesh.vertices.len));
    fields[3] = Value.fromSmallInt(@intCast(mesh.indices.len));

    return result;
}

// ============================================================================
// UI Process Primitives
// ============================================================================

/// Primitive 920: UIProcess >> processOneIteration
/// Process one iteration of the UI loop (input, rendering, transcript flush).
/// Returns true if UI should continue running, false if quit was requested.
fn primUIProcessIteration(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // receiver (UIProcess)

    if (app_mod.g_app) |app| {
        const should_continue = app.processOneIteration();
        return Value.fromBool(should_continue);
    } else {
        // No TUI running - always return true (keep going)
        return Value.@"true";
    }
}

/// Primitive 921: UIProcess >> isRunning
/// Returns true if the TUI is currently running, false otherwise.
fn primUIIsRunning(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // receiver (UIProcess)

    if (app_mod.g_app) |app| {
        return Value.fromBool(app.isRunning());
    } else {
        return Value.@"false";
    }
}
