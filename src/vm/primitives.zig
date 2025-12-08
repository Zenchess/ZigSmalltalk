const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");
const bytecodes = @import("bytecodes.zig");
const interpreter_mod = @import("interpreter.zig");

const Value = object.Value;
const Object = object.Object;
const Heap = memory.Heap;
const Interpreter = interpreter_mod.Interpreter;
const InterpreterError = interpreter_mod.InterpreterError;
const Primitive = bytecodes.Primitive;

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
        .basic_new => primBasicNew(interp),
        .basic_new_size => primBasicNewSize(interp),
        .basic_identity_hash => primHash(interp),

        // ====================================================================
        // Block/Control flow (Dolphin: 81-84)
        // ====================================================================
        .block_value => primBlockValue(interp),
        .block_value_with_args => primBlockValueWithArgs(interp),
        .perform => primPerform(interp),
        .perform_with_args => primPerformWithArgs(interp),

        // Extended block value primitives (490-493)
        .block_value_1 => primBlockValue1(interp),
        .block_value_2 => primBlockValue2(interp),
        .block_value_3 => primBlockValue3(interp),
        .block_value_4 => primBlockValue4(interp),

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
        .small_as_float => primSmallAsFloat(interp),

        // ====================================================================
        // String operations
        // ====================================================================
        .string_concat => primStringConcat(interp),
        .string_compare => primStringCompare(interp),
        .string_less_than => primStringLessThan(interp),
        .string_greater_than => primStringGreaterThan(interp),
        .string_less_or_equal => primStringLessOrEqual(interp),
        .string_greater_or_equal => primStringGreaterOrEqual(interp),
        .string_equal => primStringEqual(interp),
        .string_copy_from_to => primStringCopyFromTo(interp),

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
    // Dolphin's / primitive is exact division (fails if not evenly divisible)
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
        if (@mod(av, bv) != 0) {
            // Not exact division - fail primitive (Dolphin behavior)
            try interp.push(a);
            try interp.push(b);
            return InterpreterError.PrimitiveFailed;
        }
        const result = @divTrunc(av, bv);
        return Value.fromSmallInt(result);
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

    // Identity equality for other objects
    return Value.fromBool(a.eql(b));
}

fn primNotEqual(interp: *Interpreter) InterpreterError!Value {
    const b = try interp.pop();
    const a = try interp.pop();

    if (a.isSmallInt() and b.isSmallInt()) {
        return Value.fromBool(a.asSmallInt() != b.asSmallInt());
    }

    return Value.fromBool(!a.eql(b));
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

    // Get block data: [outerContext, startPC, numArgs, method]
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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

        interp.method = @ptrCast(@alignCast(method_val.asObject()));
        interp.ip = @intCast(start_pc.asSmallInt());

        // Execute the block (result is discarded)
        _ = interp.interpretLoop() catch |err| {
            interp.ip = saved_ip;
            interp.method = saved_method;
            return err;
        };

        interp.ip = saved_ip;
        interp.method = saved_method;
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

    var num_fields: usize = 0;
    if (format_val.isSmallInt()) {
        num_fields = @intCast(format_val.asSmallInt());
    }

    const obj = interp.heap.allocateObject(class_obj.header.class_index, num_fields, .normal) catch {
        try interp.push(class);
        return InterpreterError.OutOfMemory;
    };

    return Value.fromObject(obj);
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

    const obj = interp.heap.allocateObject(class_obj.header.class_index, num_indexed, .variable) catch {
        try interp.push(class);
        try interp.push(size);
        return InterpreterError.OutOfMemory;
    };

    return Value.fromObject(obj);
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
    const idx: usize = @intCast(index.asSmallInt() - 1); // Smalltalk is 1-indexed

    // Get size (this is simplified - real impl needs proper size tracking)
    const format = obj.header.getFormat();
    if (format == .variable or format == .normal) {
        // Assume we can read the field
        // In reality we need proper bounds checking
        const val = obj.getField(idx, 256); // Max size placeholder
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
    const idx: usize = @intCast(index.asSmallInt() - 1);

    obj.setField(idx, val, 256);
    return val;
}

fn primSize(interp: *Interpreter) InterpreterError!Value {
    const recv = try interp.pop();

    if (!recv.isObject()) {
        try interp.push(recv);
        return InterpreterError.PrimitiveFailed;
    }

    const obj = recv.asObject();

    // For variable-size objects, return the size from the header
    // This works for Arrays, Strings, ByteArrays, etc.
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

    if (code.isSmallInt()) {
        const cp = code.asSmallInt();
        if (cp >= 0 and cp <= 0x10FFFF) {
            return Value.fromCharacter(@intCast(cp));
        }
    }

    try interp.push(code);
    return InterpreterError.PrimitiveFailed;
}

// ============================================================================
// Block Evaluation Primitives
// ============================================================================

fn primBlockValue(interp: *Interpreter) InterpreterError!Value {
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

    // Get block data: [outerContext, startPC, numArgs, method]
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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

    // Execute the block by interpreting its bytecodes
    // This is a simplified version - we directly execute within the current interpreter
    const saved_ip = interp.ip;
    const saved_method = interp.method;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());

    // Execute until we hit a return
    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;

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

    // Get block data
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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

    // Push argument as temporary
    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    // Push the argument
    try interp.push(arg);

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

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

    // Get block data
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    // Push arguments
    try interp.push(arg1);
    try interp.push(arg2);

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

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

    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    try interp.push(arg1);
    try interp.push(arg2);
    try interp.push(arg3);

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

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

    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    try interp.push(arg1);
    try interp.push(arg2);
    try interp.push(arg3);
    try interp.push(arg4);

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

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

    // Get block data
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

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
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    // Push all arguments from array
    var i: usize = 0;
    while (i < array_size) : (i += 1) {
        const arg = args_obj.getField(i, array_size);
        try interp.push(arg);
    }

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

    return result;
}

// Object >> perform: (Dolphin primitive 83)
fn primPerform(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // selector
    _ = try interp.pop(); // receiver
    // Not yet implemented - would need method lookup
    return InterpreterError.PrimitiveFailed;
}

// Object >> perform:withArguments: (Dolphin primitive 84)
fn primPerformWithArgs(interp: *Interpreter) InterpreterError!Value {
    _ = try interp.pop(); // args
    _ = try interp.pop(); // selector
    _ = try interp.pop(); // receiver
    // Not yet implemented - would need method lookup
    return InterpreterError.PrimitiveFailed;
}

// ============================================================================
// Boolean Control Flow Primitives
// ============================================================================

/// Helper to evaluate a block (shared by control flow primitives)
fn evaluateBlock(interp: *Interpreter, block: Value) InterpreterError!Value {
    if (!block.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const block_obj = block.asObject();
    if (block_obj.header.class_index != Heap.CLASS_BLOCK_CLOSURE) {
        return InterpreterError.PrimitiveFailed;
    }

    // Get block data: [outerContext, startPC, numArgs, method]
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 0) {
        // Control flow blocks should take no arguments
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;

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

    // Get block data
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
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

    // Loop from start to limit with step 1
    const start_val = start.asSmallInt();
    const limit_val = limit.asSmallInt();

    var i = start_val;
    while (i <= limit_val) : (i += 1) {
        // Save interpreter state
        const saved_ip = interp.ip;
        const saved_method = interp.method;
        const saved_temp_base = interp.temp_base;

        interp.method = @ptrCast(@alignCast(method_val.asObject()));
        interp.ip = @intCast(start_pc.asSmallInt());
        interp.temp_base = interp.sp;

        // Push the loop variable as argument
        try interp.push(Value.fromSmallInt(i));

        // Execute the block
        _ = interp.interpretLoop() catch |err| {
            interp.ip = saved_ip;
            interp.method = saved_method;
            interp.temp_base = saved_temp_base;
            return err;
        };

        // Restore interpreter state
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
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

    // Get block data
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
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
            const saved_temp_base = interp.temp_base;

            interp.method = @ptrCast(@alignCast(method_val.asObject()));
            interp.ip = @intCast(start_pc.asSmallInt());
            interp.temp_base = interp.sp;

            // Push the loop variable as argument
            try interp.push(Value.fromSmallInt(i));

            // Execute the block
            _ = interp.interpretLoop() catch |err| {
                interp.ip = saved_ip;
                interp.method = saved_method;
                interp.temp_base = saved_temp_base;
                return err;
            };

            // Restore interpreter state
            interp.ip = saved_ip;
            interp.method = saved_method;
            interp.temp_base = saved_temp_base;
        }
    } else {
        while (i >= limit_val) : (i += step_val) {
            // Save interpreter state
            const saved_ip = interp.ip;
            const saved_method = interp.method;
            const saved_temp_base = interp.temp_base;

            interp.method = @ptrCast(@alignCast(method_val.asObject()));
            interp.ip = @intCast(start_pc.asSmallInt());
            interp.temp_base = interp.sp;

            // Push the loop variable as argument
            try interp.push(Value.fromSmallInt(i));

            // Execute the block
            _ = interp.interpretLoop() catch |err| {
                interp.ip = saved_ip;
                interp.method = saved_method;
                interp.temp_base = saved_temp_base;
                return err;
            };

            // Restore interpreter state
            interp.ip = saved_ip;
            interp.method = saved_method;
            interp.temp_base = saved_temp_base;
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

    // Get block data: [outerContext, startPC, numArgs, method]
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 1) {
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    // Push the argument
    try interp.push(arg);

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

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

    // Get block data
    const start_pc = block_obj.getField(1, 4);
    const num_args_val = block_obj.getField(2, 4);
    const method_val = block_obj.getField(3, 4);

    if (!start_pc.isSmallInt() or !num_args_val.isSmallInt() or !method_val.isObject()) {
        return InterpreterError.PrimitiveFailed;
    }

    const expected_args = num_args_val.asSmallInt();
    if (expected_args != 2) {
        return InterpreterError.PrimitiveFailed;
    }

    const saved_ip = interp.ip;
    const saved_method = interp.method;
    const saved_temp_base = interp.temp_base;

    interp.method = @ptrCast(@alignCast(method_val.asObject()));
    interp.ip = @intCast(start_pc.asSmallInt());
    interp.temp_base = interp.sp;

    // Push arguments
    try interp.push(arg1);
    try interp.push(arg2);

    const result = interp.interpretLoop() catch |err| {
        interp.ip = saved_ip;
        interp.method = saved_method;
        interp.temp_base = saved_temp_base;
        return err;
    };

    interp.ip = saved_ip;
    interp.method = saved_method;
    interp.temp_base = saved_temp_base;

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

    // Get array size from class format
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

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

    // Get array size
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

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

    const array_obj = array.asObject();

    // Get array size
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

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

    // Get array size
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

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

    // Get array size
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

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

    // Get array size
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(detect_block);
        try interp.push(exception_block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(detect_block);
        try interp.push(exception_block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

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

    // Get array size
    const class_val = interp.heap.getClass(array_obj.header.class_index);
    if (!class_val.isObject()) {
        try interp.push(array);
        try interp.push(initial);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const class_obj = class_val.asObject();
    const format_val = class_obj.getField(Heap.CLASS_FIELD_FORMAT, Heap.CLASS_NUM_FIELDS);

    if (!format_val.isSmallInt()) {
        try interp.push(array);
        try interp.push(initial);
        try interp.push(block);
        return InterpreterError.PrimitiveFailed;
    }

    const size: usize = @intCast(format_val.asSmallInt());

    // Fold over elements
    var accumulator = initial;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const elem = array_obj.getField(i, size);
        accumulator = try evaluateBlockWith2(interp, block, accumulator, elem);
    }

    return accumulator;
}

test "Primitives - arithmetic" {
    const allocator = std.testing.allocator;
    const heap = try memory.Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var interp = Interpreter.init(heap);

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
    const heap = try memory.Heap.init(allocator, 1024 * 1024);
    defer heap.deinit();

    var interp = Interpreter.init(heap);

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
    if (!a.isObject() or !b.isObject()) return null;

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
