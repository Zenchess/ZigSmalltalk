#!/bin/bash
# Load core Dolphin classes into Zig Smalltalk
# Usage: ./dolphin-core/load-core.sh [additional-args...]
#
# Example: ./dolphin-core/load-core.sh -e "3 + 4"

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

# Core classes in dependency order
CORE_CLASSES=(
    "Dolphin/Core/Object Arts/Dolphin/Base/Object.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/UndefinedObject.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Behavior.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/ClassDescription.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Class.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Metaclass.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Magnitude.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/ArithmeticValue.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Number.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Integer.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/SmallInteger.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Interval.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Collection.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/SequenceableCollection.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/ArrayedCollection.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Array.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/String.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/UtfEncodedString.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Utf8String.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Symbol.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/SequencedGrowableCollection.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/OrderedCollection.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Dictionary.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Set.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Boolean.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/True.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/False.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Association.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Exception.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Error.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Notification.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/Message.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/MessageNotUnderstood.cls"
    # Stream hierarchy for printString support
    "Dolphin/Core/Object Arts/Dolphin/Base/Stream.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/SequencedStream.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/PositionableStream.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/WriteStream.cls"
    "Dolphin/Core/Object Arts/Dolphin/Base/ReadStream.cls"
)

STUB_FILES=(
    "dolphin-core/stubs/shell-stub.st"
    "dolphin-core/stubs/sunit-stubs.st"
)

SUNIT_CLASSES=(
    "Dolphin/Core/Object Arts/Dolphin/Base/IdentityDictionary.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestFailure.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/ResumableTestFailure.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestSkip.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestCaseResult.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestResult.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestCase.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestSuite.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestResource.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/SimpleTestResource.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/SUnitNameResolver.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/SUnitTest.cls"
    "Dolphin/Core/Contributions/Camp Smalltalk/SUnit/TestRunner.cls"
)

POST_STUBS=(
    "dolphin-core/stubs/sunit-overrides.st"
    "dolphin-core/stubs/methoddict-overrides.st"
    "dolphin-core/stubs/testrunner-overrides.st"
)

# Build the command
if [ -f "zig-out/bin/zig-smalltalk" ]; then
    BINARY="zig-out/bin/zig-smalltalk"
else
echo "Error: zig-smalltalk binary not found. Run 'zig build' first."
    exit 1
fi

exec "$BINARY" "${CORE_CLASSES[@]}" "${STUB_FILES[@]}" "${SUNIT_CLASSES[@]}" "${POST_STUBS[@]}" "$@"
