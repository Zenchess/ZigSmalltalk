#!/bin/bash

# Load ANSI classes and launch the TUI browser
#
# Usage: ./tui.sh [options]
#   --image <path>    Load a saved image snapshot
#   --help            Show all available options
#
# Examples:
#   ./tui.sh                     Start with fresh ANSI classes
#   ./tui.sh --image my.image   Load from saved image

echo "Loading ANSI classes and launching TUI..."
echo ""

# Load files in dependency order (filter empty lines and comments)
# Exclude ANSI ExternalStructure.cls since we have our own
FILES=$(grep -v '^$' load-order-tui.txt | grep -v '^#' | grep -v 'ExternalStructure.cls' | tr '\n' ' ')
STUBS="dolphin-core/stubs/sunit-stubs.st"
FFI_CLASSES="src/image/ffi.st"
FFI="ffi-test.st"
STRUCTS="external-structure.st"
FFI_STRUCTS="load-ffi-structs.st"

# Load ANSI files first, then ffi.st (FFILibrary, ExternalStructure class definitions),
# then external-structure.st (additional methods), then auto-generate FFI structs last
# Pass through any command line arguments (like --image)
./zig-out/bin/zig-smalltalk --tui "$@" $FILES $STUBS $FFI_CLASSES $STRUCTS $FFI $FFI_STRUCTS
