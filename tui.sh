#!/bin/bash

# Load ANSI classes and launch the TUI browser

echo "Loading ANSI classes and launching TUI..."
echo ""

# Load files in dependency order (filter empty lines and comments)
FILES=$(grep -v '^$' load-order-fixed.txt | grep -v '^#' | tr '\n' ' ')
STUBS="dolphin-core/stubs/sunit-stubs.st"
FFI="ffi-test.st"
STRUCTS="external-structure.st"

# Load ANSI files, FFI bindings, external structures, and launch TUI
./zig-out/bin/zig-smalltalk --tui $FILES $STUBS $FFI $STRUCTS
