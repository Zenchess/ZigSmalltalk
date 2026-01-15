#!/bin/bash

# Load ANSI classes and Smalltalk TUI, then launch interactive REPL
#
# Usage: ./repl-tui.sh [options]
#   --image <path>    Load a saved image snapshot
#   --help            Show all available options

echo "Loading ANSI classes and Smalltalk TUI..."
echo ""

# Load files in dependency order (filter empty lines and comments)
# Exclude ANSI ExternalStructure.cls since we have our own
FILES=$(grep -v '^$' load-order-tui.txt | grep -v '^#' | grep -v 'ExternalStructure.cls' | tr '\n' ' ')
STUBS="dolphin-core/stubs/sunit-stubs.st src/image/exception-stubs.st"
FFI_CLASSES="src/image/ffi.st"
FFI="ffi-test.st"
STRUCTS="external-structure.st"
FFI_STRUCTS="load-ffi-structs.st"

# Smalltalk TUI files in dependency order
TUI_FILES="src/image/tui/AnsiTerminal.st \
           src/image/tui/Theme.st \
           src/image/tui/Screen.st \
           src/image/tui/Widget.st \
           src/image/tui/TextArea.st \
           src/image/tui/UIWidgets.st \
           src/image/tui/TuiApp.st"

# Load everything: ANSI files, FFI, structs, then TUI
./zig-out/bin/zig-smalltalk "$@" $FILES $STUBS $FFI_CLASSES $STRUCTS $FFI $FFI_STRUCTS $TUI_FILES
