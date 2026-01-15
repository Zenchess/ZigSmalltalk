"Smalltalk TUI Loader

NOTE: This file cannot load the TUI components because FileStream>>fileIn:
is not implemented. Use the shell script instead:

    ./repl-tui.sh

This will load all ANSI classes plus the TUI components, then start the REPL.
Once loaded, start the TUI with:

    TuiApp start

Or for a demo:

    TuiApp demo
"

Transcript show: '========================================'; cr.
Transcript show: 'To load the Smalltalk TUI, use:'; cr.
Transcript show: '    ./repl-tui.sh'; cr.
Transcript show: ''; cr.
Transcript show: 'Then start with: TuiApp start'; cr.
Transcript show: '========================================'; cr.
