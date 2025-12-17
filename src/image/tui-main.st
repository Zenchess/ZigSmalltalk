"Main entry point for Smalltalk TUI"
"Load this file after terminal.st, widgets.st, and browser.st"

"Main startup class"
Object subclass: #SmalltalkTUI
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Main'!

!SmalltalkTUI class methodsFor: 'startup'!
start
    "Start the Smalltalk TUI"
    Transcript show: 'Starting Smalltalk TUI...'; cr.
    TUIApp start
!

demo
    "Run a quick demo of terminal primitives"
    | term |
    term := Terminal current.
    term initialize.

    "Clear and draw"
    term clear.
    term fg: Colors title.
    term bold: true.
    term printCentered: 'Smalltalk TUI Demo' row: 2.
    term bold: false.

    "Draw a box"
    term fg: Colors border.
    term drawBoxAt: {5. 4} extent: {40. 10}.

    "Show some colored text"
    term fg: Colors keyword.
    term setCursorRow: 6 col: 8.
    term write: 'self'.
    term fg: Colors foreground.
    term write: ' printString'.

    term fg: Colors string.
    term setCursorRow: 8 col: 8.
    term write: '''Hello, Smalltalk TUI!'''.

    term fg: Colors comment.
    term setCursorRow: 10 col: 8.
    term write: '"Press any key to exit"'.

    term resetStyle.
    term flush.

    "Wait for key"
    term readKey.

    term shutdown.
    Transcript show: 'Demo finished.'; cr
!

testTerminal
    "Test basic terminal functions"
    | term size |
    term := Terminal current.

    Transcript show: 'Testing terminal primitives...'; cr.

    "Get size without init"
    size := term size.
    Transcript show: 'Terminal size: ', (size at: 1) printString, 'x', (size at: 2) printString; cr.

    "Initialize"
    term initialize.
    Transcript show: 'Terminal initialized'; cr.

    "Clear screen"
    term clear.
    Transcript show: 'Screen cleared'; cr.

    "Set cursor"
    term setCursorRow: 5 col: 10.
    term write: 'Hello from Smalltalk!'.

    "Colors"
    term setCursorRow: 7 col: 10.
    term foregroundR: 255 g: 100 b: 100.
    term write: 'Red text '.
    term foregroundR: 100 g: 255 b: 100.
    term write: 'Green text '.
    term foregroundR: 100 g: 100 b: 255.
    term write: 'Blue text'.

    term resetStyle.
    term setCursorRow: 9 col: 10.
    term write: 'Press any key...'.
    term flush.

    "Wait for input"
    term readKey.

    "Cleanup"
    term shutdown.
    Transcript show: 'Terminal test complete'; cr
! !

"Show usage"
Transcript show: 'Smalltalk TUI loaded. Commands:'; cr.
Transcript show: '  TUIApp start           - Start the TUI application'; cr.
Transcript show: '  SmalltalkTUI demo      - Run a demo'; cr.
Transcript show: '  SmalltalkTUI testTerminal - Test terminal'; cr.
