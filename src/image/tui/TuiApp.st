"TuiApp - Main TUI application class with event loop
 Orchestrates all UI components and handles the main run loop"

Object subclass: #TuiApp
    instanceVariableNames: 'terminal screen tabBar statusBar activeTabIndex tabs running'
    classVariableNames: 'Current'
    poolDictionaries: ''
    category: 'TUI-App'!

!TuiApp class methodsFor: 'instance creation'!
current
    "Return singleton app instance"
    Current isNil ifTrue: [
        Current := self basicNew initialize
    ].
    ^Current
!

new
    ^self current
!

reset
    "Reset the singleton"
    Current ifNotNil: [
        Current shutdown.
        Current := nil
    ]
! !

!TuiApp class methodsFor: 'startup'!
start
    "Start the TUI application"
    self current run
!

demo
    "Run a quick demo"
    [
        | app |
        app := self current.
        app setupDemo.
        app run
    ] on: Error do: [:ex |
        "Ensure terminal is restored on error"
        AnsiTerminal current shutdown.
        ex pass
    ]
! !

!TuiApp methodsFor: 'initialization'!
initialize
    terminal := AnsiTerminal current.
    tabs := OrderedCollection new.
    activeTabIndex := 1.
    running := false.

    self setupTerminal.
    self setupScreen.
    self setupUI
!

setupTerminal
    "Initialize terminal for TUI mode"
    terminal enterRawMode.
    terminal enterAlternateScreen.
    terminal hideCursor.
    terminal clear
!

setupScreen
    "Create screen buffer"
    terminal updateSize.
    screen := TuiScreen current.
    screen resize: terminal width height: terminal height
!

setupUI
    "Create UI components"
    self setupTabBar.
    self setupStatusBar.
    self setupDefaultTabs
!

setupTabBar
    tabBar := TuiTabBar new.
    tabBar rect: (TuiRect x: 1 y: 1 width: terminal width height: 1).
    tabBar tabs: #(
        #('Transcript' $1)
        #('Workspace' $2)
        #('Browser' $3)
        #('REPL' $4)
    ).
    tabBar onTabChange: [:index | self switchToTab: index]
!

setupStatusBar
    statusBar := TuiStatusBar new.
    statusBar rect: (TuiRect x: 1 y: terminal height width: terminal width height: 1).
    statusBar message: 'Ready'.
    statusBar items: #(
        #('Ctrl+Q' 'Quit')
        #('F1-F4' 'Tabs')
    )
!

setupDefaultTabs
    "Create default tab panels - subclasses/users can customize"
    | transcriptTab workspaceTab browserTab replTab |

    "Transcript tab"
    transcriptTab := TuiTranscriptPanel new.
    transcriptTab rect: self contentRect.
    transcriptTab title: 'Transcript'.
    tabs add: transcriptTab.

    "Workspace tab"
    workspaceTab := TuiWorkspacePanel new.
    workspaceTab rect: self contentRect.
    workspaceTab title: 'Workspace'.
    tabs add: workspaceTab.

    "Browser tab (placeholder)"
    browserTab := TuiWidget new.
    browserTab rect: self contentRect.
    browserTab title: 'Browser'.
    tabs add: browserTab.

    "REPL tab"
    replTab := TuiReplPanel new.
    replTab rect: self contentRect.
    replTab title: 'REPL'.
    tabs add: replTab.

    "Focus first tab"
    self switchToTab: 1
!

setupDemo
    "Setup demo content"
    | workspace |
    workspace := tabs at: 2.
    (workspace respondsTo: #setText:) ifTrue: [
        workspace setText: '"Welcome to Smalltalk TUI!
Use F1-F4 to switch tabs.
Press Ctrl+D to execute code.
Press Ctrl+Q to quit.

Try typing:
3 + 4
Smalltalk keys
"'
    ]
! !

!TuiApp methodsFor: 'accessing'!
terminal
    ^terminal
!

screen
    ^screen
!

tabBar
    ^tabBar
!

statusBar
    ^statusBar
!

activeTab
    (activeTabIndex > 0 and: [activeTabIndex <= tabs size])
        ifTrue: [^tabs at: activeTabIndex].
    ^nil
!

contentRect
    "Return rect for tab content (between tab bar and status bar)"
    ^TuiRect
        x: 1
        y: 2
        width: terminal width
        height: terminal height - 2
! !

!TuiApp methodsFor: 'tab management'!
switchToTab: index
    (index > 0 and: [index <= tabs size]) ifTrue: [
        self activeTab ifNotNil: [:tab | tab blur].
        activeTabIndex := index.
        tabBar activeTab: index.
        self activeTab ifNotNil: [:tab |
            tab rect: self contentRect.
            tab focus
        ].
        self updateStatusForTab: index
    ]
!

updateStatusForTab: index
    "Update status bar items based on active tab"
    index = 1 ifTrue: [
        "Transcript"
        statusBar items: #(#('Ctrl+Q' 'Quit') #('PgUp/Dn' 'Scroll'))
    ].
    index = 2 ifTrue: [
        "Workspace"
        statusBar items: #(#('Ctrl+Q' 'Quit') #('Ctrl+D' 'Do It') #('Ctrl+P' 'Print'))
    ].
    index = 3 ifTrue: [
        "Browser"
        statusBar items: #(#('Ctrl+Q' 'Quit') #('Tab' 'Navigate'))
    ].
    index = 4 ifTrue: [
        "REPL"
        statusBar items: #(#('Ctrl+Q' 'Quit') #('Enter' 'Eval') #('Up/Dn' 'History'))
    ]
! !

!TuiApp methodsFor: 'event loop'!
run
    "Main event loop"
    running := true.

    [running] whileTrue: [
        self handleResize.
        self processEvents.
        self render.
        self sleep: 10
    ].

    self shutdown
!

processEvents
    "Process pending input events"
    | event |
    [
        event := terminal pollKey.
        event notNil
    ] whileTrue: [
        self handleKey: event
    ]
!

handleKey: keyCode
    "Handle a key event"

    "Global keys"
    keyCode = AnsiTerminal keyCtrlQ ifTrue: [
        running := false.
        ^self
    ].

    keyCode = AnsiTerminal keyCtrlL ifTrue: [
        "Refresh screen"
        screen forceRedraw.
        ^self
    ].

    "Tab switching with F keys"
    (tabBar handleKey: keyCode) ifTrue: [^self].

    "Pass to active tab"
    self activeTab ifNotNil: [:tab |
        tab handleKey: keyCode
    ]
!

handleResize
    "Check for terminal resize"
    | newWidth newHeight |
    terminal updateSize.
    newWidth := terminal width.
    newHeight := terminal height.

    (newWidth ~= screen width or: [newHeight ~= screen height]) ifTrue: [
        terminal clear.
        screen resize: newWidth height: newHeight.
        screen forceRedraw.

        "Update component positions"
        tabBar rect: (TuiRect x: 1 y: 1 width: newWidth height: 1).
        statusBar rect: (TuiRect x: 1 y: newHeight width: newWidth height: 1).

        "Update active tab"
        self activeTab ifNotNil: [:tab |
            tab rect: self contentRect
        ]
    ]
!

render
    "Render all UI components"
    screen clear.

    "Draw tab bar"
    tabBar draw: screen.

    "Draw active tab content"
    self activeTab ifNotNil: [:tab |
        tab draw: screen
    ].

    "Draw status bar"
    statusBar draw: screen.

    "Flush to terminal"
    screen flush
!

sleep: milliseconds
    "Sleep for given milliseconds"
    "Use delay primitive if available, otherwise busy-wait"
    Processor yield
! !

!TuiApp methodsFor: 'shutdown'!
shutdown
    "Restore terminal and cleanup"
    running := false.
    terminal disableMouse.
    terminal disableBracketedPaste.
    terminal exitAlternateScreen.
    terminal exitRawMode.
    terminal showCursor.
    terminal resetStyle.

    "Clear singleton"
    Current := nil
!

quit
    running := false
! !

!TuiApp methodsFor: 'transcript'!
transcriptShow: aString
    "Add text to transcript"
    | transcript |
    tabs size >= 1 ifTrue: [
        transcript := tabs at: 1.
        (transcript respondsTo: #addLine:) ifTrue: [
            transcript addLine: aString
        ]
    ]
!

transcriptCr
    "Add newline to transcript"
    self transcriptShow: ''
! !


"============================================================"
"TuiTranscriptPanel - Scrollable transcript output"
"============================================================"
TuiWidget subclass: #TuiTranscriptPanel
    instanceVariableNames: 'lines scrollTop maxLines'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Panels'!

!TuiTranscriptPanel class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiTranscriptPanel methodsFor: 'initialization'!
initialize
    super initialize.
    lines := OrderedCollection new.
    scrollTop := 0.
    maxLines := 1000.

    "Welcome message"
    self addLine: 'Zig Smalltalk TUI'.
    self addLine: ''.
    self addLine: 'Press F1-F4 to switch tabs'.
    self addLine: 'Press Ctrl+Q to quit'.
    self addLine: ''
! !

!TuiTranscriptPanel methodsFor: 'output'!
addLine: aString
    lines add: aString.
    lines size > maxLines ifTrue: [
        lines removeFirst
    ].
    self scrollToBottom
!

addText: aString
    "Add text, handling newlines"
    (aString subStrings: (String with: Character lf)) do: [:line |
        self addLine: line
    ]
!

clear
    lines := OrderedCollection new.
    scrollTop := 0
!

scrollToBottom
    | visibleHeight |
    visibleHeight := self contentRect height.
    scrollTop := (lines size - visibleHeight) max: 0
! !

!TuiTranscriptPanel methodsFor: 'scrolling'!
scrollUp
    scrollTop := (scrollTop - 1) max: 0
!

scrollDown
    | maxScroll |
    maxScroll := (lines size - self contentRect height) max: 0.
    scrollTop := (scrollTop + 1) min: maxScroll
!

pageUp
    | pageSize |
    pageSize := self contentRect height.
    scrollTop := (scrollTop - pageSize) max: 0
!

pageDown
    | pageSize maxScroll |
    pageSize := self contentRect height.
    maxScroll := (lines size - pageSize) max: 0.
    scrollTop := (scrollTop + pageSize) min: maxScroll
! !

!TuiTranscriptPanel methodsFor: 'key handling'!
handleKey: keyCode
    keyCode = AnsiTerminal keyUp ifTrue: [self scrollUp. ^true].
    keyCode = AnsiTerminal keyDown ifTrue: [self scrollDown. ^true].
    keyCode = AnsiTerminal keyPageUp ifTrue: [self pageUp. ^true].
    keyCode = AnsiTerminal keyPageDown ifTrue: [self pageDown. ^true].
    keyCode = AnsiTerminal keyHome ifTrue: [scrollTop := 0. ^true].
    keyCode = AnsiTerminal keyEnd ifTrue: [self scrollToBottom. ^true].
    ^false
! !

!TuiTranscriptPanel methodsFor: 'drawing'!
drawSelf: screen
    | content row startLine endLine |
    self drawBorder: screen.

    content := self contentRect.
    startLine := scrollTop + 1.
    endLine := (scrollTop + content height) min: lines size.
    row := content y.

    startLine to: endLine do: [:i |
        | line |
        line := lines at: i.
        line size > content width ifTrue: [
            line := line copyFrom: 1 to: content width
        ].
        screen drawText: line at: content x y: row style: TuiStyle normal.
        row := row + 1
    ].

    "Clear remaining rows"
    [row < (content y + content height)] whileTrue: [
        screen fillRect: (TuiRect x: content x y: row width: content width height: 1)
               char: $  style: TuiStyle normal.
        row := row + 1
    ]
! !


"============================================================"
"TuiWorkspacePanel - Code editor with evaluation"
"============================================================"
TuiWidget subclass: #TuiWorkspacePanel
    instanceVariableNames: 'editor onDoIt onPrintIt'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Panels'!

!TuiWorkspacePanel class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiWorkspacePanel methodsFor: 'initialization'!
initialize
    super initialize.
    editor := TuiTextArea new.
    editor lineNumbers: true.
    editor syntaxHighlight: true.
    hasBorder := false.

    onDoIt := [:code |
        "Default do-it handler"
        | result |
        result := Compiler evaluate: code.
        Transcript show: '=> ', result printString; cr
    ].

    onPrintIt := [:code |
        "Default print-it handler"
        | result |
        result := Compiler evaluate: code.
        editor text: editor text, ' "', result printString, '"'
    ]
! !

!TuiWorkspacePanel methodsFor: 'accessing'!
text
    ^editor text
!

setText: aString
    editor text: aString
!

editor
    ^editor
!

onDoIt: aBlock
    onDoIt := aBlock
!

onPrintIt: aBlock
    onPrintIt := aBlock
! !

!TuiWorkspacePanel methodsFor: 'evaluation'!
doIt
    "Execute selected text or current line"
    | code |
    code := self getCodeToEvaluate.
    code isEmpty ifTrue: [^self].
    onDoIt ifNotNil: [onDoIt value: code]
!

printIt
    "Execute and print result"
    | code |
    code := self getCodeToEvaluate.
    code isEmpty ifTrue: [^self].
    onPrintIt ifNotNil: [onPrintIt value: code]
!

getCodeToEvaluate
    "Get selected text or current line"
    editor hasSelection
        ifTrue: [^editor selectedText ifNil: ['']].
    ^editor currentLine
! !

!TuiWorkspacePanel methodsFor: 'geometry'!
rect: aRect
    super rect: aRect.
    editor rect: aRect.
    editor hasBorder: true
! !

!TuiWorkspacePanel methodsFor: 'focus'!
onFocus
    editor focus
!

onBlur
    editor blur
! !

!TuiWorkspacePanel methodsFor: 'key handling'!
handleKey: keyCode
    "Ctrl+D - Do It"
    keyCode = AnsiTerminal keyCtrlD ifTrue: [self doIt. ^true].

    "Ctrl+P - Print It"
    keyCode = AnsiTerminal keyCtrlP ifTrue: [self printIt. ^true].

    "Pass to editor"
    ^editor handleKey: keyCode
! !

!TuiWorkspacePanel methodsFor: 'drawing'!
drawSelf: screen
    editor draw: screen
! !


"============================================================"
"TuiReplPanel - Interactive REPL with history"
"============================================================"
TuiWidget subclass: #TuiReplPanel
    instanceVariableNames: 'outputLines inputLine history historyIndex scrollTop'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Panels'!

!TuiReplPanel class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiReplPanel methodsFor: 'initialization'!
initialize
    super initialize.
    outputLines := OrderedCollection new.
    history := OrderedCollection new.
    historyIndex := 0.
    scrollTop := 0.

    inputLine := TuiInputLine new.
    inputLine prompt: 'st> '.
    inputLine onEnter: [:text | self evaluate: text].

    "Welcome"
    self addOutput: 'Smalltalk REPL'.
    self addOutput: 'Type expressions and press Enter to evaluate.'.
    self addOutput: ''
! !

!TuiReplPanel methodsFor: 'accessing'!
inputLine
    ^inputLine
! !

!TuiReplPanel methodsFor: 'geometry'!
rect: aRect
    super rect: aRect.
    "Position input at bottom"
    inputLine rect: (TuiRect
        x: aRect x + 1
        y: aRect y + aRect height - 2
        width: aRect width - 2
        height: 1)
!

outputHeight
    ^self contentRect height - 1  "Leave room for input"
! !

!TuiReplPanel methodsFor: 'output'!
addOutput: aString
    outputLines add: aString.
    outputLines size > 1000 ifTrue: [outputLines removeFirst].
    self scrollToBottom
!

scrollToBottom
    scrollTop := (outputLines size - self outputHeight) max: 0
!

clear
    outputLines := OrderedCollection new.
    scrollTop := 0
! !

!TuiReplPanel methodsFor: 'evaluation'!
evaluate: code
    | result |
    code isEmpty ifTrue: [^self].

    "Add to history"
    history add: code.
    historyIndex := 0.

    "Show input"
    self addOutput: 'st> ', code.

    "Evaluate"
    [
        result := Compiler evaluate: code.
        self addOutput: '=> ', result printString
    ] on: Error do: [:ex |
        self addOutput: '!! Error: ', ex description
    ].

    "Clear input"
    inputLine clear.
    self addOutput: ''
! !

!TuiReplPanel methodsFor: 'history'!
historyUp
    history isEmpty ifTrue: [^self].
    historyIndex = 0 ifTrue: [historyIndex := history size + 1].
    historyIndex > 1 ifTrue: [
        historyIndex := historyIndex - 1.
        inputLine text: (history at: historyIndex)
    ]
!

historyDown
    history isEmpty ifTrue: [^self].
    historyIndex > 0 ifTrue: [
        historyIndex < history size
            ifTrue: [
                historyIndex := historyIndex + 1.
                inputLine text: (history at: historyIndex)
            ]
            ifFalse: [
                historyIndex := 0.
                inputLine clear
            ]
    ]
! !

!TuiReplPanel methodsFor: 'focus'!
onFocus
    inputLine focus
!

onBlur
    inputLine blur
! !

!TuiReplPanel methodsFor: 'key handling'!
handleKey: keyCode
    "History navigation"
    keyCode = AnsiTerminal keyUp ifTrue: [self historyUp. ^true].
    keyCode = AnsiTerminal keyDown ifTrue: [self historyDown. ^true].

    "Scrolling output"
    keyCode = AnsiTerminal keyPageUp ifTrue: [
        scrollTop := (scrollTop - 5) max: 0.
        ^true
    ].
    keyCode = AnsiTerminal keyPageDown ifTrue: [
        scrollTop := (scrollTop + 5) min: ((outputLines size - self outputHeight) max: 0).
        ^true
    ].

    "Pass to input line"
    ^inputLine handleKey: keyCode
! !

!TuiReplPanel methodsFor: 'drawing'!
drawSelf: screen
    | content row startLine endLine outputRect |
    self drawBorder: screen.

    content := self contentRect.
    outputRect := TuiRect
        x: content x
        y: content y
        width: content width
        height: content height - 1.

    "Draw output area"
    startLine := scrollTop + 1.
    endLine := (scrollTop + outputRect height) min: outputLines size.
    row := outputRect y.

    startLine to: endLine do: [:i |
        | line |
        line := outputLines at: i.
        line size > outputRect width ifTrue: [
            line := line copyFrom: 1 to: outputRect width
        ].
        screen drawText: line at: outputRect x y: row style: TuiStyle normal.
        row := row + 1
    ].

    "Clear remaining output rows"
    [row < (outputRect y + outputRect height)] whileTrue: [
        screen fillRect: (TuiRect x: outputRect x y: row width: outputRect width height: 1)
               char: $  style: TuiStyle normal.
        row := row + 1
    ].

    "Draw input line"
    inputLine rect: (TuiRect
        x: content x
        y: content y + content height - 1
        width: content width
        height: 1).
    inputLine focused: focused.
    inputLine draw: screen
! !
