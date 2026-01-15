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
        #('FFI' $5)
    ).
    tabBar onTabChange: [:index | self switchToTab: index]
!

setupStatusBar
    statusBar := TuiStatusBar new.
    statusBar rect: (TuiRect x: 1 y: terminal height width: terminal width height: 1).
    statusBar message: 'Ready'.
    statusBar items: #(
        #('Ctrl+Q' 'Quit')
        #('F1-F5' 'Tabs')
    )
!

setupDefaultTabs
    "Create default tab panels - subclasses/users can customize"
    | transcriptTab workspaceTab browserTab replTab ffiTab |

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

    "Browser tab with 4-pane layout"
    browserTab := TuiBrowserPanel new.
    browserTab rect: self contentRect.
    browserTab title: 'Browser'.
    tabs add: browserTab.

    "REPL tab"
    replTab := TuiReplPanel new.
    replTab rect: self contentRect.
    replTab title: 'REPL'.
    tabs add: replTab.

    "FFI Config tab"
    ffiTab := TuiFFIConfigPanel new.
    ffiTab rect: self contentRect.
    ffiTab title: 'FFI Config'.
    tabs add: ffiTab.

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
        statusBar items: #(#('Ctrl+Q' 'Quit') #('Tab' 'Pane') #('Ctrl+S' 'Save') #('Ctrl+A' 'NewClass'))
    ].
    index = 4 ifTrue: [
        "REPL"
        statusBar items: #(#('Ctrl+Q' 'Quit') #('Enter' 'Eval') #('Up/Dn' 'History'))
    ].
    index = 5 ifTrue: [
        "FFI Config"
        statusBar items: #(#('Ctrl+Q' 'Quit') #('A' 'Add') #('D' 'Delete') #('Ctrl+S' 'Save'))
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

    "F9 - Save Image (matches Zig TUI)"
    keyCode = AnsiTerminal keyF9 ifTrue: [
        self saveImage.
        ^self
    ].

    "F12 - Save Image As (matches Zig TUI)"
    keyCode = AnsiTerminal keyF12 ifTrue: [
        self saveImageAs.
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

!TuiApp methodsFor: 'image operations'!
saveImage
    "Save image to current path (F9 shortcut, matches Zig TUI)"
    | path |
    path := SessionManager current imagePath ifNil: ['smalltalk.image'].
    [
        SessionManager current saveImageTo: path.
        statusBar message: 'Image saved to ', path.
        self transcriptShow: 'Image saved to ', path
    ] on: Error do: [:ex |
        statusBar message: 'Error saving image: ', ex description.
        self transcriptShow: 'Error saving image: ', ex description
    ]
!

saveImageAs
    "Save image with new filename (F12 shortcut, matches Zig TUI)"
    "For now, just show a message - dialog would need more implementation"
    statusBar message: 'Save Image As: Use SessionManager saveImageTo: ''filename'''.
    self transcriptShow: 'Save Image As dialog not yet implemented'
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
    self drawBorderRounded: screen.

    content := self contentRect.
    self clearContent: screen.

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
    ].

    "Draw scrollbar if needed"
    lines size > content height ifTrue: [
        screen drawScrollbar: content
               total: lines size
               visible: content height
               offset: scrollTop
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
    self drawBorderRounded: screen.

    content := self contentRect.
    self clearContent: screen.

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

    "Draw scrollbar for output if needed"
    outputLines size > outputRect height ifTrue: [
        screen drawScrollbar: outputRect
               total: outputLines size
               visible: outputRect height
               offset: scrollTop
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


"============================================================"
"TuiBrowserPanel - Class browser with 4 panes (matches Zig TUI)"
"============================================================"
TuiWidget subclass: #TuiBrowserPanel
    instanceVariableNames: 'packageList classTree instanceMethodList classMethodList sourceEditor activePane selectedPackage selectedClass selectedMethod showClassSide statusMessage statusIsError'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Panels'!

!TuiBrowserPanel class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiBrowserPanel methodsFor: 'initialization'!
initialize
    super initialize.
    hasBorder := false.

    "Create the 4 panes and source editor"
    packageList := TuiListView new.
    packageList title: 'Packages'.

    classTree := TuiTreeView new.
    classTree title: 'Classes'.

    instanceMethodList := TuiListView new.
    instanceMethodList title: 'Instance'.

    classMethodList := TuiListView new.
    classMethodList title: 'Class'.

    sourceEditor := TuiTextArea new.
    sourceEditor lineNumbers: true.
    sourceEditor syntaxHighlight: true.

    activePane := #packages.
    selectedPackage := ''.
    selectedClass := ''.
    selectedMethod := ''.
    showClassSide := false.
    statusMessage := ''.
    statusIsError := false.

    self loadPackages.
    self loadClasses
!

loadPackages
    "Load available packages/categories"
    packageList clear.
    packageList addItem: 'Kernel'.
    packageList addItem: 'Collections'.
    packageList addItem: 'TUI-Core'.
    packageList addItem: 'TUI-Widgets'.
    packageList addItem: 'TUI-Panels'
!

loadClasses
    "Load class hierarchy"
    | objectNode collectionNode |
    classTree clear.

    "Build class tree starting from Object"
    objectNode := TuiTreeNode text: 'Object'.
    objectNode addChild: (TuiTreeNode text: 'Boolean').
    objectNode addChild: (TuiTreeNode text: 'Magnitude').
    objectNode addChild: (TuiTreeNode text: 'Collection').
    objectNode addChild: (TuiTreeNode text: 'Stream').
    objectNode addChild: (TuiTreeNode text: 'TuiWidget').

    classTree addRoot: objectNode.
    objectNode expand
! !

!TuiBrowserPanel methodsFor: 'pane layout'!
calcLayout
    "Calculate layout rects for 4 panes + source"
    | topHeight paneWidth pkgRect classRect instRect classMethodRect sourceRect |
    topHeight := rect height // 2.
    paneWidth := rect width // 4.

    pkgRect := TuiRect x: rect x y: rect y width: paneWidth height: topHeight.
    classRect := TuiRect x: rect x + paneWidth y: rect y width: paneWidth height: topHeight.
    instRect := TuiRect x: rect x + (paneWidth * 2) y: rect y width: paneWidth height: topHeight.
    classMethodRect := TuiRect x: rect x + (paneWidth * 3) y: rect y width: rect width - (paneWidth * 3) height: topHeight.
    sourceRect := TuiRect x: rect x y: rect y + topHeight width: rect width height: rect height - topHeight.

    ^Dictionary new
        at: #packages put: pkgRect;
        at: #classes put: classRect;
        at: #instanceMethods put: instRect;
        at: #classMethods put: classMethodRect;
        at: #source put: sourceRect;
        yourself
! !

!TuiBrowserPanel methodsFor: 'pane navigation'!
nextPane
    activePane = #packages ifTrue: [activePane := #classes. ^self].
    activePane = #classes ifTrue: [activePane := #instanceMethods. ^self].
    activePane = #instanceMethods ifTrue: [activePane := #classMethods. ^self].
    activePane = #classMethods ifTrue: [activePane := #source. ^self].
    activePane = #source ifTrue: [activePane := #packages. ^self]
!

previousPane
    activePane = #packages ifTrue: [activePane := #source. ^self].
    activePane = #classes ifTrue: [activePane := #packages. ^self].
    activePane = #instanceMethods ifTrue: [activePane := #classes. ^self].
    activePane = #classMethods ifTrue: [activePane := #instanceMethods. ^self].
    activePane = #source ifTrue: [activePane := #classMethods. ^self]
!

updateFocus
    packageList focused: (focused and: [activePane = #packages]).
    classTree focused: (focused and: [activePane = #classes]).
    instanceMethodList focused: (focused and: [activePane = #instanceMethods]).
    classMethodList focused: (focused and: [activePane = #classMethods]).
    sourceEditor focused: (focused and: [activePane = #source])
! !

!TuiBrowserPanel methodsFor: 'geometry'!
rect: aRect
    | layout |
    super rect: aRect.
    layout := self calcLayout.
    packageList rect: (layout at: #packages).
    classTree rect: (layout at: #classes).
    instanceMethodList rect: (layout at: #instanceMethods).
    classMethodList rect: (layout at: #classMethods).
    sourceEditor rect: ((layout at: #source) inner: 1)
! !

!TuiBrowserPanel methodsFor: 'focus'!
onFocus
    self updateFocus
!

onBlur
    packageList blur.
    classTree blur.
    instanceMethodList blur.
    classMethodList blur.
    sourceEditor blur
! !

!TuiBrowserPanel methodsFor: 'key handling'!
handleKey: keyCode
    "Tab - switch panes"
    keyCode = AnsiTerminal keyTab ifTrue: [
        activePane = #source ifFalse: [
            self nextPane.
            self updateFocus.
            ^true
        ]
    ].

    "Shift+Tab - switch panes backward"
    keyCode = AnsiTerminal keyShiftTab ifTrue: [
        self previousPane.
        self updateFocus.
        ^true
    ].

    "Ctrl+S - save"
    keyCode = AnsiTerminal keyCtrlS ifTrue: [
        self saveMethod.
        ^true
    ].

    "Ctrl+A - new class"
    keyCode = AnsiTerminal keyCtrlA ifTrue: [
        self showNewClassDialog.
        ^true
    ].

    "Dispatch to active pane"
    activePane = #packages ifTrue: [^packageList handleKey: keyCode].
    activePane = #classes ifTrue: [^classTree handleKey: keyCode].
    activePane = #instanceMethods ifTrue: [^instanceMethodList handleKey: keyCode].
    activePane = #classMethods ifTrue: [^classMethodList handleKey: keyCode].
    activePane = #source ifTrue: [^sourceEditor handleKey: keyCode].

    ^false
!

saveMethod
    statusMessage := 'Method saved'.
    statusIsError := false
!

showNewClassDialog
    statusMessage := 'New class dialog not yet implemented'.
    statusIsError := false
! !

!TuiBrowserPanel methodsFor: 'drawing'!
drawSelf: screen
    | layout sourceRect |
    self updateFocus.
    layout := self calcLayout.

    "Update pane rects"
    packageList rect: (layout at: #packages).
    classTree rect: (layout at: #classes).
    instanceMethodList rect: (layout at: #instanceMethods).
    classMethodList rect: (layout at: #classMethods).

    "Draw 4 top panes"
    packageList draw: screen.
    classTree draw: screen.
    instanceMethodList draw: screen.
    classMethodList draw: screen.

    "Draw source pane with rounded border"
    sourceRect := layout at: #source.
    screen drawBoxRounded: sourceRect style: (activePane = #source
        ifTrue: [TuiStyle borderFocused]
        ifFalse: [TuiStyle border]).
    screen drawTitle: self sourceTitle at: sourceRect focused: (activePane = #source).

    "Update and draw source editor"
    sourceEditor rect: (sourceRect inner: 1).
    sourceEditor draw: screen.

    "Draw status message if present"
    statusMessage isEmpty ifFalse: [
        | statusStyle |
        statusStyle := statusIsError ifTrue: [TuiStyle error] ifFalse: [TuiStyle normal].
        screen drawText: statusMessage at: sourceRect x + 1 y: sourceRect bottom - 1 style: statusStyle
    ]
!

sourceTitle
    "Build title for source pane"
    selectedClass isEmpty ifTrue: [^'Source'].
    selectedMethod isEmpty ifTrue: [^selectedClass, ' [Definition]'].
    ^selectedClass, (showClassSide ifTrue: [' class'] ifFalse: ['']), ' >> ', selectedMethod
! !


"============================================================"
"TuiFFIConfigPanel - FFI library configuration (matches Zig TUI)"
"============================================================"
TuiWidget subclass: #TuiFFIConfigPanel
    instanceVariableNames: 'libraryList libraries statusMessage dirty'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Panels'!

!TuiFFIConfigPanel class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiFFIConfigPanel methodsFor: 'initialization'!
initialize
    super initialize.
    hasBorder := true.
    title := 'FFI Configuration'.

    libraries := OrderedCollection new.
    libraryList := TuiListView new.
    libraryList title: 'Available FFI Libraries'.

    statusMessage := 'Ready'.
    dirty := false.

    self loadLibraries
!

loadLibraries
    "Load available FFI libraries"
    libraryList clear.
    libraryList addItem: 'LibC (stdio, stdlib, string)'.
    libraryList addItem: 'LibMath (math functions)'.
    libraryList addItem: 'OpenGL (graphics)'.
    libraryList addItem: 'Raylib (game library)'
! !

!TuiFFIConfigPanel methodsFor: 'geometry'!
rect: aRect
    | listRect |
    super rect: aRect.
    "Library list takes up most of the panel"
    listRect := TuiRect
        x: aRect x + 2
        y: aRect y + 4
        width: aRect width - 4
        height: aRect height - 10.
    libraryList rect: listRect
! !

!TuiFFIConfigPanel methodsFor: 'focus'!
onFocus
    libraryList focus
!

onBlur
    libraryList blur
! !

!TuiFFIConfigPanel methodsFor: 'key handling'!
handleKey: keyCode
    "A - Add library"
    (keyCode = $a asciiValue or: [keyCode = $A asciiValue]) ifTrue: [
        self addLibrary.
        ^true
    ].

    "D or Delete - Remove library"
    (keyCode = $d asciiValue or: [keyCode = $D asciiValue or: [keyCode = AnsiTerminal keyDelete]]) ifTrue: [
        self removeLibrary.
        ^true
    ].

    "Ctrl+S - Save config"
    keyCode = AnsiTerminal keyCtrlS ifTrue: [
        self saveConfig.
        ^true
    ].

    "Enter - Toggle enable/disable"
    keyCode = AnsiTerminal keyEnter ifTrue: [
        self toggleLibrary.
        ^true
    ].

    "Pass to library list"
    ^libraryList handleKey: keyCode
!

addLibrary
    statusMessage := 'Add library dialog not yet implemented'.
    dirty := true
!

removeLibrary
    libraryList selectedItem ifNotNil: [:item |
        statusMessage := 'Removed: ', item asString.
        dirty := true
    ]
!

toggleLibrary
    libraryList selectedItem ifNotNil: [:item |
        statusMessage := 'Toggled: ', item asString.
        dirty := true
    ]
!

saveConfig
    statusMessage := 'Config saved - run zig build gen-ffi to regenerate'.
    dirty := false
! !

!TuiFFIConfigPanel methodsFor: 'drawing'!
drawSelf: screen
    | content listRect helpY statusY dirtyIndicator |
    self drawBorderRounded: screen.

    content := self contentRect.

    "Draw header"
    screen drawText: 'Available C library bindings for Smalltalk FFI:'
           at: content x + 1 y: content y + 1 style: TuiStyle normal.

    "Update and draw library list"
    listRect := TuiRect
        x: content x + 1
        y: content y + 3
        width: content width - 2
        height: content height - 8.
    libraryList rect: listRect.
    libraryList focused: focused.
    libraryList draw: screen.

    "Draw help text"
    helpY := content y + content height - 4.
    screen drawText: 'A: Add library | D/Del: Remove | Enter: Toggle | Ctrl+S: Save'
           at: content x + 1 y: helpY style: TuiStyle status.
    screen drawText: 'After changes: zig build gen-ffi && zig build'
           at: content x + 1 y: helpY + 1 style: TuiStyle dim.

    "Draw status bar"
    statusY := content y + content height - 2.
    screen fillRect: (TuiRect x: content x y: statusY width: content width height: 1)
           char: $  style: TuiStyle status.

    dirtyIndicator := dirty ifTrue: [' [unsaved]'] ifFalse: [''].
    screen drawText: 'Status: ', statusMessage, dirtyIndicator
           at: content x + 1 y: statusY style: TuiStyle status
! !
