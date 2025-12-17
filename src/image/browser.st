"Browser class for Smalltalk TUI"

Object subclass: #Browser
    instanceVariableNames: 'term classList methodList codeView selectedClass selectedMethod running'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Apps'!

Smalltalk at: #BrowserInstance put: nil!

!Browser class methodsFor: 'instance creation'!
current
    | inst |
    inst := Smalltalk at: #BrowserInstance ifAbsent: [nil].
    inst isNil ifTrue: [
        inst := self new initialize.
        Smalltalk at: #BrowserInstance put: inst
    ].
    ^inst
!

open
    ^self current run
! !

!Browser methodsFor: 'initialization'!
initialize
    term := Terminal current.
    running := false.
    selectedClass := nil.
    selectedMethod := nil.
    self initializeWidgets
!

initializeWidgets
    | size w h |
    size := term size.
    w := size at: 1.
    h := size at: 2.

    "Class list on left (1/3 width)"
    classList := ListView new.
    classList bounds: (Rectangle origin: 1@2 extent: (w // 3)@(h // 2 - 1)).
    classList onSelect: [:item | self onClassSelected: item].
    classList items: self allClassNames.

    "Method list on right (2/3 width)"
    methodList := ListView new.
    methodList bounds: (Rectangle origin: (w // 3 + 1)@2 extent: (w - w // 3)@(h // 2 - 1)).
    methodList onSelect: [:item | self onMethodSelected: item].
    methodList items: #().

    "Code view at bottom"
    codeView := TextView new.
    codeView bounds: (Rectangle origin: 1@(h // 2 + 1) extent: w@(h - h // 2 - 1)).
    codeView text: ''
! !

!Browser methodsFor: 'class list'!
allClassNames
    "Return sorted list of all class names"
    | names |
    names := OrderedCollection new.
    Smalltalk allClassesDo: [:cls |
        names add: cls name
    ].
    ^names asSortedCollection asArray
! !

!Browser methodsFor: 'method list'!
methodsForClass: aClass
    "Return selectors for a class"
    aClass isNil ifTrue: [^#()].
    ^aClass selectors asSortedCollection asArray
! !

!Browser methodsFor: 'selection'!
onClassSelected: className
    selectedClass := Smalltalk at: className asSymbol ifAbsent: [nil].
    selectedClass ifNotNil: [
        methodList items: (self methodsForClass: selectedClass).
        codeView text: selectedClass definition
    ] ifNil: [
        methodList items: #().
        codeView text: ''
    ].
    self redraw
!

onMethodSelected: selector
    selectedClass ifNil: [^self].
    selectedMethod := selector.
    codeView text: (selectedClass sourceCodeAt: selector ifAbsent: ['Method not found']).
    self redraw
! !

!Browser methodsFor: 'drawing'!
draw
    term clear.
    self drawTitle.
    self drawWidgets.
    self drawStatusBar.
    term flush
!

drawTitle
    term fg: Colors title.
    term bg: Colors background.
    term bold: true.
    term printCentered: 'Smalltalk Browser' row: 1.
    term bold: false.
    term resetStyle
!

drawWidgets
    classList draw.
    methodList draw.
    codeView draw
!

drawStatusBar
    | size statusText |
    size := term size.
    term fg: Colors background.
    term bg: Colors border.
    term setCursorRow: (size at: 2) col: 1.
    term fillRectAt: {1. (size at: 2)} extent: {(size at: 1). 1} char: 32.

    statusText := 'Tab: Switch pane | Enter: Select | Q: Quit'.
    term setCursorRow: (size at: 2) col: 2.
    term write: statusText.
    term resetStyle
!

redraw
    self draw
! !

!Browser methodsFor: 'focus'!
focusClassList
    classList focus.
    methodList blur.
    codeView blur
!

focusMethodList
    classList blur.
    methodList focus.
    codeView blur
!

focusCodeView
    classList blur.
    methodList blur.
    codeView focus
!

cyclePane
    classList focused ifTrue: [
        self focusMethodList.
        ^self
    ].
    methodList focused ifTrue: [
        self focusCodeView.
        ^self
    ].
    self focusClassList
! !

!Browser methodsFor: 'running'!
run
    running := true.
    term initialize.
    self focusClassList.
    self draw.

    [running] whileTrue: [
        self processInput.
        Processor yield
    ].

    term shutdown
!

stop
    running := false
!

processInput
    "Poll and handle key - inline to avoid VM block temp bug"
    (term pollKey) ifNotNil: [:key | self handleKey: key]
!

handleKey: keyCode
    "Handle global keys first"
    (keyCode = $q asciiValue or: [keyCode = $Q asciiValue]) ifTrue: [
        self stop.
        ^true
    ].

    keyCode = Terminal keyTab ifTrue: [
        self cyclePane.
        self redraw.
        ^true
    ].

    keyCode = Terminal keyEscape ifTrue: [
        self stop.
        ^true
    ].

    "Let focused widget handle"
    classList focused ifTrue: [
        (classList handleKey: keyCode) ifTrue: [
            self redraw.
            ^true
        ]
    ].

    methodList focused ifTrue: [
        (methodList handleKey: keyCode) ifTrue: [
            self redraw.
            ^true
        ]
    ].

    codeView focused ifTrue: [
        (codeView handleKey: keyCode) ifTrue: [
            self redraw.
            ^true
        ]
    ].

    ^false
! !


"Transcript class - output window for system messages"
Object subclass: #TranscriptWindow
    instanceVariableNames: 'term textView buffer maxLines running'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Apps'!

Smalltalk at: #TranscriptWindowInstance put: nil!

!TranscriptWindow class methodsFor: 'instance creation'!
current
    | inst |
    inst := Smalltalk at: #TranscriptWindowInstance ifAbsent: [nil].
    inst isNil ifTrue: [
        inst := self new initialize.
        Smalltalk at: #TranscriptWindowInstance put: inst
    ].
    ^inst
! !

!TranscriptWindow methodsFor: 'initialization'!
initialize
    term := Terminal current.
    buffer := OrderedCollection new.
    maxLines := 1000.
    running := false.
    self initializeWidgets
!

initializeWidgets
    | size |
    size := term size.
    textView := TextView new.
    textView bounds: (Rectangle origin: 1@2 extent: (size at: 1)@((size at: 2) - 2)).
    textView lines: buffer asArray
! !

!TranscriptWindow methodsFor: 'output'!
show: aString
    "Add text to transcript"
    aString lines do: [:line |
        buffer add: line.
        buffer size > maxLines ifTrue: [buffer removeFirst]
    ].
    textView lines: buffer asArray
!

cr
    self show: ''
!

clear
    buffer := OrderedCollection new.
    textView lines: #()
! !

!TranscriptWindow methodsFor: 'drawing'!
draw
    term clear.
    self drawTitle.
    textView draw.
    self drawStatusBar.
    term flush
!

drawTitle
    term fg: Colors title.
    term bg: Colors background.
    term bold: true.
    term printCentered: 'Transcript' row: 1.
    term bold: false.
    term resetStyle
!

drawStatusBar
    | size |
    size := term size.
    term fg: Colors background.
    term bg: Colors border.
    term setCursorRow: (size at: 2) col: 1.
    term fillRectAt: {1. (size at: 2)} extent: {(size at: 1). 1} char: 32.
    term setCursorRow: (size at: 2) col: 2.
    term write: 'Arrows: Scroll | C: Clear | Q: Close'.
    term resetStyle
! !

!TranscriptWindow methodsFor: 'running'!
run
    running := true.
    term initialize.
    textView focus.
    self draw.

    [running] whileTrue: [
        self processInput.
        Processor yield
    ].

    term shutdown
!

stop
    running := false
!

processInput
    "Poll and handle key - inline to avoid VM block temp bug"
    (term pollKey) ifNotNil: [:key | self handleKey: key]
!

handleKey: keyCode
    (keyCode = $q asciiValue or: [keyCode = $Q asciiValue]) ifTrue: [
        self stop. ^true
    ].
    (keyCode = $c asciiValue or: [keyCode = $C asciiValue]) ifTrue: [
        self clear. self draw. ^true
    ].
    (textView handleKey: keyCode) ifTrue: [
        self draw. ^true
    ].
    ^false
! !


"Workspace class - interactive Smalltalk evaluation"
Object subclass: #Workspace
    instanceVariableNames: 'term textInput resultView code running'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Apps'!

Smalltalk at: #WorkspaceInstance put: nil!

!Workspace class methodsFor: 'instance creation'!
current
    | inst |
    inst := Smalltalk at: #WorkspaceInstance ifAbsent: [nil].
    inst isNil ifTrue: [
        inst := self new initialize.
        Smalltalk at: #WorkspaceInstance put: inst
    ].
    ^inst
!

open
    ^self current run
! !

!Workspace methodsFor: 'initialization'!
initialize
    term := Terminal current.
    code := ''.
    running := false.
    self initializeWidgets
!

initializeWidgets
    | size |
    size := term size.

    "Text input for code (top half)"
    textInput := TextInput new.
    textInput bounds: (Rectangle origin: 2@3 extent: ((size at: 1) - 2)@1).
    textInput onChange: [:newCode | code := newCode].

    "Result display (bottom half)"
    resultView := TextView new.
    resultView bounds: (Rectangle origin: 1@((size at: 2) // 2 + 1) extent: (size at: 1)@((size at: 2) // 2 - 1)).
    resultView text: 'Enter Smalltalk expression and press Enter to evaluate'
! !

!Workspace methodsFor: 'evaluation'!
evaluate
    | result |
    code isEmpty ifTrue: [^self].

    [
        result := Compiler evaluate: code.
        resultView text: '=> ', result printString
    ] on: Error do: [:ex |
        resultView text: 'Error: ', ex messageText
    ].
    self draw
! !

!Workspace methodsFor: 'drawing'!
draw
    term clear.
    self drawTitle.
    self drawCodeArea.
    textInput draw.
    resultView draw.
    self drawStatusBar.
    term flush
!

drawTitle
    term fg: Colors title.
    term bg: Colors background.
    term bold: true.
    term printCentered: 'Workspace' row: 1.
    term bold: false.
    term resetStyle
!

drawCodeArea
    | size |
    size := term size.
    term fg: Colors border.
    term bg: Colors background.
    term drawBoxAt: {1. 2} extent: {(size at: 1). (size at: 2) // 2 - 1}.
    term setCursorRow: 2 col: 3.
    term fg: Colors comment.
    term write: ' Code '.
    term resetStyle
!

drawStatusBar
    | size |
    size := term size.
    term fg: Colors background.
    term bg: Colors border.
    term setCursorRow: (size at: 2) col: 1.
    term fillRectAt: {1. (size at: 2)} extent: {(size at: 1). 1} char: 32.
    term setCursorRow: (size at: 2) col: 2.
    term write: 'Enter: Evaluate | Esc: Cancel | Q: Quit'.
    term resetStyle
! !

!Workspace methodsFor: 'running'!
run
    running := true.
    term initialize.
    textInput focus.
    self draw.

    [running] whileTrue: [
        self processInput.
        Processor yield
    ].

    term shutdown
!

stop
    running := false
!

processInput
    "Poll and handle key - inline to avoid VM block temp bug"
    (term pollKey) ifNotNil: [:key | self handleKey: key]
!

handleKey: keyCode
    (keyCode = $q asciiValue or: [keyCode = $Q asciiValue]) ifTrue: [
        self stop. ^true
    ].

    keyCode = Terminal keyEnter ifTrue: [
        self evaluate. ^true
    ].

    keyCode = Terminal keyEscape ifTrue: [
        textInput text: ''.
        code := ''.
        self draw.
        ^true
    ].

    (textInput handleKey: keyCode) ifTrue: [
        self draw. ^true
    ].

    ^false
! !


"TUI Application - main launcher"
Object subclass: #TUIApp
    instanceVariableNames: 'term menu running loopCount'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Apps'!

!TUIApp class methodsFor: 'launching'!
start
    ^self new initialize run
! !

!TUIApp methodsFor: 'initialization'!
initialize
    term := Terminal current.
    running := false.
    self initializeMenu
!

initializeMenu
    menu := ListView new.
    menu bounds: (Rectangle origin: 10@5 extent: 30@10).
    menu items: #('Browser' 'Workspace' 'Transcript' 'Quit').
    menu onSelect: [:item | self onMenuSelect: item]
! !

!TUIApp methodsFor: 'menu'!
onMenuSelect: item
    item = 'Browser' ifTrue: [Browser open. ^self].
    item = 'Workspace' ifTrue: [Workspace open. ^self].
    item = 'Transcript' ifTrue: [TranscriptWindow current run. ^self].
    item = 'Quit' ifTrue: [self stop. ^self]
! !

!TUIApp methodsFor: 'drawing'!
draw
    term clear.
    self drawTitle.
    self drawMenu.
    self drawStatusBar.
    term flush
!

drawTitle
    | size |
    size := term size.
    term fg: Colors title.
    term bg: Colors background.
    term bold: true.
    term printCentered: 'Smalltalk TUI' row: 2.
    term printCentered: 'Version 0.1' row: 3.
    term bold: false.
    term resetStyle
!

drawMenu
    menu draw
!

drawStatusBar
    | size |
    size := term size.
    term fg: Colors background.
    term bg: Colors border.
    term setCursorRow: (size at: 2) col: 1.
    term fillRectAt: {1. (size at: 2)} extent: {(size at: 1). 1} char: 32.
    term setCursorRow: (size at: 2) col: 2.
    term write: 'Up/Down: Navigate | Enter: Select | Q: Quit'.
    term resetStyle
! !

!TUIApp methodsFor: 'running'!
run
    running := true.
    loopCount := 0.
    term initialize.
    menu focus.
    self draw.

    "Event loop - all work done in methods to avoid VM block temp bug"
    [self shouldContinue] whileTrue: [self doOneIteration].

    term shutdown
!

shouldContinue
    "Check if loop should continue"
    running ifFalse: [^false].
    loopCount > 1000000 ifTrue: [^false].
    ^true
!

doOneIteration
    "Process one iteration of the event loop"
    self processInput.
    loopCount := loopCount + 1
!

stop
    running := false
!

processInput
    "Poll and handle key"
    | key |
    key := term pollKey.
    key isNil ifTrue: [^self].
    self handleKey: key
!

handleKey: keyCode
    "Check for quit"
    keyCode = 113 ifTrue: [self stop. ^true].  "q"
    keyCode = 81 ifTrue: [self stop. ^true].   "Q"
    (menu handleKey: keyCode) ifTrue: [self draw. ^true].
    ^false
! !
