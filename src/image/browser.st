"Full TUI Application for Smalltalk - Replicating Zig TUI"

"============================================================"
"TextArea - Multi-line text editor widget"
"============================================================"
Object subclass: #TextArea
    instanceVariableNames: 'bounds lines cursorLine cursorCol scrollTop scrollLeft focused lineNumbers syntaxHighlight readonly selection selectionStart selectionEnd'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TextArea class methodsFor: 'instance creation'!
new
    ^super new initialize
!

bounds: aRect
    ^self new bounds: aRect
! !

!TextArea methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 0@0 extent: 80@20.
    lines := OrderedCollection new.
    lines add: ''.
    cursorLine := 1.
    cursorCol := 1.
    scrollTop := 0.
    scrollLeft := 0.
    focused := false.
    lineNumbers := false.
    syntaxHighlight := false.
    readonly := false.
    selection := false.
    selectionStart := nil.
    selectionEnd := nil
! !

!TextArea methodsFor: 'accessing'!
bounds: aRect
    bounds := aRect
!

bounds
    ^bounds
!

focused: aBoolean
    focused := aBoolean
!

focused
    ^focused
!

lineNumbers: aBoolean
    lineNumbers := aBoolean
!

syntaxHighlight: aBoolean
    syntaxHighlight := aBoolean
!

readonly: aBoolean
    readonly := aBoolean
!

readonly
    ^readonly
!

text
    "Return all text as a single string"
    | result |
    result := ''.
    lines do: [:line |
        result isEmpty ifFalse: [result := result, String lf].
        result := result, line
    ].
    ^result
!

text: aString
    "Set all text from a string"
    lines := OrderedCollection new.
    aString lines do: [:line | lines add: line].
    lines isEmpty ifTrue: [lines add: ''].
    cursorLine := 1.
    cursorCol := 1.
    scrollTop := 0
!

currentLine
    ^lines at: cursorLine
!

linesCount
    ^lines size
! !

!TextArea methodsFor: 'editing'!
insertChar: aChar
    | line before after |
    readonly ifTrue: [^self].
    line := lines at: cursorLine.
    before := line copyFrom: 1 to: cursorCol - 1.
    after := line copyFrom: cursorCol to: line size.
    lines at: cursorLine put: before, (String with: aChar), after.
    cursorCol := cursorCol + 1.
    self ensureCursorVisible
!

insertNewline
    | line before after |
    readonly ifTrue: [^self].
    line := lines at: cursorLine.
    before := line copyFrom: 1 to: cursorCol - 1.
    after := line copyFrom: cursorCol to: line size.
    lines at: cursorLine put: before.
    lines add: after afterIndex: cursorLine.
    cursorLine := cursorLine + 1.
    cursorCol := 1.
    self ensureCursorVisible
!

deleteChar
    | line before after |
    readonly ifTrue: [^self].
    cursorCol > 1 ifTrue: [
        line := lines at: cursorLine.
        before := line copyFrom: 1 to: cursorCol - 2.
        after := line copyFrom: cursorCol to: line size.
        lines at: cursorLine put: before, after.
        cursorCol := cursorCol - 1
    ] ifFalse: [
        cursorLine > 1 ifTrue: [
            | prevLine |
            prevLine := lines at: cursorLine - 1.
            cursorCol := prevLine size + 1.
            lines at: cursorLine - 1 put: prevLine, (lines at: cursorLine).
            lines removeAt: cursorLine.
            cursorLine := cursorLine - 1
        ]
    ].
    self ensureCursorVisible
!

deleteForward
    | line before after |
    readonly ifTrue: [^self].
    line := lines at: cursorLine.
    cursorCol <= line size ifTrue: [
        before := line copyFrom: 1 to: cursorCol - 1.
        after := line copyFrom: cursorCol + 1 to: line size.
        lines at: cursorLine put: before, after
    ] ifFalse: [
        cursorLine < lines size ifTrue: [
            lines at: cursorLine put: line, (lines at: cursorLine + 1).
            lines removeAt: cursorLine + 1
        ]
    ]
! !

!TextArea methodsFor: 'cursor movement'!
moveLeft
    cursorCol > 1
        ifTrue: [cursorCol := cursorCol - 1]
        ifFalse: [
            cursorLine > 1 ifTrue: [
                cursorLine := cursorLine - 1.
                cursorCol := (lines at: cursorLine) size + 1
            ]
        ].
    self ensureCursorVisible
!

moveRight
    | line |
    line := lines at: cursorLine.
    cursorCol <= line size
        ifTrue: [cursorCol := cursorCol + 1]
        ifFalse: [
            cursorLine < lines size ifTrue: [
                cursorLine := cursorLine + 1.
                cursorCol := 1
            ]
        ].
    self ensureCursorVisible
!

moveUp
    cursorLine > 1 ifTrue: [
        cursorLine := cursorLine - 1.
        cursorCol := cursorCol min: ((lines at: cursorLine) size + 1)
    ].
    self ensureCursorVisible
!

moveDown
    cursorLine < lines size ifTrue: [
        cursorLine := cursorLine + 1.
        cursorCol := cursorCol min: ((lines at: cursorLine) size + 1)
    ].
    self ensureCursorVisible
!

moveHome
    cursorCol := 1.
    self ensureCursorVisible
!

moveEnd
    cursorCol := (lines at: cursorLine) size + 1.
    self ensureCursorVisible
!

moveToStart
    cursorLine := 1.
    cursorCol := 1.
    scrollTop := 0.
    self ensureCursorVisible
!

moveToEnd
    cursorLine := lines size.
    cursorCol := (lines at: cursorLine) size + 1.
    self ensureCursorVisible
!

ensureCursorVisible
    | visibleHeight gutterWidth visibleWidth |
    visibleHeight := bounds height - 2.
    gutterWidth := lineNumbers ifTrue: [5] ifFalse: [0].
    visibleWidth := bounds width - 2 - gutterWidth.

    "Vertical scrolling"
    cursorLine <= scrollTop ifTrue: [scrollTop := cursorLine - 1].
    cursorLine > (scrollTop + visibleHeight) ifTrue: [
        scrollTop := cursorLine - visibleHeight
    ].
    scrollTop := scrollTop max: 0.

    "Horizontal scrolling"
    cursorCol <= scrollLeft ifTrue: [scrollLeft := cursorCol - 1].
    cursorCol > (scrollLeft + visibleWidth) ifTrue: [
        scrollLeft := cursorCol - visibleWidth
    ].
    scrollLeft := scrollLeft max: 0
! !

!TextArea methodsFor: 'drawing'!
draw
    | term visibleHeight gutterWidth contentWidth startLine endLine y |
    term := Terminal current.
    visibleHeight := bounds height - 2.
    gutterWidth := lineNumbers ifTrue: [5] ifFalse: [0].
    contentWidth := bounds width - 2 - gutterWidth.

    "Draw border"
    self drawBorder.

    "Draw content"
    startLine := scrollTop + 1.
    endLine := (scrollTop + visibleHeight) min: lines size.
    y := bounds origin y + 1.

    startLine to: endLine do: [:lineNum |
        | line displayLine |
        term setCursorRow: y col: bounds origin x + 1.

        "Draw line number gutter"
        lineNumbers ifTrue: [
            term fg: Colors comment.
            term write: (self formatLineNumber: lineNum width: gutterWidth - 1).
            term write: ' '.
            term fg: Colors foreground
        ].

        "Draw line content"
        line := lines at: lineNum.
        displayLine := self visiblePortion: line from: scrollLeft + 1 width: contentWidth.

        syntaxHighlight
            ifTrue: [self drawHighlighted: displayLine on: term]
            ifFalse: [term write: displayLine].

        "Clear rest of line"
        term write: (String new: (contentWidth - displayLine size) withAll: $ ).
        y := y + 1
    ].

    "Clear remaining lines"
    [y <= (bounds origin y + bounds height - 2)] whileTrue: [
        term setCursorRow: y col: bounds origin x + 1.
        term write: (String new: bounds width - 2 withAll: $ ).
        y := y + 1
    ].

    "Position cursor if focused"
    focused ifTrue: [
        | cursorX cursorY |
        cursorY := bounds origin y + cursorLine - scrollTop.
        cursorX := bounds origin x + gutterWidth + cursorCol - scrollLeft.
        term setCursorRow: cursorY col: cursorX.
        term showCursor
    ]
!

drawBorder
    | term |
    term := Terminal current.
    term fg: Colors border.

    "Top border"
    term setCursorRow: bounds origin y col: bounds origin x.
    term write: '+'.
    2 to: bounds width - 1 do: [:i | term write: '-'].
    term write: '+'.

    "Side borders"
    1 to: bounds height - 2 do: [:i |
        term setCursorRow: bounds origin y + i col: bounds origin x.
        term write: '|'.
        term setCursorRow: bounds origin y + i col: bounds origin x + bounds width - 1.
        term write: '|'
    ].

    "Bottom border"
    term setCursorRow: bounds origin y + bounds height - 1 col: bounds origin x.
    term write: '+'.
    2 to: bounds width - 1 do: [:i | term write: '-'].
    term write: '+'.

    term fg: Colors foreground
!

formatLineNumber: num width: w
    | str |
    str := num printString.
    [str size < w] whileTrue: [str := ' ', str].
    ^str
!

visiblePortion: aString from: start width: w
    | result actualStart |
    actualStart := start min: aString size + 1.
    actualStart > aString size ifTrue: [^''].
    result := aString copyFrom: actualStart to: (aString size min: actualStart + w - 1).
    ^result
!

drawHighlighted: aString on: term
    "Simple syntax highlighting"
    | i ch inString inComment quote |
    i := 1.
    inString := false.
    inComment := false.
    quote := Character value: 39.  "Single quote character"

    [i <= aString size] whileTrue: [
        ch := aString at: i.

        inComment ifTrue: [
            term fg: Colors comment.
            term write: (String with: ch).
            ch = $" ifTrue: [inComment := false]
        ] ifFalse: [
            inString ifTrue: [
                term fg: Colors string.
                term write: (String with: ch).
                ch = quote ifTrue: [inString := false]
            ] ifFalse: [
                ch = $" ifTrue: [
                    term fg: Colors comment.
                    term write: (String with: ch).
                    inComment := true
                ] ifFalse: [
                    ch = quote ifTrue: [
                        term fg: Colors string.
                        term write: (String with: ch).
                        inString := true
                    ] ifFalse: [
                        (ch isDigit)
                            ifTrue: [term fg: Colors number]
                            ifFalse: [
                                (ch = $: or: [ch = $^ or: [ch = $[ or: [ch = $]]]])
                                    ifTrue: [term fg: Colors keyword]
                                    ifFalse: [term fg: Colors foreground]
                            ].
                        term write: (String with: ch)
                    ]
                ]
            ]
        ].
        i := i + 1
    ].
    term fg: Colors foreground
! !

!TextArea methodsFor: 'key handling'!
handleKey: keyCode
    "Handle key input, return true if handled"

    "Enter"
    keyCode = 13 ifTrue: [self insertNewline. ^true].

    "Backspace"
    (keyCode = 8 or: [keyCode = 127]) ifTrue: [self deleteChar. ^true].

    "Arrow keys (ANSI escape sequences are decoded by Terminal)"
    keyCode = Terminal keyUp ifTrue: [self moveUp. ^true].
    keyCode = Terminal keyDown ifTrue: [self moveDown. ^true].
    keyCode = Terminal keyLeft ifTrue: [self moveLeft. ^true].
    keyCode = Terminal keyRight ifTrue: [self moveRight. ^true].
    keyCode = Terminal keyHome ifTrue: [self moveHome. ^true].
    keyCode = Terminal keyEnd ifTrue: [self moveEnd. ^true].
    keyCode = Terminal keyPageUp ifTrue: [self pageUp. ^true].
    keyCode = Terminal keyPageDown ifTrue: [self pageDown. ^true].

    "Printable characters"
    (keyCode >= 32 and: [keyCode < 127]) ifTrue: [
        self insertChar: (Character value: keyCode).
        ^true
    ].

    ^false
!

pageUp
    | visibleHeight |
    visibleHeight := bounds height - 2.
    cursorLine := (cursorLine - visibleHeight) max: 1.
    self ensureCursorVisible
!

pageDown
    | visibleHeight |
    visibleHeight := bounds height - 2.
    cursorLine := (cursorLine + visibleHeight) min: lines size.
    self ensureCursorVisible
! !


"============================================================"
"ListView - Scrollable list widget"
"============================================================"
Object subclass: #ListView
    instanceVariableNames: 'bounds items selectedIndex scrollTop focused title'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!ListView class methodsFor: 'instance creation'!
new
    ^super new initialize
!

bounds: aRect
    ^self new bounds: aRect
! !

!ListView methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 0@0 extent: 20@10.
    items := OrderedCollection new.
    selectedIndex := 0.
    scrollTop := 0.
    focused := false.
    title := ''
! !

!ListView methodsFor: 'accessing'!
bounds: aRect
    bounds := aRect
!

bounds
    ^bounds
!

title: aString
    title := aString
!

title
    ^title
!

focused: aBoolean
    focused := aBoolean
!

focused
    ^focused
!

items
    ^items
!

items: aCollection
    items := aCollection asOrderedCollection.
    selectedIndex := items isEmpty ifTrue: [0] ifFalse: [1].
    scrollTop := 0
!

addItem: aString
    items add: aString.
    selectedIndex = 0 ifTrue: [selectedIndex := 1]
!

clear
    items := OrderedCollection new.
    selectedIndex := 0.
    scrollTop := 0
!

selectedIndex
    ^selectedIndex
!

selectedItem
    selectedIndex > 0 ifTrue: [^items at: selectedIndex].
    ^nil
! !

!ListView methodsFor: 'selection'!
selectIndex: index
    | newIndex |
    newIndex := index max: 0.
    newIndex := newIndex min: items size.
    selectedIndex := newIndex.
    self ensureSelectionVisible
!

selectNext
    self selectIndex: selectedIndex + 1
!

selectPrevious
    self selectIndex: selectedIndex - 1
!

selectFirst
    self selectIndex: 1
!

selectLast
    self selectIndex: items size
!

ensureSelectionVisible
    | visibleHeight |
    visibleHeight := bounds height - 2.
    title isEmpty ifFalse: [visibleHeight := visibleHeight - 1].

    selectedIndex <= scrollTop ifTrue: [
        scrollTop := (selectedIndex - 1) max: 0
    ].
    selectedIndex > (scrollTop + visibleHeight) ifTrue: [
        scrollTop := selectedIndex - visibleHeight
    ]
! !

!ListView methodsFor: 'drawing'!
draw
    | term visibleHeight y startIndex endIndex titleOffset |
    term := Terminal current.
    visibleHeight := bounds height - 2.
    titleOffset := 0.

    "Draw border"
    self drawBorder.

    "Draw title if present"
    title isEmpty ifFalse: [
        term setCursorRow: bounds origin y + 1 col: bounds origin x + 1.
        focused
            ifTrue: [term fg: Colors title. term bold: true]
            ifFalse: [term fg: Colors comment].
        term write: (self truncate: title to: bounds width - 2).
        term bold: false.
        term fg: Colors foreground.
        titleOffset := 1.
        visibleHeight := visibleHeight - 1
    ].

    "Draw items"
    startIndex := scrollTop + 1.
    endIndex := (scrollTop + visibleHeight) min: items size.
    y := bounds origin y + 1 + titleOffset.

    startIndex to: endIndex do: [:i |
        | item |
        term setCursorRow: y col: bounds origin x + 1.
        item := items at: i.

        i = selectedIndex
            ifTrue: [
                focused
                    ifTrue: [term bg: Colors selection]
                    ifFalse: [term bg: Colors highlight].
                term fg: Colors foreground
            ]
            ifFalse: [
                term bg: Colors background.
                term fg: Colors foreground
            ].

        term write: ' '.
        term write: (self truncate: item to: bounds width - 4).
        term write: (String new: (bounds width - 3 - (item size min: bounds width - 4)) withAll: $ ).
        term bg: Colors background.
        y := y + 1
    ].

    "Clear remaining lines"
    [y <= (bounds origin y + bounds height - 2)] whileTrue: [
        term setCursorRow: y col: bounds origin x + 1.
        term bg: Colors background.
        term write: (String new: bounds width - 2 withAll: $ ).
        y := y + 1
    ].

    term resetStyle
!

drawBorder
    | term |
    term := Terminal current.
    focused
        ifTrue: [term fg: Colors title]
        ifFalse: [term fg: Colors border].

    "Top border"
    term setCursorRow: bounds origin y col: bounds origin x.
    term write: '+'.
    2 to: bounds width - 1 do: [:i | term write: '-'].
    term write: '+'.

    "Side borders"
    1 to: bounds height - 2 do: [:i |
        term setCursorRow: bounds origin y + i col: bounds origin x.
        term write: '|'.
        term setCursorRow: bounds origin y + i col: bounds origin x + bounds width - 1.
        term write: '|'
    ].

    "Bottom border"
    term setCursorRow: bounds origin y + bounds height - 1 col: bounds origin x.
    term write: '+'.
    2 to: bounds width - 1 do: [:i | term write: '-'].
    term write: '+'.

    term fg: Colors foreground
!

truncate: aString to: maxLen
    aString size <= maxLen ifTrue: [^aString].
    ^aString copyFrom: 1 to: maxLen
! !

!ListView methodsFor: 'key handling'!
handleKey: keyCode
    keyCode = Terminal keyUp ifTrue: [self selectPrevious. ^true].
    keyCode = Terminal keyDown ifTrue: [self selectNext. ^true].
    keyCode = Terminal keyPageUp ifTrue: [
        self selectIndex: selectedIndex - (bounds height - 3).
        ^true
    ].
    keyCode = Terminal keyPageDown ifTrue: [
        self selectIndex: selectedIndex + (bounds height - 3).
        ^true
    ].
    keyCode = Terminal keyHome ifTrue: [self selectFirst. ^true].
    keyCode = Terminal keyEnd ifTrue: [self selectLast. ^true].
    ^false
! !


"============================================================"
"TabBar - Tab switching widget"
"============================================================"
Object subclass: #TabBar
    instanceVariableNames: 'bounds tabs activeTab'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TabBar class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TabBar methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 0@0 extent: 80@1.
    tabs := #(#('Transcript' $1) #('Workspace' $2) #('Browser' $3) #('FFI Config' $4)).
    activeTab := 1
! !

!TabBar methodsFor: 'accessing'!
bounds: aRect
    bounds := aRect
!

activeTab
    ^activeTab
!

activeTab: index
    activeTab := index
! !

!TabBar methodsFor: 'drawing'!
draw
    | term x |
    term := Terminal current.
    x := 1.

    term setCursorRow: bounds origin y + 1 col: 1.
    term bg: Colors background.

    "Draw each tab"
    1 to: tabs size do: [:i |
        | tab tabName shortcut |
        tab := tabs at: i.
        tabName := tab at: 1.
        shortcut := tab at: 2.

        i = activeTab
            ifTrue: [
                term bg: Colors selection.
                term fg: Colors foreground.
                term bold: true
            ]
            ifFalse: [
                term bg: Colors background.
                term fg: Colors comment.
                term bold: false
            ].

        term write: ' '.
        term write: (String with: shortcut).
        term write: ':'.
        term write: tabName.
        term write: ' '.
        term resetStyle.
        term bg: Colors background.
        term write: ' '
    ].

    "Fill rest of line"
    term write: (String new: (bounds width - x) withAll: $ ).
    term resetStyle
! !


"============================================================"
"StatusBar - Bottom status bar"
"============================================================"
Object subclass: #StatusBar
    instanceVariableNames: 'bounds message items'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!StatusBar class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!StatusBar methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 0@23 extent: 80@1.
    message := 'Ready'.
    items := #()
! !

!StatusBar methodsFor: 'accessing'!
bounds: aRect
    bounds := aRect
!

message: aString
    message := aString
!

items: anArray
    items := anArray
! !

!StatusBar methodsFor: 'drawing'!
draw
    | term itemsText |
    term := Terminal current.

    term setCursorRow: bounds origin y + 1 col: 1.
    term bg: Colors border.
    term fg: Colors background.

    "Clear status bar"
    term write: (String new: bounds width withAll: $ ).

    "Write message on left"
    term setCursorRow: bounds origin y + 1 col: 2.
    term write: message.

    "Write items on right"
    items isEmpty ifFalse: [
        itemsText := ''.
        items do: [:item | itemsText := itemsText, ' | ', item].
        term setCursorRow: bounds origin y + 1 col: bounds width - itemsText size.
        term write: itemsText
    ].

    term resetStyle
! !


"============================================================"
"TranscriptTab - Scrollable transcript output"
"============================================================"
Object subclass: #TranscriptTab
    instanceVariableNames: 'bounds lines scrollTop maxLines focused'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Tabs'!

!TranscriptTab class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TranscriptTab methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 0@1 extent: 80@21.
    lines := OrderedCollection new.
    scrollTop := 0.
    maxLines := 1000.
    focused := false.

    "Welcome message"
    self addLine: 'Zig Smalltalk TUI'.
    self addLine: ''.
    self addLine: 'Press 1-4 to switch tabs'.
    self addLine: 'Press Q to quit'.
    self addLine: ''
! !

!TranscriptTab methodsFor: 'accessing'!
bounds: aRect
    bounds := aRect
!

focused: aBoolean
    focused := aBoolean
! !

!TranscriptTab methodsFor: 'output'!
addLine: aString
    lines add: aString.
    lines size > maxLines ifTrue: [
        lines removeFirst
    ].
    "Auto-scroll to bottom"
    self scrollToBottom
!

addText: aString
    "Add text, handling newlines"
    aString lines do: [:line | self addLine: line]
!

clear
    lines := OrderedCollection new.
    scrollTop := 0
!

scrollToBottom
    | visibleHeight |
    visibleHeight := bounds height - 2.
    scrollTop := (lines size - visibleHeight) max: 0
! !

!TranscriptTab methodsFor: 'scrolling'!
scrollUp
    scrollTop := (scrollTop - 1) max: 0
!

scrollDown
    | maxScroll |
    maxScroll := (lines size - (bounds height - 2)) max: 0.
    scrollTop := (scrollTop + 1) min: maxScroll
!

pageUp
    scrollTop := (scrollTop - (bounds height - 2)) max: 0
!

pageDown
    | maxScroll |
    maxScroll := (lines size - (bounds height - 2)) max: 0.
    scrollTop := (scrollTop + (bounds height - 2)) min: maxScroll
! !

!TranscriptTab methodsFor: 'drawing'!
draw
    | term visibleHeight y startLine endLine |
    term := Terminal current.
    visibleHeight := bounds height - 2.

    term bg: Colors background.
    term fg: Colors foreground.

    "Draw border"
    self drawBorder.

    "Draw content"
    startLine := scrollTop + 1.
    endLine := (scrollTop + visibleHeight) min: lines size.
    y := bounds origin y + 1.

    startLine to: endLine do: [:i |
        term setCursorRow: y col: bounds origin x + 1.
        term write: (self truncate: (lines at: i) to: bounds width - 2).
        y := y + 1
    ].

    "Clear remaining lines"
    [y <= (bounds origin y + bounds height - 2)] whileTrue: [
        term setCursorRow: y col: bounds origin x + 1.
        term write: (String new: bounds width - 2 withAll: $ ).
        y := y + 1
    ].

    term resetStyle
!

drawBorder
    | term |
    term := Terminal current.
    term fg: Colors border.

    "Top border with title"
    term setCursorRow: bounds origin y col: bounds origin x.
    term write: '+'.
    term write: '[ Transcript ]'.
    (bounds width - 15) timesRepeat: [term write: '-'].
    term write: '+'.

    "Side borders"
    1 to: bounds height - 2 do: [:i |
        term setCursorRow: bounds origin y + i col: bounds origin x.
        term write: '|'.
        term setCursorRow: bounds origin y + i col: bounds origin x + bounds width - 1.
        term write: '|'
    ].

    "Bottom border"
    term setCursorRow: bounds origin y + bounds height - 1 col: bounds origin x.
    term write: '+'.
    (bounds width - 2) timesRepeat: [term write: '-'].
    term write: '+'.

    term fg: Colors foreground
!

truncate: aString to: maxLen
    aString size <= maxLen ifTrue: [^aString].
    ^aString copyFrom: 1 to: maxLen
! !

!TranscriptTab methodsFor: 'key handling'!
handleKey: keyCode
    keyCode = Terminal keyUp ifTrue: [self scrollUp. ^true].
    keyCode = Terminal keyDown ifTrue: [self scrollDown. ^true].
    keyCode = Terminal keyPageUp ifTrue: [self pageUp. ^true].
    keyCode = Terminal keyPageDown ifTrue: [self pageDown. ^true].
    ^false
! !


"============================================================"
"WorkspaceTab - Code editor with evaluation"
"============================================================"
Object subclass: #WorkspaceTab
    instanceVariableNames: 'bounds editor focused onDoIt onPrintIt'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Tabs'!

!WorkspaceTab class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!WorkspaceTab methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 0@1 extent: 80@21.
    editor := TextArea new.
    editor bounds: (Rectangle origin: bounds origin extent: bounds extent).
    editor lineNumbers: true.
    editor syntaxHighlight: true.
    focused := false.
    onDoIt := nil.
    onPrintIt := nil
! !

!WorkspaceTab methodsFor: 'accessing'!
bounds: aRect
    bounds := aRect.
    editor bounds: aRect
!

focused: aBoolean
    focused := aBoolean.
    editor focused: aBoolean
!

text
    ^editor text
!

text: aString
    editor text: aString
!

onDoIt: aBlock
    onDoIt := aBlock
!

onPrintIt: aBlock
    onPrintIt := aBlock
! !

!WorkspaceTab methodsFor: 'evaluation'!
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
    ^editor currentLine
!

insertResult: aString
    "Insert result at cursor"
    editor insertChar: $ .
    editor insertChar: $".
    editor insertChar: $ .
    aString do: [:ch | editor insertChar: ch].
    editor insertChar: $ .
    editor insertChar: $"
! !

!WorkspaceTab methodsFor: 'drawing'!
draw
    editor draw
! !

!WorkspaceTab methodsFor: 'key handling'!
handleKey: keyCode
    "Ctrl+D = Do It (ASCII 4)"
    keyCode = 4 ifTrue: [self doIt. ^true].

    "Ctrl+P = Print It (ASCII 16)"
    keyCode = 16 ifTrue: [self printIt. ^true].

    ^editor handleKey: keyCode
! !


"============================================================"
"BrowserTab - Class and method browser with 5 panes"
"============================================================"
Object subclass: #BrowserTab
    instanceVariableNames: 'bounds packageList classList instanceMethodList classMethodList sourceEditor activePane focused onSelectClass onSelectMethod onLoadClasses'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Tabs'!

!BrowserTab class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!BrowserTab methodsFor: 'initialization'!
initialize
    | topHeight paneWidth sourceHeight |
    bounds := Rectangle origin: 0@1 extent: 80@21.

    topHeight := bounds height // 2.
    paneWidth := bounds width // 4.
    sourceHeight := bounds height - topHeight.

    "Create the 4 list panes"
    packageList := ListView new.
    packageList bounds: (Rectangle origin: bounds origin extent: paneWidth @ topHeight).
    packageList title: 'Packages'.
    packageList addItem: 'All'.

    classList := ListView new.
    classList bounds: (Rectangle origin: (bounds origin x + paneWidth) @ bounds origin y extent: paneWidth @ topHeight).
    classList title: 'Classes'.

    instanceMethodList := ListView new.
    instanceMethodList bounds: (Rectangle origin: (bounds origin x + (paneWidth * 2)) @ bounds origin y extent: paneWidth @ topHeight).
    instanceMethodList title: 'Instance'.

    classMethodList := ListView new.
    classMethodList bounds: (Rectangle origin: (bounds origin x + (paneWidth * 3)) @ bounds origin y extent: (bounds width - (paneWidth * 3)) @ topHeight).
    classMethodList title: 'Class'.

    "Create source editor"
    sourceEditor := TextArea new.
    sourceEditor bounds: (Rectangle origin: bounds origin x @ (bounds origin y + topHeight) extent: bounds width @ sourceHeight).
    sourceEditor lineNumbers: true.
    sourceEditor syntaxHighlight: true.
    sourceEditor readonly: true.

    activePane := #packages.
    focused := false.

    "Load initial data"
    self loadClasses
! !

!BrowserTab methodsFor: 'accessing'!
bounds: aRect
    | topHeight paneWidth sourceHeight |
    bounds := aRect.
    topHeight := bounds height // 2.
    paneWidth := bounds width // 4.
    sourceHeight := bounds height - topHeight.

    packageList bounds: (Rectangle origin: bounds origin extent: paneWidth @ topHeight).
    classList bounds: (Rectangle origin: (bounds origin x + paneWidth) @ bounds origin y extent: paneWidth @ topHeight).
    instanceMethodList bounds: (Rectangle origin: (bounds origin x + (paneWidth * 2)) @ bounds origin y extent: paneWidth @ topHeight).
    classMethodList bounds: (Rectangle origin: (bounds origin x + (paneWidth * 3)) @ bounds origin y extent: (bounds width - (paneWidth * 3)) @ topHeight).
    sourceEditor bounds: (Rectangle origin: bounds origin x @ (bounds origin y + topHeight) extent: bounds width @ sourceHeight)
!

focused: aBoolean
    focused := aBoolean.
    self updatePaneFocus
!

activePane
    ^activePane
! !

!BrowserTab methodsFor: 'data loading'!
loadClasses
    "Load all class names"
    | names |
    names := OrderedCollection new.
    Smalltalk keys do: [:key |
        | val |
        val := Smalltalk at: key ifAbsent: [nil].
        (val notNil and: [val isKindOf: Class]) ifTrue: [
            names add: key asString
        ]
    ].
    classList items: names asSortedCollection
!

loadMethodsFor: className
    "Load methods for selected class"
    | cls instMethods classMethods |
    cls := Smalltalk at: className asSymbol ifAbsent: [nil].
    cls ifNil: [^self].

    instMethods := OrderedCollection new.
    classMethods := OrderedCollection new.

    cls selectors do: [:sel | instMethods add: sel asString].
    cls class selectors do: [:sel | classMethods add: sel asString].

    instanceMethodList items: instMethods asSortedCollection.
    classMethodList items: classMethods asSortedCollection.

    "Show class definition in source"
    sourceEditor text: (self classDefinitionFor: cls)
!

classDefinitionFor: aClass
    | def |
    def := ''.
    aClass superclass
        ifNil: [def := 'nil']
        ifNotNil: [def := aClass superclass name].
    def := def, ' subclass: #', aClass name.
    def := def, String lf, '    instanceVariableNames: '''''.
    def := def, String lf, '    classVariableNames: '''''.
    def := def, String lf, '    poolDictionaries: '''''.
    ^def
!

loadSourceFor: className selector: selector classMethod: isClassMethod
    "Load method source"
    | cls |
    cls := Smalltalk at: className asSymbol ifAbsent: [nil].
    cls ifNil: [^self].
    isClassMethod ifTrue: [cls := cls class].

    "Source not available - show placeholder"
    sourceEditor text: selector, String lf, '    "Source code not available"'
! !

!BrowserTab methodsFor: 'pane management'!
updatePaneFocus
    packageList focused: (focused and: [activePane = #packages]).
    classList focused: (focused and: [activePane = #classes]).
    instanceMethodList focused: (focused and: [activePane = #instanceMethods]).
    classMethodList focused: (focused and: [activePane = #classMethods]).
    sourceEditor focused: (focused and: [activePane = #source])
!

nextPane
    activePane = #packages ifTrue: [activePane := #classes. ^self updatePaneFocus].
    activePane = #classes ifTrue: [activePane := #instanceMethods. ^self updatePaneFocus].
    activePane = #instanceMethods ifTrue: [activePane := #classMethods. ^self updatePaneFocus].
    activePane = #classMethods ifTrue: [activePane := #source. ^self updatePaneFocus].
    activePane = #source ifTrue: [activePane := #packages. ^self updatePaneFocus]
!

previousPane
    activePane = #packages ifTrue: [activePane := #source. ^self updatePaneFocus].
    activePane = #classes ifTrue: [activePane := #packages. ^self updatePaneFocus].
    activePane = #instanceMethods ifTrue: [activePane := #classes. ^self updatePaneFocus].
    activePane = #classMethods ifTrue: [activePane := #instanceMethods. ^self updatePaneFocus].
    activePane = #source ifTrue: [activePane := #classMethods. ^self updatePaneFocus]
! !

!BrowserTab methodsFor: 'selection'!
classSelected
    "Called when a class is selected"
    | className |
    className := classList selectedItem.
    className ifNotNil: [self loadMethodsFor: className]
!

instanceMethodSelected
    "Called when an instance method is selected"
    | className methodName |
    className := classList selectedItem.
    methodName := instanceMethodList selectedItem.
    (className notNil and: [methodName notNil]) ifTrue: [
        self loadSourceFor: className selector: methodName classMethod: false
    ]
!

classMethodSelected
    "Called when a class method is selected"
    | className methodName |
    className := classList selectedItem.
    methodName := classMethodList selectedItem.
    (className notNil and: [methodName notNil]) ifTrue: [
        self loadSourceFor: className selector: methodName classMethod: true
    ]
! !

!BrowserTab methodsFor: 'drawing'!
draw
    packageList draw.
    classList draw.
    instanceMethodList draw.
    classMethodList draw.
    sourceEditor draw
! !

!BrowserTab methodsFor: 'key handling'!
handleKey: keyCode
    "Tab to switch panes"
    keyCode = 9 ifTrue: [self nextPane. ^true].

    "Handle key in active pane"
    activePane = #packages ifTrue: [
        (packageList handleKey: keyCode) ifTrue: [^true]
    ].
    activePane = #classes ifTrue: [
        (classList handleKey: keyCode) ifTrue: [
            keyCode = Terminal keyEnter ifTrue: [self classSelected].
            ^true
        ]
    ].
    activePane = #instanceMethods ifTrue: [
        (instanceMethodList handleKey: keyCode) ifTrue: [
            keyCode = Terminal keyEnter ifTrue: [self instanceMethodSelected].
            ^true
        ]
    ].
    activePane = #classMethods ifTrue: [
        (classMethodList handleKey: keyCode) ifTrue: [
            keyCode = Terminal keyEnter ifTrue: [self classMethodSelected].
            ^true
        ]
    ].
    activePane = #source ifTrue: [
        (sourceEditor handleKey: keyCode) ifTrue: [^true]
    ].

    ^false
! !


"============================================================"
"FFIConfigTab - FFI Configuration"
"============================================================"
Object subclass: #FFIConfigTab
    instanceVariableNames: 'bounds libraryList functionList focused activePane'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Tabs'!

!FFIConfigTab class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!FFIConfigTab methodsFor: 'initialization'!
initialize
    | halfWidth |
    bounds := Rectangle origin: 0@1 extent: 80@21.
    halfWidth := bounds width // 2.

    libraryList := ListView new.
    libraryList bounds: (Rectangle origin: bounds origin extent: halfWidth @ bounds height).
    libraryList title: 'Libraries'.

    functionList := ListView new.
    functionList bounds: (Rectangle origin: (bounds origin x + halfWidth) @ bounds origin y extent: halfWidth @ bounds height).
    functionList title: 'Functions'.

    activePane := #libraries.
    focused := false.

    "Add sample libraries"
    libraryList addItem: 'libc'.
    libraryList addItem: 'libm'
! !

!FFIConfigTab methodsFor: 'accessing'!
bounds: aRect
    | halfWidth |
    bounds := aRect.
    halfWidth := bounds width // 2.
    libraryList bounds: (Rectangle origin: bounds origin extent: halfWidth @ bounds height).
    functionList bounds: (Rectangle origin: (bounds origin x + halfWidth) @ bounds origin y extent: halfWidth @ bounds height)
!

focused: aBoolean
    focused := aBoolean.
    libraryList focused: (focused and: [activePane = #libraries]).
    functionList focused: (focused and: [activePane = #functions])
! !

!FFIConfigTab methodsFor: 'drawing'!
draw
    libraryList draw.
    functionList draw
! !

!FFIConfigTab methodsFor: 'key handling'!
handleKey: keyCode
    keyCode = 9 ifTrue: [
        activePane = #libraries
            ifTrue: [activePane := #functions]
            ifFalse: [activePane := #libraries].
        self focused: focused.
        ^true
    ].

    activePane = #libraries ifTrue: [
        ^libraryList handleKey: keyCode
    ].
    activePane = #functions ifTrue: [
        ^functionList handleKey: keyCode
    ].

    ^false
! !


"============================================================"
"TUIApp - Main application"
"============================================================"
Object subclass: #TUIApp
    instanceVariableNames: 'term tabBar statusBar transcriptTab workspaceTab browserTab ffiConfigTab activeTabIndex running'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Apps'!

!TUIApp class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TUIApp class methodsFor: 'launching'!
start
    ^self new run
! !

!TUIApp methodsFor: 'initialization'!
initialize
    | size contentHeight |
    term := Terminal current.
    size := term size.
    contentHeight := (size at: 2) - 2.

    "Create tab bar"
    tabBar := TabBar new.
    tabBar bounds: (Rectangle origin: 0@0 extent: (size at: 1)@1).

    "Create status bar"
    statusBar := StatusBar new.
    statusBar bounds: (Rectangle origin: 0@((size at: 2) - 1) extent: (size at: 1)@1).
    statusBar message: 'Ready'.

    "Create tabs"
    transcriptTab := TranscriptTab new.
    transcriptTab bounds: (Rectangle origin: 0@1 extent: (size at: 1)@contentHeight).

    workspaceTab := WorkspaceTab new.
    workspaceTab bounds: (Rectangle origin: 0@1 extent: (size at: 1)@contentHeight).
    workspaceTab onDoIt: [:code | self doIt: code].
    workspaceTab onPrintIt: [:code | self printIt: code].

    browserTab := BrowserTab new.
    browserTab bounds: (Rectangle origin: 0@1 extent: (size at: 1)@contentHeight).

    ffiConfigTab := FFIConfigTab new.
    ffiConfigTab bounds: (Rectangle origin: 0@1 extent: (size at: 1)@contentHeight).

    activeTabIndex := 1.
    running := false.

    self updateFocus
! !

!TUIApp methodsFor: 'tabs'!
activeTab
    activeTabIndex = 1 ifTrue: [^transcriptTab].
    activeTabIndex = 2 ifTrue: [^workspaceTab].
    activeTabIndex = 3 ifTrue: [^browserTab].
    activeTabIndex = 4 ifTrue: [^ffiConfigTab].
    ^transcriptTab
!

switchToTab: index
    (index < 1) ifTrue: [^self].
    (index > 4) ifTrue: [^self].
    activeTabIndex := index.
    tabBar activeTab: index.
    self updateFocus.
    self updateStatusBar
!

updateFocus
    transcriptTab focused: activeTabIndex = 1.
    workspaceTab focused: activeTabIndex = 2.
    browserTab focused: activeTabIndex = 3.
    ffiConfigTab focused: activeTabIndex = 4
!

updateStatusBar
    activeTabIndex = 1 ifTrue: [
        statusBar message: 'Transcript - Up/Down to scroll'.
        statusBar items: #()
    ].
    activeTabIndex = 2 ifTrue: [
        statusBar message: 'Workspace'.
        statusBar items: #('Ctrl+D Do It' 'Ctrl+P Print It')
    ].
    activeTabIndex = 3 ifTrue: [
        statusBar message: 'Browser - Tab to switch panes'.
        statusBar items: #()
    ].
    activeTabIndex = 4 ifTrue: [
        statusBar message: 'FFI Config'.
        statusBar items: #()
    ]
! !

!TUIApp methodsFor: 'evaluation'!
doIt: code
    "Execute code"
    | result |
    [
        result := Compiler evaluate: code.
        transcriptTab addLine: '>> ', code.
        transcriptTab addLine: '=> ', result printString
    ] on: Error do: [:ex |
        transcriptTab addLine: '>> ', code.
        transcriptTab addLine: 'Error: ', ex messageText
    ]
!

printIt: code
    "Execute code and insert result"
    | result |
    [
        result := Compiler evaluate: code.
        workspaceTab insertResult: result printString.
        transcriptTab addLine: '>> ', code.
        transcriptTab addLine: '=> ', result printString
    ] on: Error do: [:ex |
        transcriptTab addLine: '>> ', code.
        transcriptTab addLine: 'Error: ', ex messageText
    ]
! !

!TUIApp methodsFor: 'transcript'!
show: aString
    transcriptTab addLine: aString asString
!

cr
    transcriptTab addLine: ''
! !

!TUIApp methodsFor: 'drawing'!
draw
    term hideCursor.
    term clear.
    term bg: Colors background.
    tabBar draw.
    self activeTab draw.
    statusBar draw.
    term flush.

    "Show cursor in workspace"
    activeTabIndex = 2 ifTrue: [term showCursor]
! !

!TUIApp methodsFor: 'running'!
run
    running := true.
    term initialize.
    self updateStatusBar.
    self draw.

    [running] whileTrue: [
        self processInput.
        Processor notNil ifTrue: [Processor yield]
    ].

    term shutdown
!

stop
    running := false
!

processInput
    | key |
    key := term pollKey.
    key isNil ifTrue: [^self].
    self handleKey: key.
    self draw
!

handleKey: keyCode
    "Quit"
    (keyCode = $q asciiValue or: [keyCode = $Q asciiValue]) ifTrue: [
        self stop. ^true
    ].

    "Tab switching with number keys"
    keyCode = $1 asciiValue ifTrue: [self switchToTab: 1. ^true].
    keyCode = $2 asciiValue ifTrue: [self switchToTab: 2. ^true].
    keyCode = $3 asciiValue ifTrue: [self switchToTab: 3. ^true].
    keyCode = $4 asciiValue ifTrue: [self switchToTab: 4. ^true].

    "Pass to active tab"
    ^self activeTab handleKey: keyCode
! !


"============================================================"
"Helper methods"
"============================================================"

!Array methodsFor: 'copying'!
copy
    | newArray |
    newArray := Array new: self size.
    1 to: self size do: [:i | newArray at: i put: (self at: i)].
    ^newArray
! !

!Array methodsFor: 'converting'!
asSortedCollection
    | result n temp |
    result := self copy.
    n := result size.
    2 to: n do: [:i |
        | j |
        j := i.
        [j > 1 and: [(result at: j - 1) asString > (result at: j) asString]] whileTrue: [
            temp := result at: j.
            result at: j put: (result at: j - 1).
            result at: j - 1 put: temp.
            j := j - 1
        ]
    ].
    ^result
!

asOrderedCollection
    | oc |
    oc := OrderedCollection new.
    self do: [:each | oc add: each].
    ^oc
! !

!OrderedCollection methodsFor: 'converting'!
asSortedCollection
    ^self asArray asSortedCollection
!

asArray
    | arr |
    arr := Array new: self size.
    1 to: self size do: [:i | arr at: i put: (self at: i)].
    ^arr
! !

!OrderedCollection methodsFor: 'removing'!
removeFirst
    | first |
    self size = 0 ifTrue: [^nil].
    first := self at: 1.
    "Shift elements"
    1 to: self size - 1 do: [:i |
        self at: i put: (self at: i + 1)
    ].
    self instVarAt: 3 put: (self instVarAt: 3) - 1.
    ^first
!

removeAt: index
    | element |
    element := self at: index.
    index to: self size - 1 do: [:i |
        self at: i put: (self at: i + 1)
    ].
    self instVarAt: 3 put: (self instVarAt: 3) - 1.
    ^element
! !

!OrderedCollection methodsFor: 'adding'!
add: anObject afterIndex: index
    "Add anObject after the given index"
    | array lastIndex newArray |
    array := self instVarAt: 1.
    lastIndex := self instVarAt: 3.

    "Make room if needed"
    lastIndex >= array size ifTrue: [
        newArray := Array new: array size * 2.
        1 to: array size do: [:i | newArray at: i put: (array at: i)].
        array := newArray.
        self instVarAt: 1 put: array
    ].

    "Shift elements after index"
    lastIndex to: index + 1 by: -1 do: [:i |
        array at: i + 1 put: (array at: i)
    ].

    array at: index + 1 put: anObject.
    self instVarAt: 3 put: lastIndex + 1.
    ^anObject
! !

!String methodsFor: 'accessing'!
lines
    | result current |
    result := OrderedCollection new.
    current := ''.
    self do: [:ch |
        (ch = Character cr or: [ch = Character lf])
            ifTrue: [
                result add: current.
                current := ''
            ]
            ifFalse: [current := current, (String with: ch)]
    ].
    result add: current.
    ^result
!

lf
    ^String with: (Character value: 10)
! !

!String class methodsFor: 'instance creation'!
lf
    ^String with: (Character value: 10)
!

new: size withAll: aChar
    | str |
    str := String new: size.
    1 to: size do: [:i | str at: i put: aChar].
    ^str
! !

!Character methodsFor: 'testing'!
isDigit
    ^self asciiValue >= 48 and: [self asciiValue <= 57]
! !

!Character class methodsFor: 'accessing'!
lf
    ^Character value: 10
!

cr
    ^Character value: 13
! !

!Symbol methodsFor: 'converting'!
asString
    ^self printString copyFrom: 2 to: self printString size
! !


"Startup message"
Transcript show: 'Smalltalk TUI loaded. Type: TUIApp start'; cr!
