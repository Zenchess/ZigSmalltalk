"TuiTextArea - Multi-line text editor widget with full editing support
 Features: cursor movement, scrolling, selection, line numbers, syntax highlighting"

TuiWidget subclass: #TuiTextArea
    instanceVariableNames: 'lines cursorLine cursorCol scrollY scrollX lineNumbers syntaxHighlight readonly selectionStartLine selectionStartCol selectionEndLine selectionEndCol mouseSelecting onChange highlightCache highlightDirty'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiTextArea class methodsFor: 'instance creation'!
new
    ^super new initialize
!

rect: aRect
    ^self new rect: aRect
! !

!TuiTextArea methodsFor: 'initialization'!
initialize
    super initialize.
    lines := OrderedCollection new.
    lines add: ''.
    cursorLine := 1.
    cursorCol := 1.
    scrollY := 0.
    scrollX := 0.
    lineNumbers := true.
    syntaxHighlight := true.
    readonly := false.
    selectionStartLine := nil.
    selectionStartCol := nil.
    selectionEndLine := nil.
    selectionEndCol := nil.
    mouseSelecting := false.
    onChange := nil.
    hasBorder := true.
    highlightCache := Dictionary new.
    highlightDirty := true
! !

!TuiTextArea methodsFor: 'accessing'!
text
    "Return all text as single string"
    | result |
    result := ''.
    lines doWithIndex: [:line :i |
        i > 1 ifTrue: [result := result, String lf].
        result := result, line
    ].
    ^result
!

text: aString
    "Set text from string"
    lines := OrderedCollection new.
    aString isEmpty
        ifTrue: [lines add: '']
        ifFalse: [
            (aString subStrings: (String with: Character lf)) do: [:line |
                lines add: line
            ].
            lines isEmpty ifTrue: [lines add: '']
        ].
    cursorLine := 1.
    cursorCol := 1.
    scrollY := 0.
    scrollX := 0.
    self clearSelection.
    self invalidateHighlight
!

currentLine
    "Return current line text"
    ^lines at: cursorLine
!

lineCount
    ^lines size
!

lineNumbers: aBoolean
    lineNumbers := aBoolean
!

syntaxHighlight: aBoolean
    syntaxHighlight := aBoolean
!

readonly
    ^readonly
!

readonly: aBoolean
    readonly := aBoolean
!

onChange: aBlock
    "Set callback for when text changes"
    onChange := aBlock
! !

!TuiTextArea methodsFor: 'geometry'!
lineNumberWidth
    "Calculate width needed for line numbers"
    | numLines w |
    lineNumbers ifFalse: [^0].
    numLines := lines size.
    w := 1.
    [numLines >= 10] whileTrue: [
        numLines := numLines // 10.
        w := w + 1
    ].
    ^(w max: 3) + 1  "min 3 digits plus separator"
!

textContentRect
    "Return rect for text content (excluding line numbers)"
    | content lnWidth |
    content := self contentRect.
    lnWidth := self lineNumberWidth.
    ^TuiRect
        x: content x + lnWidth
        y: content y
        width: (content width - lnWidth max: 0)
        height: content height
!

visibleLines
    "Number of visible lines"
    ^self contentRect height
!

visibleCols
    "Number of visible columns for text"
    ^self textContentRect width
! !

!TuiTextArea methodsFor: 'cursor movement'!
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
    | lineLen |
    lineLen := (lines at: cursorLine) size.
    cursorCol <= lineLen
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
        self clampCursorCol
    ].
    self ensureCursorVisible
!

moveDown
    cursorLine < lines size ifTrue: [
        cursorLine := cursorLine + 1.
        self clampCursorCol
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
    scrollY := 0.
    scrollX := 0
!

moveToEnd
    cursorLine := lines size.
    cursorCol := (lines at: cursorLine) size + 1.
    self ensureCursorVisible
!

pageUp
    | pageSize |
    pageSize := self visibleLines.
    cursorLine := (cursorLine - pageSize) max: 1.
    self clampCursorCol.
    self ensureCursorVisible
!

pageDown
    | pageSize |
    pageSize := self visibleLines.
    cursorLine := (cursorLine + pageSize) min: lines size.
    self clampCursorCol.
    self ensureCursorVisible
!

clampCursorCol
    "Ensure cursor col is within current line"
    | lineLen |
    lineLen := (lines at: cursorLine) size.
    cursorCol := cursorCol min: (lineLen + 1)
!

ensureCursorVisible
    "Scroll to make cursor visible"
    | visLines visCols |
    visLines := self visibleLines.
    visCols := self visibleCols.

    "Vertical scroll"
    cursorLine <= scrollY ifTrue: [scrollY := cursorLine - 1].
    cursorLine > (scrollY + visLines) ifTrue: [
        scrollY := cursorLine - visLines
    ].
    scrollY := scrollY max: 0.

    "Horizontal scroll"
    cursorCol <= scrollX ifTrue: [scrollX := cursorCol - 1].
    cursorCol > (scrollX + visCols) ifTrue: [
        scrollX := cursorCol - visCols
    ].
    scrollX := scrollX max: 0
! !

!TuiTextArea methodsFor: 'editing'!
insertChar: aChar
    readonly ifTrue: [^self].
    self deleteSelection.
    | line before after |
    line := lines at: cursorLine.
    before := line copyFrom: 1 to: cursorCol - 1.
    after := line copyFrom: cursorCol to: line size.
    lines at: cursorLine put: before, (String with: aChar), after.
    cursorCol := cursorCol + 1.
    self ensureCursorVisible.
    self textChanged
!

insertNewline
    readonly ifTrue: [^self].
    self deleteSelection.
    | line before after newLine |
    line := lines at: cursorLine.
    before := line copyFrom: 1 to: cursorCol - 1.
    after := line copyFrom: cursorCol to: line size.
    lines at: cursorLine put: before.
    newLine := after.
    lines add: newLine afterIndex: cursorLine.
    cursorLine := cursorLine + 1.
    cursorCol := 1.
    self ensureCursorVisible.
    self textChanged
!

deleteBackward
    readonly ifTrue: [^self].
    self hasSelection ifTrue: [^self deleteSelection].
    cursorCol > 1 ifTrue: [
        | line before after |
        line := lines at: cursorLine.
        before := line copyFrom: 1 to: cursorCol - 2.
        after := line copyFrom: cursorCol to: line size.
        lines at: cursorLine put: before, after.
        cursorCol := cursorCol - 1.
        self textChanged
    ] ifFalse: [
        cursorLine > 1 ifTrue: [
            | prevLine currLine |
            prevLine := lines at: cursorLine - 1.
            currLine := lines at: cursorLine.
            cursorCol := prevLine size + 1.
            lines at: cursorLine - 1 put: prevLine, currLine.
            lines removeAt: cursorLine.
            cursorLine := cursorLine - 1.
            self textChanged
        ]
    ].
    self ensureCursorVisible
!

deleteForward
    readonly ifTrue: [^self].
    self hasSelection ifTrue: [^self deleteSelection].
    | line lineLen |
    line := lines at: cursorLine.
    lineLen := line size.
    cursorCol <= lineLen ifTrue: [
        | before after |
        before := line copyFrom: 1 to: cursorCol - 1.
        after := line copyFrom: cursorCol + 1 to: lineLen.
        lines at: cursorLine put: before, after.
        self textChanged
    ] ifFalse: [
        cursorLine < lines size ifTrue: [
            | nextLine |
            nextLine := lines at: cursorLine + 1.
            lines at: cursorLine put: line, nextLine.
            lines removeAt: cursorLine + 1.
            self textChanged
        ]
    ]
!

textChanged
    "Called when text is modified"
    self invalidateHighlight.
    onChange ifNotNil: [onChange value: self]
!

invalidateHighlight
    "Mark highlight cache as dirty"
    highlightCache := Dictionary new.
    highlightDirty := true
! !

!TuiTextArea methodsFor: 'selection'!
hasSelection
    ^selectionStartLine notNil and: [selectionEndLine notNil]
!

clearSelection
    selectionStartLine := nil.
    selectionStartCol := nil.
    selectionEndLine := nil.
    selectionEndCol := nil
!

startSelection
    "Start selection at current cursor"
    self hasSelection ifFalse: [
        selectionStartLine := cursorLine.
        selectionStartCol := cursorCol
    ]
!

extendSelection
    "Extend selection to current cursor"
    selectionEndLine := cursorLine.
    selectionEndCol := cursorCol
!

selectAll
    selectionStartLine := 1.
    selectionStartCol := 1.
    selectionEndLine := lines size.
    selectionEndCol := (lines at: lines size) size + 1
!

normalizedSelection
    "Return selection as {startLine. startCol. endLine. endCol} in order"
    | sl sc el ec |
    self hasSelection ifFalse: [^nil].
    sl := selectionStartLine.
    sc := selectionStartCol.
    el := selectionEndLine.
    ec := selectionEndCol.
    (sl > el or: [sl = el and: [sc > ec]]) ifTrue: [
        "Swap"
        | t |
        t := sl. sl := el. el := t.
        t := sc. sc := ec. ec := t
    ].
    ^Array with: sl with: sc with: el with: ec
!

selectedText
    "Return selected text"
    | sel result |
    sel := self normalizedSelection.
    sel isNil ifTrue: [^nil].

    sel first = (sel at: 3) ifTrue: [
        "Single line"
        | line s e |
        line := lines at: sel first.
        s := (sel at: 2) min: (line size + 1).
        e := (sel at: 4) min: (line size + 1).
        s >= e ifTrue: [^''].
        ^line copyFrom: s to: e - 1
    ].

    "Multi-line"
    result := ''.
    sel first to: (sel at: 3) do: [:lineNum |
        | line start end |
        line := lines at: lineNum.
        lineNum = sel first ifTrue: [
            start := (sel at: 2) min: (line size + 1).
            end := line size + 1
        ] ifFalse: [
            lineNum = (sel at: 3) ifTrue: [
                start := 1.
                end := (sel at: 4) min: (line size + 1)
            ] ifFalse: [
                start := 1.
                end := line size + 1
            ]
        ].
        start < end ifTrue: [
            result isEmpty ifFalse: [result := result, String lf].
            result := result, (line copyFrom: start to: end - 1)
        ]
    ].
    ^result
!

deleteSelection
    "Delete selected text"
    | sel |
    sel := self normalizedSelection.
    sel isNil ifTrue: [^self].

    sel first = (sel at: 3) ifTrue: [
        "Single line deletion"
        | line s e before after |
        line := lines at: sel first.
        s := (sel at: 2) min: (line size + 1).
        e := (sel at: 4) min: (line size + 1).
        s >= e ifTrue: [^self clearSelection].
        before := line copyFrom: 1 to: s - 1.
        after := line copyFrom: e to: line size.
        lines at: sel first put: before, after.
        cursorLine := sel first.
        cursorCol := s.
        self clearSelection.
        self textChanged.
        ^self
    ].

    "Multi-line deletion"
    | firstLine lastLine firstPart lastPart |
    firstLine := lines at: sel first.
    lastLine := lines at: (sel at: 3).
    firstPart := firstLine copyFrom: 1 to: (sel at: 2) - 1.
    lastPart := lastLine copyFrom: (sel at: 4) to: lastLine size.

    "Remove lines from end to start"
    (sel at: 3) to: sel first + 1 by: -1 do: [:i |
        lines removeAt: i
    ].
    lines at: sel first put: firstPart, lastPart.

    cursorLine := sel first.
    cursorCol := (sel at: 2).
    self clearSelection.
    self ensureCursorVisible.
    self textChanged
! !

!TuiTextArea methodsFor: 'key handling'!
handleKey: keyCode
    "Handle key input"

    "Arrow keys"
    keyCode = AnsiTerminal keyLeft ifTrue: [self clearSelection. self moveLeft. ^true].
    keyCode = AnsiTerminal keyRight ifTrue: [self clearSelection. self moveRight. ^true].
    keyCode = AnsiTerminal keyUp ifTrue: [self clearSelection. self moveUp. ^true].
    keyCode = AnsiTerminal keyDown ifTrue: [self clearSelection. self moveDown. ^true].
    keyCode = AnsiTerminal keyHome ifTrue: [self clearSelection. self moveHome. ^true].
    keyCode = AnsiTerminal keyEnd ifTrue: [self clearSelection. self moveEnd. ^true].
    keyCode = AnsiTerminal keyPageUp ifTrue: [self clearSelection. self pageUp. ^true].
    keyCode = AnsiTerminal keyPageDown ifTrue: [self clearSelection. self pageDown. ^true].

    "Enter"
    keyCode = AnsiTerminal keyEnter ifTrue: [
        readonly ifFalse: [self insertNewline].
        ^true
    ].

    "Backspace"
    keyCode = AnsiTerminal keyBackspace ifTrue: [
        readonly ifFalse: [self deleteBackward].
        ^true
    ].

    "Delete"
    keyCode = AnsiTerminal keyDelete ifTrue: [
        readonly ifFalse: [self deleteForward].
        ^true
    ].

    "Ctrl+A - Select all"
    keyCode = AnsiTerminal keyCtrlA ifTrue: [self selectAll. ^true].

    "Printable characters"
    (keyCode >= 32 and: [keyCode < 127]) ifTrue: [
        readonly ifFalse: [self insertChar: (Character value: keyCode)].
        ^true
    ].

    ^false
! !

!TuiTextArea methodsFor: 'scrolling'!
scrollUp: amount
    scrollY := (scrollY - amount) max: 0
!

scrollDown: amount
    | maxScroll |
    maxScroll := (lines size - self visibleLines) max: 0.
    scrollY := (scrollY + amount) min: maxScroll
! !

!TuiTextArea methodsFor: 'drawing'!
drawSelf: screen
    | content textRect |

    "Draw border with rounded corners (matches Zig TUI)"
    self drawBorderRounded: screen.

    "Get content areas"
    content := self contentRect.
    textRect := self textContentRect.

    "Clear content area"
    screen fillRect: content char: $  style: TuiStyle normal.

    "Draw line numbers"
    self drawLineNumbers: screen.

    "Draw text content"
    self drawTextContent: screen.

    "Draw scrollbar if content exceeds visible area"
    lines size > self visibleLines ifTrue: [
        screen drawScrollbar: textRect
               total: lines size
               visible: self visibleLines
               offset: scrollY
    ].

    "Position cursor if focused"
    focused ifTrue: [
        | cursorScreenX cursorScreenY |
        cursorScreenY := textRect y + cursorLine - 1 - scrollY.
        cursorScreenX := textRect x + cursorCol - 1 - scrollX.
        screen setCursor: cursorScreenX y: cursorScreenY.
        screen cursorVisible: true
    ]
!

drawLineNumbers: screen
    | content lnWidth row lineNum style |
    lineNumbers ifFalse: [^self].

    content := self contentRect.
    lnWidth := self lineNumberWidth.
    row := content y.
    lineNum := scrollY + 1.

    1 to: self visibleLines do: [:i |
        lineNum <= lines size ifTrue: [
            | numStr |
            style := lineNum = cursorLine
                ifTrue: [TuiStyle normal]
                ifFalse: [TuiStyle dim].
            numStr := self formatLineNumber: lineNum width: lnWidth - 1.
            screen drawText: numStr at: content x y: row style: style.
            screen setChar: $  at: content x + lnWidth - 1 y: row style: style.
        ].
        row := row + 1.
        lineNum := lineNum + 1
    ]
!

formatLineNumber: num width: w
    | str |
    str := num printString.
    [str size < w] whileTrue: [str := ' ', str].
    ^str
!

drawTextContent: screen
    | textRect row lineNum visibleWidth |
    textRect := self textContentRect.
    row := textRect y.
    lineNum := scrollY + 1.
    visibleWidth := textRect width.

    "Build highlight cache for visible lines if needed"
    syntaxHighlight ifTrue: [
        self updateHighlightCache
    ].

    1 to: self visibleLines do: [:i |
        lineNum <= lines size ifTrue: [
            self drawLine: lineNum at: row on: screen textRect: textRect
        ].
        row := row + 1.
        lineNum := lineNum + 1
    ]
!

updateHighlightCache
    "Update syntax highlight cache for all lines (with multi-line state tracking)"
    | inComment inString |
    highlightDirty ifFalse: [^self].

    highlightCache := Dictionary new.
    inComment := false.
    inString := false.

    1 to: lines size do: [:lineNum |
        | line result |
        line := lines at: lineNum.
        result := SmalltalkHighlighter highlightLine: line inComment: inComment inString: inString.
        highlightCache at: lineNum put: (result at: 1).
        inComment := result at: 2.
        inString := result at: 3
    ].

    highlightDirty := false
!

drawLine: lineNum at: row on: screen textRect: textRect
    "Draw a single line with token-based syntax highlighting (matches Zig TUI)"
    | line col visibleStart visibleEnd sel isLineInSelection lineTokens |
    line := lines at: lineNum.
    col := textRect x.
    visibleStart := scrollX + 1.
    visibleEnd := scrollX + textRect width.

    "Get highlight tokens for this line"
    lineTokens := syntaxHighlight
        ifTrue: [highlightCache at: lineNum ifAbsent: [nil]]
        ifFalse: [nil].

    "Check selection"
    sel := self normalizedSelection.
    isLineInSelection := sel notNil and: [
        lineNum >= sel first and: [lineNum <= (sel at: 3)]
    ].

    visibleStart to: visibleEnd do: [:charIdx |
        | ch style inSelection tokenType |
        charIdx <= line size
            ifTrue: [ch := line at: charIdx]
            ifFalse: [ch := $ ].

        "Determine if in selection"
        inSelection := false.
        isLineInSelection ifTrue: [
            lineNum = sel first ifTrue: [
                lineNum = (sel at: 3)
                    ifTrue: [inSelection := charIdx >= (sel at: 2) and: [charIdx < (sel at: 4)]]
                    ifFalse: [inSelection := charIdx >= (sel at: 2)]
            ] ifFalse: [
                lineNum = (sel at: 3)
                    ifTrue: [inSelection := charIdx < (sel at: 4)]
                    ifFalse: [inSelection := true]
            ]
        ].

        "Determine style"
        inSelection ifTrue: [
            style := TuiStyle selected
        ] ifFalse: [
            (lineTokens notNil and: [charIdx <= lineTokens size]) ifTrue: [
                tokenType := lineTokens at: charIdx.
                style := SmalltalkHighlighter styleForToken: tokenType
            ] ifFalse: [
                style := TuiStyle normal
            ]
        ].

        screen setChar: ch at: col y: row style: style.
        col := col + 1
    ]
! !
