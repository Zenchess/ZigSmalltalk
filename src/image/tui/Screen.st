"Screen - Double-buffered screen rendering
 Manages a cell buffer for efficient screen updates"

Object subclass: #Cell
    instanceVariableNames: 'char style'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!Cell class methodsFor: 'instance creation'!
new
    ^super new initialize
!

char: aChar style: aStyle
    ^self new char: aChar; style: aStyle
!

space
    ^self char: $  style: TuiStyle default
! !

!Cell methodsFor: 'initialization'!
initialize
    char := $ .
    style := TuiStyle default
! !

!Cell methodsFor: 'accessing'!
char
    ^char
!

char: aCharacter
    char := aCharacter
!

style
    ^style
!

style: aStyle
    style := aStyle
! !

!Cell methodsFor: 'comparing'!
= other
    (other isKindOf: Cell) ifFalse: [^false].
    ^char = other char and: [style = other style]
!

hash
    ^char hash bitXor: style hash
! !

!Cell methodsFor: 'copying'!
copy
    ^self class char: char style: style copy
! !


"Rect helper class for rectangular regions"
Object subclass: #TuiRect
    instanceVariableNames: 'x y width height'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!TuiRect class methodsFor: 'instance creation'!
x: x y: y width: w height: h
    ^self new setX: x y: y width: w height: h
!

origin: aPoint extent: extentPoint
    ^self x: aPoint x y: aPoint y width: extentPoint x height: extentPoint y
! !

!TuiRect methodsFor: 'initialization'!
setX: ax y: ay width: w height: h
    x := ax.
    y := ay.
    width := w.
    height := h
! !

!TuiRect methodsFor: 'accessing'!
x ^x!
y ^y!
width ^width!
height ^height!

x: aNumber x := aNumber!
y: aNumber y := aNumber!
width: aNumber width := aNumber!
height: aNumber height := aNumber!

origin
    ^x @ y
!

extent
    ^width @ height
!

right
    ^x + width - 1
!

bottom
    ^y + height - 1
! !

!TuiRect methodsFor: 'testing'!
containsX: px y: py
    ^px >= x and: [px < (x + width) and: [py >= y and: [py < (y + height)]]]
!

containsPoint: aPoint
    ^self containsX: aPoint x y: aPoint y
! !

!TuiRect methodsFor: 'transforming'!
inner: margin
    "Return rect with margin inset"
    ^TuiRect
        x: x + margin
        y: y + margin
        width: (width - (margin * 2) max: 0)
        height: (height - (margin * 2) max: 0)
!

splitHorizontalAt: topHeight
    "Split into top and bottom rects"
    | actualTop |
    actualTop := topHeight min: height.
    ^Array
        with: (TuiRect x: x y: y width: width height: actualTop)
        with: (TuiRect x: x y: y + actualTop width: width height: height - actualTop)
!

splitVerticalAt: leftWidth
    "Split into left and right rects"
    | actualLeft |
    actualLeft := leftWidth min: width.
    ^Array
        with: (TuiRect x: x y: y width: actualLeft height: height)
        with: (TuiRect x: x + actualLeft y: y width: width - actualLeft height: height)
! !

!TuiRect methodsFor: 'copying'!
copy
    ^self class x: x y: y width: width height: height
! !


"Screen - Double buffered screen"
Object subclass: #TuiScreen
    instanceVariableNames: 'width height cells prevCells cursorX cursorY cursorVisible'
    classVariableNames: 'Current'
    poolDictionaries: ''
    category: 'TUI-Core'!

!TuiScreen class methodsFor: 'instance creation'!
current
    "Return singleton screen"
    Current isNil ifTrue: [
        Current := self new
    ].
    ^Current
!

reset
    "Reset singleton"
    Current := nil
!

width: w height: h
    ^self new initWidth: w height: h
! !

!TuiScreen methodsFor: 'initialization'!
initialize
    | term |
    term := AnsiTerminal current.
    self initWidth: term width height: term height
!

initWidth: w height: h
    width := w.
    height := h.
    cells := Array new: w * h.
    prevCells := Array new: w * h.
    cursorX := 1.
    cursorY := 1.
    cursorVisible := false.
    self clearBuffer.
    self markAllDirty
!

clearBuffer
    "Clear buffer with default cells"
    | defaultCell |
    defaultCell := Cell space.
    1 to: cells size do: [:i |
        cells at: i put: defaultCell copy
    ]
!

markAllDirty
    "Mark all cells as needing redraw"
    1 to: prevCells size do: [:i |
        prevCells at: i put: nil
    ]
! !

!TuiScreen methodsFor: 'accessing'!
width ^width!
height ^height!

cursorX ^cursorX!
cursorY ^cursorY!
cursorVisible ^cursorVisible!

cursorVisible: aBoolean
    cursorVisible := aBoolean
! !

!TuiScreen methodsFor: 'resizing'!
resize: newWidth height: newHeight
    "Resize the screen buffers"
    width := newWidth.
    height := newHeight.
    cells := Array new: newWidth * newHeight.
    prevCells := Array new: newWidth * newHeight.
    self clearBuffer.
    self markAllDirty
!

resizeToTerminal
    "Resize to match current terminal size"
    | term |
    term := AnsiTerminal current.
    term updateSize.
    (term width ~= width or: [term height ~= height]) ifTrue: [
        self resize: term width height: term height
    ]
! !

!TuiScreen methodsFor: 'cell access'!
indexFor: x y: y
    "Convert x,y to buffer index (1-based coords)"
    (x < 1 or: [x > width]) ifTrue: [^nil].
    (y < 1 or: [y > height]) ifTrue: [^nil].
    ^((y - 1) * width) + x
!

cellAt: x y: y
    "Get cell at position"
    | idx |
    idx := self indexFor: x y: y.
    idx isNil ifTrue: [^nil].
    ^cells at: idx
!

cellAt: x y: y put: aCell
    "Set cell at position"
    | idx |
    idx := self indexFor: x y: y.
    idx isNil ifTrue: [^self].
    cells at: idx put: aCell
!

setChar: ch at: x y: y style: style
    "Set character and style at position"
    self cellAt: x y: y put: (Cell char: ch style: style)
! !

!TuiScreen methodsFor: 'drawing'!
clear
    "Clear screen with default style"
    self clearWithStyle: TuiStyle default
!

clearWithStyle: style
    "Clear screen with given style"
    | cell |
    cell := Cell char: $  style: style.
    1 to: cells size do: [:i |
        cells at: i put: cell copy
    ]
!

drawText: text at: x y: y style: style
    "Draw text at position"
    | col |
    col := x.
    text do: [:ch |
        col > width ifTrue: [^self].
        self setChar: ch at: col y: y style: style.
        col := col + 1
    ]
!

drawTextClipped: text at: x y: y maxWidth: maxW style: style
    "Draw text clipped to max width"
    | col endCol |
    col := x.
    endCol := (x + maxW - 1) min: width.
    text do: [:ch |
        col > endCol ifTrue: [^self].
        self setChar: ch at: col y: y style: style.
        col := col + 1
    ]
!

fillRect: rect char: ch style: style
    "Fill rectangle with character"
    rect y to: (rect y + rect height - 1) do: [:row |
        row > height ifTrue: [^self].
        rect x to: (rect x + rect width - 1) do: [:col |
            col > width ifTrue: [^self].
            self setChar: ch at: col y: row style: style
        ]
    ]
!

drawHLine: length at: x y: y char: ch style: style
    "Draw horizontal line"
    x to: (x + length - 1) do: [:col |
        col > width ifTrue: [^self].
        self setChar: ch at: col y: y style: style
    ]
!

drawVLine: length at: x y: y char: ch style: style
    "Draw vertical line"
    y to: (y + length - 1) do: [:row |
        row > height ifTrue: [^self].
        self setChar: ch at: x y: row style: style
    ]
!

drawBox: rect style: style
    "Draw box border with ASCII characters"
    rect width < 2 ifTrue: [^self].
    rect height < 2 ifTrue: [^self].

    "Corners"
    self setChar: $+ at: rect x y: rect y style: style.
    self setChar: $+ at: rect right y: rect y style: style.
    self setChar: $+ at: rect x y: rect bottom style: style.
    self setChar: $+ at: rect right y: rect bottom style: style.

    "Top and bottom"
    (rect x + 1) to: (rect right - 1) do: [:col |
        self setChar: $- at: col y: rect y style: style.
        self setChar: $- at: col y: rect bottom style: style
    ].

    "Left and right"
    (rect y + 1) to: (rect bottom - 1) do: [:row |
        self setChar: $| at: rect x y: row style: style.
        self setChar: $| at: rect right y: row style: style
    ]
!

drawBoxRounded: rect style: style
    "Draw box border with Unicode rounded corners - matches Zig TUI"
    | hChar vChar tlChar trChar blChar brChar |
    rect width < 2 ifTrue: [^self].
    rect height < 2 ifTrue: [^self].

    "Use Unicode box drawing characters"
    hChar := (BoxChars horizontalU) first.
    vChar := (BoxChars verticalU) first.
    tlChar := (BoxChars roundTopLeft) first.
    trChar := (BoxChars roundTopRight) first.
    blChar := (BoxChars roundBottomLeft) first.
    brChar := (BoxChars roundBottomRight) first.

    "Corners"
    self setChar: tlChar at: rect x y: rect y style: style.
    self setChar: trChar at: rect right y: rect y style: style.
    self setChar: blChar at: rect x y: rect bottom style: style.
    self setChar: brChar at: rect right y: rect bottom style: style.

    "Top and bottom"
    (rect x + 1) to: (rect right - 1) do: [:col |
        self setChar: hChar at: col y: rect y style: style.
        self setChar: hChar at: col y: rect bottom style: style
    ].

    "Left and right"
    (rect y + 1) to: (rect bottom - 1) do: [:row |
        self setChar: vChar at: rect x y: row style: style.
        self setChar: vChar at: rect right y: row style: style
    ]
!

drawBoxUnicode: rect style: style
    "Draw box border with Unicode single-line characters"
    | hChar vChar tlChar trChar blChar brChar |
    rect width < 2 ifTrue: [^self].
    rect height < 2 ifTrue: [^self].

    "Use Unicode box drawing characters"
    hChar := (BoxChars horizontalU) first.
    vChar := (BoxChars verticalU) first.
    tlChar := (BoxChars topLeftU) first.
    trChar := (BoxChars topRightU) first.
    blChar := (BoxChars bottomLeftU) first.
    brChar := (BoxChars bottomRightU) first.

    "Corners"
    self setChar: tlChar at: rect x y: rect y style: style.
    self setChar: trChar at: rect right y: rect y style: style.
    self setChar: blChar at: rect x y: rect bottom style: style.
    self setChar: brChar at: rect right y: rect bottom style: style.

    "Top and bottom"
    (rect x + 1) to: (rect right - 1) do: [:col |
        self setChar: hChar at: col y: rect y style: style.
        self setChar: hChar at: col y: rect bottom style: style
    ].

    "Left and right"
    (rect y + 1) to: (rect bottom - 1) do: [:row |
        self setChar: vChar at: rect x y: row style: style.
        self setChar: vChar at: rect right y: row style: style
    ]
!

drawTitle: title at: rect focused: isFocused
    "Draw title in top border"
    | style x |
    style := isFocused ifTrue: [TuiStyle title] ifFalse: [TuiStyle border].
    title size > 0 ifTrue: [
        x := rect x + 2.
        self drawText: ' ' at: x y: rect y style: style.
        self drawText: title at: x + 1 y: rect y style: style.
        self drawText: ' ' at: x + 1 + title size y: rect y style: style
    ]
!

drawScrollbar: contentRect total: totalItems visible: visibleItems offset: scrollOffset
    "Draw vertical scrollbar matching Zig TUI style"
    | scrollbarX thumbSize thumbPos trackStyle thumbStyle thumbChar trackChar row |
    totalItems <= visibleItems ifTrue: [^self].
    contentRect height = 0 ifTrue: [^self].

    scrollbarX := contentRect x + contentRect width - 1.

    "Calculate thumb size - at least 1"
    thumbSize := ((visibleItems * visibleItems) / totalItems) max: 1.
    thumbSize := thumbSize min: visibleItems.

    "Calculate thumb position"
    thumbPos := 0.
    (totalItems - visibleItems) > 0 ifTrue: [
        | trackRange |
        trackRange := contentRect height - thumbSize.
        trackRange > 0 ifTrue: [
            thumbPos := (scrollOffset * trackRange / (totalItems - visibleItems)) min: trackRange
        ]
    ].

    "Draw scrollbar"
    trackStyle := TuiStyle dim.
    thumbStyle := TuiStyle dim.
    thumbChar := (BoxChars blockFull) first.
    trackChar := (BoxChars blockLight) first.

    row := 0.
    [row < contentRect height] whileTrue: [
        | char |
        char := (row >= thumbPos and: [row < (thumbPos + thumbSize)])
            ifTrue: [thumbChar]
            ifFalse: [trackChar].
        self setChar: char at: scrollbarX y: contentRect y + row style: trackStyle.
        row := row + 1
    ]
! !

!TuiScreen methodsFor: 'cursor'!
setCursor: x y: y
    "Set cursor position"
    cursorX := x.
    cursorY := y
!

setCursorAt: aPoint
    self setCursor: aPoint x y: aPoint y
! !

!TuiScreen methodsFor: 'rendering'!
flush
    "Render changes to terminal"
    | term currentStyle |
    term := AnsiTerminal current.
    term hideCursor.
    currentStyle := nil.

    1 to: height do: [:row |
        | lineDirty |
        lineDirty := false.

        "Check if line has changes"
        1 to: width do: [:col |
            | idx cell prev |
            idx := self indexFor: col y: row.
            cell := cells at: idx.
            prev := prevCells at: idx.
            (prev isNil or: [cell ~= prev]) ifTrue: [
                lineDirty := true
            ]
        ].

        lineDirty ifTrue: [
            term moveTo: row col: 1.
            1 to: width do: [:col |
                | idx cell |
                idx := self indexFor: col y: row.
                cell := cells at: idx.

                "Apply style if changed"
                (currentStyle isNil or: [currentStyle ~= cell style]) ifTrue: [
                    cell style apply.
                    currentStyle := cell style
                ].

                "Write character"
                term write: (String with: cell char).

                "Update prev buffer"
                prevCells at: idx put: cell copy
            ]
        ]
    ].

    term resetStyle.

    "Position and show cursor if needed"
    cursorVisible ifTrue: [
        term moveTo: cursorY col: cursorX.
        term showCursor
    ] ifFalse: [
        term hideCursor
    ]
!

flushFull
    "Force full redraw"
    self markAllDirty.
    self flush
!

forceRedraw
    "Mark everything as needing redraw"
    self markAllDirty
! !
