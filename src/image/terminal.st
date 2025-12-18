"Terminal and Screen classes for Smalltalk TUI"

Object subclass: #Terminal
    instanceVariableNames: 'isInitialized'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

"Store singleton in Smalltalk dictionary"
Smalltalk at: #TerminalInstance put: nil!

!Terminal class methodsFor: 'accessing'!
current
    "Return the singleton Terminal instance"
    | inst |
    inst := Smalltalk at: #TerminalInstance ifAbsent: [nil].
    inst isNil ifTrue: [
        inst := self new.
        Smalltalk at: #TerminalInstance put: inst
    ].
    ^inst
! !

!Terminal class methodsFor: 'key constants'!
keyUp
    ^16r1001
!
keyDown
    ^16r1002
!
keyRight
    ^16r1003
!
keyLeft
    ^16r1004
!
keyHome
    ^16r1005
!
keyEnd
    ^16r1006
!
keyDelete
    ^16r1007
!
keyPageUp
    ^16r1008
!
keyPageDown
    ^16r1009
!
keyEscape
    ^27
!
keyEnter
    ^13
!
keyTab
    ^9
!
keyBackspace
    ^127
! !

!Terminal methodsFor: 'initialization'!
initialize
    "Initialize terminal - enter raw/alternate screen mode"
    <primitive: 940>
    self primitiveFailed
!

shutdown
    "Restore terminal to normal mode"
    <primitive: 941>
    self primitiveFailed
! !

!Terminal methodsFor: 'output'!
write: aString
    "Write string at current cursor position"
    <primitive: 942>
    self primitiveFailed
!

clear
    "Clear entire screen"
    <primitive: 943>
    self primitiveFailed
!

setCursorRow: row col: col
    "Move cursor to row, col (1-based)"
    <primitive: 944>
    self primitiveFailed
!

cursorPosition
    "Get current cursor position as Point"
    <primitive: 945>
    self primitiveFailed
!

flush
    "Flush output buffer"
    <primitive: 952>
    self primitiveFailed
! !

!Terminal methodsFor: 'colors'!
foregroundR: r g: g b: b
    "Set foreground color using RGB values (0-255)"
    <primitive: 946>
    self primitiveFailed
!

backgroundR: r g: g b: b
    "Set background color using RGB values (0-255)"
    <primitive: 947>
    self primitiveFailed
!

resetStyle
    "Reset all colors and attributes to default"
    <primitive: 948>
    self primitiveFailed
!

foregroundIndex: anInteger
    "Set foreground color by 256-color index"
    <primitive: 960>
    self primitiveFailed
!

backgroundIndex: anInteger
    "Set background color by 256-color index"
    <primitive: 961>
    self primitiveFailed
! !

!Terminal methodsFor: 'attributes'!
bold: aBoolean
    "Set bold mode on/off"
    <primitive: 953>
    self primitiveFailed
!

italic: aBoolean
    "Set italic mode on/off"
    <primitive: 954>
    self primitiveFailed
!

underline: aBoolean
    "Set underline mode on/off"
    <primitive: 955>
    self primitiveFailed
! !

!Terminal methodsFor: 'cursor control'!
hideCursor
    "Hide the cursor"
    <primitive: 956>
    self primitiveFailed
!

showCursor
    "Show the cursor"
    <primitive: 957>
    self primitiveFailed
! !

!Terminal methodsFor: 'line operations'!
clearLine
    "Clear the current line"
    <primitive: 958>
    self primitiveFailed
!

clearToEndOfLine
    "Clear from cursor to end of line"
    <primitive: 959>
    self primitiveFailed
! !

!Terminal methodsFor: 'input'!
pollKey
    "Poll for key press (non-blocking). Returns key code or nil"
    <primitive: 949>
    ^nil
!

readKey
    "Read key press (blocking). Returns key code"
    <primitive: 950>
    self primitiveFailed
! !

!Terminal methodsFor: 'size'!
size
    "Return terminal size as an Array {cols. rows}"
    <primitive: 951>
    ^#(80 24)
!

cols
    "Return terminal width in columns"
    ^(self size) at: 1
!

rows
    "Return terminal height in rows"
    ^(self size) at: 2
! !

!Terminal methodsFor: 'drawing'!
drawBoxAt: origin extent: size
    "Draw a box with Unicode box-drawing characters.
     origin is {x. y}, size is {width. height}"
    <primitive: 962>
    self primitiveFailed
!

fillRectAt: origin extent: size char: char
    "Fill a rectangle with a character.
     origin is {x. y}, size is {width. height}, char is ASCII code"
    <primitive: 963>
    self primitiveFailed
! !

!Terminal methodsFor: 'convenience'!
moveTo: aPoint
    "Move cursor to aPoint (x@y)"
    self setCursorRow: aPoint y col: aPoint x
!

goto: row at: col
    "Move cursor to row, col"
    self setCursorRow: row col: col
!

print: aString at: aPoint
    "Print string at position"
    self moveTo: aPoint.
    self write: aString
!

printCentered: aString row: row
    "Print string centered on given row"
    | col |
    col := (self cols - aString size) // 2.
    self goto: row at: col.
    self write: aString
!

fg: aColor
    "Set foreground color. aColor is an Array {r. g. b}"
    self foregroundR: (aColor at: 1) g: (aColor at: 2) b: (aColor at: 3)
!

bg: aColor
    "Set background color. aColor is an Array {r. g. b}"
    self backgroundR: (aColor at: 1) g: (aColor at: 2) b: (aColor at: 3)
! !


"Color constants"
Object subclass: #Colors
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!Colors class methodsFor: 'standard colors'!
black
    ^#(0 0 0)
!
white
    ^#(255 255 255)
!
red
    ^#(255 0 0)
!
green
    ^#(0 255 0)
!
blue
    ^#(0 0 255)
!
yellow
    ^#(255 255 0)
!
cyan
    ^#(0 255 255)
!
magenta
    ^#(255 0 255)
!
gray
    ^#(128 128 128)
!
darkGray
    ^#(64 64 64)
!
lightGray
    ^#(192 192 192)
! !

!Colors class methodsFor: 'theme colors'!
background
    ^#(30 30 40)
!
foreground
    ^#(220 220 230)
!
selection
    ^#(60 70 100)
!
highlight
    ^#(80 90 120)
!
border
    ^#(70 80 100)
!
title
    ^#(150 170 220)
!
keyword
    ^#(130 170 255)
!
string
    ^#(150 220 150)
!
comment
    ^#(120 120 140)
!
number
    ^#(220 180 120)
! !


"Screen class for double-buffered rendering"
Object subclass: #Screen
    instanceVariableNames: 'width height buffer dirtyRegions'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

"Store singleton in Smalltalk dictionary"
Smalltalk at: #ScreenInstance put: nil!

!Screen class methodsFor: 'instance creation'!
current
    "Return the singleton Screen instance"
    | inst |
    inst := Smalltalk at: #ScreenInstance ifAbsent: [nil].
    inst isNil ifTrue: [
        inst := self new.
        inst initializeFromTerminal.
        Smalltalk at: #ScreenInstance put: inst
    ].
    ^inst
!

new
    ^super new initialize
! !

!Screen methodsFor: 'initialization'!
initialize
    width := 80.
    height := 24.
    buffer := Array new: (width * height).
    dirtyRegions := OrderedCollection new.
    self clearBuffer
!

initializeFromTerminal
    | size |
    size := Terminal current size.
    width := size at: 1.
    height := size at: 2.
    buffer := Array new: (width * height).
    self clearBuffer
! !

!Screen methodsFor: 'accessing'!
width
    ^width
!

height
    ^height
!

charAt: aPoint
    "Get character at position"
    | idx |
    idx := self indexFor: aPoint.
    ^buffer at: idx
!

charAt: aPoint put: aChar
    "Set character at position"
    | idx |
    idx := self indexFor: aPoint.
    buffer at: idx put: aChar.
    self markDirty: aPoint
! !

!Screen methodsFor: 'private'!
indexFor: aPoint
    "Convert point to buffer index"
    ^((aPoint y - 1) * width) + aPoint x
!

markDirty: aPoint
    "Mark a position as needing redraw"
    dirtyRegions add: aPoint
! !

!Screen methodsFor: 'drawing'!
clearBuffer
    "Clear the buffer with spaces"
    1 to: buffer size do: [:i | buffer at: i put: $ ]
!

drawString: aString at: aPoint
    "Draw string into buffer"
    | x |
    x := aPoint x.
    aString do: [:ch |
        (x <= width) ifTrue: [
            self charAt: (x @ aPoint y) put: ch.
            x := x + 1
        ]
    ]
!

drawBox: aRect
    "Draw a box outline"
    | x1 y1 x2 y2 |
    x1 := aRect origin x.
    y1 := aRect origin y.
    x2 := aRect corner x.
    y2 := aRect corner y.

    "Top border"
    self charAt: (x1 @ y1) put: $+.
    (x1 + 1) to: (x2 - 1) do: [:x | self charAt: (x @ y1) put: $-].
    self charAt: (x2 @ y1) put: $+.

    "Side borders"
    (y1 + 1) to: (y2 - 1) do: [:y |
        self charAt: (x1 @ y) put: $|.
        self charAt: (x2 @ y) put: $|
    ].

    "Bottom border"
    self charAt: (x1 @ y2) put: $+.
    (x1 + 1) to: (x2 - 1) do: [:x | self charAt: (x @ y2) put: $-].
    self charAt: (x2 @ y2) put: $+
!

fill: aRect with: aChar
    "Fill a rectangle with a character"
    aRect origin y to: aRect corner y do: [:y |
        aRect origin x to: aRect corner x do: [:x |
            self charAt: (x @ y) put: aChar
        ]
    ]
! !

!Screen methodsFor: 'rendering'!
refresh
    "Render buffer to terminal"
    | term |
    term := Terminal current.
    term setCursorRow: 1 col: 1.
    1 to: height do: [:y |
        term setCursorRow: y col: 1.
        term write: (self rowString: y)
    ].
    term flush.
    dirtyRegions := OrderedCollection new
!

rowString: rowNum
    "Get string for a row"
    | start end |
    start := ((rowNum - 1) * width) + 1.
    end := start + width - 1.
    ^String withAll: (buffer copyFrom: start to: end)
! !


"Point class for coordinate pairs"
Object subclass: #Point
    instanceVariableNames: 'x y'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!Point class methodsFor: 'instance creation'!
x: xCoord y: yCoord
    ^self new x: xCoord y: yCoord
! !

!Point methodsFor: 'accessing'!
x
    ^x
!

y
    ^y
!

x: aNumber
    x := aNumber
!

y: aNumber
    y := aNumber
!

x: xCoord y: yCoord
    x := xCoord.
    y := yCoord
! !

!Point methodsFor: 'arithmetic'!
+ aPoint
    ^Point x: x + aPoint x y: y + aPoint y
!

- aPoint
    ^Point x: x - aPoint x y: y - aPoint y
!

* aNumber
    ^Point x: x * aNumber y: y * aNumber
!

// aNumber
    ^Point x: x // aNumber y: y // aNumber
! !

!Point methodsFor: 'comparing'!
= aPoint
    ^(x = aPoint x) and: [y = aPoint y]
!

hash
    ^x hash bitXor: y hash
! !

!Point methodsFor: 'printing'!
printOn: aStream
    aStream nextPutAll: x printString.
    aStream nextPut: $@.
    aStream nextPutAll: y printString
! !

"Add @ method to Number for Point creation"
!Number methodsFor: 'point creation'!
@ aNumber
    "Create a Point with receiver as x and argument as y"
    ^Point x: self y: aNumber
! !


"Rectangle helper"
Object subclass: #Rectangle
    instanceVariableNames: 'origin corner'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!Rectangle class methodsFor: 'instance creation'!
origin: originPoint corner: cornerPoint
    ^self new origin: originPoint corner: cornerPoint
!

origin: originPoint extent: extentPoint
    ^self origin: originPoint corner: originPoint + extentPoint - (1@1)
! !

!Rectangle methodsFor: 'accessing'!
origin
    ^origin
!

corner
    ^corner
!

origin: aPoint
    origin := aPoint
!

corner: aPoint
    corner := aPoint
!

origin: originPoint corner: cornerPoint
    origin := originPoint.
    corner := cornerPoint
!

extent
    ^corner - origin + (1@1)
!

width
    ^corner x - origin x + 1
!

height
    ^corner y - origin y + 1
! !

!Rectangle methodsFor: 'testing'!
containsPoint: aPoint
    ^(aPoint x >= origin x) and: [
        (aPoint x <= corner x) and: [
            (aPoint y >= origin y) and: [
                aPoint y <= corner y]]]
! !

"String extensions needed for TUI"
!String methodsFor: 'converting'!
asString
    "Return self - a String is already a String"
    ^self
! !

"Object extensions needed for primitives"
!Object methodsFor: 'error handling'!
primitiveFailed
    "A primitive has failed. Raise an error."
    self error: 'Primitive failed'
! !
