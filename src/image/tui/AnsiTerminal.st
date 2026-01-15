"AnsiTerminal - Low-level ANSI terminal control using pure escape codes
 This class provides all terminal functionality needed for a TUI without
 relying on external libraries."

Object subclass: #AnsiTerminal
    instanceVariableNames: 'width height rawMode inputBuffer'
    classVariableNames: 'Current'
    poolDictionaries: ''
    category: 'TUI-Core'!

!AnsiTerminal class methodsFor: 'instance creation'!
current
    "Return the singleton terminal instance"
    Current isNil ifTrue: [
        Current := self basicNew initialize
    ].
    ^Current
!

new
    "Use current instead"
    ^self current
!

reset
    "Reset the singleton (for testing)"
    Current ifNotNil: [
        Current shutdown.
        Current := nil
    ]
! !

!AnsiTerminal class methodsFor: 'escape codes'!
escape
    "Return the escape character"
    ^Character value: 27
!

csi
    "Return CSI (Control Sequence Introducer)"
    ^(String with: self escape), '['
! !

!AnsiTerminal class methodsFor: 'key constants'!
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

keyInsert
    ^16r100A
!

keyF1
    ^16r1011
!

keyF2
    ^16r1012
!

keyF3
    ^16r1013
!

keyF4
    ^16r1014
!

keyF5
    ^16r1015
!

keyF6
    ^16r1016
!

keyF7
    ^16r1017
!

keyF8
    ^16r1018
!

keyF9
    ^16r1019
!

keyF10
    ^16r101A
!

keyF11
    ^16r101B
!

keyF12
    ^16r101C
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

keyShiftTab
    ^16r100B
!

keyBackspace
    ^127
!

keyCtrlA ^1!
keyCtrlB ^2!
keyCtrlC ^3!
keyCtrlD ^4!
keyCtrlE ^5!
keyCtrlF ^6!
keyCtrlG ^7!
keyCtrlH ^8!
keyCtrlI ^9!
keyCtrlJ ^10!
keyCtrlK ^11!
keyCtrlL ^12!
keyCtrlM ^13!
keyCtrlN ^14!
keyCtrlO ^15!
keyCtrlP ^16!
keyCtrlQ ^17!
keyCtrlR ^18!
keyCtrlS ^19!
keyCtrlT ^20!
keyCtrlU ^21!
keyCtrlV ^22!
keyCtrlW ^23!
keyCtrlX ^24!
keyCtrlY ^25!
keyCtrlZ ^26!
! !

!AnsiTerminal methodsFor: 'initialization'!
initialize
    width := 80.
    height := 24.
    rawMode := false.
    inputBuffer := OrderedCollection new.
    self updateSize
!

shutdown
    "Restore terminal to normal state"
    rawMode ifTrue: [
        self exitRawMode.
        self exitAlternateScreen.
        self showCursor.
        self resetStyle
    ]
! !

!AnsiTerminal methodsFor: 'raw mode'!
enterRawMode
    "Enter raw mode using primitive"
    <primitive: 940>
    rawMode := true
!

exitRawMode
    "Exit raw mode using primitive"
    <primitive: 941>
    rawMode := false
!

enterAlternateScreen
    "Switch to alternate screen buffer"
    self writeEscape: '[?1049h'
!

exitAlternateScreen
    "Switch back to main screen buffer"
    self writeEscape: '[?1049l'
!

enableMouse
    "Enable mouse tracking"
    self writeEscape: '[?1000h'.  "X10 compatibility"
    self writeEscape: '[?1002h'.  "Button tracking"
    self writeEscape: '[?1006h'   "SGR extended mode"
!

disableMouse
    "Disable mouse tracking"
    self writeEscape: '[?1006l'.
    self writeEscape: '[?1002l'.
    self writeEscape: '[?1000l'
!

enableBracketedPaste
    "Enable bracketed paste mode"
    self writeEscape: '[?2004h'
!

disableBracketedPaste
    "Disable bracketed paste mode"
    self writeEscape: '[?2004l'
! !

!AnsiTerminal methodsFor: 'output - primitive based'!
write: aString
    "Write string to terminal"
    <primitive: 942>
    self primitiveFailed
!

flush
    "Flush output buffer"
    <primitive: 952>
!

writeEscape: suffix
    "Write an escape sequence"
    self write: (String with: (Character value: 27)), suffix
! !

!AnsiTerminal methodsFor: 'screen control'!
clear
    "Clear entire screen"
    <primitive: 943>
    self primitiveFailed
!

clearLine
    "Clear current line"
    <primitive: 958>
    self primitiveFailed
!

clearToEndOfLine
    "Clear from cursor to end of line"
    <primitive: 959>
    self primitiveFailed
!

clearToEndOfScreen
    "Clear from cursor to end of screen"
    self writeEscape: '[0J'
! !

!AnsiTerminal methodsFor: 'cursor control'!
moveTo: row col: col
    "Move cursor to position (1-based)"
    <primitive: 944>
    self primitiveFailed
!

moveTo: aPoint
    "Move cursor to aPoint (x@y where x=col, y=row)"
    self moveTo: aPoint y col: aPoint x
!

moveUp: n
    "Move cursor up n rows"
    self writeEscape: '[', n printString, 'A'
!

moveDown: n
    "Move cursor down n rows"
    self writeEscape: '[', n printString, 'B'
!

moveRight: n
    "Move cursor right n columns"
    self writeEscape: '[', n printString, 'C'
!

moveLeft: n
    "Move cursor left n columns"
    self writeEscape: '[', n printString, 'D'
!

hideCursor
    "Hide the cursor"
    <primitive: 956>
    self primitiveFailed
!

showCursor
    "Show the cursor"
    <primitive: 957>
    self primitiveFailed
!

saveCursor
    "Save cursor position"
    self writeEscape: '[s'
!

restoreCursor
    "Restore saved cursor position"
    self writeEscape: '[u'
! !

!AnsiTerminal methodsFor: 'colors'!
setForeground: r g: g b: b
    "Set foreground color using RGB (0-255)"
    <primitive: 946>
    self primitiveFailed
!

setBackground: r g: g b: b
    "Set background color using RGB (0-255)"
    <primitive: 947>
    self primitiveFailed
!

setForeground: aColor
    "Set foreground from Color object or array"
    (aColor isKindOf: Array)
        ifTrue: [self setForeground: (aColor at: 1) g: (aColor at: 2) b: (aColor at: 3)]
        ifFalse: [self setForeground: aColor red g: aColor green b: aColor blue]
!

setBackground: aColor
    "Set background from Color object or array"
    (aColor isKindOf: Array)
        ifTrue: [self setBackground: (aColor at: 1) g: (aColor at: 2) b: (aColor at: 3)]
        ifFalse: [self setBackground: aColor red g: aColor green b: aColor blue]
!

setForegroundIndex: index
    "Set foreground using 256-color palette"
    <primitive: 960>
    self primitiveFailed
!

setBackgroundIndex: index
    "Set background using 256-color palette"
    <primitive: 961>
    self primitiveFailed
!

resetStyle
    "Reset all colors and attributes"
    <primitive: 948>
    self primitiveFailed
! !

!AnsiTerminal methodsFor: 'text attributes'!
setBold: aBoolean
    "Set bold mode"
    <primitive: 953>
    self primitiveFailed
!

setItalic: aBoolean
    "Set italic mode"
    <primitive: 954>
    self primitiveFailed
!

setUnderline: aBoolean
    "Set underline mode"
    <primitive: 955>
    self primitiveFailed
!

setDim
    "Set dim/faint mode"
    self writeEscape: '[2m'
!

setReverse
    "Set reverse video mode"
    self writeEscape: '[7m'
!

setStrikethrough
    "Set strikethrough mode"
    self writeEscape: '[9m'
! !

!AnsiTerminal methodsFor: 'size'!
size
    "Return terminal size as Array {cols. rows}"
    <primitive: 951>
    ^Array with: 80 with: 24
!

updateSize
    "Update cached size"
    | sz |
    sz := self size.
    width := sz at: 1.
    height := sz at: 2
!

width
    ^width
!

height
    ^height
!

cols
    ^width
!

rows
    ^height
! !

!AnsiTerminal methodsFor: 'input'!
pollKey
    "Poll for key (non-blocking). Returns key code or nil"
    <primitive: 949>
    ^nil
!

readKey
    "Read a key (blocking)"
    <primitive: 950>
    self primitiveFailed
!

readEvent
    "Read an input event (key, mouse, or paste).
     Returns an InputEvent object or nil if no input."
    | key |
    key := self pollKey.
    key isNil ifTrue: [^nil].
    ^InputEvent key: key
! !

!AnsiTerminal methodsFor: 'drawing'!
drawBoxAt: origin extent: extent
    "Draw a box using Unicode box characters"
    <primitive: 962>
    self primitiveFailed
!

fillRectAt: origin extent: extent char: charCode
    "Fill rectangle with character"
    <primitive: 963>
    self primitiveFailed
!

drawText: text at: row col: col
    "Draw text at position"
    self moveTo: row col: col.
    self write: text
!

drawText: text at: aPoint
    "Draw text at point"
    self drawText: text at: aPoint y col: aPoint x
!

drawHorizontalLine: length at: row col: col char: ch
    "Draw horizontal line"
    self moveTo: row col: col.
    length timesRepeat: [self write: (String with: ch)]
!

drawVerticalLine: length at: row col: col char: ch
    "Draw vertical line"
    1 to: length do: [:i |
        self moveTo: row + i - 1 col: col.
        self write: (String with: ch)
    ]
! !

!AnsiTerminal methodsFor: 'convenience'!
print: text at: aPoint
    "Print text at position"
    self moveTo: aPoint.
    self write: text
!

printCentered: text row: row
    "Print text centered on row"
    | col |
    col := (self width - text size) // 2 + 1.
    self moveTo: row col: col.
    self write: text
!

withStyle: aBlock
    "Execute block and reset style after"
    aBlock value.
    self resetStyle
!

withColor: fg background: bg do: aBlock
    "Execute block with colors, then reset"
    self setForeground: fg.
    self setBackground: bg.
    aBlock value.
    self resetStyle
! !

!AnsiTerminal methodsFor: 'testing'!
isRawMode
    ^rawMode
! !


"InputEvent class to wrap different event types"
Object subclass: #InputEvent
    instanceVariableNames: 'type keyCode mouseButton mouseX mouseY pasteText'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!InputEvent class methodsFor: 'instance creation'!
key: keyCode
    ^self new initKey: keyCode
!

mouse: button x: x y: y
    ^self new initMouse: button x: x y: y
!

paste: text
    ^self new initPaste: text
! !

!InputEvent methodsFor: 'initialization'!
initKey: code
    type := #key.
    keyCode := code
!

initMouse: button x: x y: y
    type := #mouse.
    mouseButton := button.
    mouseX := x.
    mouseY := y
!

initPaste: text
    type := #paste.
    pasteText := text
! !

!InputEvent methodsFor: 'testing'!
isKey
    ^type = #key
!

isMouse
    ^type = #mouse
!

isPaste
    ^type = #paste
!

isChar
    ^type = #key and: [keyCode >= 32 and: [keyCode < 127]]
!

isControl
    ^type = #key and: [keyCode >= 1 and: [keyCode <= 26]]
!

isSpecial
    ^type = #key and: [keyCode >= 16r1000]
! !

!InputEvent methodsFor: 'accessing'!
keyCode
    ^keyCode
!

char
    "Return character if this is a printable key event"
    self isChar ifTrue: [^Character value: keyCode].
    ^nil
!

controlChar
    "Return control character (a-z) if this is a ctrl event"
    self isControl ifTrue: [^Character value: keyCode + 96].
    ^nil
!

mouseButton
    ^mouseButton
!

mouseX
    ^mouseX
!

mouseY
    ^mouseY
!

mousePosition
    ^mouseX @ mouseY
!

pasteText
    ^pasteText
! !
