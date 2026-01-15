"Theme - Catppuccin Mocha color theme matching the Zig TUI
 Plus Style class for combining colors and text attributes"

Object subclass: #TuiColor
    instanceVariableNames: 'red green blue'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!TuiColor class methodsFor: 'instance creation'!
r: r g: g b: b
    ^self new setR: r g: g b: b
!

fromHex: hexString
    "Parse hex color like '#1e1e2e' or '1e1e2e'"
    | str r g b |
    str := hexString.
    (str first = $#) ifTrue: [str := str copyFrom: 2 to: str size].
    "Parse hex digits directly without using streams"
    r := self parseHexPair: (str copyFrom: 1 to: 2).
    g := self parseHexPair: (str copyFrom: 3 to: 4).
    b := self parseHexPair: (str copyFrom: 5 to: 6).
    ^self r: r g: g b: b
!

parseHexPair: twoChars
    "Parse a two-character hex string to integer"
    | high low |
    high := self hexDigitValue: (twoChars at: 1).
    low := self hexDigitValue: (twoChars at: 2).
    ^(high * 16) + low
!

hexDigitValue: char
    "Return hex digit value 0-15 for 0-9, a-f, A-F"
    char = $0 ifTrue: [^0].
    char = $1 ifTrue: [^1].
    char = $2 ifTrue: [^2].
    char = $3 ifTrue: [^3].
    char = $4 ifTrue: [^4].
    char = $5 ifTrue: [^5].
    char = $6 ifTrue: [^6].
    char = $7 ifTrue: [^7].
    char = $8 ifTrue: [^8].
    char = $9 ifTrue: [^9].
    char = $a ifTrue: [^10]. char = $A ifTrue: [^10].
    char = $b ifTrue: [^11]. char = $B ifTrue: [^11].
    char = $c ifTrue: [^12]. char = $C ifTrue: [^12].
    char = $d ifTrue: [^13]. char = $D ifTrue: [^13].
    char = $e ifTrue: [^14]. char = $E ifTrue: [^14].
    char = $f ifTrue: [^15]. char = $F ifTrue: [^15].
    ^0
!

fromArray: anArray
    ^self r: (anArray at: 1) g: (anArray at: 2) b: (anArray at: 3)
! !

!TuiColor methodsFor: 'initialization'!
setR: r g: g b: b
    red := r.
    green := g.
    blue := b
! !

!TuiColor methodsFor: 'accessing'!
red ^red!
green ^green!
blue ^blue!

asArray
    ^Array with: red with: green with: blue
! !

!TuiColor methodsFor: 'applying'!
applyAsForeground
    AnsiTerminal current setForeground: red g: green b: blue
!

applyAsBackground
    AnsiTerminal current setBackground: red g: green b: blue
! !

!TuiColor methodsFor: 'comparing'!
= other
    (other isKindOf: TuiColor) ifFalse: [^false].
    ^red = other red and: [green = other green and: [blue = other blue]]
!

hash
    ^red hash bitXor: (green hash bitXor: blue hash)
! !


"Catppuccin Mocha Theme"
Object subclass: #Theme
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!Theme class methodsFor: 'base colors'!
base
    ^TuiColor fromHex: '#1e1e2e'
!

mantle
    ^TuiColor fromHex: '#181825'
!

crust
    ^TuiColor fromHex: '#11111b'
! !

!Theme class methodsFor: 'surface colors'!
surface0
    ^TuiColor fromHex: '#313244'
!

surface1
    ^TuiColor fromHex: '#45475a'
!

surface2
    ^TuiColor fromHex: '#585b70'
! !

!Theme class methodsFor: 'overlay colors'!
overlay0
    ^TuiColor fromHex: '#6c7086'
!

overlay1
    ^TuiColor fromHex: '#7f849c'
!

overlay2
    ^TuiColor fromHex: '#9399b2'
! !

!Theme class methodsFor: 'text colors'!
text
    ^TuiColor fromHex: '#cdd6f4'
!

subtext1
    ^TuiColor fromHex: '#bac2de'
!

subtext0
    ^TuiColor fromHex: '#a6adc8'
! !

!Theme class methodsFor: 'accent colors'!
lavender
    ^TuiColor fromHex: '#b4befe'
!

blue
    ^TuiColor fromHex: '#89b4fa'
!

sapphire
    ^TuiColor fromHex: '#74c7ec'
!

sky
    ^TuiColor fromHex: '#89dceb'
!

teal
    ^TuiColor fromHex: '#94e2d5'
!

green
    ^TuiColor fromHex: '#a6e3a1'
!

yellow
    ^TuiColor fromHex: '#f9e2af'
!

peach
    ^TuiColor fromHex: '#fab387'
!

maroon
    ^TuiColor fromHex: '#eba0ac'
!

red
    ^TuiColor fromHex: '#f38ba8'
!

mauve
    ^TuiColor fromHex: '#cba6f7'
!

pink
    ^TuiColor fromHex: '#f5c2e7'
!

flamingo
    ^TuiColor fromHex: '#f2cdcd'
!

rosewater
    ^TuiColor fromHex: '#f5e0dc'
! !

!Theme class methodsFor: 'semantic ui colors'!
background
    ^self base
!

backgroundAlt
    ^self mantle
!

foreground
    ^self text
!

foregroundDim
    ^self subtext0
!

tabActive
    ^self blue
!

tabInactive
    ^self surface2
!

statusBar
    ^self surface0
!

statusText
    ^self subtext1
!

selection
    ^self surface1
!

cursor
    ^self rosewater
!

border
    ^self surface2
!

borderFocused
    ^self blue
!

errorText
    ^self red
!

warningText
    ^self yellow
!

successText
    ^self green
!

infoText
    ^self blue
!

scrollbar
    ^self surface2
!

scrollbarThumb
    ^self overlay0
! !

!Theme class methodsFor: 'syntax highlighting'!
syntaxKeyword
    ^self mauve
!

syntaxString
    ^self green
!

syntaxSymbol
    ^self yellow
!

syntaxNumber
    ^self peach
!

syntaxComment
    ^self overlay0
!

syntaxSelf
    ^self red
!

syntaxClassName
    ^self blue
!

syntaxMethodName
    ^self sapphire
!

syntaxVariable
    ^self text
!

syntaxOperator
    ^self sky
!

syntaxBlock
    ^self pink
!

syntaxLiteral
    ^self flamingo
! !


"Style class for combining colors and attributes"
Object subclass: #TuiStyle
    instanceVariableNames: 'foreground background bold dim italic underline reverse'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!TuiStyle class methodsFor: 'instance creation'!
new
    ^super new initialize
!

foreground: fg background: bg
    ^self new foreground: fg; background: bg
!

default
    ^self foreground: Theme foreground background: Theme background
! !

!TuiStyle class methodsFor: 'predefined styles'!
normal
    ^self foreground: Theme foreground background: Theme background
!

dim
    ^self foreground: Theme foregroundDim background: Theme background
!

selected
    ^self foreground: Theme foreground background: Theme selection
!

tabActive
    ^(self foreground: Theme base background: Theme tabActive) bold: true
!

tabInactive
    ^self foreground: Theme foregroundDim background: Theme tabInactive
!

status
    ^self foreground: Theme statusText background: Theme statusBar
!

statusKey
    ^(self foreground: Theme yellow background: Theme statusBar) bold: true
!

error
    ^self foreground: Theme errorText background: Theme background
!

border
    ^self foreground: Theme border background: Theme background
!

borderFocused
    ^self foreground: Theme borderFocused background: Theme background
!

title
    ^(self foreground: Theme blue background: Theme background) bold: true
! !

!TuiStyle methodsFor: 'initialization'!
initialize
    foreground := nil.
    background := nil.
    bold := false.
    dim := false.
    italic := false.
    underline := false.
    reverse := false
! !

!TuiStyle methodsFor: 'accessing'!
foreground
    ^foreground
!

foreground: aColor
    foreground := aColor
!

background
    ^background
!

background: aColor
    background := aColor
!

bold
    ^bold
!

bold: aBoolean
    bold := aBoolean
!

dim
    ^dim
!

dim: aBoolean
    dim := aBoolean
!

italic
    ^italic
!

italic: aBoolean
    italic := aBoolean
!

underline
    ^underline
!

underline: aBoolean
    underline := aBoolean
!

reverse
    ^reverse
!

reverse: aBoolean
    reverse := aBoolean
! !

!TuiStyle methodsFor: 'fluent interface'!
withFg: aColor
    ^self copy foreground: aColor
!

withBg: aColor
    ^self copy background: aColor
!

withBold
    ^self copy bold: true
!

withDim
    ^self copy dim: true
!

withItalic
    ^self copy italic: true
!

withUnderline
    ^self copy underline: true
!

withReverse
    ^self copy reverse: true
! !

!TuiStyle methodsFor: 'applying'!
apply
    "Apply this style to the terminal"
    | term |
    term := AnsiTerminal current.
    term resetStyle.
    bold ifTrue: [term setBold: true].
    dim ifTrue: [term setDim].
    italic ifTrue: [term setItalic: true].
    underline ifTrue: [term setUnderline: true].
    reverse ifTrue: [term setReverse].
    foreground ifNotNil: [foreground applyAsForeground].
    background ifNotNil: [background applyAsBackground]
! !

!TuiStyle methodsFor: 'comparing'!
= other
    (other isKindOf: TuiStyle) ifFalse: [^false].
    ^foreground = other foreground and: [
        background = other background and: [
            bold = other bold and: [
                dim = other dim and: [
                    italic = other italic and: [
                        underline = other underline and: [
                            reverse = other reverse]]]]]]
!

hash
    ^foreground hash bitXor: background hash
! !

!TuiStyle methodsFor: 'copying'!
copy
    | s |
    s := self class new.
    s foreground: foreground.
    s background: background.
    s bold: bold.
    s dim: dim.
    s italic: italic.
    s underline: underline.
    s reverse: reverse.
    ^s
! !


"Box drawing characters"
Object subclass: #BoxChars
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Core'!

!BoxChars class methodsFor: 'single line'!
horizontal ^'-'!
vertical ^'|'!
topLeft ^'+'!
topRight ^'+'!
bottomLeft ^'+'!
bottomRight ^'+'!
teeDown ^'+'!
teeUp ^'+'!
teeRight ^'+'!
teeLeft ^'+'!
cross ^'+'!
! !

!BoxChars class methodsFor: 'unicode single line'!
horizontalU ^(String with: (Character value: 16r2500))!
verticalU ^(String with: (Character value: 16r2502))!
topLeftU ^(String with: (Character value: 16r250C))!
topRightU ^(String with: (Character value: 16r2510))!
bottomLeftU ^(String with: (Character value: 16r2514))!
bottomRightU ^(String with: (Character value: 16r2518))!
! !

!BoxChars class methodsFor: 'unicode rounded'!
roundTopLeft ^(String with: (Character value: 16r256D))!
roundTopRight ^(String with: (Character value: 16r256E))!
roundBottomLeft ^(String with: (Character value: 16r2570))!
roundBottomRight ^(String with: (Character value: 16r256F))!
! !

!BoxChars class methodsFor: 'block elements'!
blockFull ^(String with: (Character value: 16r2588))!
blockLight ^(String with: (Character value: 16r2591))!
blockMedium ^(String with: (Character value: 16r2592))!
blockDark ^(String with: (Character value: 16r2593))!
! !

!BoxChars class methodsFor: 'tree view'!
treeBranch ^(String with: (Character value: 16r251C))!
treeLast ^(String with: (Character value: 16r2514))!
treeVertical ^(String with: (Character value: 16r2502))!
treeHorizontal ^(String with: (Character value: 16r2500))!
treeCollapsed ^(String with: (Character value: 16r25B6))!
treeExpanded ^(String with: (Character value: 16r25BC))!
! !

!BoxChars class methodsFor: 'icons'!
checkMark ^(String with: (Character value: 16r2713))!
crossMark ^(String with: (Character value: 16r2717))!
arrowRight ^(String with: (Character value: 16r2192))!
arrowLeft ^(String with: (Character value: 16r2190))!
arrowUp ^(String with: (Character value: 16r2191))!
arrowDown ^(String with: (Character value: 16r2193))!
bullet ^(String with: (Character value: 16r2022))!
! !
