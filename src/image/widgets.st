"Widget classes for Smalltalk TUI"

"Add methods to OrderedCollection"
!OrderedCollection class methodsFor: 'instance creation'!
new
    "Create a new empty OrderedCollection"
    ^self new: 10
!

new: capacity
    "Create a new OrderedCollection with given capacity"
    | inst |
    inst := super new.
    inst initializeWithCapacity: capacity.
    ^inst
! !

!OrderedCollection methodsFor: 'initialization'!
initializeWithCapacity: capacity
    "Initialize with an array of given capacity"
    self instVarAt: 1 put: (Array new: capacity).  "array"
    self instVarAt: 2 put: 1.                       "firstIndex"
    self instVarAt: 3 put: 1.                       "lastIndex (empty = firstIndex)"
! !

!OrderedCollection methodsFor: 'enumerating'!
do: aBlock
    "Evaluate aBlock for each element in the collection"
    | array firstIndex lastIndex |
    array := self instVarAt: 1.
    firstIndex := self instVarAt: 2.
    lastIndex := self instVarAt: 3.
    (array isNil or: [firstIndex isNil or: [lastIndex isNil]]) ifTrue: [^self].
    firstIndex to: lastIndex - 1 do: [:i |
        aBlock value: (array at: i)
    ]
! !

!OrderedCollection methodsFor: 'adding'!
add: anObject
    "Add anObject to the end of the collection"
    | array lastIndex |
    array := self instVarAt: 1.
    lastIndex := self instVarAt: 3.
    array isNil ifTrue: [
        self initializeWithCapacity: 10.
        array := self instVarAt: 1.
        lastIndex := self instVarAt: 3.
    ].
    "Grow array if needed"
    lastIndex > array size ifTrue: [
        | newArray |
        newArray := Array new: array size * 2.
        1 to: array size do: [:i | newArray at: i put: (array at: i)].
        array := newArray.
        self instVarAt: 1 put: array.
    ].
    array at: lastIndex put: anObject.
    self instVarAt: 3 put: lastIndex + 1.
    ^anObject
! !

!OrderedCollection methodsFor: 'accessing'!
size
    "Return the number of elements"
    | firstIndex lastIndex |
    firstIndex := self instVarAt: 2.
    lastIndex := self instVarAt: 3.
    firstIndex isNil ifTrue: [^0].
    lastIndex isNil ifTrue: [^0].
    ^lastIndex - firstIndex
!

isEmpty
    "Return true if the collection has no elements"
    ^self size = 0
!

at: index
    "Return element at index (1-based)"
    | array firstIndex |
    array := self instVarAt: 1.
    firstIndex := self instVarAt: 2.
    ^array at: firstIndex + index - 1
!

at: index put: anObject
    "Set element at index (1-based)"
    | array firstIndex |
    array := self instVarAt: 1.
    firstIndex := self instVarAt: 2.
    array at: firstIndex + index - 1 put: anObject.
    ^anObject
! !

"Add String class >> with: method"
!String class methodsFor: 'instance creation'!
with: aCharacter
    "Create a string containing a single character"
    | str |
    str := String new: 1.
    str at: 1 put: aCharacter.
    ^str
! !

"Add Array >> isEmpty"
!Array methodsFor: 'testing'!
isEmpty
    "Return true if the array has no elements"
    ^self size = 0
! !

Object subclass: #Widget
    instanceVariableNames: 'bounds parent children focused visible enabled'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!Widget class methodsFor: 'instance creation'!
new
    ^super new initialize
!

bounds: aRect
    ^self new bounds: aRect
! !

!Widget methodsFor: 'initialization'!
initialize
    bounds := Rectangle origin: 1@1 extent: 10@5.
    children := OrderedCollection new.
    focused := false.
    visible := true.
    enabled := true.
    parent := nil
! !

!Widget methodsFor: 'accessing'!
bounds
    ^bounds
!

bounds: aRect
    bounds := aRect
!

x
    ^bounds origin x
!

y
    ^bounds origin y
!

width
    ^bounds width
!

height
    ^bounds height
!

parent
    ^parent
!

parent: aWidget
    parent := aWidget
!

children
    ^children
! !

!Widget methodsFor: 'hierarchy'!
addChild: aWidget
    aWidget parent: self.
    children add: aWidget
!

removeChild: aWidget
    children remove: aWidget ifAbsent: [].
    aWidget parent: nil
! !

!Widget methodsFor: 'focus'!
focused
    ^focused
!

focused: aBoolean
    focused := aBoolean
!

focus
    focused := true.
    self onFocus
!

blur
    focused := false.
    self onBlur
!

onFocus
    "Subclasses can override"
!

onBlur
    "Subclasses can override"
! !

!Widget methodsFor: 'visibility'!
visible
    ^visible
!

visible: aBoolean
    visible := aBoolean
!

show
    visible := true.
    self needsRedraw
!

hide
    visible := false.
    self needsRedraw
!

enabled
    ^enabled
!

enabled: aBoolean
    enabled := aBoolean
! !

!Widget methodsFor: 'drawing'!
draw
    "Subclasses should override to draw themselves"
    visible ifFalse: [^self].
    self drawSelf.
    self drawChildren
!

drawSelf
    "Draw this widget - subclasses override"
!

drawChildren
    "Draw all children"
    children do: [:child | child draw]
!

needsRedraw
    "Mark this widget as needing redraw"
    "In a full implementation, this would add to a dirty list"
! !

!Widget methodsFor: 'events'!
handleKey: keyCode
    "Handle a key press. Return true if handled."
    ^false
!

handleMouse: event
    "Handle mouse event. Return true if handled."
    ^false
!

containsPoint: aPoint
    ^bounds containsPoint: aPoint
! !

!Widget methodsFor: 'geometry'!
moveTo: aPoint
    bounds := Rectangle origin: aPoint extent: bounds extent
!

resize: aPoint
    bounds := Rectangle origin: bounds origin extent: aPoint
!

innerBounds
    "Return bounds for content (inside border)"
    ^Rectangle origin: (bounds origin + (1@1)) extent: (bounds extent - (2@2))
! !


"Frame widget - draws a border"
Widget subclass: #Frame
    instanceVariableNames: 'title borderStyle'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!Frame class methodsFor: 'instance creation'!
title: aString
    ^self new title: aString
! !

!Frame methodsFor: 'initialization'!
initialize
    super initialize.
    title := ''.
    borderStyle := #single
! !

!Frame methodsFor: 'accessing'!
title
    ^title
!

title: aString
    title := aString
! !

!Frame methodsFor: 'drawing'!
drawSelf
    | term x1 y1 w h |
    term := Terminal current.
    x1 := self x.
    y1 := self y.
    w := self width.
    h := self height.

    "Set border color"
    focused
        ifTrue: [term fg: Colors highlight]
        ifFalse: [term fg: Colors border].
    term bg: Colors background.

    "Draw box using primitive"
    term drawBoxAt: {x1. y1} extent: {w. h}.

    "Draw title if present"
    title isEmpty ifFalse: [
        term fg: Colors title.
        term setCursorRow: y1 col: x1 + 2.
        term write: ' ', title, ' '
    ].

    term resetStyle
! !


"Label widget - displays text"
Widget subclass: #Label
    instanceVariableNames: 'text alignment'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!Label class methodsFor: 'instance creation'!
text: aString
    ^self new text: aString
! !

!Label methodsFor: 'initialization'!
initialize
    super initialize.
    text := ''.
    alignment := #left
! !

!Label methodsFor: 'accessing'!
text
    ^text
!

text: aString
    text := aString.
    self needsRedraw
!

alignment
    ^alignment
!

alignment: aSymbol
    "aSymbol is #left, #center, or #right"
    alignment := aSymbol
! !

!Label methodsFor: 'drawing'!
drawSelf
    | term x str |
    term := Terminal current.
    term fg: Colors foreground.
    term bg: Colors background.

    str := text copyFrom: 1 to: (text size min: self width).

    alignment = #center ifTrue: [
        x := self x + ((self width - str size) // 2)
    ] ifFalse: [
        alignment = #right ifTrue: [
            x := self x + self width - str size
        ] ifFalse: [
            x := self x
        ]
    ].

    term setCursorRow: self y col: x.
    term write: str.
    term resetStyle
! !


"Button widget"
Widget subclass: #Button
    instanceVariableNames: 'text action'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!Button class methodsFor: 'instance creation'!
text: aString action: aBlock
    ^self new text: aString; action: aBlock
! !

!Button methodsFor: 'initialization'!
initialize
    super initialize.
    text := 'Button'.
    action := []
! !

!Button methodsFor: 'accessing'!
text
    ^text
!

text: aString
    text := aString
!

action
    ^action
!

action: aBlock
    action := aBlock
! !

!Button methodsFor: 'drawing'!
drawSelf
    | term label |
    term := Terminal current.

    focused
        ifTrue: [
            term fg: Colors background.
            term bg: Colors highlight]
        ifFalse: [
            term fg: Colors foreground.
            term bg: Colors border].

    label := '[ ', text, ' ]'.
    term setCursorRow: self y col: self x.
    term write: label.
    term resetStyle
! !

!Button methodsFor: 'events'!
handleKey: keyCode
    (keyCode = Terminal keyEnter or: [keyCode = 32]) ifTrue: [
        action value.
        ^true
    ].
    ^false
!

press
    action value
! !


"ListView widget - scrollable list of items"
Widget subclass: #ListView
    instanceVariableNames: 'items selectedIndex scrollOffset onSelect drawY drawIndex'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!ListView class methodsFor: 'instance creation'!
items: anArray
    ^self new items: anArray
! !

!ListView methodsFor: 'initialization'!
initialize
    super initialize.
    items := #().
    selectedIndex := 1.
    scrollOffset := 0.
    onSelect := [:item | ]
! !

!ListView methodsFor: 'accessing'!
items
    ^items
!

items: anArray
    items := anArray.
    selectedIndex := 1.
    scrollOffset := 0.
    self needsRedraw
!

selectedIndex
    ^selectedIndex
!

selectedIndex: anInteger
    selectedIndex := anInteger.
    self ensureVisible.
    self needsRedraw
!

selectedItem
    (selectedIndex > 0 and: [selectedIndex <= items size])
        ifTrue: [^items at: selectedIndex]
        ifFalse: [^nil]
!

onSelect: aBlock
    onSelect := aBlock
! !

!ListView methodsFor: 'scrolling'!
visibleItemCount
    ^self height - 2  "Account for border"
!

ensureVisible
    | visible |
    visible := self visibleItemCount.
    (selectedIndex <= scrollOffset) ifTrue: [
        scrollOffset := selectedIndex - 1
    ].
    (selectedIndex > (scrollOffset + visible)) ifTrue: [
        scrollOffset := selectedIndex - visible
    ].
    scrollOffset := scrollOffset max: 0
! !

!ListView methodsFor: 'drawing'!
drawSelf
    | term y itemY visible |
    term := Terminal current.

    "Draw border"
    focused
        ifTrue: [term fg: Colors highlight]
        ifFalse: [term fg: Colors border].
    term bg: Colors background.
    term drawBoxAt: {self x. self y} extent: {self width. self height}.

    "Draw items"
    visible := self visibleItemCount.
    y := 1.
    (scrollOffset + 1) to: ((scrollOffset + visible) min: items size) do: [:i |
        | item str isSelected |
        item := items at: i.
        isSelected := (i = selectedIndex).
        itemY := self y + y.

        "Set colors - use Terminal current directly due to VM block temp bug"
        isSelected ifTrue: [
            focused
                ifTrue: [Terminal current fg: Colors background. Terminal current bg: Colors selection]
                ifFalse: [Terminal current fg: Colors foreground. Terminal current bg: Colors highlight]
        ] ifFalse: [
            Terminal current fg: Colors foreground.
            Terminal current bg: Colors background
        ].

        "Draw item text"
        str := item asString.
        str := str copyFrom: 1 to: (str size min: self width - 2).
        Terminal current setCursorRow: itemY col: self x + 1.
        Terminal current write: str.

        "Clear rest of line"
        (str size) to: (self width - 3) do: [:j |
            Terminal current write: ' '
        ].

        y := y + 1
    ].

    term resetStyle
! !

!ListView methodsFor: 'events'!
handleKey: keyCode
    keyCode = Terminal keyUp ifTrue: [
        selectedIndex > 1 ifTrue: [
            selectedIndex := selectedIndex - 1.
            self ensureVisible.
            self needsRedraw
        ].
        ^true
    ].

    keyCode = Terminal keyDown ifTrue: [
        selectedIndex < items size ifTrue: [
            selectedIndex := selectedIndex + 1.
            self ensureVisible.
            self needsRedraw
        ].
        ^true
    ].

    keyCode = Terminal keyEnter ifTrue: [
        self selectedItem ifNotNil: [:item |
            onSelect value: item
        ].
        ^true
    ].

    keyCode = Terminal keyPageUp ifTrue: [
        selectedIndex := (selectedIndex - self visibleItemCount) max: 1.
        self ensureVisible.
        self needsRedraw.
        ^true
    ].

    keyCode = Terminal keyPageDown ifTrue: [
        selectedIndex := (selectedIndex + self visibleItemCount) min: items size.
        self ensureVisible.
        self needsRedraw.
        ^true
    ].

    keyCode = Terminal keyHome ifTrue: [
        selectedIndex := 1.
        scrollOffset := 0.
        self needsRedraw.
        ^true
    ].

    keyCode = Terminal keyEnd ifTrue: [
        selectedIndex := items size.
        self ensureVisible.
        self needsRedraw.
        ^true
    ].

    ^false
! !


"TextView widget - multi-line text display"
Widget subclass: #TextView
    instanceVariableNames: 'lines scrollOffset lineOffset'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TextView methodsFor: 'initialization'!
initialize
    super initialize.
    lines := #().
    scrollOffset := 0.
    lineOffset := 0
! !

!TextView methodsFor: 'accessing'!
text
    ^String cr join: lines
!

text: aString
    lines := aString lines.
    scrollOffset := 0.
    lineOffset := 0.
    self needsRedraw
!

lines
    ^lines
!

lines: anArray
    lines := anArray.
    self needsRedraw
! !

!TextView methodsFor: 'scrolling'!
visibleLineCount
    ^self height - 2
!

scrollUp
    scrollOffset > 0 ifTrue: [
        scrollOffset := scrollOffset - 1.
        self needsRedraw
    ]
!

scrollDown
    scrollOffset < (lines size - self visibleLineCount) ifTrue: [
        scrollOffset := scrollOffset + 1.
        self needsRedraw
    ]
!

scrollLeft
    lineOffset > 0 ifTrue: [
        lineOffset := lineOffset - 1.
        self needsRedraw
    ]
!

scrollRight
    lineOffset := lineOffset + 1.
    self needsRedraw
! !

!TextView methodsFor: 'drawing'!
drawSelf
    | term y maxY contentWidth |
    term := Terminal current.

    "Draw border"
    focused
        ifTrue: [term fg: Colors highlight]
        ifFalse: [term fg: Colors border].
    term bg: Colors background.
    term drawBoxAt: {self x. self y} extent: {self width. self height}.

    "Draw content"
    term fg: Colors foreground.
    contentWidth := self width - 2.
    y := 1.
    maxY := self visibleLineCount.

    (scrollOffset + 1) to: ((scrollOffset + maxY) min: lines size) do: [:i |
        | line visiblePart |
        line := lines at: i.

        "Handle horizontal scroll"
        lineOffset < line size ifTrue: [
            visiblePart := line copyFrom: lineOffset + 1 to: (line size min: lineOffset + contentWidth)
        ] ifFalse: [
            visiblePart := ''
        ].

        "Use Terminal current directly due to VM block temp bug"
        Terminal current setCursorRow: self y + y col: self x + 1.
        Terminal current write: visiblePart.

        "Clear rest of line"
        visiblePart size to: contentWidth - 1 do: [:j | Terminal current write: ' '].

        y := y + 1
    ].

    term resetStyle
! !

!TextView methodsFor: 'events'!
handleKey: keyCode
    keyCode = Terminal keyUp ifTrue: [self scrollUp. ^true].
    keyCode = Terminal keyDown ifTrue: [self scrollDown. ^true].
    keyCode = Terminal keyLeft ifTrue: [self scrollLeft. ^true].
    keyCode = Terminal keyRight ifTrue: [self scrollRight. ^true].
    keyCode = Terminal keyPageUp ifTrue: [
        scrollOffset := (scrollOffset - self visibleLineCount) max: 0.
        self needsRedraw.
        ^true
    ].
    keyCode = Terminal keyPageDown ifTrue: [
        scrollOffset := (scrollOffset + self visibleLineCount) min: (lines size - self visibleLineCount max: 0).
        self needsRedraw.
        ^true
    ].
    ^false
! !


"TextInput widget - single line text input"
Widget subclass: #TextInput
    instanceVariableNames: 'text cursorPos onChange'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TextInput methodsFor: 'initialization'!
initialize
    super initialize.
    text := ''.
    cursorPos := 1.
    onChange := [:newText | ]
! !

!TextInput methodsFor: 'accessing'!
text
    ^text
!

text: aString
    text := aString.
    cursorPos := text size + 1.
    self needsRedraw
!

cursorPos
    ^cursorPos
!

onChange: aBlock
    onChange := aBlock
! !

!TextInput methodsFor: 'editing'!
insertChar: aChar
    text := (text copyFrom: 1 to: cursorPos - 1),
            (String with: aChar),
            (text copyFrom: cursorPos to: text size).
    cursorPos := cursorPos + 1.
    onChange value: text.
    self needsRedraw
!

deleteBackward
    cursorPos > 1 ifTrue: [
        text := (text copyFrom: 1 to: cursorPos - 2),
                (text copyFrom: cursorPos to: text size).
        cursorPos := cursorPos - 1.
        onChange value: text.
        self needsRedraw
    ]
!

deleteForward
    cursorPos <= text size ifTrue: [
        text := (text copyFrom: 1 to: cursorPos - 1),
                (text copyFrom: cursorPos + 1 to: text size).
        onChange value: text.
        self needsRedraw
    ]
! !

!TextInput methodsFor: 'drawing'!
drawSelf
    | term displayText |
    term := Terminal current.

    "Background"
    focused
        ifTrue: [term bg: Colors selection]
        ifFalse: [term bg: Colors highlight].
    term fg: Colors foreground.

    "Draw text"
    displayText := text copyFrom: 1 to: (text size min: self width).
    term setCursorRow: self y col: self x.
    term write: displayText.

    "Fill remaining space"
    displayText size to: self width - 1 do: [:i | term write: ' '].

    "Show cursor position if focused"
    focused ifTrue: [
        term setCursorRow: self y col: self x + cursorPos - 1.
        term showCursor
    ].

    term resetStyle
! !

!TextInput methodsFor: 'events'!
handleKey: keyCode
    "Handle printable characters"
    (keyCode >= 32 and: [keyCode < 127]) ifTrue: [
        self insertChar: (Character value: keyCode).
        ^true
    ].

    keyCode = Terminal keyBackspace ifTrue: [
        self deleteBackward.
        ^true
    ].

    keyCode = Terminal keyDelete ifTrue: [
        self deleteForward.
        ^true
    ].

    keyCode = Terminal keyLeft ifTrue: [
        cursorPos > 1 ifTrue: [
            cursorPos := cursorPos - 1.
            self needsRedraw
        ].
        ^true
    ].

    keyCode = Terminal keyRight ifTrue: [
        cursorPos <= text size ifTrue: [
            cursorPos := cursorPos + 1.
            self needsRedraw
        ].
        ^true
    ].

    keyCode = Terminal keyHome ifTrue: [
        cursorPos := 1.
        self needsRedraw.
        ^true
    ].

    keyCode = Terminal keyEnd ifTrue: [
        cursorPos := text size + 1.
        self needsRedraw.
        ^true
    ].

    ^false
! !


"Container widget - manages layout of children"
Widget subclass: #Container
    instanceVariableNames: 'layout focusedChild'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!Container methodsFor: 'initialization'!
initialize
    super initialize.
    layout := #vertical.
    focusedChild := nil
! !

!Container methodsFor: 'accessing'!
layout
    ^layout
!

layout: aSymbol
    "#vertical or #horizontal"
    layout := aSymbol
!

focusedChild
    ^focusedChild
! !

!Container methodsFor: 'focus'!
focusFirst
    children isEmpty ifFalse: [
        focusedChild := children at: 1.
        focusedChild focus
    ]
!

focusNext
    | idx |
    focusedChild isNil ifTrue: [
        self focusFirst.
        ^self
    ].
    idx := children indexOf: focusedChild.
    focusedChild blur.
    idx < children size
        ifTrue: [focusedChild := children at: idx + 1]
        ifFalse: [focusedChild := children at: 1].
    focusedChild focus
!

focusPrev
    | idx |
    focusedChild isNil ifTrue: [
        self focusFirst.
        ^self
    ].
    idx := children indexOf: focusedChild.
    focusedChild blur.
    idx > 1
        ifTrue: [focusedChild := children at: idx - 1]
        ifFalse: [focusedChild := children at: children size].
    focusedChild focus
! !

!Container methodsFor: 'events'!
handleKey: keyCode
    "First let focused child handle"
    focusedChild ifNotNil: [
        (focusedChild handleKey: keyCode) ifTrue: [^true]
    ].

    "Tab cycles focus"
    keyCode = Terminal keyTab ifTrue: [
        self focusNext.
        self needsRedraw.
        ^true
    ].

    ^false
! !

!Container methodsFor: 'layout'!
layoutChildren
    "Simple layout - override for more complex layouts"
    | x y |
    x := self x + 1.
    y := self y + 1.

    children do: [:child |
        child moveTo: (x @ y).
        layout = #vertical
            ifTrue: [y := y + child height]
            ifFalse: [x := x + child width]
    ]
! !
