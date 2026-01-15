"UIWidgets - TabBar, StatusBar, ListView and other UI components"

"============================================================"
"TuiTabBar - Tab switching widget"
"============================================================"
TuiWidget subclass: #TuiTabBar
    instanceVariableNames: 'tabs activeTab onTabChange'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiTabBar class methodsFor: 'instance creation'!
new
    ^super new initialize
!

tabs: tabArray
    ^self new tabs: tabArray
! !

!TuiTabBar methodsFor: 'initialization'!
initialize
    super initialize.
    tabs := #().
    activeTab := 1.
    onTabChange := nil.
    hasBorder := false.
    rect := TuiRect x: 1 y: 1 width: 80 height: 1
! !

!TuiTabBar methodsFor: 'accessing'!
tabs
    ^tabs
!

tabs: anArray
    "Array of tab info arrays: #(#('Title' $shortcut) ...)"
    tabs := anArray.
    activeTab := 1
!

activeTab
    ^activeTab
!

activeTab: index
    (index > 0 and: [index <= tabs size]) ifTrue: [
        activeTab ~= index ifTrue: [
            activeTab := index.
            onTabChange ifNotNil: [onTabChange value: index]
        ]
    ]
!

onTabChange: aBlock
    onTabChange := aBlock
! !

!TuiTabBar methodsFor: 'navigation'!
nextTab
    | newTab |
    newTab := activeTab + 1.
    newTab > tabs size ifTrue: [newTab := 1].
    self activeTab: newTab
!

previousTab
    | newTab |
    newTab := activeTab - 1.
    newTab < 1 ifTrue: [newTab := tabs size].
    self activeTab: newTab
! !

!TuiTabBar methodsFor: 'key handling'!
handleKey: keyCode
    "F1-F4 switch tabs"
    keyCode = AnsiTerminal keyF1 ifTrue: [self activeTab: 1. ^true].
    keyCode = AnsiTerminal keyF2 ifTrue: [self activeTab: 2. ^true].
    keyCode = AnsiTerminal keyF3 ifTrue: [self activeTab: 3. ^true].
    keyCode = AnsiTerminal keyF4 ifTrue: [self activeTab: 4. ^true].

    "Ctrl+1 through Ctrl+9"
    (keyCode >= 1 and: [keyCode <= 9]) ifTrue: [
        keyCode <= tabs size ifTrue: [
            self activeTab: keyCode.
            ^true
        ]
    ].

    ^false
! !

!TuiTabBar methodsFor: 'drawing'!
drawSelf: screen
    | x row |
    row := rect y.
    x := rect x.

    "Draw background"
    screen fillRect: rect char: $  style: TuiStyle normal.

    "Draw each tab"
    1 to: tabs size do: [:i |
        | tab tabTitle shortcut style tabWidth |
        tab := tabs at: i.
        tabTitle := tab at: 1.
        shortcut := tab at: 2.

        style := i = activeTab
            ifTrue: [TuiStyle tabActive]
            ifFalse: [TuiStyle tabInactive].

        tabWidth := tabTitle size + 4.

        "Draw tab background"
        screen fillRect: (TuiRect x: x y: row width: tabWidth height: 1)
               char: $  style: style.

        "Draw tab text"
        screen drawText: tabTitle at: x + 2 y: row style: style.

        x := x + tabWidth + 1.
        x > rect width ifTrue: [^self]
    ]
! !


"============================================================"
"TuiStatusBar - Bottom status bar"
"============================================================"
TuiWidget subclass: #TuiStatusBar
    instanceVariableNames: 'message items'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiStatusBar class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiStatusBar methodsFor: 'initialization'!
initialize
    super initialize.
    message := 'Ready'.
    items := #().
    hasBorder := false.
    rect := TuiRect x: 1 y: 24 width: 80 height: 1
! !

!TuiStatusBar methodsFor: 'accessing'!
message
    ^message
!

message: aString
    message := aString
!

items
    ^items
!

items: anArray
    "Array of status items: #(#('Key' 'Description') ...)"
    items := anArray
! !

!TuiStatusBar methodsFor: 'drawing'!
drawSelf: screen
    | x itemsText |
    "Draw background"
    screen fillRect: rect char: $  style: TuiStyle status.

    "Draw message on left"
    screen drawText: message at: rect x + 1 y: rect y style: TuiStyle status.

    "Draw items on right"
    items isEmpty ifFalse: [
        x := rect x + rect width - 1.
        items reverseDo: [:item |
            | key desc |
            key := item at: 1.
            desc := item at: 2.

            "Draw description"
            x := x - desc size.
            screen drawText: desc at: x y: rect y style: TuiStyle status.

            "Draw key"
            x := x - key size - 1.
            screen drawText: key at: x y: rect y style: TuiStyle statusKey.

            "Separator"
            x := x - 2.
            x > rect x ifTrue: [
                screen drawText: '|' at: x + 1 y: rect y style: TuiStyle status
            ]
        ]
    ]
! !


"============================================================"
"TuiListView - Scrollable list of items"
"============================================================"
TuiWidget subclass: #TuiListView
    instanceVariableNames: 'items selectedIndex scrollTop onSelect'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiListView class methodsFor: 'instance creation'!
new
    ^super new initialize
!

items: anArray
    ^self new items: anArray
! !

!TuiListView methodsFor: 'initialization'!
initialize
    super initialize.
    items := OrderedCollection new.
    selectedIndex := 0.
    scrollTop := 0.
    onSelect := nil
! !

!TuiListView methodsFor: 'accessing'!
items
    ^items
!

items: aCollection
    items := aCollection asOrderedCollection.
    selectedIndex := items isEmpty ifTrue: [0] ifFalse: [1].
    scrollTop := 0
!

addItem: anItem
    items add: anItem.
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
    (selectedIndex > 0 and: [selectedIndex <= items size])
        ifTrue: [^items at: selectedIndex].
    ^nil
!

onSelect: aBlock
    onSelect := aBlock
! !

!TuiListView methodsFor: 'selection'!
selectIndex: index
    | newIndex |
    newIndex := (index max: 0) min: items size.
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
    | visibleCount |
    visibleCount := self visibleItemCount.
    selectedIndex <= scrollTop ifTrue: [
        scrollTop := (selectedIndex - 1) max: 0
    ].
    selectedIndex > (scrollTop + visibleCount) ifTrue: [
        scrollTop := selectedIndex - visibleCount
    ]
!

visibleItemCount
    ^self contentRect height
! !

!TuiListView methodsFor: 'key handling'!
handleKey: keyCode
    keyCode = AnsiTerminal keyUp ifTrue: [self selectPrevious. ^true].
    keyCode = AnsiTerminal keyDown ifTrue: [self selectNext. ^true].
    keyCode = AnsiTerminal keyHome ifTrue: [self selectFirst. ^true].
    keyCode = AnsiTerminal keyEnd ifTrue: [self selectLast. ^true].
    keyCode = AnsiTerminal keyPageUp ifTrue: [
        self selectIndex: selectedIndex - self visibleItemCount.
        ^true
    ].
    keyCode = AnsiTerminal keyPageDown ifTrue: [
        self selectIndex: selectedIndex + self visibleItemCount.
        ^true
    ].
    keyCode = AnsiTerminal keyEnter ifTrue: [
        self selectedItem ifNotNil: [:item |
            onSelect ifNotNil: [onSelect value: item]
        ].
        ^true
    ].
    ^false
! !

!TuiListView methodsFor: 'drawing'!
drawSelf: screen
    | content row startIndex endIndex |
    self drawBorder: screen.

    content := self contentRect.
    row := content y.
    startIndex := scrollTop + 1.
    endIndex := (scrollTop + self visibleItemCount) min: items size.

    startIndex to: endIndex do: [:i |
        | item style text |
        item := items at: i.
        style := i = selectedIndex
            ifTrue: [focused ifTrue: [TuiStyle selected] ifFalse: [TuiStyle dim withReverse]]
            ifFalse: [TuiStyle normal].

        text := item asString.
        text size > content width ifTrue: [
            text := text copyFrom: 1 to: content width
        ].

        "Draw item"
        screen fillRect: (TuiRect x: content x y: row width: content width height: 1)
               char: $  style: style.
        screen drawText: text at: content x y: row style: style.

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
"TuiInputLine - Single-line text input"
"============================================================"
TuiWidget subclass: #TuiInputLine
    instanceVariableNames: 'text cursorPos prompt onEnter onChange'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiInputLine class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiInputLine methodsFor: 'initialization'!
initialize
    super initialize.
    text := ''.
    cursorPos := 1.
    prompt := '> '.
    onEnter := nil.
    onChange := nil.
    hasBorder := false.
    rect := TuiRect x: 1 y: 1 width: 80 height: 1
! !

!TuiInputLine methodsFor: 'accessing'!
text
    ^text
!

text: aString
    text := aString.
    cursorPos := text size + 1
!

prompt
    ^prompt
!

prompt: aString
    prompt := aString
!

onEnter: aBlock
    onEnter := aBlock
!

onChange: aBlock
    onChange := aBlock
!

clear
    text := ''.
    cursorPos := 1
! !

!TuiInputLine methodsFor: 'editing'!
insertChar: ch
    | before after |
    before := text copyFrom: 1 to: cursorPos - 1.
    after := text copyFrom: cursorPos to: text size.
    text := before, (String with: ch), after.
    cursorPos := cursorPos + 1.
    onChange ifNotNil: [onChange value: text]
!

deleteBackward
    cursorPos > 1 ifTrue: [
        | before after |
        before := text copyFrom: 1 to: cursorPos - 2.
        after := text copyFrom: cursorPos to: text size.
        text := before, after.
        cursorPos := cursorPos - 1.
        onChange ifNotNil: [onChange value: text]
    ]
!

deleteForward
    cursorPos <= text size ifTrue: [
        | before after |
        before := text copyFrom: 1 to: cursorPos - 1.
        after := text copyFrom: cursorPos + 1 to: text size.
        text := before, after.
        onChange ifNotNil: [onChange value: text]
    ]
! !

!TuiInputLine methodsFor: 'key handling'!
handleKey: keyCode
    "Enter - submit"
    keyCode = AnsiTerminal keyEnter ifTrue: [
        onEnter ifNotNil: [onEnter value: text].
        ^true
    ].

    "Backspace"
    keyCode = AnsiTerminal keyBackspace ifTrue: [
        self deleteBackward.
        ^true
    ].

    "Delete"
    keyCode = AnsiTerminal keyDelete ifTrue: [
        self deleteForward.
        ^true
    ].

    "Left arrow"
    keyCode = AnsiTerminal keyLeft ifTrue: [
        cursorPos > 1 ifTrue: [cursorPos := cursorPos - 1].
        ^true
    ].

    "Right arrow"
    keyCode = AnsiTerminal keyRight ifTrue: [
        cursorPos <= text size ifTrue: [cursorPos := cursorPos + 1].
        ^true
    ].

    "Home"
    keyCode = AnsiTerminal keyHome ifTrue: [
        cursorPos := 1.
        ^true
    ].

    "End"
    keyCode = AnsiTerminal keyEnd ifTrue: [
        cursorPos := text size + 1.
        ^true
    ].

    "Ctrl+U - clear line"
    keyCode = AnsiTerminal keyCtrlU ifTrue: [
        self clear.
        ^true
    ].

    "Printable characters"
    (keyCode >= 32 and: [keyCode < 127]) ifTrue: [
        self insertChar: (Character value: keyCode).
        ^true
    ].

    ^false
! !

!TuiInputLine methodsFor: 'drawing'!
drawSelf: screen
    | displayText cursorX visibleWidth textStart |
    visibleWidth := rect width - prompt size.

    "Calculate visible portion of text"
    cursorPos > visibleWidth
        ifTrue: [textStart := cursorPos - visibleWidth + 1]
        ifFalse: [textStart := 1].

    displayText := text copyFrom: textStart to: (text size min: textStart + visibleWidth - 1).

    "Draw background"
    screen fillRect: rect char: $  style: TuiStyle normal.

    "Draw prompt"
    screen drawText: prompt at: rect x y: rect y style: TuiStyle dim.

    "Draw text"
    screen drawText: displayText at: rect x + prompt size y: rect y style: TuiStyle normal.

    "Position cursor"
    focused ifTrue: [
        cursorX := rect x + prompt size + cursorPos - textStart.
        screen setCursor: cursorX y: rect y.
        screen cursorVisible: true
    ]
! !
