"UIWidgets - TabBar, StatusBar, ListView, TreeView and other UI components"

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
    "F1-F5 switch tabs"
    keyCode = AnsiTerminal keyF1 ifTrue: [self activeTab: 1. ^true].
    keyCode = AnsiTerminal keyF2 ifTrue: [self activeTab: 2. ^true].
    keyCode = AnsiTerminal keyF3 ifTrue: [self activeTab: 3. ^true].
    keyCode = AnsiTerminal keyF4 ifTrue: [self activeTab: 4. ^true].
    keyCode = AnsiTerminal keyF5 ifTrue: [self activeTab: 5. ^true].

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
    | x row hChar |
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
    ].

    "Draw bottom border line (matches Zig TUI)"
    hChar := (BoxChars horizontalU) first.
    rect x to: (rect x + rect width - 1) do: [:col |
        screen setChar: hChar at: col y: row style: TuiStyle border
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
    | x maxMsgWidth clippedMsg |
    "Draw background"
    screen fillRect: rect char: $  style: TuiStyle status.

    "Draw message on left (clipped to 1/3 width, matches Zig TUI)"
    maxMsgWidth := rect width // 3.
    clippedMsg := message size <= maxMsgWidth
        ifTrue: [message]
        ifFalse: [message copyFrom: 1 to: maxMsgWidth].
    screen drawText: clippedMsg at: rect x + 1 y: rect y style: TuiStyle status.

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
    self drawBorderRounded: screen.

    content := self contentRect.
    self clearContent: screen.

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
    ].

    "Draw scrollbar if needed"
    items size > self visibleItemCount ifTrue: [
        screen drawScrollbar: content
               total: items size
               visible: self visibleItemCount
               offset: scrollTop
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


"============================================================"
"TuiTreeNode - Node for hierarchical tree data"
"============================================================"
Object subclass: #TuiTreeNode
    instanceVariableNames: 'text data children parent expanded level'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiTreeNode class methodsFor: 'instance creation'!
new
    ^super new initialize
!

text: aString
    ^self new text: aString
!

text: aString data: anObject
    ^self new text: aString; data: anObject
! !

!TuiTreeNode methodsFor: 'initialization'!
initialize
    text := ''.
    data := nil.
    children := OrderedCollection new.
    parent := nil.
    expanded := false.
    level := 0
! !

!TuiTreeNode methodsFor: 'accessing'!
text
    ^text
!

text: aString
    text := aString
!

data
    ^data
!

data: anObject
    data := anObject
!

children
    ^children
!

parent
    ^parent
!

parent: aNode
    parent := aNode
!

expanded
    ^expanded
!

expanded: aBoolean
    expanded := aBoolean
!

level
    ^level
!

level: anInteger
    level := anInteger
! !

!TuiTreeNode methodsFor: 'tree operations'!
addChild: aNode
    aNode parent: self.
    aNode level: level + 1.
    children add: aNode
!

removeChild: aNode
    children remove: aNode ifAbsent: [].
    aNode parent: nil
!

hasChildren
    ^children notEmpty
!

toggle
    self hasChildren ifTrue: [
        expanded := expanded not
    ]
!

expand
    expanded := true
!

collapse
    expanded := false
! !


"============================================================"
"TuiTreeView - Hierarchical tree widget (matches Zig TUI)"
"============================================================"
TuiWidget subclass: #TuiTreeView
    instanceVariableNames: 'roots flatList selectedIndex scrollOffset onSelect onActivate'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiTreeView class methodsFor: 'instance creation'!
new
    ^super new initialize
! !

!TuiTreeView methodsFor: 'initialization'!
initialize
    super initialize.
    roots := OrderedCollection new.
    flatList := OrderedCollection new.
    selectedIndex := nil.
    scrollOffset := 0.
    onSelect := nil.
    onActivate := nil
! !

!TuiTreeView methodsFor: 'accessing'!
roots
    ^roots
!

flatList
    ^flatList
!

selectedIndex
    ^selectedIndex
!

scrollOffset
    ^scrollOffset
!

scrollOffset: anInteger
    scrollOffset := anInteger
!

onSelect: aBlock
    onSelect := aBlock
!

onActivate: aBlock
    onActivate := aBlock
!

getSelectedNode
    selectedIndex ifNil: [^nil].
    (selectedIndex > 0 and: [selectedIndex <= flatList size])
        ifTrue: [^flatList at: selectedIndex].
    ^nil
! !

!TuiTreeView methodsFor: 'tree operations'!
clear
    roots do: [:r | "Could deinit nodes here"].
    roots := OrderedCollection new.
    flatList := OrderedCollection new.
    selectedIndex := nil.
    scrollOffset := 0
!

addRoot: aNode
    aNode level: 0.
    roots add: aNode.
    self rebuildFlatList
!

rebuildFlatList
    flatList := OrderedCollection new.
    roots do: [:root |
        self addToFlatList: root
    ]
!

addToFlatList: aNode
    flatList add: aNode.
    aNode expanded ifTrue: [
        aNode children do: [:child |
            self addToFlatList: child
        ]
    ]
!

findNode: aName
    "Find a node by its text"
    ^self findNode: aName in: roots
!

findNode: aName in: nodeList
    nodeList do: [:node |
        node text = aName ifTrue: [^node].
        node children notEmpty ifTrue: [
            | found |
            found := self findNode: aName in: node children.
            found ifNotNil: [^found]
        ]
    ].
    ^nil
! !

!TuiTreeView methodsFor: 'selection'!
selectIndex: index
    (index > 0 and: [index <= flatList size]) ifTrue: [
        selectedIndex := index.
        self ensureVisible: index.
        onSelect ifNotNil: [
            onSelect value: (flatList at: index)
        ]
    ]
!

selectNode: aNode
    1 to: flatList size do: [:i |
        (flatList at: i) = aNode ifTrue: [
            self selectIndex: i.
            ^self
        ]
    ]
!

visibleCount
    ^self contentRect height
!

ensureVisible: index
    | visible |
    visible := self visibleCount.
    visible = 0 ifTrue: [^self].

    index <= scrollOffset ifTrue: [
        scrollOffset := (index - 1) max: 0
    ].
    index > (scrollOffset + visible) ifTrue: [
        scrollOffset := index - visible
    ]
! !

!TuiTreeView methodsFor: 'key handling'!
handleKey: keyCode
    flatList isEmpty ifTrue: [^false].

    keyCode = AnsiTerminal keyUp ifTrue: [
        selectedIndex ifNotNil: [:idx |
            idx > 1 ifTrue: [self selectIndex: idx - 1]
        ] ifNil: [self selectIndex: 1].
        ^true
    ].

    keyCode = AnsiTerminal keyDown ifTrue: [
        selectedIndex ifNotNil: [:idx |
            idx < flatList size ifTrue: [self selectIndex: idx + 1]
        ] ifNil: [self selectIndex: 1].
        ^true
    ].

    keyCode = AnsiTerminal keyLeft ifTrue: [
        self getSelectedNode ifNotNil: [:node |
            (node expanded and: [node hasChildren]) ifTrue: [
                node expanded: false.
                self rebuildFlatList
            ] ifFalse: [
                node parent ifNotNil: [:p | self selectNode: p]
            ]
        ].
        ^true
    ].

    keyCode = AnsiTerminal keyRight ifTrue: [
        self getSelectedNode ifNotNil: [:node |
            node hasChildren ifTrue: [
                node expanded ifFalse: [
                    node expanded: true.
                    self rebuildFlatList
                ] ifTrue: [
                    node children notEmpty ifTrue: [
                        self selectNode: (node children first)
                    ]
                ]
            ]
        ].
        ^true
    ].

    keyCode = AnsiTerminal keyEnter ifTrue: [
        self getSelectedNode ifNotNil: [:node |
            node hasChildren ifTrue: [
                node toggle.
                self rebuildFlatList
            ] ifFalse: [
                onActivate ifNotNil: [onActivate value: node]
            ]
        ].
        ^true
    ].

    keyCode = AnsiTerminal keyHome ifTrue: [
        self selectIndex: 1.
        ^true
    ].

    keyCode = AnsiTerminal keyEnd ifTrue: [
        flatList notEmpty ifTrue: [
            self selectIndex: flatList size
        ].
        ^true
    ].

    keyCode = AnsiTerminal keyPageUp ifTrue: [
        | visible |
        visible := self visibleCount.
        selectedIndex ifNotNil: [:idx |
            idx > visible
                ifTrue: [self selectIndex: idx - visible]
                ifFalse: [self selectIndex: 1]
        ].
        ^true
    ].

    keyCode = AnsiTerminal keyPageDown ifTrue: [
        | visible |
        visible := self visibleCount.
        selectedIndex ifNotNil: [:idx |
            | newIdx |
            newIdx := idx + visible.
            newIdx < flatList size
                ifTrue: [self selectIndex: newIdx]
                ifFalse: [flatList notEmpty ifTrue: [self selectIndex: flatList size]]
        ].
        ^true
    ].

    ^false
! !

!TuiTreeView methodsFor: 'drawing'!
drawSelf: screen
    | content row |
    self drawBorderRounded: screen.

    content := self contentRect.
    self clearContent: screen.

    "Draw nodes"
    row := 0.
    [row < content height] whileTrue: [
        | nodeIdx |
        nodeIdx := scrollOffset + row + 1.
        nodeIdx > flatList size ifTrue: [^self].

        self drawNode: (flatList at: nodeIdx) at: row on: screen content: content.
        row := row + 1
    ].

    "Draw scrollbar if needed"
    flatList size > content height ifTrue: [
        screen drawScrollbar: content
               total: flatList size
               visible: content height
               offset: scrollOffset
    ]
!

drawNode: node at: row on: screen content: content
    | isSelected itemStyle indent col indicator maxWidth |
    isSelected := selectedIndex notNil and: [selectedIndex = (scrollOffset + row + 1)].
    itemStyle := isSelected
        ifTrue: [TuiStyle selected]
        ifFalse: [TuiStyle normal].

    "Fill background for selected item"
    isSelected ifTrue: [
        screen fillRect: (TuiRect x: content x y: content y + row width: content width height: 1)
               char: $  style: itemStyle
    ].

    "Calculate indent"
    indent := node level * 2.
    col := content x + indent.

    "Draw expand/collapse indicator"
    node hasChildren ifTrue: [
        indicator := node expanded
            ifTrue: [BoxChars treeExpanded]
            ifFalse: [BoxChars treeCollapsed].
        screen drawText: indicator at: col y: content y + row style: itemStyle.
        col := col + 2
    ] ifFalse: [
        col := col + 2
    ].

    "Draw node text"
    maxWidth := content x + content width - col.
    maxWidth > 0 ifTrue: [
        screen drawTextClipped: node text at: col y: content y + row maxWidth: maxWidth style: itemStyle
    ]
! !
