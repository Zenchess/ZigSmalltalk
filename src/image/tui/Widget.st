"TuiWidget - Base class for all TUI widgets
 Provides common functionality for positioning, focus, and event handling"

Object subclass: #TuiWidget
    instanceVariableNames: 'rect focused visible enabled parent title hasBorder'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiWidget class methodsFor: 'instance creation'!
new
    ^super new initialize
!

rect: aRect
    ^self new rect: aRect
! !

!TuiWidget methodsFor: 'initialization'!
initialize
    rect := TuiRect x: 1 y: 1 width: 10 height: 5.
    focused := false.
    visible := true.
    enabled := true.
    parent := nil.
    title := nil.
    hasBorder := true
! !

!TuiWidget methodsFor: 'accessing'!
rect
    ^rect
!

rect: aRect
    rect := aRect
!

x ^rect x!
y ^rect y!
width ^rect width!
height ^rect height!

parent
    ^parent
!

parent: aWidget
    parent := aWidget
!

title
    ^title
!

title: aString
    title := aString
!

hasBorder
    ^hasBorder
!

hasBorder: aBoolean
    hasBorder := aBoolean
! !

!TuiWidget methodsFor: 'geometry'!
contentRect
    "Return the inner content area (inside border if present)"
    hasBorder ifTrue: [^rect inner: 1].
    ^rect
!

moveTo: aPoint
    rect := TuiRect x: aPoint x y: aPoint y width: rect width height: rect height
!

resize: extent
    rect := TuiRect x: rect x y: rect y width: extent x height: extent y
!

containsPoint: aPoint
    ^rect containsPoint: aPoint
! !

!TuiWidget methodsFor: 'focus'!
focused
    ^focused
!

focused: aBoolean
    focused := aBoolean.
    focused ifTrue: [self onFocus] ifFalse: [self onBlur]
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

!TuiWidget methodsFor: 'visibility'!
visible
    ^visible
!

visible: aBoolean
    visible := aBoolean
!

show
    visible := true
!

hide
    visible := false
!

enabled
    ^enabled
!

enabled: aBoolean
    enabled := aBoolean
! !

!TuiWidget methodsFor: 'event handling'!
handleKey: keyCode
    "Handle key event. Return true if handled."
    ^false
!

handleEvent: anInputEvent
    "Handle an InputEvent. Return true if handled."
    anInputEvent isKey ifTrue: [^self handleKey: anInputEvent keyCode].
    anInputEvent isMouse ifTrue: [^self handleMouse: anInputEvent].
    ^false
!

handleMouse: anInputEvent
    "Handle mouse event. Return true if handled."
    ^false
! !

!TuiWidget methodsFor: 'drawing'!
draw: screen
    "Draw this widget to the screen. Subclasses override."
    visible ifFalse: [^self].
    self drawSelf: screen
!

drawSelf: screen
    "Draw just this widget. Subclasses override."
!

drawBorder: screen
    "Draw border if enabled"
    | style |
    hasBorder ifFalse: [^self].
    style := focused ifTrue: [TuiStyle borderFocused] ifFalse: [TuiStyle border].
    screen drawBox: rect style: style.
    title ifNotNil: [
        screen drawTitle: title at: rect focused: focused
    ]
!

needsRedraw
    "Mark widget as needing redraw"
    "In full implementation would add to dirty list"
! !


"EventResult - Enum-like class for event handling results"
Object subclass: #EventResult
    instanceVariableNames: 'value'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!EventResult class methodsFor: 'constants'!
ignored
    "Event was not handled"
    ^self new value: #ignored
!

consumed
    "Event was handled"
    ^self new value: #consumed
!

quit
    "Request to quit application"
    ^self new value: #quit
! !

!EventResult methodsFor: 'accessing'!
value
    ^value
!

value: aSymbol
    value := aSymbol
! !

!EventResult methodsFor: 'testing'!
isIgnored
    ^value = #ignored
!

isConsumed
    ^value = #consumed
!

isQuit
    ^value = #quit
! !


"Container widget for holding child widgets"
TuiWidget subclass: #TuiContainer
    instanceVariableNames: 'children focusedChild'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'TUI-Widgets'!

!TuiContainer methodsFor: 'initialization'!
initialize
    super initialize.
    children := OrderedCollection new.
    focusedChild := nil
! !

!TuiContainer methodsFor: 'children'!
children
    ^children
!

addChild: aWidget
    aWidget parent: self.
    children add: aWidget
!

removeChild: aWidget
    children remove: aWidget ifAbsent: [].
    aWidget parent: nil.
    focusedChild = aWidget ifTrue: [focusedChild := nil]
!

childAt: index
    (index > 0 and: [index <= children size]) ifTrue: [^children at: index].
    ^nil
! !

!TuiContainer methodsFor: 'focus management'!
focusedChild
    ^focusedChild
!

focusChild: aWidget
    "Focus a specific child"
    focusedChild ifNotNil: [focusedChild blur].
    focusedChild := aWidget.
    focusedChild ifNotNil: [focusedChild focus]
!

focusFirst
    "Focus first child"
    children isEmpty ifFalse: [
        self focusChild: (children at: 1)
    ]
!

focusNext
    "Focus next child in sequence"
    | idx |
    focusedChild isNil ifTrue: [
        ^self focusFirst
    ].
    idx := children indexOf: focusedChild.
    idx < children size
        ifTrue: [self focusChild: (children at: idx + 1)]
        ifFalse: [self focusChild: (children at: 1)]
!

focusPrevious
    "Focus previous child"
    | idx |
    focusedChild isNil ifTrue: [
        ^self focusFirst
    ].
    idx := children indexOf: focusedChild.
    idx > 1
        ifTrue: [self focusChild: (children at: idx - 1)]
        ifFalse: [self focusChild: (children at: children size)]
! !

!TuiContainer methodsFor: 'event handling'!
handleKey: keyCode
    "First give focused child a chance to handle"
    focusedChild ifNotNil: [
        (focusedChild handleKey: keyCode) ifTrue: [^true]
    ].

    "Tab cycles focus"
    keyCode = AnsiTerminal keyTab ifTrue: [
        self focusNext.
        ^true
    ].

    ^false
! !

!TuiContainer methodsFor: 'drawing'!
draw: screen
    visible ifFalse: [^self].
    self drawSelf: screen.
    self drawChildren: screen
!

drawChildren: screen
    children do: [:child |
        child draw: screen
    ]
! !
