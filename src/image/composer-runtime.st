"Runtime support for View Composer generated classes"
"Includes optional native macOS bridge via FFI (libzscocoa_bridge.dylib)."

Object subclass: #MacUIBridge
    instanceVariableNames: ''
    classVariableNames: 'Library CallbackKeepAlive IsLoaded'
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!MacUIBridge class methodsFor!

initialize
    IsLoaded := false.
    Library := nil.
    CallbackKeepAlive := OrderedCollection new!

available
    IsLoaded ifFalse: [self loadBridge].
    Library isNil ifTrue: [^false].
    ^(self appInit) notNil!

loadBridge
    "Try local and common install locations"
    IsLoaded := true.
    Library := ExternalLibrary load: 'native/mac/libzscocoa_bridge.dylib'.
    Library isNil ifTrue: [
        Library := ExternalLibrary load: './native/mac/libzscocoa_bridge.dylib'
    ].
    Library isNil ifTrue: [
        Library := ExternalLibrary load: '/usr/local/lib/libzscocoa_bridge.dylib'
    ].
    Library isNil ifTrue: [
        Library := ExternalLibrary load: '/opt/homebrew/lib/libzscocoa_bridge.dylib'
    ].
    ^Library notNil!

library
    ^Library!

call: functionName signature: signature args: args
    | fnPtr |
    (self available not) ifTrue: [^nil].
    fnPtr := Library getProcAddress: functionName.
    fnPtr isNil ifTrue: [^nil].
    fnPtr = 0 ifTrue: [^nil].
    ^fnPtr ffiCallWithSignature: signature args: args!

appInit
    ^self call: 'zs_app_init' signature: 'pointer()' args: #()!

appActivate
    ^self call: 'zs_app_activate' signature: 'void()' args: #()!

run
    ^self call: 'zs_app_run' signature: 'void()' args: #()!

pump
    ^self call: 'zs_app_pump' signature: 'void()' args: #()!

windowCreateX: x y: y width: w height: h title: title
    ^self call: 'zs_window_create'
        signature: 'pointer(double,double,double,double,string)'
        args: { x asFloat. y asFloat. w asFloat. h asFloat. title }!

windowShow: windowPtr
    ^self call: 'zs_window_show' signature: 'void(pointer)' args: { windowPtr }!

windowContentView: windowPtr
    ^self call: 'zs_window_content_view' signature: 'pointer(pointer)' args: { windowPtr }!

viewAdd: childPtr to: parentPtr
    ^self call: 'zs_view_add_subview' signature: 'void(pointer,pointer)' args: { parentPtr. childPtr }!

buttonX: x y: y width: w height: h title: title
    ^self call: 'zs_button_create'
        signature: 'pointer(double,double,double,double,string)'
        args: { x asFloat. y asFloat. w asFloat. h asFloat. title }!

labelX: x y: y width: w height: h text: text
    ^self call: 'zs_label_create'
        signature: 'pointer(double,double,double,double,string)'
        args: { x asFloat. y asFloat. w asFloat. h asFloat. text }!

textFieldX: x y: y width: w height: h text: text
    ^self call: 'zs_textfield_create'
        signature: 'pointer(double,double,double,double,string)'
        args: { x asFloat. y asFloat. w asFloat. h asFloat. text }!

sliderX: x y: y width: w height: h min: minVal max: maxVal value: value
    ^self call: 'zs_slider_create'
        signature: 'pointer(double,double,double,double,double,double,double)'
        args: { x asFloat. y asFloat. w asFloat. h asFloat. minVal asFloat. maxVal asFloat. value asFloat }!

checkboxX: x y: y width: w height: h title: title checked: checked
    ^self call: 'zs_checkbox_create'
        signature: 'pointer(double,double,double,double,string,int32)'
        args: { x asFloat. y asFloat. w asFloat. h asFloat. title. (checked ifTrue: [1] ifFalse: [0]) }!

connectControl: controlPtr widget: aWidget
    "Install a native callback and keep it alive"
    | cb eventSym |
    eventSym := aWidget nativeEventSymbol.
    eventSym isNil ifTrue: [^self].

    cb := FFICallback signature: 'void(pointer)' block: [:unused |
        unused.
        aWidget trigger: eventSym
    ].

    CallbackKeepAlive add: cb.

    self call: 'zs_control_set_callback'
        signature: 'void(pointer,pointer,pointer)'
        args: { controlPtr. cb functionPointer. 0 }!

clearCallbacks
    CallbackKeepAlive do: [:cb | cb free].
    CallbackKeepAlive := OrderedCollection new!

!

Object subclass: #ComposerShell
    instanceVariableNames: 'title widgets nativeWindow nativeContentView nativeMode'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerShell class methodsFor!

title: aString
    ^self new title: aString; yourself!

!ComposerShell methodsFor!

initialize
    widgets := OrderedCollection new.
    title := 'Untitled'.
    nativeWindow := nil.
    nativeContentView := nil.
    nativeMode := false!

title
    ^title!

title: aString
    title := aString!

add: aWidget
    widgets add: aWidget.
    ^aWidget!

widgets
    ^widgets!

nativeMode
    ^nativeMode!

open
    (MacUIBridge available)
        ifTrue: [self openNative]
        ifFalse: [self openTranscript].
    ^self!

openTranscript
    nativeMode := false.
    Transcript show: 'ComposerShell open: '; show: title; cr.
    Transcript show: 'Widgets: '; show: widgets size printString; cr.
    widgets do: [:w |
        Transcript show: '  '; show: w name; show: ' '; show: w bounds printString; cr
    ]!

openNative
    | content |
    nativeMode := true.
    MacUIBridge appInit.
    MacUIBridge appActivate.

    nativeWindow := MacUIBridge windowCreateX: 200 y: 200 width: 900 height: 640 title: title.
    nativeWindow isNil ifTrue: [
        Transcript show: 'Native bridge failed. Falling back to transcript.'; cr.
        self openTranscript.
        ^self
    ].

    content := MacUIBridge windowContentView: nativeWindow.
    nativeContentView := content.

    widgets do: [:w |
        w createNativeIn: content.
        w installNativeCallbacks
    ].

    MacUIBridge windowShow: nativeWindow!

runNativeLoop
    nativeMode ifTrue: [MacUIBridge run]!

pumpNative
    nativeMode ifTrue: [MacUIBridge pump]!

widgetNamed: aString
    widgets do: [:w |
        (w name = aString) ifTrue: [^w]
    ].
    ^nil!

!

Object subclass: #ComposerWidget
    instanceVariableNames: 'name bounds label bindings nativeHandle nativeValue'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerWidget methodsFor!

initialize
    bounds := #(0 0 10 1).
    label := ''.
    name := ''.
    bindings := OrderedCollection new.
    nativeHandle := nil.
    nativeValue := nil!

name
    ^name!

name: aString
    name := aString!

bounds
    ^bounds!

bounds: anArray
    bounds := anArray!

label
    ^label!

label: aString
    label := aString!

nativeHandle
    ^nativeHandle!

nativeHandle: aPointer
    nativeHandle := aPointer!

nativeValue
    ^nativeValue!

nativeValue: anObject
    nativeValue := anObject!

x
    ^bounds at: 1!

y
    ^bounds at: 2!

width
    ^bounds at: 3!

height
    ^bounds at: 4!

when: eventSymbol send: selector to: aTarget
    bindings add: { eventSymbol. selector. aTarget }!

trigger: eventSymbol
    bindings do: [:binding |
        (binding at: 1) = eventSymbol ifTrue: [
            (binding at: 3) perform: (binding at: 2)
        ]
    ]!

nativeEventSymbol
    ^nil!

createNativeIn: parentView
    parentView.
    self subclassResponsibility!

installNativeCallbacks
    (nativeHandle isNil or: [nativeHandle = 0]) ifTrue: [^self].
    self nativeEventSymbol notNil ifTrue: [
        MacUIBridge connectControl: nativeHandle widget: self
    ]!

!

ComposerWidget subclass: #ComposerButton
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerButton methodsFor!

nativeEventSymbol
    ^#onClick!

createNativeIn: parentView
    nativeHandle := MacUIBridge buttonX: self x y: self y width: self width height: self height title: label.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

press
    self trigger: #onClick!

!

ComposerWidget subclass: #ComposerLabel
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerLabel methodsFor!

createNativeIn: parentView
    nativeHandle := MacUIBridge labelX: self x y: self y width: self width height: self height text: label.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

!

ComposerWidget subclass: #ComposerTextField
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerTextField methodsFor!

nativeEventSymbol
    ^#onChange!

createNativeIn: parentView
    nativeHandle := MacUIBridge textFieldX: self x y: self y width: self width height: self height text: label.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

textChanged
    self trigger: #onChange!

!

ComposerWidget subclass: #ComposerSlider
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerSlider methodsFor!

nativeEventSymbol
    ^#onChange!

createNativeIn: parentView
    nativeHandle := MacUIBridge sliderX: self x y: self y width: self width height: self height min: 0 max: 100 value: 0.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

valueChanged
    self trigger: #onChange!

!

ComposerWidget subclass: #ComposerCheckbox
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerCheckbox methodsFor!

nativeEventSymbol
    ^#onToggle!

createNativeIn: parentView
    nativeHandle := MacUIBridge checkboxX: self x y: self y width: self width height: self height title: label checked: false.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

toggled
    self trigger: #onToggle!

!

ComposerWidget subclass: #ComposerList
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerList methodsFor!

nativeEventSymbol
    ^#onSelect!

createNativeIn: parentView
    "MVP: use label placeholder until native list view is added"
    nativeHandle := MacUIBridge labelX: self x y: self y width: self width height: self height text: '[ListView]'.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

selected
    self trigger: #onSelect!

!

ComposerWidget subclass: #ComposerTabs
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerTabs methodsFor!

createNativeIn: parentView
    "MVP placeholder"
    nativeHandle := MacUIBridge labelX: self x y: self y width: self width height: self height text: '[Tabs]'.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

!

ComposerWidget subclass: #ComposerContainer
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ComposerContainer methodsFor!

createNativeIn: parentView
    "MVP placeholder"
    nativeHandle := MacUIBridge labelX: self x y: self y width: self width height: self height text: '[Container]'.
    nativeHandle notNil ifTrue: [MacUIBridge viewAdd: nativeHandle to: parentView]!

!

MacUIBridge initialize!
