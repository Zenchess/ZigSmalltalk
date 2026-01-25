| term done k activeTab width height tabNames |
term := AnsiTerminal current.
width := term width.
height := term height.
activeTab := 1.
tabNames := #('Transcript' 'Workspace' 'Browser' 'REPL' 'FFI').

term enterRawMode.
term enterAlternateScreen.
term hideCursor.

"Draw function"
[ "redraw"
    term clear.

    "Tab bar"
    term moveTo: 1 col: 1.
    1 to: tabNames size do: [:i |
        i = activeTab
            ifTrue: [term write: ' ['; write: (tabNames at: i); write: '] ']
            ifFalse: [term write: '  '; write: (tabNames at: i); write: '  ']
    ].

    "Separator"
    term moveTo: 2 col: 1.
    1 to: width do: [:i | term write: '-'].

    "Content"
    term moveTo: 4 col: 3.
    term write: 'Zig Smalltalk TUI'.
    term moveTo: 6 col: 3.
    term write: 'Active tab: ', (tabNames at: activeTab).
    term moveTo: 8 col: 3.
    term write: 'F1-F5: Switch tabs'.
    term moveTo: 9 col: 3.
    term write: 'Q or Ctrl+Q: Quit'.

    "Status bar"
    term moveTo: height col: 1.
    term write: ' Ready | ', width printString, 'x', height printString
] value.

"Event loop"
done := false.
[done not] whileTrue: [
    k := term pollKey.
    k notNil ifTrue: [
        "Quit"
        (k = $q or: [k = $Q or: [k = 17]]) ifTrue: [done := true].

        "F1-F5"
        k = 265 ifTrue: [activeTab := 1].
        k = 266 ifTrue: [activeTab := 2].
        k = 267 ifTrue: [activeTab := 3].
        k = 268 ifTrue: [activeTab := 4].
        k = 269 ifTrue: [activeTab := 5].

        "Redraw"
        done not ifTrue: [
            term clear.
            term moveTo: 1 col: 1.
            1 to: tabNames size do: [:i |
                i = activeTab
                    ifTrue: [term write: ' ['; write: (tabNames at: i); write: '] ']
                    ifFalse: [term write: '  '; write: (tabNames at: i); write: '  ']
            ].
            term moveTo: 2 col: 1.
            1 to: width do: [:i | term write: '-'].
            term moveTo: 4 col: 3.
            term write: 'Zig Smalltalk TUI'.
            term moveTo: 6 col: 3.
            term write: 'Active tab: ', (tabNames at: activeTab).
            term moveTo: 8 col: 3.
            term write: 'F1-F5: Switch tabs'.
            term moveTo: 9 col: 3.
            term write: 'Q or Ctrl+Q: Quit'.
            term moveTo: height col: 1.
            term write: ' Ready | ', width printString, 'x', height printString
        ]
    ]
].

term exitAlternateScreen.
term exitRawMode.
term showCursor!