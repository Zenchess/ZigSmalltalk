"Test struct info for Vector2"
| info |
info := FFILibrary structInfo: #Vector2 for: 'Raylib'.
Transcript show: 'Info: '; show: info printString; cr.
info isNil ifFalse: [
    Transcript show: 'Size: '; show: info size printString; cr.
    Transcript show: 'ByteSize: '; show: (info at: 1) printString; cr.
    info size > 1 ifTrue: [
        Transcript show: 'Field 1: '; show: (info at: 2) printString; cr
    ]
].
