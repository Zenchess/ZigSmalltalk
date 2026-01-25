"Load FFI struct definitions for configured libraries"
"Called by TUI.bat after loading external-structure.st"
"Uses primitive 790/791 to query structs and create ExternalStructure subclasses"

| names sz |

Transcript show: '=== Starting FFI Struct Loading ==='; cr.
Transcript show: 'Loading OpenXR struct definitions...'; cr.
names := FFILibrary structNamesFor: 'OpenXR'.
Transcript show: 'Got names array: '; show: names printString; cr.
sz := names size.
Transcript show: 'Found '; show: sz printString; show: ' OpenXR structs'; cr.
1 to: sz do: [:i |
    | structName |
    structName := names at: i.
    Transcript show: 'Creating struct class: '; show: structName printString; cr.
    FFILibrary createStructClass: structName inLibrary: 'OpenXR'
].
Transcript show: 'OpenXR structs loaded successfully!'; cr.
