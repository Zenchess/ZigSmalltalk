"Load FFI struct definitions for configured libraries"
"Called by TUI.bat after loading external-structure.st"
"Uses primitive 799 which creates subclass AND compiles accessor methods in one call"

| names sz |
Transcript show: 'Loading OpenXR struct definitions...'; cr.
names := FFILibrary structNamesFor: 'OpenXR'.
sz := names size.
Transcript show: 'Found '; show: sz printString; show: ' OpenXR structs'; cr.
1 to: sz do: [:i |
    FFILibrary createStructClass: (names at: i) for: 'OpenXR'
].
Transcript show: 'OpenXR structs loaded successfully!'; cr.
