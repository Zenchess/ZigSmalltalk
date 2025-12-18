"Load FFI struct definitions for configured libraries"
"Called by TUI.bat after loading external-structure.st"
"Uses primitive 799 which creates subclass AND compiles accessor methods in one call"

| names sz |
names := FFILibrary structNamesFor: 'Raylib'.
sz := names size.
1 to: sz do: [:i |
    FFILibrary createStructClass: (names at: i) for: 'Raylib'
].
