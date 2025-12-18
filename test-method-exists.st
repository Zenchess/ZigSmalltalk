"Check if structNamesFor: works then try structInfo"
| names |
names := FFILibrary structNamesFor: 'Raylib'.
names size printString.
FFILibrary structInfo: (names at: 1) for: 'Raylib'.
