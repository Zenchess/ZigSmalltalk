"Test simple whileTrue loop"
| i |
i := 1.
[i <= 3] whileTrue: [
    FFILibrary structInfo: #Vector2 for: 'Raylib'.
    i := i + 1
].
