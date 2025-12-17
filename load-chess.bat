@echo off
REM Chess Scene 3D demo launcher
REM
REM Usage: load-chess.bat
REM
REM NOTE: Runs WITHOUT --tui mode since OpenGL needs direct window access
REM Loads ChessScene3D package from packages/ directory

echo Loading Chess Scene 3D demo (from packages/ChessScene3D.st)...
echo.

REM Run without TUI - load dependencies then ChessScene3D package
echo ChessScene3D demo. | zig-out\bin\zig-smalltalk.exe external-structure.st src/image/ffi.st packages/ChessScene3D.st

echo.
echo Demo finished.
