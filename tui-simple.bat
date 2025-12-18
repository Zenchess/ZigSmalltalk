@echo off
REM Simple Smalltalk TUI launcher (no ANSI test files)

REM Enable UTF-8 for Unicode box-drawing characters
chcp 65001 >nul

echo Starting Smalltalk TUI...
echo.

REM Use the built executable
set EXE=zig-out\bin\zig-smalltalk.exe
if not exist %EXE% (
    echo Error: %EXE% not found. Please build first with: zig build
    exit /b 1
)

REM Load only the TUI files
%EXE% src/image/terminal.st src/image/widgets.st src/image/browser.st src/image/tui-main.st

echo.
echo Smalltalk TUI exited.
