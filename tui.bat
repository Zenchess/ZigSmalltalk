@echo off
REM Set console to UTF-8 for proper box drawing characters
chcp 65001 >nul

REM Disable PSReadLine key handlers that interfere with TUI (if running in PowerShell)
REM Note: F1-F4 may be captured by PowerShell. Run from cmd.exe for full key support.

REM Load ANSI classes and launch the TUI browser
REM
REM Usage: tui.bat [options]
REM   --image <path>    Load a saved image snapshot
REM   --help            Show all available options
REM
REM Examples:
REM   tui.bat                     Start with fresh ANSI classes
REM   tui.bat --image my.image   Load from saved image

echo Loading ANSI classes and launching TUI...
echo.

REM Build the file list from load-order-tui.txt (same as fixed but without test runner)
REM Filter out empty lines, comments, and ExternalStructure.cls
setlocal enabledelayedexpansion

set "FILES="
for /f "usebackq eol=# tokens=*" %%a in ("load-order-tui.txt") do (
    set "line=%%a"
    if not "!line!"=="" (
        echo !line! | findstr /i "ExternalStructure.cls" >nul
        if errorlevel 1 (
            set "FILES=!FILES! %%a"
        )
    )
)

set "STUBS=dolphin-core/stubs/sunit-stubs.st"
set "FFI=ffi-test.st"
set "STRUCTS=external-structure.st"
set "FFI_STRUCTS=load-ffi-structs.st"
set "CHESS=packages/ChessScene3D.st"  REM ChessScene3D package

REM Load ANSI files first, then external-structure.st (so our Class>>subclass: and Class>>compile:
REM primitives override any ANSI definitions), then auto-generate FFI structs last
REM Pass through any command line arguments (like --image)
zig-out\bin\zig-smalltalk.exe --tui %* %FILES% %STUBS% %STRUCTS% %FFI% %FFI_STRUCTS% %CHESS%

endlocal
