@echo off
REM Launch Smalltalk TUI (pure Smalltalk version)

REM Enable UTF-8 for Unicode box-drawing characters
chcp 65001 >nul

echo Starting Smalltalk TUI...
echo.

REM Use the built executable (prefer tmp build)
set EXE=C:\tmp\ZigSmalltalkBuild\zig-out\bin\zig-smalltalk.exe
if not exist %EXE% (
    set EXE=zig-out\bin\zig-smalltalk.exe
)

REM Build the file list from load-order-tui.txt
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
set "STRUCTS=external-structure.st"

REM Smalltalk TUI classes
set "TUI_TERMINAL=src/image/terminal.st"
set "TUI_WIDGETS=src/image/widgets.st"
set "TUI_BROWSER=src/image/browser.st"
set "TUI_MAIN=src/image/tui-main.st"

REM Load ANSI files first, then external-structure.st, then TUI classes
%EXE% %FILES% %STUBS% %STRUCTS% %TUI_TERMINAL% %TUI_WIDGETS% %TUI_BROWSER% %TUI_MAIN%

endlocal

echo.
echo Smalltalk TUI exited.
