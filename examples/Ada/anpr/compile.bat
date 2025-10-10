@echo off
REM TS-ANPR Ada Compilation Script for Windows
REM Copyright (c) 2025 TS-Solution Corp.

echo Compiling TS-ANPR Ada programs...

REM Check if GNAT is available
gnatmake --version >nul 2>&1
if errorlevel 1 (
    echo Error: GNAT compiler not found in PATH
    echo Please install GNAT or add it to your PATH
    pause
    exit /b 1
)

REM Create output directories
if not exist "obj" mkdir obj
if not exist "bin" mkdir bin

REM Compile using GPRbuild if available
gprbuild --version >nul 2>&1
if not errorlevel 1 (
    echo Using GPRbuild...
    gprbuild -p -P anpr.gpr -XOS=Windows_NT
    if errorlevel 1 (
        echo Error: GPRbuild compilation failed
        pause
        exit /b 1
    )
) else (
    echo Using gnatmake...
    gnatmake -D obj -o bin\anpr.exe anpr.adb -largs -ldl
    if errorlevel 1 (
        echo Error: gnatmake compilation failed
        pause
        exit /b 1
    )
)

echo.
echo Compilation completed successfully!
echo.
echo Output files:
echo   bin\anpr.exe       - Main executable
echo.
echo To run the example:
echo   For UTF-8 character display: chcp 65001 ^& bin\anpr.exe
echo   Or: bin\anpr.exe
echo.
pause