@echo off
REM TS-ANPR COBOL Example Build Script (Windows)
REM Copyright (c) 2025 TS-Solution Corp.

echo Building TS-ANPR COBOL Example...
echo:

REM Check if GnuCOBOL is available
cobc --version >nul 2>&1
if errorlevel 1 (
    echo Error: GnuCOBOL compiler ^(cobc^) not found in PATH
    echo Please install GnuCOBOL or add it to your PATH
    pause
    exit /b 1
)

REM Check if GCC is available
gcc --version >nul 2>&1
if errorlevel 1 (
    echo Error: GCC compiler not found in PATH
    echo Please install GCC ^(MinGW^) or add it to your PATH
    pause
    exit /b 1
)

REM Create bin directory
if not exist bin mkdir bin

REM Build C wrapper library
echo Building C wrapper library...
gcc -shared -o bin\tsanpr_cobol.dll src\c\tsanpr_cobol.c -I src\c
if errorlevel 1 (
    echo Error: Failed to build C wrapper library
    pause
    exit /b 1
)
echo C wrapper built successfully: bin\tsanpr_cobol.dll
echo:

REM Build COBOL program
echo Building COBOL program...
cobc -free -x -o bin\anpr.exe src\cobol\anpr.cbl bin\tsanpr_cobol.dll
if errorlevel 1 (
    echo Error: Failed to compile COBOL program
    pause
    exit /b 1
)
echo COBOL program built successfully: bin\anpr.exe
echo:

echo Build completed successfully!
echo:
echo To run the program:
echo   run.bat
echo: