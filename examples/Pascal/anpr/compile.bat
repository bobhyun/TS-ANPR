@echo off
REM TS-ANPR Pascal Compilation Script for Windows
REM Copyright (c) 2025 TS-Solution Corp.

echo Compiling TS-ANPR Pascal programs...

REM Check if Free Pascal is available
fpc -h >nul 2>&1
if errorlevel 1 (
    echo Error: Free Pascal compiler (fpc) not found in PATH
    echo Please install Free Pascal or add it to your PATH
    pause
    exit /b 1
)

REM Create output directory
if not exist "bin" mkdir bin

REM Compile stb_image shared library (optional, for ReadPixelBuffer)
echo Compiling stb_image library...
cl /LD /O2 lib\stb_image_lib.c /Fe:bin\stb_image.dll 2>nul
if errorlevel 1 (
    echo   cl.exe not found, trying gcc...
    gcc -shared -O2 lib\stb_image_lib.c -o bin\stb_image.dll 2>nul
    if errorlevel 1 (
        echo   Warning: Failed to compile stb_image library (ReadPixelBuffer will not work)
    ) else (
        echo   stb_image library compiled successfully
    )
) else (
    echo   stb_image library compiled successfully
)

REM Compile main program
echo Compiling anpr.pas...
fpc -FEbin -FUbin -Fuunits src\anpr.pas
if errorlevel 1 (
    echo Error: Failed to compile anpr.pas
    pause
    exit /b 1
)

echo.
echo Compilation completed successfully!
echo.
echo Output files:
echo   bin\anpr.exe       - Main executable
echo   bin\stb_image.dll  - stb_image library (if compiled)
echo.
echo To run the example:
echo   cd bin
echo   anpr.exe
echo.
pause
