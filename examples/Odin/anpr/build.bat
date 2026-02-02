@echo off
REM Build script for Odin ANPR example

echo Compiling stb_image...
cl /c /O2 stb_image\stb_image_impl.c /Fostb_image\stb_image_impl.obj 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo cl.exe not found, trying gcc...
    gcc -c -O2 stb_image\stb_image_impl.c -o stb_image\stb_image_impl.obj
    if %ERRORLEVEL% NEQ 0 (
        echo Failed to compile stb_image!
        echo Please ensure you have Visual Studio or MinGW installed.
        exit /b 1
    )
)

echo Building Odin ANPR example...
odin build . -out:anpr.exe

if %ERRORLEVEL% EQU 0 (
    echo Build successful!
    echo Run with: anpr.exe
) else (
    echo Build failed!
)
