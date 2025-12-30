@echo off
setlocal enabledelayedexpansion

REM Build script for Gleam ANPR NIF on Windows
REM The MIT License (MIT)
REM Copyright 2022-2025 TS-Solution Corp.

REM This script assumes you are running in a Visual Studio Command Prompt

set ENGINE_DIR=%~dp0..\..\bin\windows-x86_64

echo Searching for Erlang...
for /f "tokens=*" %%i in ('erl -noshell -eval "io:format(code:root_dir())" -s init stop 2^>nul') do set ERLANG_ROOT=%%i
for /f "tokens=*" %%i in ('erl -noshell -eval "io:format(erlang:system_info(version))" -s init stop 2^>nul') do set ERTS_VSN=%%i

if not defined ERLANG_ROOT (
    echo Failed to find Erlang root directory. Is 'erl' in your PATH?
    exit /b 1
)

set ERTS_INCLUDE_DIR=%ERLANG_ROOT%\erts-%ERTS_VSN%\include
echo Using Erlang include path: %ERTS_INCLUDE_DIR%

if not exist priv mkdir priv
if not exist build\dev\erlang\anpr\priv mkdir build\dev\erlang\anpr\priv

echo Compiling and linking NIF...

cl.exe /W3 /O2 /MD /EHsc /I"%ERTS_INCLUDE_DIR%" c_src\tsanpr_nif.c /link /DLL /OUT:priv\tsanpr_nif.dll advapi32.lib user32.lib kernel32.lib /LIBPATH:"%ENGINE_DIR%"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo Build failed!
    echo Ensure you are running this from a Visual Studio developer command prompt.
    exit /b 1
)

copy /Y priv\tsanpr_nif.dll build\dev\erlang\anpr\priv\ >nul

echo.
echo Build successful!
echo NIF created at: priv\tsanpr_nif.dll
echo NIF copied to: build\dev\erlang\anpr\priv\tsanpr_nif.dll
endlocal
