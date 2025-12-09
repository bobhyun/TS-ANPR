@echo off
REM Build script for Elixir ANPR NIF on Windows

REM Find Erlang installation
for /f "delims=" %%i in ('erl -eval "io:format(\"~s~n\", [code:root_dir()])" -s init stop -noshell') do set ERLANG_ROOT=%%i
for /f "delims=" %%i in ('erl -eval "io:format(\"~s~n\", [erlang:system_info(version)])" -s init stop -noshell') do set ERTS_VSN=%%i

echo Erlang Root: %ERLANG_ROOT%
echo ERTS Version: %ERTS_VSN%

set ERTS_INCLUDE_DIR=%ERLANG_ROOT%\erts-%ERTS_VSN%\include

REM Create priv directory
if not exist priv mkdir priv

REM Compile using MSVC
REM /c: compile only, don't link
cl.exe /c /W3 /O2 /MD /EHsc /I"%ERTS_INCLUDE_DIR%" c_src\tsanpr_nif.c

if %ERRORLEVEL% NEQ 0 (
    echo Compilation failed!
    exit /b 1
)

REM Link to create DLL
link.exe /DLL /OUT:priv\tsanpr_nif.dll tsanpr_nif.obj

if %ERRORLEVEL% NEQ 0 (
    echo Linking failed!
    del tsanpr_nif.obj
    exit /b 1
)

REM Clean up object file
del tsanpr_nif.obj

echo Build successful! NIF created at priv\tsanpr_nif.dll
