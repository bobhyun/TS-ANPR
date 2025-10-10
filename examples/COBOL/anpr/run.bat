@echo off
REM TS-ANPR COBOL Example Run Script (Windows)
REM Copyright (c) 2025 TS-Solution Corp.

REM Set environment variables for GnuCOBOL runtime
REM COB_PRE_LOAD: Preload the C wrapper DLL (without ".dll" extension)
REM COB_LIBRARY_PATH: Where to find COBOL modules (bin directory)
set COB_PRE_LOAD=tsanpr_cobol
set COB_LIBRARY_PATH=bin;.

REM Run the COBOL program
bin\anpr.exe