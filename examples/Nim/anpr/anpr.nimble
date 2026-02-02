# Package

version       = "1.0.0"
author        = "TS-ANPR Team"
description   = "Nim example for TS-ANPR (Automatic Number Plate Recognition)"
license       = "MIT"
srcDir        = "src"
bin           = @["anpr"]

# Dependencies

requires "nim >= 1.6.0"

# Tasks

task build, "Build the ANPR example":
  exec "nim compile --verbosity:1 --hints:off src/anpr.nim"

task release, "Build optimized release version":
  exec "nim compile -d:release --opt:speed --verbosity:1 --hints:off src/anpr.nim"

task clean, "Clean build artifacts":
  when defined(windows):
    exec "del /f anpr.exe 2>nul || ver>nul"
  else:
    exec "rm -f anpr anpr.exe"

task run, "Run the ANPR example":
  exec "nim compile --run --verbosity:1 --hints:off src/anpr.nim"
