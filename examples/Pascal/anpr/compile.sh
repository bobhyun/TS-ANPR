#!/bin/bash
# TS-ANPR Pascal Compilation Script for Linux
# Copyright (c) 2025 TS-Solution Corp.

echo "Compiling TS-ANPR Pascal programs..."

# Check if Free Pascal is available
if ! command -v fpc &> /dev/null; then
    echo "Error: Free Pascal compiler (fpc) not found in PATH"
    echo "Please install Free Pascal:"
    echo "  Ubuntu/Debian: sudo apt-get install fpc"
    echo "  CentOS/RHEL:   sudo yum install fpc"
    echo "  Fedora:        sudo dnf install fpc"
    exit 1
fi

# Create output directory
mkdir -p bin

# Compile stb_image shared library (optional, for ReadPixelBuffer)
echo "Compiling stb_image library..."
if command -v cc &> /dev/null; then
    cc -shared -fPIC -O2 lib/stb_image_lib.c -o bin/libstb_image.so -lm 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "  stb_image library compiled successfully"
    else
        echo "  Warning: Failed to compile stb_image library (ReadPixelBuffer will not work)"
    fi
else
    echo "  Warning: C compiler not found (ReadPixelBuffer will not work)"
fi

# Compile main program
echo "Compiling anpr.pas..."
fpc -FEbin -FUbin -Fuunits src/anpr.pas
if [ $? -ne 0 ]; then
    echo "Error: Failed to compile anpr.pas"
    exit 1
fi

echo ""
echo "Compilation completed successfully!"
echo ""
echo "Output files:"
echo "  bin/anpr           - Main executable"
echo "  bin/libstb_image.so - stb_image library (if compiled)"
echo ""
echo "To run the example:"
echo "  cd bin"
echo "  ./anpr"
echo ""
