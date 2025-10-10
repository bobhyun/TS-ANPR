#!/bin/bash
# TS-ANPR Ada Compilation Script for Linux
# Copyright (c) 2025 TS-Solution Corp.

echo "Compiling TS-ANPR Ada programs..."

# Check if GNAT is available
if ! command -v gnatmake &> /dev/null; then
    echo "Error: GNAT compiler not found in PATH"
    echo "Please install GNAT:"
    echo "  Ubuntu/Debian: sudo apt-get install gnat"
    echo "  CentOS/RHEL:   sudo yum install gcc-gnat"
    echo "  Fedora:        sudo dnf install gcc-gnat"
    exit 1
fi

# Create output directories
mkdir -p obj
mkdir -p bin

# Compile using GPRbuild if available
if command -v gprbuild &> /dev/null; then
    echo "Using GPRbuild..."
    gprbuild -p -P anpr.gpr -XOS=UNIX
    if [ $? -ne 0 ]; then
        echo "Error: GPRbuild compilation failed"
        exit 1
    fi
else
    echo "Using gnatmake..."
    gnatmake -D obj -o bin/anpr anpr.adb -largs -ldl
    if [ $? -ne 0 ]; then
        echo "Error: gnatmake compilation failed"
        exit 1
    fi
fi

echo ""
echo "Compilation completed successfully!"
echo ""
echo "Output files:"
echo "  bin/anpr          - Main executable"
echo ""
echo "To run the example:"
echo "  bin/anpr"
echo ""