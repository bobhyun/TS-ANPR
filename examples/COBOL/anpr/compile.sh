#!/bin/bash
# TS-ANPR COBOL Example Build Script (Linux)
# Copyright (c) 2025 TS-Solution Corp.

echo "Building TS-ANPR COBOL Example..."
echo ""

# Check if GnuCOBOL is available
if ! command -v cobc &> /dev/null; then
    echo "Error: GnuCOBOL compiler (cobc) not found in PATH"
    echo "Please install GnuCOBOL:"
    echo "  Ubuntu/Debian: sudo apt-get install gnucobol"
    echo "  CentOS/RHEL:   sudo yum install gnucobol"
    echo "  Fedora:        sudo dnf install gnucobol"
    exit 1
fi

# Check if GCC is available
if ! command -v gcc &> /dev/null; then
    echo "Error: GCC compiler not found in PATH"
    echo "Please install GCC:"
    echo "  Ubuntu/Debian: sudo apt-get install gcc"
    echo "  CentOS/RHEL:   sudo yum install gcc"
    echo "  Fedora:        sudo dnf install gcc"
    exit 1
fi

# Create bin directory
mkdir -p bin

# Build C wrapper library
echo "Building C wrapper library..."
gcc -shared -fPIC -o bin/libtsanpr_cobol.so src/c/tsanpr_cobol.c -I src/c
if [ $? -ne 0 ]; then
    echo "Error: Failed to build C wrapper library"
    exit 1
fi
echo "C wrapper built successfully: bin/libtsanpr_cobol.so"
echo ""

# Build COBOL program
echo "Building COBOL program..."
cobc -free -x -o bin/anpr src/cobol/anpr.cbl bin/libtsanpr_cobol.so
if [ $? -ne 0 ]; then
    echo "Error: Failed to compile COBOL program"
    exit 1
fi
echo "COBOL program built successfully: bin/anpr"
echo ""

echo "Build completed successfully!"
echo ""
echo "To run the program:"
echo "  ./run.sh"
echo ""