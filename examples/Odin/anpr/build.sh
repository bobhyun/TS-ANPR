#!/bin/bash
# Build script for Odin ANPR example

echo "Compiling stb_image..."
cc -c -O2 -fPIC stb_image/stb_image_impl.c -o stb_image/stb_image_impl.o
if [ $? -ne 0 ]; then
    echo "Failed to compile stb_image!"
    exit 1
fi

echo "Building Odin ANPR example..."
odin build . -out:anpr

if [ $? -eq 0 ]; then
    echo "Build successful!"
    echo "Run with: ./anpr"
else
    echo "Build failed!"
fi
