#!/bin/bash
# Build script for Gleam ANPR NIF on Linux
# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.

set -e

# Find Erlang installation paths
echo "Searching for Erlang..."
ERLANG_ROOT=$(erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell)
ERTS_VSN=$(erl -eval 'io:format("~s~n", [erlang:system_info(version)])' -s init stop -noshell)
ERTS_INCLUDE_DIR="${ERLANG_ROOT}/erts-${ERTS_VSN}/include"

echo "Using Erlang include path: ${ERTS_INCLUDE_DIR}"

# Create output directories
mkdir -p priv
mkdir -p build/dev/erlang/anpr/priv

echo "Compiling NIF..."

# Compile
gcc -Wall -O3 -fPIC -I"${ERTS_INCLUDE_DIR}" -shared -o priv/tsanpr_nif.so c_src/tsanpr_nif.c -ldl

# Copy to build directory for gleam run
cp priv/tsanpr_nif.so build/dev/erlang/anpr/priv/

echo ""
echo "Build successful!"
echo "NIF created at: priv/tsanpr_nif.so"
echo "NIF copied to: build/dev/erlang/anpr/priv/tsanpr_nif.so"
