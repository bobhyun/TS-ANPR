#!/bin/bash
# Build script for Erlang ANPR NIF on Linux

# Find Erlang installation
ERLANG_ROOT=$(erl -eval "io:format(\"~s~n\", [code:root_dir()])" -s init stop -noshell)
ERTS_VSN=$(erl -eval "io:format(\"~s~n\", [erlang:system_info(version)])" -s init stop -noshell)

echo "Erlang Root: $ERLANG_ROOT"
echo "ERTS Version: $ERTS_VSN"

ERTS_INCLUDE_DIR="$ERLANG_ROOT/erts-$ERTS_VSN/include"

# Create priv directory
mkdir -p priv

# Compile using gcc
gcc -Wall -O3 -fPIC -I"$ERTS_INCLUDE_DIR" -shared -o priv/tsanpr_nif.so c_src/tsanpr_nif.c

if [ $? -ne 0 ]; then
    echo "Build failed!"
    exit 1
fi

echo "Build successful! NIF created at priv/tsanpr_nif.so"

