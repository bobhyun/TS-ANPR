#!/bin/bash
# TS-ANPR COBOL Example Run Script (Linux)
# Copyright (c) 2025 TS-Solution Corp.

# Get absolute path to script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set environment variables for GnuCOBOL runtime
# LD_PRELOAD: Force load the C wrapper library before execution
# LD_LIBRARY_PATH: Where to find shared libraries
export LD_PRELOAD="$SCRIPT_DIR/bin/libtsanpr_cobol.so"
export LD_LIBRARY_PATH="$SCRIPT_DIR/bin:$SCRIPT_DIR:$LD_LIBRARY_PATH"

# Run the COBOL program
"$SCRIPT_DIR/bin/anpr"