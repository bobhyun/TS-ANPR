# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to all conditions.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Fortran compiler
FC = gfortran

# Directories
SRC_DIR = src
BUILD_DIR = build

# Compiler flags
FFLAGS = -Wall -Wextra -std=f2008 -fPIC -O2 -cpp -J$(BUILD_DIR)

# Platform-specific settings
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Linux)
    FFLAGS += -D__linux__
    LDFLAGS = -ldl
    ifeq ($(UNAME_M),x86_64)
        FFLAGS += -D__x86_64__
    else ifeq ($(UNAME_M),aarch64)
        FFLAGS += -D__aarch64__
    endif
else
    # Windows (MinGW/MSYS2)
    FFLAGS += -D_WIN32
    ifeq ($(UNAME_M),x86_64)
        FFLAGS += -D_WIN64
    endif
    LDFLAGS = -lkernel32
endif

# Source files
TSANPR_MODULE = $(SRC_DIR)/tsanpr_module.f90
ANPR_SRC = $(SRC_DIR)/anpr.f90

# Object files
TSANPR_OBJ = $(BUILD_DIR)/tsanpr_module.o
ANPR_OBJ = $(BUILD_DIR)/anpr.o

# Executables
ANPR_EXE = $(BUILD_DIR)/anpr

# Default target
all: $(BUILD_DIR) $(ANPR_EXE)

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Build ANPR example
$(ANPR_EXE): $(TSANPR_OBJ) $(ANPR_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# Compile module
$(TSANPR_OBJ): $(TSANPR_MODULE) | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Compile ANPR source
$(ANPR_OBJ): $(ANPR_SRC) $(TSANPR_OBJ) | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)

# Install dependencies (Ubuntu/Debian)
install-deps:
	sudo apt-get update
	sudo apt-get install -y gfortran build-essential

# Install dependencies (CentOS/RHEL/Fedora)
install-deps-rpm:
	sudo yum install -y gcc-gfortran make
	# or for newer versions: sudo dnf install -y gcc-gfortran make

# Help target
help:
	@echo "Available targets:"
	@echo "  all           - Build ANPR example"
	@echo "  anpr          - Build ANPR example"
	@echo "  clean         - Remove build artifacts"
	@echo "  install-deps  - Install dependencies (Ubuntu/Debian)"
	@echo "  install-deps-rpm - Install dependencies (CentOS/RHEL/Fedora)"
	@echo "  help          - Show this help message"

.PHONY: all clean install-deps install-deps-rpm help