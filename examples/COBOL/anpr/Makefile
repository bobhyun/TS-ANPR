# TS-ANPR COBOL Makefile
# Copyright (c) 2025 TS-Solution Corp.

# Detect platform
ifeq ($(OS),Windows_NT)
    PLATFORM = windows
    EXE_EXT = .exe
    LIB_PREFIX =
    LIB_EXT = .dll
    RM = cmd //c del /f /q
    RMDIR = cmd //c rmdir /s /q
    MKDIR = cmd //c if not exist
    COPY = cmd //c copy /y
    PATHSEP = \\
else
    PLATFORM = linux
    EXE_EXT =
    LIB_PREFIX = lib
    LIB_EXT = .so
    RM = rm -f
    RMDIR = rm -rf
    MKDIR = mkdir -p
    COPY = cp -f
    PATHSEP = /
endif

# Compilers and flags
CC = gcc
CFLAGS = -Wall -O2
COBC = cobc
COBCFLAGS = -free -Wall

# Directories
SRCDIR_C = src$(PATHSEP)c
SRCDIR_COBOL = src$(PATHSEP)cobol
BINDIR = bin

# Source files
C_WRAPPER_SRC = $(SRCDIR_C)$(PATHSEP)tsanpr_cobol.c
COBOL_MAIN_SRC = $(SRCDIR_COBOL)$(PATHSEP)anpr.cbl

# Output files
C_WRAPPER_LIB = $(BINDIR)$(PATHSEP)$(LIB_PREFIX)tsanpr_cobol$(LIB_EXT)
COBOL_MAIN_EXE = $(BINDIR)$(PATHSEP)anpr$(EXE_EXT)

# Environment for running
ifeq ($(PLATFORM),windows)
    RUN_CMD = set COB_PRE_LOAD=tsanpr_cobol&& set COB_LIBRARY_PATH=$(BINDIR);.&& $(COBOL_MAIN_EXE)
else
    RUN_CMD = LD_PRELOAD=$(shell pwd)/$(BINDIR)/$(LIB_PREFIX)tsanpr_cobol$(LIB_EXT) LD_LIBRARY_PATH=$(shell pwd)/$(BINDIR):$(shell pwd):$$LD_LIBRARY_PATH ./$(COBOL_MAIN_EXE)
endif

# Default target
all: $(C_WRAPPER_LIB) $(COBOL_MAIN_EXE)
	@echo "Build completed successfully!"
	@echo "To run: make run"

# Create bin directory
$(BINDIR):
ifeq ($(PLATFORM),windows)
	$(MKDIR) $(BINDIR) $(BINDIR) 2>nul || echo Directory exists
else
	$(MKDIR) $(BINDIR)
endif

# Build C wrapper library
$(C_WRAPPER_LIB): $(C_WRAPPER_SRC) | $(BINDIR)
	@echo "Building C wrapper library..."
ifeq ($(PLATFORM),windows)
	$(CC) -shared $(CFLAGS) -o $@ $< -I $(SRCDIR_C)
else
	$(CC) -shared -fPIC $(CFLAGS) -o $@ $< -I $(SRCDIR_C)
endif

# Build COBOL main program
$(COBOL_MAIN_EXE): $(COBOL_MAIN_SRC) $(C_WRAPPER_LIB) | $(BINDIR)
	@echo "Building COBOL program..."
	$(COBC) $(COBCFLAGS) -x -o $@ $< $(C_WRAPPER_LIB)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
ifeq ($(PLATFORM),windows)
	$(RMDIR) $(BINDIR) 2>nul || echo Already clean
	$(RM) *.log 2>nul || echo Already clean
else
	$(RMDIR) $(BINDIR)
	$(RM) *.log
endif

# Run the example
run: all
	@echo "Running COBOL ANPR example..."
	$(RUN_CMD)

# Check if required tools are available
check:
	@echo "Checking build requirements..."
	@which $(CC) > /dev/null 2>&1 || (echo "Error: GCC compiler not found" && exit 1)
	@echo "GCC found: $$($(CC) --version | head -1)"
	@which $(COBC) > /dev/null 2>&1 || (echo "Error: GnuCOBOL compiler not found" && exit 1)
	@echo "GnuCOBOL found: $$($(COBC) --version | head -1)"

# Help
help:
	@echo "TS-ANPR COBOL Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all     - Build C wrapper and COBOL program (default)"
	@echo "  clean   - Remove all build artifacts"
	@echo "  run     - Build and run the ANPR example"
	@echo "  check   - Check if required tools are available"
	@echo "  help    - Show this help message"
	@echo ""
	@echo "Platform: $(PLATFORM)"

.PHONY: all clean run check help