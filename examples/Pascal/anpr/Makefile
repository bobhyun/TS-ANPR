# TS-ANPR Pascal Makefile
# Copyright (c) 2025 TS-Solution Corp.

# Compiler and flags
FPC = fpc
FPCFLAGS = -O2 -Xs -XX
CC = cc
CFLAGS = -shared -fPIC -O2

# Directories
SRCDIR = src
UNITDIR = units
LIBDIR = lib
BINDIR = bin

# Source files
MAIN_SRC = $(SRCDIR)/anpr.pas
UNIT_SRC = $(UNITDIR)/tsanpr.pas $(UNITDIR)/stb_image.pas
STB_SRC = $(LIBDIR)/stb_image_lib.c

# Output files
ifeq ($(OS),Windows_NT)
    MAIN_EXE = $(BINDIR)/anpr.exe
    STB_LIB = $(BINDIR)/stb_image.dll
else
    MAIN_EXE = $(BINDIR)/anpr
    STB_LIB = $(BINDIR)/libstb_image.so
endif

# Default target
all: $(STB_LIB) $(MAIN_EXE)

# Create bin directory
$(BINDIR):
	mkdir -p $(BINDIR)

# Compile stb_image shared library
$(STB_LIB): $(STB_SRC) | $(BINDIR)
	-$(CC) $(CFLAGS) $(STB_SRC) -o $(STB_LIB) -lm 2>/dev/null || echo "Warning: stb_image library not compiled"

# Compile main executable
$(MAIN_EXE): $(MAIN_SRC) $(UNIT_SRC) | $(BINDIR)
	$(FPC) $(FPCFLAGS) -FE$(BINDIR) -FU$(BINDIR) -Fu$(UNITDIR) $(MAIN_SRC)

# Clean build artifacts
clean:
	rm -rf $(BINDIR)
	rm -f $(SRCDIR)/*.o $(SRCDIR)/*.ppu
	rm -f $(UNITDIR)/*.o $(UNITDIR)/*.ppu
	rm -f *.o *.ppu *.compiled

# Install (copy to system directories)
install: all
	@echo "Install target not implemented yet"

# Run the example
run: $(MAIN_EXE)
	cd $(BINDIR) && ./$(notdir $(MAIN_EXE))

# Check if Free Pascal is available
check:
	@which $(FPC) > /dev/null || (echo "Error: Free Pascal compiler not found" && exit 1)
	@echo "Free Pascal compiler found: $$($(FPC) -iV)"

# Help
help:
	@echo "TS-ANPR Pascal Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all     - Build all targets (default)"
	@echo "  clean   - Remove build artifacts"
	@echo "  run     - Build and run the example"
	@echo "  check   - Check if Free Pascal is available"
	@echo "  help    - Show this help message"

.PHONY: all clean install run check help
