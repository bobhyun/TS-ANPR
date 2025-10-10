# TS-ANPR Ada Makefile
# Copyright (c) 2025 TS-Solution Corp.

# Compiler and tools
GNATMAKE = gnatmake
GPRBUILD = gprbuild
PROJECT = anpr.gpr

# Directories
SRCDIR = src
OBJDIR = obj
BINDIR = bin

# Source files
MAIN_SRC = anpr.adb

# Output files
ifeq ($(OS),Windows_NT)
    MAIN_EXE = $(BINDIR)/anpr.exe
    LDFLAGS = -lkernel32
else
    MAIN_EXE = $(BINDIR)/anpr
    LDFLAGS = -ldl
endif

# Default target
all: $(MAIN_EXE)

# Create directories
$(OBJDIR):
	mkdir -p $(OBJDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

# Compile using GPRbuild if available
$(MAIN_EXE): $(MAIN_SRC) | $(OBJDIR) $(BINDIR)
	@if command -v $(GPRBUILD) >/dev/null 2>&1; then \
		echo "Using GPRbuild..."; \
		$(GPRBUILD) -p -P $(PROJECT) -XOS=$(OS); \
	else \
		echo "Using gnatmake..."; \
		$(GNATMAKE) -D $(OBJDIR) -o $@ $(MAIN_SRC) -largs $(LDFLAGS); \
	fi

# Clean build artifacts
clean:
	rm -rf $(OBJDIR) $(BINDIR)
	rm -f *.ali *.o

# Install (copy to system directories)
install: all
	@echo "Install target not implemented yet"

# Run the example
run: $(MAIN_EXE)
	cd $(BINDIR) && ./$(notdir $(MAIN_EXE))

# Check if GNAT is available
check:
	@which $(GNATMAKE) > /dev/null || (echo "Error: GNAT compiler not found" && exit 1)
	@echo "GNAT compiler found: $$($(GNATMAKE) --version | head -1)"

# Help
help:
	@echo "TS-ANPR Ada Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all     - Build all targets (default)"
	@echo "  clean   - Remove build artifacts"
	@echo "  run     - Build and run the example"
	@echo "  check   - Check if GNAT is available"
	@echo "  help    - Show this help message"

.PHONY: all clean install run check help