# Makefile for Pony ANPR example

# Compiler
PONYC = ponyc
CC = gcc

# Target executable name
TARGET = anpr

# Pony package directory
PKG_DIR = src

# FFI directory
FFI_DIR = ffi

# C wrapper files
WRAPPER_OBJ = $(FFI_DIR)/tsanpr_wrapper.o
IMAGE_OBJ = $(FFI_DIR)/image_loader.o
FFI_LIB = $(FFI_DIR)/libffi_wrapper.a

# Detect OS
ifeq ($(OS),Windows_NT)
    # Windows
    FFI_LIB = $(FFI_DIR)/ffi_wrapper.lib
    RM = del /Q
    RMDIR = rmdir /S /Q
    CC = cl
    CFLAGS = /c /O2
    AR = lib
    ARFLAGS = /OUT:$(FFI_LIB)
else
    # Linux/Unix
    RM = rm -f
    RMDIR = rm -rf
    CFLAGS = -c -O2 -fPIC
    AR = ar
    ARFLAGS = rcs
endif

# Build flags
BUILD_FLAGS = --debug

# Default target
all: $(TARGET)

# Compile C wrapper
$(WRAPPER_OBJ): $(FFI_DIR)/tsanpr_wrapper.c $(FFI_DIR)/tsanpr_wrapper.h
ifeq ($(OS),Windows_NT)
	$(CC) $(CFLAGS) /Fo$(WRAPPER_OBJ) $(FFI_DIR)/tsanpr_wrapper.c
else
	$(CC) $(CFLAGS) $(FFI_DIR)/tsanpr_wrapper.c -o $(WRAPPER_OBJ)
endif

# Compile image loader
$(IMAGE_OBJ): $(FFI_DIR)/image_loader.c $(FFI_DIR)/stb_image.h
ifeq ($(OS),Windows_NT)
	$(CC) $(CFLAGS) /Fo$(IMAGE_OBJ) $(FFI_DIR)/image_loader.c
else
	$(CC) $(CFLAGS) $(FFI_DIR)/image_loader.c -o $(IMAGE_OBJ)
endif

# Create static library from wrappers
$(FFI_LIB): $(WRAPPER_OBJ) $(IMAGE_OBJ)
ifeq ($(OS),Windows_NT)
	$(AR) $(ARFLAGS) $(WRAPPER_OBJ) $(IMAGE_OBJ)
else
	$(AR) $(ARFLAGS) $(FFI_LIB) $(WRAPPER_OBJ) $(IMAGE_OBJ)
endif

# Build the executable
$(TARGET): $(FFI_LIB) $(PKG_DIR)/*.pony
	$(PONYC) $(BUILD_FLAGS) -p $(FFI_DIR) -o . -b $(TARGET) $(PKG_DIR)

# Clean build artifacts
clean:
	$(RM) $(TARGET) $(TARGET).exe $(TARGET).o $(WRAPPER_OBJ) $(IMAGE_OBJ) $(FFI_LIB)
	$(RMDIR) .deps 2>/dev/null || true

# Run the example
run: $(TARGET)
	./$(TARGET)

# Build release version
release: $(FFI_LIB)
	$(PONYC) --release -p $(FFI_DIR) -o . -b $(TARGET) $(PKG_DIR)

# Install dependencies (if any)
deps:
	@echo "No external dependencies required"

.PHONY: all clean run release deps
