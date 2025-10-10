///
/// The MIT License (MIT)
/// Copyright © 2022-2025 TS-Solution Corp.
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to deal
/// in the Software without restriction, including without limitation the rights
/// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
/// copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to all conditions.
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
/// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
/// SOFTWARE.
///

const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("tsanpr.h");
});

/// Error types for the TSANPR wrapper
pub const Error = error{
    /// Failed to initialize the TSANPR library
    InitializationFailed,
    /// The requested platform/architecture is not supported
    UnsupportedPlatform,
    /// Failed to load the TSANPR library
    LibraryLoadFailed,
    /// Invalid arguments were provided
    InvalidArguments,
    /// Failed to read or process the image file
    ImageProcessingFailed,
    /// The operation is not supported
    NotSupported,
};

/// Represents the TSANPR instance
pub const TSANPR = struct {
    const Self = @This();
    
    /// Internal C TSANPR instance
    tsanpr: c.TSANPR,
    
    /// Initialize a new TSANPR instance
    pub fn init() !Self {
        var tsanpr: c.TSANPR = undefined;
        
        // Load the TSANPR library
        const engine_file = try getEngineFileName();
        
        if (builtin.os.tag == .windows) {
            const wpath = try std.unicode.utf8ToUtf16LeWithNull(std.heap.page_allocator, engine_file);
            defer std.heap.page_allocator.free(wpath);
            
            if (c.TSANPR_load(&tsanpr, wpath.ptr) != 0) {
                return Error.LibraryLoadFailed;
            }
        } else {
            if (c.TSANPR_load(&tsanpr, engine_file) != 0) {
                return Error.LibraryLoadFailed;
            }
        }
        
        // Initialize the library
        const mode = "default";
        const result = tsanpr.anpr_initialize(mode);
        if (result == null) {
            c.TSANPR_unload();
            return Error.InitializationFailed;
        }
        
        return Self{ .tsanpr = tsanpr };
    }
    
    /// Deinitialize the TSANPR instance and release resources
    pub fn deinit(self: *Self) void {
        _ = self; // Mark as used
        c.TSANPR_unload();
    }
    
    /// Read and process an image file
    pub fn readFile(self: *Self, file_path: []const u8, output_format: []const u8, options: []const u8) ![]const u8 {
        const result = self.tsanpr.anpr_read_file(
            file_path.ptr,
            if (output_format.len > 0) output_format.ptr else null,
            if (options.len > 0) options.ptr else null
        );
        
        if (result == null) {
            return Error.ImageProcessingFailed;
        }
        
        return std.mem.span(result);
    }
    
    /// Process image data from memory
    pub fn readPixels(
        self: *Self,
        pixels: []const u8,
        width: u32,
        height: u32,
        stride: i32,
        pixel_format: []const u8,
        output_format: []const u8,
        options: []const u8
    ) ![]const u8 {
        const result = self.tsanpr.anpr_read_pixels(
            pixels.ptr,
            width,
            height,
            stride,
            pixel_format.ptr,
            if (output_format.len > 0) output_format.ptr else null,
            if (options.len > 0) options.ptr else null
        );
        
        if (result == null) {
            return Error.ImageProcessingFailed;
        }
        
        return std.mem.span(result);
    }
};

/// Get the platform-specific engine file name
fn getEngineFileName() ![:0]const u8 {
    const base_path = "../../bin";
    
    const path = switch (builtin.os.tag) {
        .windows => switch (builtin.cpu.arch) {
            .x86_64 => try std.fmt.allocPrintZ(std.heap.page_allocator, "{s}\\windows-x86_64\\tsanpr.dll", .{base_path}),
            .x86 => try std.fmt.allocPrintZ(std.heap.page_allocator, "{s}\\windows-x86\\tsanpr.dll", .{base_path}),
            else => return Error.UnsupportedPlatform,
        },
        .linux => switch (builtin.cpu.arch) {
            .x86_64 => try std.fmt.allocPrintZ(std.heap.page_allocator, "{s}/linux-x86_64/libtsanpr.so", .{base_path}),
            .aarch64 => try std.fmt.allocPrintZ(std.heap.page_allocator, "{s}/linux-aarch64/libtsanpr.so", .{base_path}),
            else => return Error.UnsupportedPlatform,
        },
        else => return Error.UnsupportedPlatform,
    };
    
    return path;
}

// Tests
const testing = std.testing;

test "TSANPR initialization and cleanup" {
    var anpr = try TSANPR.init();
    defer anpr.deinit();
    
    // Basic test to ensure initialization works
    try testing.expect(@ptrToInt(anpr.tsanpr.anpr_initialize) != 0);
}

test "getEngineFileName returns valid path" {
    const path = try getEngineFileName();
    
    // Verify the path contains the expected components
    try testing.expect(std.mem.indexOf(u8, path, "tsanpr") != null);
    
    // Verify the file extension matches the platform
    if (builtin.os.tag == .windows) {
        try testing.expect(std.mem.endsWith(u8, path, ".dll"));
    } else if (builtin.os.tag == .linux) {
        try testing.expect(std.mem.endsWith(u8, path, ".so"));
    }
}
