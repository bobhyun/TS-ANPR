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

/// Global TSANPR instance
var tsanpr: c.TSANPR = undefined;

/// Base directory for examples
const examplesBaseDir = "../../";

/// Generate engine filename depending on platform
fn getEngineFileName() ?[]const u8 {
    return switch (builtin.os.tag) {
        .windows => switch (builtin.cpu.arch) {
            .x86_64 => examplesBaseDir ++ "bin\\windows-x86_64\\tsanpr.dll",
            .x86 => examplesBaseDir ++ "bin\\windows-x86\\tsanpr.dll",
            else => null,
        },
        .linux => switch (builtin.cpu.arch) {
            .x86_64 => examplesBaseDir ++ "bin/linux-x86_64/libtsanpr.so",
            .aarch64 => examplesBaseDir ++ "bin/linux-aarch64/libtsanpr.so",
            else => null,
        },
        else => null,
    };
}

/// Convert UTF-8 string to wchar_t* on Windows
fn toWstring(allocator: std.mem.Allocator, str: []const u8) ![:0]const u16 {
    if (builtin.os.tag != .windows) return error.NotWindows;
    
    // Validate UTF-8 first
    if (!std.unicode.utf8ValidateSlice(str)) {
        std.log.err("Invalid UTF-8 string: {s}", .{str});
        return error.InvalidUtf8;
    }
    
    return std.unicode.utf8ToUtf16LeAllocZ(allocator, str) catch |err| {
        std.log.err("Failed to convert UTF-8 to UTF-16: {s} (error: {})", .{str, err});
        return err;
    };
}

/// Read image file using anpr_read_file
fn readImageFile(imgfile: []const u8, outputFormat: ?[]const u8, options: ?[]const u8) ![]const u8 {
    std.log.info("readImageFile: {s} (outputFormat: {?s}, options: {?s})", .{
        imgfile, outputFormat, options
    });
    
    const result = if (tsanpr.anpr_read_file) |read_fn| read_fn(
        imgfile.ptr,
        if (outputFormat) |fmt| fmt.ptr else null,
        if (options) |opt| opt.ptr else null
    ) else null;
    
    if (result == null) {
        return error.ReadFailed;
    }
    
    return std.mem.span(result);
}

/// Read encoded image as binary and call anpr_read_pixels
fn readEncodedImage(allocator: std.mem.Allocator, imgfile: []const u8, outputFormat: ?[]const u8, options: ?[]const u8) !void {
    var file = try std.fs.cwd().openFile(imgfile, .{});
    defer file.close();
    
    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);
    
    _ = try file.readAll(buffer);
    
    // In a real implementation, you would decode the image and call anpr_read_pixels
    _ = try readImageFile(imgfile, outputFormat, options);
}

/// Get file extension in lowercase
fn getLowercaseExt(filename: []const u8, buffer: []u8) ![]const u8 {
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot_pos| {
        const ext = filename[dot_pos + 1 ..];
        for (ext, 0..) |char, i| {
            buffer[i] = std.ascii.toLower(char);
        }
        return buffer[0..ext.len];
    }
    return "";
}

/// Get pixel format string for tsanpr based on channels
fn getPixelFormat(channels: u8) ?[]const u8 {
    return switch (channels) {
        1 => "GRAY",
        2 => "BGR565",
        3 => "BGR",
        4 => "BGRA",
        else => null,
    };
}

/// Read pixel buffer from image file
fn readPixelBuffer(_: std.mem.Allocator, imgfile: []const u8, outputFormat: ?[]const u8, options: ?[]const u8) !void {
    var ext_buf: [16]u8 = undefined;
    _ = try getLowercaseExt(imgfile, &ext_buf);
    
    // In a real implementation, you would read the actual image data
    // For now, we'll just call readImageFile as a fallback
    _ = try readImageFile(imgfile, outputFormat, options);
}

/// Read license plates from images based on C implementation
fn readLicensePlates(allocator: std.mem.Allocator, countryCode: []const u8) !void {
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.
    const init_params = try std.fmt.allocPrint(allocator, "text;country={s}", .{countryCode});
    defer allocator.free(init_params);
    
    const init_result = if (tsanpr.anpr_initialize) |init_fn| init_fn(init_params.ptr) else null;
    if (init_result != null and init_result.?[0] != 0) {
        std.log.err("anpr_initialize() failed (error={s})", .{init_result.?});
        return error.InitializationFailed;
    }
    
    const image_dir = try std.fmt.allocPrint(allocator, "{s}img/{s}/", .{examplesBaseDir, countryCode});
    defer allocator.free(image_dir);
    
    // Try each output format as needed
    const outputFormat = "text";
    // const outputFormat = "json";
    // const outputFormat = "yaml";
    // const outputFormat = "xml";
    // const outputFormat = "csv";
    
    var path_buf: [1024]u8 = undefined;
    
    // Single license plate recognition (default)
    const single_path = try std.fmt.bufPrint(&path_buf, "{s}licensePlate.jpg", .{image_dir});
    
    // Test with absolute path
    const abs_path = try std.fs.cwd().realpathAlloc(allocator, single_path);
    defer allocator.free(abs_path);
    
    std.log.info("Trying absolute path: {s}", .{abs_path});
    if (readImageFile(abs_path, outputFormat, "")) |result| {
        std.log.info("{s} => {s}", .{abs_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_path, err});
        
        // Also try the relative path
        if (readImageFile(single_path, outputFormat, "")) |result| {
            std.log.info("{s} => {s}", .{single_path, result});
        } else |err2| {
            std.log.err("Failed to process {s}: {}", .{single_path, err2});
        }
    }
    
    // Recognize multiple license plates attached to vehicles
    const multiple_path = try std.fmt.bufPrint(&path_buf, "{s}multiple.jpg", .{image_dir});
    const abs_multiple_path = try std.fs.cwd().realpathAlloc(allocator, multiple_path);
    defer allocator.free(abs_multiple_path);
    
    if (readImageFile(abs_multiple_path, outputFormat, "vm")) |result| {
        std.log.info("{s} (vm) => {s}", .{abs_multiple_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_multiple_path, err});
    }
    
    // Recognize multiple license plates including motorcycles
    if (readImageFile(abs_multiple_path, outputFormat, "vmb")) |result| {
        std.log.info("{s} (vmb) => {s}", .{abs_multiple_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_multiple_path, err});
    }
    
    // Recognize multiple license plates with surround detection
    const surround_path = try std.fmt.bufPrint(&path_buf, "{s}surround.jpg", .{image_dir});
    const abs_surround_path = try std.fs.cwd().realpathAlloc(allocator, surround_path);
    defer allocator.free(abs_surround_path);
    
    if (readImageFile(abs_surround_path, outputFormat, "vms")) |result| {
        std.log.info("{s} (vms) => {s}", .{abs_surround_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_surround_path, err});
    }
    
    // Recognize multiple surrounding objects (vehicles)
    if (readImageFile(abs_surround_path, outputFormat, "dms")) |result| {
        std.log.info("{s} (dms) => {s}", .{abs_surround_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_surround_path, err});
    }
    
    // Recognize multiple surrounding objects and license plates
    if (readImageFile(abs_surround_path, outputFormat, "dmsr")) |result| {
        std.log.info("{s} (dmsr) => {s}", .{abs_surround_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_surround_path, err});
    }
    
    // Recognize multiple surrounding objects and license plates within RoI
    if (readImageFile(abs_surround_path, outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")) |result| {
        std.log.info("{s} (dmsri549,700,549,2427,1289,2427,1289,700) => {s}", .{abs_surround_path, result});
    } else |err| {
        std.log.err("Failed to process {s}: {}", .{abs_surround_path, err});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    
    const allocator = gpa.allocator();
    
    // Load TSANPR library
    const engine_file = getEngineFileName() orelse {
        std.log.err("Unsupported platform", .{});
        return error.UnsupportedPlatform;
    };
    
    std.log.info("Attempting to load TSANPR library from: {s}", .{engine_file});
    
    if (builtin.os.tag == .windows) {
        const wpath = toWstring(allocator, engine_file) catch |err| {
            std.log.err("Failed to convert path to Windows format: {} for path: {s}", .{err, engine_file});
            std.log.err("This usually means the path contains invalid UTF-8 characters", .{});
            std.log.err("Try moving the project to a path with only ASCII characters", .{});
            return error.PathConversionFailed;
        };
        defer allocator.free(wpath);
        
        if (c.TSANPR_load(&tsanpr, wpath.ptr) != 0) {
            std.log.err("Failed to load TSANPR library from {s}", .{engine_file});
            return error.LibraryLoadFailed;
        }
    } else {
        if (c.TSANPR_load(&tsanpr, engine_file.ptr) != 0) {
            std.log.err("Failed to load TSANPR library from {s}", .{engine_file});
            return error.LibraryLoadFailed;
        }
    }
    
    defer c.TSANPR_unload();
    
    // Try each country code as needed (based on C implementation)
    try readLicensePlates(allocator, "KR");
    // try readLicensePlates(allocator, "JP");
    // try readLicensePlates(allocator, "VN");
}
