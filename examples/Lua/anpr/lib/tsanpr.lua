--[[
The MIT License (MIT)
Copyright © 2022-2025 TS-Solution Corp.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to all conditions.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]

local ffi = require("ffi")

-- FFI signatures for TSANPR DLL/so
ffi.cdef[[
const char *anpr_initialize(const char *mode);
const char *anpr_read_file(const char *imgFileName, const char *outputFormat, const char *options);
const char *anpr_read_pixels(
    const unsigned char *pixels,
    unsigned long width,
    unsigned long height,
    long stride,
    const char *pixelFormat,
    const char *outputFormat,
    const char *options
);
]]

-- Load TSANPR library from a given path
local function loadTSANPR(libName)
    local tsanpr = ffi.load(libName)

    -- Initialize the TSANPR engine
    local function initialize(mode)
        local err = tsanpr.anpr_initialize(mode)
        if err ~= nil and ffi.string(err) ~= "" then
            error("anpr_initialize failed: " .. ffi.string(err))
        end
    end

    -- Recognize license plate from image file
    local function recognize_file(imgFileName, outputFormat, options)
        local result = tsanpr.anpr_read_file(imgFileName, outputFormat, options)
        return ffi.string(result)
    end

    -- Recognize license plate from raw pixel buffer
    local function recognize_pixels(pixels, width, height, stride, pixelFormat, outputFormat, options)
        local result = tsanpr.anpr_read_pixels(
            pixels, width, height, stride, pixelFormat, outputFormat, options
        )
        return ffi.string(result)
    end

    -- Return only the API functions (do not expose the raw tsanpr FFI object)
    return {
        initialize = initialize,
        recognize_file = recognize_file,
        recognize_pixels = recognize_pixels
    }
end

return loadTSANPR
