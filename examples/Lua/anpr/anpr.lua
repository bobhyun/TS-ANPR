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

local loadTSANPR = require("lib.tsanpr")
local examplesBaseDir = "../.."

-- Generate engine filename depending on platform
local function getEngineFileName()
    if jit.os == "Windows" then
        return examplesBaseDir .. "/bin/windows-x86_64/tsanpr.dll"
    elseif jit.os == "Linux" then
        return examplesBaseDir .. "/bin/linux-x86_64/libtsanpr.so"
    else
        error("Unsupported OS: " .. jit.os)
    end
end

-- Read image file and recognize license plate
local function readImageFile(tsanpr, imgfile, outputFormat, options)
    print(string.format('%s (outputFormat="%s", options="%s") => ', imgfile, outputFormat, options))
    local result = tsanpr.recognize_file(imgfile, outputFormat, options)
    print(result)
end

-- Read encoded image file (raw bytes, e.g. JPEG/PNG)
local function readEncodedImage(tsanpr, imgfile, outputFormat, options)
    print(string.format('%s (outputFormat="%s", options="%s") => ', imgfile, outputFormat, options))
    local f = assert(io.open(imgfile, "rb"))
    local encodedImg = f:read("*all")
    f:close()
    local ffi = require("ffi")
    local cdata = ffi.new("uint8_t[?]", #encodedImg, encodedImg)
    local result = tsanpr.recognize_pixels(
        cdata,
        #encodedImg,
        0,
        0,
        "encoded",
        outputFormat,
        options
    )
    print(result)
end

-- Read pixel buffer (for example, from OpenCV or other raw image loader)
-- This is a stub; in real use, you would use a Lua OpenCV binding or FFI to load the image and get pixel buffer, width, height, stride, pixelFormat
local function readPixelBuffer(tsanpr, imgfile, outputFormat, options)
    print(string.format('%s (outputFormat="%s", options="%s") => ', imgfile, outputFormat, options))
    -- For demonstration, we just print a warning
    print("readPixelBuffer: Not implemented in pure Lua. Use OpenCV via FFI or a Lua image library.")
end

-- Recognize license plates for a given country code
local function readLicensePlates(tsanpr, countryCode)
    -- NOTICE:
    -- anpr_initialize should be called only once after library load.
    -- Therefore, it is not possible to change the country code after anpr_initialize has been called.
    -- While using the free trial license, you can try all languages.
    -- When you purchase a commercial license, you can only use the selected language.
    local initParams = "text;country=" .. countryCode
    tsanpr.initialize(initParams)

    local imageDir = string.format("%s/img/%s/", examplesBaseDir, countryCode)

    -- TODO: Try each function as needed
    -- local anprFunc = readImageFile
    local anprFunc = readEncodedImage
    -- local anprFunc = readPixelBuffer

    -- TODO: Try each output format as needed
    local outputFormat = "text"
    -- local outputFormat = "json"
    -- local outputFormat = "yaml"
    -- local outputFormat = "xml"
    -- local outputFormat = "csv"

    anprFunc(tsanpr, imageDir .. "licensePlate.jpg", outputFormat, "") -- Single license plate recognition (default)
    anprFunc(tsanpr, imageDir .. "multiple.jpg", outputFormat, "vm") -- Recognize multiple license plates attached to vehicles
    anprFunc(tsanpr, imageDir .. "multiple.jpg", outputFormat, "vmb") -- Recognize multiple license plates including motorcycles
    anprFunc(tsanpr, imageDir .. "surround.jpg", outputFormat, "vms") -- Recognize multiple license plates with surround detection
    anprFunc(tsanpr, imageDir .. "surround.jpg", outputFormat, "dms") -- Recognize multiple surrounding objects (vehicles)
    anprFunc(tsanpr, imageDir .. "surround.jpg", outputFormat, "dmsr") -- Recognize multiple surrounding objects and license plates
    -- Recognize multiple surrounding objects and license plates within RoI
    anprFunc(tsanpr, imageDir .. "surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")
end

-- Main entry point
local function main()
    local engineFileName = getEngineFileName()
    local tsanpr = loadTSANPR(engineFileName)

    -- TODO: Try each country code as needed
    readLicensePlates(tsanpr, "KR")
    -- readLicensePlates(tsanpr, "JP")
    -- readLicensePlates(tsanpr, "VN")
end

main()
