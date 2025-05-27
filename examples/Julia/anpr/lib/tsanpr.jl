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

module TSANPR

using Libdl

# Add these missing variable definitions
const _lib_path = Ref{String}("")
const _lib_loaded = Ref{Bool}(false)

function load_tsanpr(libpath::String)
    try
        # Test if library can be loaded
        handle = Libdl.dlopen(libpath)
        Libdl.dlclose(handle)
        
        _lib_path[] = libpath
        _lib_loaded[] = true
        return true
    catch e
        println("Failed to load TSANPR library: $libpath")
        println("Error: ", e)
        _lib_loaded[] = false
        return false
    end
end

function anpr_initialize(mode::String)
    if !_lib_loaded[]
        error("TSANPR library is not loaded.")
    end
    ptr = ccall((:anpr_initialize, _lib_path[]), Cstring, (Cstring,), mode)
    err = unsafe_string(ptr)
    if !isempty(err)
        error("anpr_initialize failed: $err")
    end
end

function anpr_read_file(imgfile::String, outputFormat::String, options::String)
    if !_lib_loaded[]
        error("TSANPR library is not loaded.")
    end
    ptr = ccall((:anpr_read_file, _lib_path[]), Cstring, (Cstring, Cstring, Cstring),
                imgfile, outputFormat, options)
    return unsafe_string(ptr)
end

function anpr_read_pixels(pixels::Vector{UInt8}, width::UInt32, height::UInt32, stride::Int32,
                         pixelFormat::String, outputFormat::String, options::String)
    if !_lib_loaded[]
        error("TSANPR library is not loaded.")
    end
    ptr = ccall((:anpr_read_pixels, _lib_path[]), Cstring,
                (Ptr{UInt8}, UInt32, UInt32, Int32, Cstring, Cstring, Cstring),
                pointer(pixels), width, height, stride, pixelFormat, outputFormat, options)
    return unsafe_string(ptr)
end

end # module