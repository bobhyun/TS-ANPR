// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

package stb_image

import "core:c"

// Link with precompiled stb_image implementation
when ODIN_OS == .Windows {
    foreign import stb_image "stb_image_impl.obj"
} else {
    foreign import stb_image "stb_image_impl.o"
}

@(default_calling_convention="c")
foreign stb_image {
    // Load image file and return pixel data
    // Returns nil on failure
    // desired_channels: 0=auto, 1=gray, 2=gray+alpha, 3=RGB, 4=RGBA
    @(link_name="stbi_load")
    load :: proc(filename: cstring, x: ^c.int, y: ^c.int, channels_in_file: ^c.int, desired_channels: c.int) -> [^]u8 ---

    // Free image data returned by load
    @(link_name="stbi_image_free")
    image_free :: proc(retval_from_stbi_load: rawptr) ---
}
