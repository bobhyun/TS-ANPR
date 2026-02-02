// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

#ifndef TSANPR_WRAPPER_H
#define TSANPR_WRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif

// Load the TSANPR library dynamically
// Returns 0 on success, non-zero on failure
int tsanpr_load(const char* library_path);

// Unload the TSANPR library
void tsanpr_unload(void);

// Check if the library is loaded
int tsanpr_is_loaded(void);

// Wrapper functions for TSANPR API
const char* tsanpr_initialize(const char* mode);
const char* tsanpr_read_file(const char* img_file, const char* output_format, const char* options);
const char* tsanpr_read_pixels(const unsigned char* pixels, unsigned long long width,
                               unsigned long long height, long long stride,
                               const char* pixel_format, const char* output_format,
                               const char* options);

#ifdef __cplusplus
}
#endif

#endif // TSANPR_WRAPPER_H
