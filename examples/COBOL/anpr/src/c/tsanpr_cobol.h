/*
 * The MIT License (MIT)
 * Copyright © 2022-2025 TS-Solution Corp.
 *
 * COBOL-friendly wrapper for TS-ANPR engine
 */

#ifndef TSANPR_COBOL_H
#define TSANPR_COBOL_H

#ifdef _WIN32
    #define EXPORT __declspec(dllexport)
#else
    #define EXPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Initialize ANPR engine with options
 * Parameters:
 *   options - COBOL string with initialization options (e.g., "text;country=KR")
 *   options_len - Length of options buffer
 *   result - COBOL string buffer for result message
 *   result_len - Length of result buffer
 * Returns: 0 on success, negative error code on failure
 */
EXPORT int tsanpr_cobol_initialize(char* options, int options_len,
                                    char* result, int result_len);

/* Read image file and perform ANPR
 * Parameters:
 *   image_path - COBOL string with path to image file
 *   image_path_len - Length of image_path buffer
 *   output_format - COBOL string with format ("text", "json", etc.)
 *   output_format_len - Length of output_format buffer
 *   options - COBOL string with recognition options (e.g., "vm" for multi-vehicle)
 *   options_len - Length of options buffer
 *   result - COBOL string buffer for recognition result
 *   result_len - Length of result buffer
 * Returns: 0 on success, negative error code on failure
 */
EXPORT int tsanpr_cobol_read_file(char* image_path, int image_path_len,
                                   char* output_format, int output_format_len,
                                   char* options, int options_len,
                                   char* result, int result_len);

/* Cleanup resources */
EXPORT void tsanpr_cobol_cleanup(void);

#ifdef __cplusplus
}
#endif

#endif /* TSANPR_COBOL_H */