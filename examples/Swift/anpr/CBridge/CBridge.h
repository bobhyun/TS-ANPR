#pragma once

#ifdef _WIN32
#ifdef CBRIDGE_EXPORTS
#define CBRIDGE_API __declspec(dllexport)
#else
#define CBRIDGE_API __declspec(dllimport)
#endif
#else
#define CBRIDGE_API
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct
    {
        unsigned char *data;
        unsigned long width;
        unsigned long height;
        long stride;
        const char *pixelFormat; // "BGR", "BGRA", "GRAY"
    } ImageBuffer;

    CBRIDGE_API ImageBuffer load_image(const char *imgfile);
    CBRIDGE_API void free_image_buffer(ImageBuffer buf);

#ifdef __cplusplus
}
#endif
