#include "CBridge.h"
#include <opencv2/opencv.hpp>
#include <cstring>
#include <cstdlib>

// Helper: get pixel format string from cv::Mat
static const char *getPixelFormat(const cv::Mat &img)
{
    int channels = img.channels();
    if (channels == 1)
        return "GRAY";
    if (channels == 3)
        return "BGR";
    if (channels == 4)
        return "BGRA";
    return nullptr;
}

ImageBuffer load_image(const char *imgfile)
{
    ImageBuffer buf = {};
    // Read image using OpenCV (keep original channels)
    cv::Mat img = cv::imread(imgfile, cv::IMREAD_UNCHANGED);
    if (img.empty())
    {
        // Image load failed
        return buf;
    }

    const char *pixelFormat = getPixelFormat(img);
    if (!pixelFormat)
    {
        // Unsupported pixel format
        return buf;
    }

    buf.width = img.cols;
    buf.height = img.rows;
    buf.stride = static_cast<long>(img.step);
    buf.pixelFormat = pixelFormat;

    size_t totalBytes = buf.stride * buf.height;
    buf.data = (unsigned char *)malloc(totalBytes);
    if (!buf.data)
    {
        // Memory allocation failed
        return buf;
    }

    std::memcpy(buf.data, img.data, totalBytes);
    return buf;
}

void free_image_buffer(ImageBuffer buf)
{
    if (buf.data)
        free(buf.data);
}
