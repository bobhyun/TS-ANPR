/**
* The MIT License (MIT)
* Copyright © 2022-2025 TS-Solution Corp.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to all conditions.
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
**/

// main.go
package main

import (
	"anpr/tsanpr"
	"fmt"
	"io/ioutil"
	"runtime"
	"unsafe"

	"gocv.io/x/gocv"
)

const examplesBaseDir = "../.."

func getEngineFileName() string {
	switch runtime.GOOS {
	case "windows":
		switch runtime.GOARCH {
		case "386":
			return examplesBaseDir + "/bin/windows-x86/tsanpr.dll"
		case "amd64":
			return examplesBaseDir + "/bin/windows-x86_64/tsanpr.dll"
		}
	case "linux":
		switch runtime.GOARCH {
		case "amd64":
			return examplesBaseDir + "/bin/linux-x86_64/libtsanpr.so"
		case "arm64":
			return examplesBaseDir + "/bin/linux-aarch64/libtsanpr.so"
		}
	}
	return ""
}

func readImageFile(ts *tsanpr.TSANPR, imgfile, outputFormat, options string) {
	fmt.Printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
	result := ts.AnprReadFile(imgfile, outputFormat, options)
	fmt.Println(result)
}

func readEncodedImage(ts *tsanpr.TSANPR, imgfile, outputFormat, options string) {
	fmt.Printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
	encodedImg, err := ioutil.ReadFile(imgfile)
	if err != nil {
		fmt.Printf("\nERROR: %v\n", err)
		return
	}
	ptr := unsafe.Pointer(&encodedImg[0])
	result := ts.AnprReadPixels(ptr, uint64(len(encodedImg)), 0, 0, "encoded", outputFormat, options)
	fmt.Println(result)
}

func getPixelFormat(img gocv.Mat) string {
	channels := img.Channels()
	switch channels {
	case 1:
		return "GRAY"
	case 2:
		return "BGR565" // or "BGR555"
	case 3:
		return "BGR"
	case 4:
		return "BGRA"
	default:
		return ""
	}
}

func readPixelBuffer(ts *tsanpr.TSANPR, imgfile, outputFormat, options string) {
	fmt.Printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
	img := gocv.IMRead(imgfile, gocv.IMReadUnchanged)
	defer img.Close()
	if img.Empty() {
		fmt.Println("Image load failed!")
		return
	}
	pixelFormat := getPixelFormat(img)
	if pixelFormat == "" {
		fmt.Println("Unknown pixel format!")
		return
	}
	height := img.Rows()
	width := img.Cols()
	stride := int64(img.Step())
	ptr := unsafe.Pointer(img.DataPtrUint8())
	result := ts.AnprReadPixels(ptr, uint64(width), uint64(height), stride, pixelFormat, outputFormat, options)
	fmt.Println(result)
}

func readLicensePlates(ts *tsanpr.TSANPR, countryCode string) {
	// NOTICE:
	// anpr_initialize should be called only once after library load.
	// Therefore, it is not possible to change the country code after anpr_initialize has been called.
	// While using the free trial license, you can try all languages.
	// When you purchase a commercial license, you can only use the selected language.

	errorMsg := ts.AnprInitialize("text;country=" + countryCode)
	if errorMsg != "" {
		fmt.Printf("anpr_initialize() failed: %s\n", errorMsg)
		return
	}

	imageDir := examplesBaseDir
	imageDir += "/img/" + countryCode + "/"

	// TODO: Try each function as needed
	anprFunc := readImageFile
	// anprFunc := readEncodedImage
	// anprFunc := readPixelBuffer

	// TODO: Try each output format as needed
	outputFormat := "text"
	// outputFormat = "json"
	// outputFormat = "yaml"
	// outputFormat = "xml"
	// outputFormat = "csv"

	anprFunc(ts, imageDir+"licensePlate.jpg", outputFormat, "") // Single license plate recognition (default)
	anprFunc(ts, imageDir+"multiple.jpg", outputFormat, "vm")   // Recognize multiple license plates attached to vehicles
	anprFunc(ts, imageDir+"multiple.jpg", outputFormat, "vmb")  // Recognize multiple license plates attached to vehicles (including motorcycles)
	anprFunc(ts, imageDir+"surround.jpg", outputFormat, "vms")  // Recognize multiple license plates attached to vehicles with surround detection
	anprFunc(ts, imageDir+"surround.jpg", outputFormat, "dms")  // Recognize multiple surrounding objects (vehicles)
	anprFunc(ts, imageDir+"surround.jpg", outputFormat, "dmsr") // Recognize multiple surrounding objects (vehicles) and license plates

	// Recognize multiple surrounding objects and license plates within RoI
    anprFunc(ts, imageDir+"surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")
}

func main() {
	engineFileName := getEngineFileName()
	if engineFileName == "" {
		fmt.Println("Unsupported operating system")
		return
	}
	fmt.Printf("engine file name: %s\n", engineFileName)
	tsanprObj, err := tsanpr.LoadLibrary(engineFileName)
	if err != nil {
		fmt.Printf("TSANPR initialization failed: %v\n", err)
		return
	}
	defer tsanprObj.Dispose()

	// TODO: Try each country code as needed
	readLicensePlates(tsanprObj, "KR")
	// readLicensePlates(tsanprObj, "JP")
	// readLicensePlates(tsanprObj, "VN")
}
