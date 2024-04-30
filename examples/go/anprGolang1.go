//
// 빌드 방법:
//  go build -o ..\bin\x64\anprGolang1.exe anprGolang1.go
// 실행 방법:
//  cd ..\bin\x64 && anprGolang1.exe
//

package main

import (
	"C"
	"fmt"
	"image"
	"image/jpeg"
	"io"
	"os"
	"syscall"
	"unsafe"
)

const IMG_PATH = "..\\..\\img\\"

var (
	dll              = syscall.NewLazyDLL("tsanpr.dll")
	anpr_initialize  = dll.NewProc("anpr_initialize")
	anpr_read_file   = dll.NewProc("anpr_read_file")
	anpr_read_pixels = dll.NewProc("anpr_read_pixels")
)

// Go String -> C string
func CString(s string) uintptr {
	return uintptr(unsafe.Pointer(C.CString(s)))
}

// C String -> Go string
func GoString(p uintptr) string {
	return C.GoString((*C.char)(unsafe.Pointer(p)))
}

func initialize(outputFormat string) string {
	error, _, _ := anpr_initialize.Call(CString(outputFormat))
	return GoString(error)
}

func readFile(imgfile, outputFormat, options string) {
	fmt.Printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
	result, _, _ := anpr_read_file.Call(CString(imgfile), CString(outputFormat), CString(options))
	fmt.Println(GoString(result))
}

// Get the bi-dimensional pixel array
func getPixels(file io.Reader) ([]byte, int, int, error) {
	img, _, err := image.Decode(file)

	if err != nil {
		return nil, 0, 0, err
	}

	bounds := img.Bounds()
	width := bounds.Dx()
	height := bounds.Dy()

	bytes := make([]byte, 0, width*height*3)
	for j := bounds.Min.Y; j < bounds.Max.Y; j++ {
		for i := bounds.Min.X; i < bounds.Max.X; i++ {
			r, g, b, _ := img.At(i, j).RGBA()
			bytes = append(bytes, byte(b>>8), byte(g>>8), byte(r>>8))
		}
	}

	return bytes, width, height, nil
}

func readPixels(imgfile, outputFormat, options string) {
	fmt.Printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)

	file, err := os.Open(imgfile)
	if err != nil {
		fmt.Printf("Failed to load image \"%s\"\n", imgfile)
		return
	}

	defer file.Close()

	pixels, width, height, err := getPixels(file)

	result, _, _ := anpr_read_pixels.Call(uintptr(unsafe.Pointer(&pixels[0])),
		uintptr(int32(width)), uintptr(int32(height)), 0, CString("BGR"), CString(outputFormat), CString(options))
	fmt.Println(GoString(result))
}

func anprDemo1(outputFormat string) {
	// anpr
	readFile(IMG_PATH + "licensePlate.jpg", outputFormat, "v")
	readFile(IMG_PATH + "licensePlate.jpg", outputFormat, "")
	readFile(IMG_PATH + "multiple.jpg", outputFormat, "vm")
	readFile(IMG_PATH + "multiple.jpg", outputFormat, "")
	readFile(IMG_PATH + "surround.jpg", outputFormat, "vms")
	readFile(IMG_PATH + "surround.jpg", outputFormat, "")

	// object detection
	readFile(IMG_PATH + "surround.jpg", outputFormat, "dms")
	readFile(IMG_PATH + "surround.jpg", outputFormat, "dmsr")
}

func anprDemo2(outputFormat string) {
	// anpr
	readPixels(IMG_PATH + "licensePlate.jpg", outputFormat, "v")
	readPixels(IMG_PATH + "licensePlate.jpg", outputFormat, "")
	readPixels(IMG_PATH + "multiple.jpg", outputFormat, "vm")
	readPixels(IMG_PATH + "multiple.jpg", outputFormat, "")
	readPixels(IMG_PATH + "surround.jpg", outputFormat, "vms")
	readPixels(IMG_PATH + "surround.jpg", outputFormat, "")

	// object detection
	readPixels(IMG_PATH + "surround.jpg", outputFormat, "dms")
	readPixels(IMG_PATH + "surround.jpg", outputFormat, "dmsr")
}

func main() {

	error := initialize("text")
	if error != "" {
		fmt.Println(error)
		os.Exit(1)
		return
	}

	anprDemo1("text")
	anprDemo1("json")
	anprDemo1("yaml")
	anprDemo1("xml")
	anprDemo1("csv")

	// 이미지 파일을 불러오기 위해 사용
	image.RegisterFormat("jpeg", "jpeg", jpeg.Decode, jpeg.DecodeConfig)

	anprDemo2("text")
	anprDemo2("json")
	anprDemo2("yaml")
	anprDemo2("xml")
	anprDemo2("csv")
}
