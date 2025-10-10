! The MIT License (MIT)
! Copyright © 2022-2025 TS-Solution Corp.
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to all conditions.
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

program anpr_example
    use iso_c_binding
    use tsanpr_module
    implicit none
    
    character(len=512) :: engine_file_name
    type(tsanpr_handle) :: tsanpr
    integer :: status
    logical :: file_exists

    ! Get engine file name based on platform
    call get_engine_file_name(engine_file_name)

    if (len_trim(engine_file_name) == 0) then
        print *, "Unsupported operating system or engine not found"
        stop 1
    end if

    ! Check if engine file exists
    inquire(file=trim(engine_file_name), exist=file_exists)
    if (.not. file_exists) then
        print *, "Engine file not found: ", trim(engine_file_name)
        stop 1
    end if
    
    ! Initialize TSANPR
    call tsanpr_init(tsanpr, trim(engine_file_name), status)
    if (status /= 0) then
        print *, "TSANPR initialization failed"
        stop 1
    end if
    
    ! TODO: Try each country code as needed
    call read_license_plates(tsanpr, "KR")
    ! call read_license_plates(tsanpr, "JP")
    ! call read_license_plates(tsanpr, "VN")
    
    ! Cleanup
    call tsanpr_cleanup(tsanpr)
    
end program anpr_example

! Generate engine filename depending on platform and architecture
subroutine get_engine_file_name(engine_file_name)
    implicit none
    character(len=*), intent(out) :: engine_file_name
    character(len=256) :: examples_base_dir
    
    ! Get examples base directory (simplified - assumes relative path)
    examples_base_dir = "../.."
    
#ifdef _WIN32
    ! Windows platform
#ifdef _WIN64
    ! 64-bit Windows
    engine_file_name = trim(examples_base_dir) // "/bin/windows-x86_64/tsanpr.dll"
#else
    ! 32-bit Windows  
    engine_file_name = trim(examples_base_dir) // "/bin/windows-x86/tsanpr.dll"
#endif
#elif defined(__linux__)
    ! Linux platform
#ifdef __x86_64__
    ! 64-bit Linux
    engine_file_name = trim(examples_base_dir) // "/bin/linux-x86_64/libtsanpr.so"
#elif defined(__aarch64__)
    ! ARM64 Linux
    engine_file_name = trim(examples_base_dir) // "/bin/linux-aarch64/libtsanpr.so"
#else
    engine_file_name = ""
#endif
#else
    engine_file_name = ""
#endif
end subroutine get_engine_file_name

! Read an image file and call anpr_read_file
subroutine read_image_file(tsanpr, imgfile, output_format, options)
    use tsanpr_module
    implicit none
    type(tsanpr_handle), intent(in) :: tsanpr
    character(len=*), intent(in) :: imgfile, output_format, options
    character(len=4096) :: result
    integer :: status
    
    write(*, '(A, A, A, A, A, A, A)', advance='no') &
        trim(imgfile), ' (outputFormat="', trim(output_format), &
        '", options="', trim(options), '") => '
    
    call tsanpr_read_file(tsanpr, trim(imgfile), trim(output_format), &
                         trim(options), result, status)
    
    if (status == 0) then
        print *, trim(result)
    else
        print *, "Error reading file"
    end if
end subroutine read_image_file

! Read an encoded image file as bytes and call tsanpr_read_pixels with 'encoded' pixel format
subroutine read_encoded_image(tsanpr, imgfile, output_format, options)
    use tsanpr_module
    implicit none
    type(tsanpr_handle), intent(in) :: tsanpr
    character(len=*), intent(in) :: imgfile, output_format, options
    character(len=4096) :: result
    integer :: status, file_unit, file_size, ios
    integer(c_int8_t), allocatable, target :: encoded_img(:)
    logical :: file_exists
    
    write(*, '(A, A, A, A, A, A, A)', advance='no') &
        trim(imgfile), ' (outputFormat="', trim(output_format), &
        '", options="', trim(options), '") => '
    
    ! Check if file exists
    inquire(file=trim(imgfile), exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "File does not exist"
        return
    end if
    
    ! Allocate buffer and read file
    allocate(encoded_img(file_size))
    
    open(newunit=file_unit, file=trim(imgfile), access='stream', &
         form='unformatted', status='old', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file"
        deallocate(encoded_img)
        return
    end if
    
    read(file_unit, iostat=ios) encoded_img
    close(file_unit)
    
    if (ios /= 0) then
        print *, "Error reading file"
        deallocate(encoded_img)
        return
    end if
    
    ! Process encoded image
    call tsanpr_read_pixels(tsanpr, c_loc(encoded_img), int(file_size, c_int64_t), &
                           0_c_int64_t, 0_c_int64_t, "encoded", &
                           trim(output_format), trim(options), result, status)
    
    if (status == 0) then
        print *, trim(result)
    else
        print *, "Error processing encoded image"
    end if
    
    deallocate(encoded_img)
end subroutine read_encoded_image

! Determine pixel format string based on image channels
function get_pixel_format(channels) result(pixel_format)
    implicit none
    integer, intent(in) :: channels
    character(len=16) :: pixel_format
    
    select case(channels)
    case(1)
        pixel_format = "GRAY"
    case(2)
        pixel_format = "BGR565"  ! or "BGR555"
    case(3)
        pixel_format = "BGR"
    case(4)
        pixel_format = "BGRA"
    case default
        pixel_format = ""
    end select
end function get_pixel_format

! Use the pixel buffer-based ANPR function
! Note: This is a simplified version. In a real implementation,
! you would need to integrate with an image processing library.
subroutine read_pixel_buffer(tsanpr, imgfile, output_format, options)
    use tsanpr_module
    implicit none
    type(tsanpr_handle), intent(in) :: tsanpr
    character(len=*), intent(in) :: imgfile, output_format, options
    integer :: dummy

    write(*, '(A, A, A, A, A, A, A)', advance='no') &
        trim(imgfile), ' (outputFormat="', trim(output_format), &
        '", options="', trim(options), '") => '

    print *, "Pixel buffer reading not implemented in this example"

    ! Avoid unused parameter warning
    if (c_associated(tsanpr%lib_handle)) dummy = 0
end subroutine read_pixel_buffer

! NOTICE:
! anpr_initialize should be called only once after library load.
! Therefore, it is not possible to change the country code after anpr_initialize has been called.
! While using the free trial license, you can try all languages.
! When you purchase a commercial license, you can only use the selected language.
subroutine read_license_plates(tsanpr, country_code)
    use tsanpr_module
    implicit none
    type(tsanpr_handle), intent(in) :: tsanpr
    character(len=*), intent(in) :: country_code
    character(len=256) :: init_params, image_dir, image_path
    character(len=4096) :: error_msg
    integer :: status
    
    ! Initialize ANPR engine
    init_params = "text;country=" // trim(country_code)
    call tsanpr_initialize(tsanpr, trim(init_params), error_msg, status)
    if (status /= 0) then
        print *, "anpr_initialize() failed: ", trim(error_msg)
        return
    end if
    
    ! Set image directory
    image_dir = "../../img/" // trim(country_code)
    
    ! TODO: Try each function as needed
    ! Use read_image_file, read_encoded_image, or read_pixel_buffer
    
    ! TODO: Try each output format as needed
    ! Use "text", "json", "yaml", "xml", or "csv"
    
    ! Single license plate recognition (default)
    image_path = trim(image_dir) // "/licensePlate.jpg"
    call read_image_file(tsanpr, image_path, "text", "")
    
    ! Recognize multiple license plates attached to vehicles
    image_path = trim(image_dir) // "/multiple.jpg"
    call read_image_file(tsanpr, image_path, "text", "vm")
    
    ! Recognize multiple license plates attached to vehicles (including motorcycles)
    image_path = trim(image_dir) // "/multiple.jpg"
    call read_image_file(tsanpr, image_path, "text", "vmb")
    
    ! Recognize multiple license plates attached to vehicles with surround detection
    image_path = trim(image_dir) // "/surround.jpg"
    call read_image_file(tsanpr, image_path, "text", "vms")
    
    ! Recognize multiple surrounding objects (vehicles)
    image_path = trim(image_dir) // "/surround.jpg"
    call read_image_file(tsanpr, image_path, "text", "dms")
    
    ! Recognize multiple surrounding objects (vehicles) and license plates
    image_path = trim(image_dir) // "/surround.jpg"
    call read_image_file(tsanpr, image_path, "text", "dmsr")
    
    ! Recognize multiple surrounding objects and license plates within RoI
    image_path = trim(image_dir) // "/surround.jpg"
    call read_image_file(tsanpr, image_path, "text", "dmsri549,700,549,2427,1289,2427,1289,700")
    
end subroutine read_license_plates