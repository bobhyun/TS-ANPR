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

module tsanpr_module
    use iso_c_binding
    implicit none
    
    ! TSANPR handle type
    type :: tsanpr_handle
        type(c_ptr) :: lib_handle = c_null_ptr
        type(c_funptr) :: anpr_initialize_ptr = c_null_funptr
        type(c_funptr) :: anpr_read_file_ptr = c_null_funptr
        type(c_funptr) :: anpr_read_pixels_ptr = c_null_funptr
    end type tsanpr_handle
    
    ! C function interfaces
    interface
#ifdef _WIN32
        ! Windows API functions
        function LoadLibraryA(filename) bind(c, name="LoadLibraryA")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: filename(*)
            type(c_ptr) :: LoadLibraryA
        end function LoadLibraryA

        function GetProcAddress(handle, symbol) bind(c, name="GetProcAddress")
            import :: c_ptr, c_funptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: symbol(*)
            type(c_funptr) :: GetProcAddress
        end function GetProcAddress

        function FreeLibrary(handle) bind(c, name="FreeLibrary")
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int) :: FreeLibrary
        end function FreeLibrary
#else
        ! POSIX dynamic library loading functions
        function dlopen(filename, flag) bind(c, name="dlopen")
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value :: flag
            type(c_ptr) :: dlopen
        end function dlopen

        function dlsym(handle, symbol) bind(c, name="dlsym")
            import :: c_ptr, c_funptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: symbol(*)
            type(c_funptr) :: dlsym
        end function dlsym

        function dlclose(handle) bind(c, name="dlclose")
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int) :: dlclose
        end function dlclose

        function dlerror() bind(c, name="dlerror")
            import :: c_ptr
            type(c_ptr) :: dlerror
        end function dlerror
#endif
        
        ! TSANPR function signatures
        function c_anpr_initialize(mode) bind(c)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: mode(*)
            type(c_ptr) :: c_anpr_initialize
        end function c_anpr_initialize
        
        function c_anpr_read_file(img_file_name, output_format, options) bind(c)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: img_file_name(*)
            character(kind=c_char), intent(in) :: output_format(*)
            character(kind=c_char), intent(in) :: options(*)
            type(c_ptr) :: c_anpr_read_file
        end function c_anpr_read_file
        
        function c_anpr_read_pixels(pixels, width, height, stride, pixel_format, &
                                   output_format, options) bind(c)
            import :: c_ptr, c_char, c_int64_t
            type(c_ptr), value :: pixels
            integer(c_int64_t), value :: width
            integer(c_int64_t), value :: height
            integer(c_int64_t), value :: stride
            character(kind=c_char), intent(in) :: pixel_format(*)
            character(kind=c_char), intent(in) :: output_format(*)
            character(kind=c_char), intent(in) :: options(*)
            type(c_ptr) :: c_anpr_read_pixels
        end function c_anpr_read_pixels
    end interface

#ifndef _WIN32
    ! Constants for dlopen (POSIX only)
    integer(c_int), parameter :: RTLD_LAZY = 1
    integer(c_int), parameter :: RTLD_NOW = 2
#endif
    
contains

    ! Initialize TSANPR with the given library path
    subroutine tsanpr_init(tsanpr, library_path, status)
        implicit none
        type(tsanpr_handle), intent(out) :: tsanpr
        character(len=*), intent(in) :: library_path
        integer, intent(out) :: status
        character(len=len(library_path)+1) :: c_library_path

        status = 0

        ! Convert Fortran string to C string
        c_library_path = trim(library_path) // c_null_char

#ifdef _WIN32
        ! Load dynamic library (Windows)
        tsanpr%lib_handle = LoadLibraryA(c_library_path)
#else
        ! Load dynamic library (POSIX)
        tsanpr%lib_handle = dlopen(c_library_path, RTLD_LAZY)
#endif
        if (.not. c_associated(tsanpr%lib_handle)) then
            status = -1
            return
        end if

        ! Get function pointers
#ifdef _WIN32
        tsanpr%anpr_initialize_ptr = GetProcAddress(tsanpr%lib_handle, "anpr_initialize" // c_null_char)
        tsanpr%anpr_read_file_ptr = GetProcAddress(tsanpr%lib_handle, "anpr_read_file" // c_null_char)
        tsanpr%anpr_read_pixels_ptr = GetProcAddress(tsanpr%lib_handle, "anpr_read_pixels" // c_null_char)
#else
        tsanpr%anpr_initialize_ptr = dlsym(tsanpr%lib_handle, "anpr_initialize" // c_null_char)
        tsanpr%anpr_read_file_ptr = dlsym(tsanpr%lib_handle, "anpr_read_file" // c_null_char)
        tsanpr%anpr_read_pixels_ptr = dlsym(tsanpr%lib_handle, "anpr_read_pixels" // c_null_char)
#endif

        if (.not. c_associated(tsanpr%anpr_initialize_ptr) .or. &
            .not. c_associated(tsanpr%anpr_read_file_ptr) .or. &
            .not. c_associated(tsanpr%anpr_read_pixels_ptr)) then
#ifdef _WIN32
            status = FreeLibrary(tsanpr%lib_handle)
#else
            status = dlclose(tsanpr%lib_handle)
#endif
            tsanpr%lib_handle = c_null_ptr
            status = -2
            return
        end if
    end subroutine tsanpr_init
    
    ! Cleanup TSANPR resources
    subroutine tsanpr_cleanup(tsanpr)
        implicit none
        type(tsanpr_handle), intent(inout) :: tsanpr
        integer(c_int) :: result

        if (c_associated(tsanpr%lib_handle)) then
#ifdef _WIN32
            result = FreeLibrary(tsanpr%lib_handle)
#else
            result = dlclose(tsanpr%lib_handle)
#endif
            tsanpr%lib_handle = c_null_ptr
            tsanpr%anpr_initialize_ptr = c_null_funptr
            tsanpr%anpr_read_file_ptr = c_null_funptr
            tsanpr%anpr_read_pixels_ptr = c_null_funptr
        end if
    end subroutine tsanpr_cleanup
    
    ! Initialize the ANPR engine with the specified mode
    subroutine tsanpr_initialize(tsanpr, mode, error_msg, status)
        implicit none
        type(tsanpr_handle), intent(in) :: tsanpr
        character(len=*), intent(in) :: mode
        character(len=*), intent(out) :: error_msg
        integer, intent(out) :: status
        character(len=len(mode)+1) :: c_mode
        type(c_ptr) :: result_ptr
        character(len=4096), pointer :: result_str
        
        procedure(c_anpr_initialize), pointer :: anpr_initialize_func
        
        status = 0
        error_msg = ""
        
        if (.not. c_associated(tsanpr%anpr_initialize_ptr)) then
            error_msg = "Function pointer not initialized"
            status = -1
            return
        end if
        
        ! Convert to C string
        c_mode = trim(mode) // c_null_char
        
        ! Get function pointer
        call c_f_procpointer(tsanpr%anpr_initialize_ptr, anpr_initialize_func)
        
        ! Call function
        result_ptr = anpr_initialize_func(c_mode)
        
        if (c_associated(result_ptr)) then
            call c_f_pointer(result_ptr, result_str)
            call c_to_f_string(result_ptr, error_msg)
            if (len_trim(error_msg) > 0) then
                status = -1
            end if
        end if
    end subroutine tsanpr_initialize
    
    ! Read and process an image file
    subroutine tsanpr_read_file(tsanpr, img_file_name, output_format, options, result, status)
        implicit none
        type(tsanpr_handle), intent(in) :: tsanpr
        character(len=*), intent(in) :: img_file_name, output_format, options
        character(len=*), intent(out) :: result
        integer, intent(out) :: status
        character(len=len(img_file_name)+1) :: c_img_file_name
        character(len=len(output_format)+1) :: c_output_format
        character(len=len(options)+1) :: c_options
        type(c_ptr) :: result_ptr
        
        procedure(c_anpr_read_file), pointer :: anpr_read_file_func
        
        status = 0
        result = ""
        
        if (.not. c_associated(tsanpr%anpr_read_file_ptr)) then
            status = -1
            return
        end if
        
        ! Convert to C strings
        c_img_file_name = trim(img_file_name) // c_null_char
        c_output_format = trim(output_format) // c_null_char
        c_options = trim(options) // c_null_char
        
        ! Get function pointer
        call c_f_procpointer(tsanpr%anpr_read_file_ptr, anpr_read_file_func)
        
        ! Call function
        result_ptr = anpr_read_file_func(c_img_file_name, c_output_format, c_options)
        
        if (c_associated(result_ptr)) then
            call c_to_f_string(result_ptr, result)
        else
            status = -1
        end if
    end subroutine tsanpr_read_file
    
    ! Process pixel data directly
    subroutine tsanpr_read_pixels(tsanpr, pixels, width, height, stride, &
                                 pixel_format, output_format, options, result, status)
        implicit none
        type(tsanpr_handle), intent(in) :: tsanpr
        type(c_ptr), value :: pixels
        integer(c_int64_t), intent(in) :: width, height, stride
        character(len=*), intent(in) :: pixel_format, output_format, options
        character(len=*), intent(out) :: result
        integer, intent(out) :: status
        character(len=len(pixel_format)+1) :: c_pixel_format
        character(len=len(output_format)+1) :: c_output_format
        character(len=len(options)+1) :: c_options
        type(c_ptr) :: result_ptr
        
        procedure(c_anpr_read_pixels), pointer :: anpr_read_pixels_func
        
        status = 0
        result = ""
        
        if (.not. c_associated(tsanpr%anpr_read_pixels_ptr)) then
            status = -1
            return
        end if
        
        ! Convert to C strings
        c_pixel_format = trim(pixel_format) // c_null_char
        c_output_format = trim(output_format) // c_null_char
        c_options = trim(options) // c_null_char
        
        ! Get function pointer
        call c_f_procpointer(tsanpr%anpr_read_pixels_ptr, anpr_read_pixels_func)
        
        ! Call function
        result_ptr = anpr_read_pixels_func(pixels, width, height, stride, &
                                          c_pixel_format, c_output_format, c_options)
        
        if (c_associated(result_ptr)) then
            call c_to_f_string(result_ptr, result)
        else
            status = -1
        end if
    end subroutine tsanpr_read_pixels
    
    ! Helper function to convert C string to Fortran string
    subroutine c_to_f_string(c_str_ptr, f_str)
        implicit none
        type(c_ptr), intent(in) :: c_str_ptr
        character(len=*), intent(out) :: f_str
        character(len=1), pointer :: c_str_array(:)
        integer :: i, str_len
        
        f_str = ""
        if (.not. c_associated(c_str_ptr)) return
        
        ! This is a simplified version - in practice you'd need to determine string length
        str_len = min(len(f_str), 4095)
        call c_f_pointer(c_str_ptr, c_str_array, [str_len])
        
        do i = 1, str_len
            if (c_str_array(i) == c_null_char) exit
            f_str(i:i) = c_str_array(i)
        end do
    end subroutine c_to_f_string

end module tsanpr_module