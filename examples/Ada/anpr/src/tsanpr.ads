--  The MIT License (MIT)
--  Copyright © 2022-2025 TS-Solution Corp.
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to all conditions.
--
--  The above copyright notice and this permission notice shall be included
--  in all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
--  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with System;
with Interfaces.C;
with Interfaces.C.Strings;

package TSANPR is

   type TSANPR_Handle is private;

   --  Load the TSANPR library
   function Load_Library (Library_Path : String) return TSANPR_Handle;

   --  Initialize ANPR engine
   function ANPR_Initialize (Handle : TSANPR_Handle; Mode : String)
      return String;

   --  Read image file and perform ANPR
   function ANPR_Read_File (Handle : TSANPR_Handle;
                           Image_Path : String;
                           Output_Format : String;
                           Options : String) return String;

   --  Read pixel data and perform ANPR
   function ANPR_Read_Pixels (Handle : TSANPR_Handle;
                             Pixel_Data : System.Address;
                             Width : Natural;
                             Height : Natural;
                             Stride : Natural;
                             Pixel_Format : String;
                             Output_Format : String;
                             Options : String) return String;

   --  Clean up resources
   procedure Dispose (Handle : in out TSANPR_Handle);

private

   type TSANPR_Handle is record
      Library_Handle : System.Address := System.Null_Address;
      Init_Func : System.Address := System.Null_Address;
      Read_File_Func : System.Address := System.Null_Address;
      Read_Pixels_Func : System.Address := System.Null_Address;
   end record;

   --  C function prototypes
   type ANPR_Initialize_Func is access function
      (Mode : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, ANPR_Initialize_Func);

   type ANPR_Read_File_Func is access function
      (Image_Path : Interfaces.C.Strings.chars_ptr;
       Output_Format : Interfaces.C.Strings.chars_ptr;
       Options : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, ANPR_Read_File_Func);

   type ANPR_Read_Pixels_Func is access function
      (Pixel_Data : System.Address;
       Width : Interfaces.C.unsigned_long;
       Height : Interfaces.C.unsigned_long;
       Stride : Interfaces.C.long;
       Pixel_Format : Interfaces.C.Strings.chars_ptr;
       Output_Format : Interfaces.C.Strings.chars_ptr;
       Options : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, ANPR_Read_Pixels_Func);

   --  Platform detection and cross-platform wrapper functions
   type Platform_Type is (Windows, Unix);

   function Get_Platform return Platform_Type;

   function Load_Dynamic_Library (Path : Interfaces.C.Strings.chars_ptr;
                                 Platform : Platform_Type)
      return System.Address;

   function Get_Symbol_Address (Lib_Handle : System.Address;
                               Symbol : Interfaces.C.Strings.chars_ptr;
                               Platform : Platform_Type)
      return System.Address;

   function Close_Dynamic_Library (Lib_Handle : System.Address;
                                  Platform : Platform_Type)
      return Interfaces.C.int;

end TSANPR;