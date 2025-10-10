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

with Ada.Unchecked_Conversion;

package body TSANPR is

   use Interfaces.C;
   use Interfaces.C.Strings;
   use System;

   --  Windows API functions
   function LoadLibraryA (lpFileName : chars_ptr) return System.Address;
   pragma Import (Stdcall, LoadLibraryA, "LoadLibraryA");

   function GetProcAddress (hModule : System.Address; lpProcName : chars_ptr)
      return System.Address;
   pragma Import (Stdcall, GetProcAddress, "GetProcAddress");

   function FreeLibrary (hLibModule : System.Address) return Interfaces.C.int;
   pragma Import (Stdcall, FreeLibrary, "FreeLibrary");

   function GetLastError return Interfaces.C.unsigned_long;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   function Get_Platform return Platform_Type is
   begin
      return Windows;
   end Get_Platform;

   function Load_Dynamic_Library (Path : Interfaces.C.Strings.chars_ptr;
                                 Platform : Platform_Type)
      return System.Address is
      Handle : System.Address;
   begin
      if Platform = Windows then
         Handle := LoadLibraryA (Path);
         if Handle = System.Null_Address then
            declare
               Error_Code : constant Interfaces.C.unsigned_long :=
                  GetLastError;
            begin
               raise Program_Error with "LoadLibraryA failed, error code:" &
                  Interfaces.C.unsigned_long'Image (Error_Code);
            end;
         end if;
         return Handle;
      else
         raise Program_Error with "Unix platform not supported";
      end if;
   end Load_Dynamic_Library;

   function Get_Symbol_Address (Lib_Handle : System.Address;
                               Symbol : Interfaces.C.Strings.chars_ptr;
                               Platform : Platform_Type)
      return System.Address is
   begin
      if Platform = Windows then
         return GetProcAddress (Lib_Handle, Symbol);
      else
         raise Program_Error with "Unix platform not supported";
      end if;
   end Get_Symbol_Address;

   function Close_Dynamic_Library (Lib_Handle : System.Address;
                                  Platform : Platform_Type)
      return Interfaces.C.int is
   begin
      if Platform = Windows then
         return FreeLibrary (Lib_Handle);
      else
         return 0;  -- Success fallback
      end if;
   end Close_Dynamic_Library;

   function Load_Library (Library_Path : String) return TSANPR_Handle is
      Handle : TSANPR_Handle;
      C_Path : chars_ptr := New_String (Library_Path);
      Platform : constant Platform_Type := Get_Platform;
   begin
      --  Load dynamic library using platform-specific wrapper
      Handle.Library_Handle := Load_Dynamic_Library (C_Path, Platform);
      Free (C_Path);

      if Handle.Library_Handle = Null_Address then
         raise Program_Error with "Failed to load library: " & Library_Path;
      end if;

      --  Load function pointers
      declare
         Init_Name : chars_ptr := New_String ("anpr_initialize");
         Read_File_Name : chars_ptr := New_String ("anpr_read_file");
         Read_Pixels_Name : chars_ptr := New_String ("anpr_read_pixels");
      begin
         Handle.Init_Func := Get_Symbol_Address (Handle.Library_Handle,
                                                 Init_Name, Platform);
         Handle.Read_File_Func := Get_Symbol_Address (Handle.Library_Handle,
                                                      Read_File_Name,
                                                      Platform);
         Handle.Read_Pixels_Func := Get_Symbol_Address (Handle.Library_Handle,
                                                        Read_Pixels_Name,
                                                        Platform);

         Free (Init_Name);
         Free (Read_File_Name);
         Free (Read_Pixels_Name);
      end;

      if Handle.Init_Func = Null_Address or
         Handle.Read_File_Func = Null_Address or
         Handle.Read_Pixels_Func = Null_Address
      then
         raise Program_Error with "Failed to load TSANPR functions";
      end if;

      return Handle;
   end Load_Library;

   function ANPR_Initialize (Handle : TSANPR_Handle; Mode : String)
      return String is
      function To_Init_Func is new Ada.Unchecked_Conversion
         (System.Address, ANPR_Initialize_Func);
      C_Mode : chars_ptr := New_String (Mode);
      Result_Ptr : chars_ptr;
   begin
      if Handle.Init_Func = Null_Address then
         return "TSANPR not initialized";
      end if;

      Result_Ptr := To_Init_Func (Handle.Init_Func) (C_Mode);
      Free (C_Mode);

      if Result_Ptr = Null_Ptr then
         return "";
      else
         declare
            C_Result : constant String := Value (Result_Ptr);
         begin
            return C_Result;
         end;
      end if;
   end ANPR_Initialize;

   function ANPR_Read_File (Handle : TSANPR_Handle;
                           Image_Path : String;
                           Output_Format : String;
                           Options : String) return String is
      function To_Read_File_Func is new Ada.Unchecked_Conversion
         (System.Address, ANPR_Read_File_Func);
      C_Path : chars_ptr := New_String (Image_Path);
      C_Format : chars_ptr := New_String (Output_Format);
      C_Options : chars_ptr := New_String (Options);
      Result_Ptr : chars_ptr;
   begin
      if Handle.Read_File_Func = Null_Address then
         return "TSANPR not initialized";
      end if;

      Result_Ptr := To_Read_File_Func (Handle.Read_File_Func)
         (C_Path, C_Format, C_Options);

      Free (C_Path);
      Free (C_Format);
      Free (C_Options);

      if Result_Ptr = Null_Ptr then
         return "Failed to process image";
      else
         declare
            C_Result : constant String := Value (Result_Ptr);
         begin
            return C_Result;
         end;
      end if;
   end ANPR_Read_File;

   function ANPR_Read_Pixels (Handle : TSANPR_Handle;
                             Pixel_Data : System.Address;
                             Width : Natural;
                             Height : Natural;
                             Stride : Natural;
                             Pixel_Format : String;
                             Output_Format : String;
                             Options : String) return String is
      function To_Read_Pixels_Func is new Ada.Unchecked_Conversion
         (System.Address, ANPR_Read_Pixels_Func);
      C_Pixel_Format : chars_ptr := New_String (Pixel_Format);
      C_Output_Format : chars_ptr := New_String (Output_Format);
      C_Options : chars_ptr := New_String (Options);
      Result_Ptr : chars_ptr;
   begin
      if Handle.Read_Pixels_Func = Null_Address then
         return "TSANPR not initialized";
      end if;

      Result_Ptr := To_Read_Pixels_Func (Handle.Read_Pixels_Func)
         (Pixel_Data, unsigned_long (Width), unsigned_long (Height),
          long (Stride), C_Pixel_Format, C_Output_Format, C_Options);

      Free (C_Pixel_Format);
      Free (C_Output_Format);
      Free (C_Options);

      if Result_Ptr = Null_Ptr then
         return "Failed to process pixels";
      else
         declare
            C_Result : constant String := Value (Result_Ptr);
         begin
            return C_Result;
         end;
      end if;
   end ANPR_Read_Pixels;

   procedure Dispose (Handle : in out TSANPR_Handle) is
      Platform : constant Platform_Type := Get_Platform;
      Dummy : Interfaces.C.int;
   begin
      if Handle.Library_Handle /= Null_Address then
         Dummy := Close_Dynamic_Library (Handle.Library_Handle, Platform);
         Handle.Library_Handle := Null_Address;
         Handle.Init_Func := Null_Address;
         Handle.Read_File_Func := Null_Address;
         Handle.Read_Pixels_Func := Null_Address;
      end if;
   end Dispose;
end TSANPR;