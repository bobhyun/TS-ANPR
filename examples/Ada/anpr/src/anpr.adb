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

with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Exceptions;
with TSANPR;

procedure ANPR is
   use Ada.Text_IO;

   Examples_Base_Dir : constant String := "../..";

   --  Forward declarations
   function Get_Engine_File_Name return String;
   procedure Read_Image_File (TS : TSANPR.TSANPR_Handle;
                             Img_File : String;
                             Output_Format : String;
                             Options : String);
   --  procedure Read_Encoded_Image (TS : TSANPR.TSANPR_Handle;
   --                               Img_File : String;
   --                               Output_Format : String;
   --                               Options : String);
   --  procedure Read_Pixel_Buffer (TS : TSANPR.TSANPR_Handle;
   --                              Img_File : String;
   --                              Output_Format : String;
   --                              Options : String);
   procedure Read_License_Plates (TS : TSANPR.TSANPR_Handle;
                                 Country_Code : String);

   function Get_Engine_File_Name return String is
      OS : constant String := Ada.Environment_Variables.Value ("OS", "");
      Arch : constant String :=
         Ada.Environment_Variables.Value ("PROCESSOR_ARCHITECTURE", "");
   begin
      if OS'Length >= 7 and then OS (OS'First .. OS'First + 6) = "Windows" then
         if Arch = "AMD64" or Arch = "x86_64" then
            return Examples_Base_Dir & "/bin/windows-x86_64/tsanpr.dll";
         else
            return Examples_Base_Dir & "/bin/windows-x86/tsanpr.dll";
         end if;
      else
         --  Assume Linux
         declare
            Hosttype : constant String :=
               Ada.Environment_Variables.Value ("HOSTTYPE", "x86_64");
         begin
            if Hosttype = "aarch64" then
               return Examples_Base_Dir &
                  "/bin/linux-aarch64/libtsanpr.so";
            else
               return Examples_Base_Dir & "/bin/linux-x86_64/libtsanpr.so";
            end if;
         end;
      end if;
   end Get_Engine_File_Name;

   procedure Read_Image_File (TS : TSANPR.TSANPR_Handle;
                             Img_File : String;
                             Output_Format : String;
                             Options : String) is
   begin
      Put (Img_File & " (outputFormat=""" & Output_Format &
           """, options=""" & Options & """) => ");
      declare
         Result : constant String :=
            TSANPR.ANPR_Read_File (TS, Img_File, Output_Format, Options);
      begin
         Put_Line (Result);
      end;
   end Read_Image_File;

   --  procedure Read_Encoded_Image (TS : TSANPR.TSANPR_Handle;
   --                               Img_File : String;
   --                               Output_Format : String;
   --                               Options : String) is
   --     pragma Unreferenced (TS);
   --  begin
   --     Put (Img_File & " (outputFormat=""" & Output_Format &
   --          """, options=""" & Options & """) => ");
   --     --  In a real implementation, this would read the file as binary data
   --     --  and call ANPR_Read_Pixels with "encoded" pixel format
   --     Put_Line ("Encoded image processing not implemented in demo");
   --  end Read_Encoded_Image;

   --  procedure Read_Pixel_Buffer (TS : TSANPR.TSANPR_Handle;
   --                              Img_File : String;
   --                              Output_Format : String;
   --                              Options : String) is
   --     pragma Unreferenced (TS);
   --  begin
   --     Put (Img_File & " (outputFormat=""" & Output_Format &
   --          """, options=""" & Options & """) => ");
   --     --  In a real implementation, this would load the image into a pixel
   --     --  buffer and call ANPR_Read_Pixels
   --     Put_Line ("Pixel buffer processing not implemented in demo");
   --  end Read_Pixel_Buffer;

   procedure Read_License_Plates (TS : TSANPR.TSANPR_Handle;
                                 Country_Code : String) is
      --  NOTICE:
      --  anpr_initialize should be called only once after library load.
      --  Therefore, it is not possible to change the country code after
      --  anpr_initialize has been called.
      --  While using the free trial license, you can try all languages.
      --  When you purchase a commercial license, you can only use the
      --  selected language.

      Error_Msg : constant String :=
         TSANPR.ANPR_Initialize (TS, "text;country=" & Country_Code);
      Image_Dir : constant String :=
         Examples_Base_Dir & "/img/" & Country_Code & "/";

      --  Forward declaration for local procedure
      procedure ANPR_Func (Img_File, Output_Format, Options : String);

      --  TODO: Try each function as needed
      --  To use encoded image processing, uncomment Read_Encoded_Image above
      --  and change the implementation below
      --  To use pixel buffer processing, uncomment Read_Pixel_Buffer above
      --  and change the implementation below
      procedure ANPR_Func (Img_File, Output_Format, Options : String) is
      begin
         Read_Image_File (TS, Img_File, Output_Format, Options);
         --  Read_Encoded_Image (TS, Img_File, Output_Format, Options);
         --  Read_Pixel_Buffer (TS, Img_File, Output_Format, Options);
      end ANPR_Func;

      --  TODO: Try each output format as needed
      Output_Format : constant String := "text";
      --  Output_Format : String := "json";
      --  Output_Format : String := "yaml";
      --  Output_Format : String := "xml";
      --  Output_Format : String := "csv";

   begin
      if Error_Msg /= "" then
         Put_Line ("anpr_initialize() failed: " & Error_Msg);
         return;
      end if;

      --  Single license plate recognition (default)
      ANPR_Func (Image_Dir & "licensePlate.jpg", Output_Format, "");

      --  Recognize multiple license plates attached to vehicles
      ANPR_Func (Image_Dir & "multiple.jpg", Output_Format, "vm");

      --  Recognize multiple license plates attached to vehicles (including
      --  motorcycles)
      ANPR_Func (Image_Dir & "multiple.jpg", Output_Format, "vmb");

      --  Recognize multiple license plates attached to vehicles with surround
      --  detection
      ANPR_Func (Image_Dir & "surround.jpg", Output_Format, "vms");

      --  Recognize multiple surrounding objects (vehicles)
      ANPR_Func (Image_Dir & "surround.jpg", Output_Format, "dms");

      --  Recognize multiple surrounding objects (vehicles) and license plates
      ANPR_Func (Image_Dir & "surround.jpg", Output_Format, "dmsr");

      --  Recognize multiple surrounding objects and license plates within RoI
      ANPR_Func (Image_Dir & "surround.jpg", Output_Format,
                 "dmsri549,700,549,2427,1289,2427,1289,700");
   end Read_License_Plates;

   Engine_File_Name : constant String := Get_Engine_File_Name;
   TS : TSANPR.TSANPR_Handle;
begin
   if Engine_File_Name = "" then
      Put_Line ("Error: Unsupported operating system");
      return;
   elsif not Ada.Directories.Exists (Engine_File_Name) then
      Put_Line ("Error: Engine file not found: " & Engine_File_Name);
      Put_Line ("Please run from the source directory " &
                "(examples/Ada/anpr), not from bin/");
      return;
   end if;

   Put_Line ("engine file name: " & Engine_File_Name);
   begin
      TS := TSANPR.Load_Library (Engine_File_Name);
   exception
      when E : others =>
         Put_Line ("TSANPR initialization failed: " &
                  Ada.Exceptions.Exception_Message (E));
         return;
   end;

   --  TODO: Try each country code as needed
   Read_License_Plates (TS, "KR");
   --  Read_License_Plates (TS, "JP");
   --  Read_License_Plates (TS, "VN");
   TSANPR.Dispose (TS);
end ANPR;