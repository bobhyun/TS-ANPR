(* The MIT License (MIT)
   Copyright © 2022-2025 TS-Solution Corp.

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to all conditions.

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Ctypes
open Foreign

type t = {
  lib : Dl.library;
  anpr_initialize : string -> string;
  anpr_read_file : string -> string -> string -> string;
  anpr_read_pixels : unit ptr -> int -> int -> int -> string -> string -> string -> string;
}

let create library_path =
  let lib = Dl.dlopen ~filename:library_path ~flags:[Dl.RTLD_NOW] in
  let anpr_initialize = foreign ~from:lib "anpr_initialize" (string @-> returning string) in
  let anpr_read_file = foreign ~from:lib "anpr_read_file" (string @-> string @-> string @-> returning string) in
  let anpr_read_pixels = foreign ~from:lib "anpr_read_pixels"
    (ptr void @-> int @-> int @-> int @-> string @-> string @-> string @-> returning string) in
  { lib; anpr_initialize; anpr_read_file; anpr_read_pixels }

let anpr_initialize t mode =
  t.anpr_initialize mode

let anpr_read_file t img_file_name output_format options =
  t.anpr_read_file img_file_name output_format options

let anpr_read_pixels t pixels width height stride pixel_format output_format options =
  let ptr = to_voidp (Ctypes.bigarray_start Ctypes.array1 pixels) in
  t.anpr_read_pixels ptr width height stride pixel_format output_format options

let anpr_read_pixels_encoded t encoded_data len output_format options =
  let ptr = to_voidp (Ctypes.bigarray_start Ctypes.array1 encoded_data) in
  t.anpr_read_pixels ptr len 0 0 "encoded" output_format options
