-- Image I/O: output in PPM, BMP, PNG, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Directories;
with Image_IO.Holders;

package Image_IO.Operations is
   Invalid_File : exception;

   procedure Write_P3 (File_Name : in String; Image : in Image_Data);
   -- Creates a file named File_Name; writes Image to it in P3 format; and closes the file
   -- The file will have Unix (LF) line terminators

   procedure Write_P6 (File_Name : in String; Image : in Image_Data);
   -- Creates a file named File_Name; writes Image to it in P6 format; and closes the file
   -- The file will have Unix (LF) line terminators

   procedure Write_BMP (File_Name : in String; Image : in Image_Data);
   -- Creates a file named Name; writes Image to it in BMP format; and closes the file

   procedure Write_QOI (File_Name : in String; Image : in Image_Data);
   -- Creates a file named Name; writes Image to it in QOI format; and closes the file

   procedure Write_PNG (File_Name : in String; Image : in Image_Data; Grayscale : in Boolean := False);
   -- Creates a file named Name; writes Image to it in PNG format; and closes the file
   -- If Grayscale, treats Image as a grayscale image, with all pixels having equal Red, Green, and Blue components. The Red
   -- component is the only one used for the output
   -- Otherwise, treats Image as a color image

   procedure Read (Name : in String; Image : in out Holders.Handle) with
      Pre => Ada.Directories.Exists (Name) and then Ada.Directories.Kind (Name) in Ada.Directories.Ordinary_File;
end Image_IO.Operations;
