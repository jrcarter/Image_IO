-- Image I/O: output in PPM and BMP formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Containers.Indefinite_Holders;
with Ada.Directories;
with Interfaces;

package Image_IO is
   subtype RGB_Value is Interfaces.Unsigned_8;

   type Color_Info is record
      Red   : RGB_Value;
      Green : RGB_Value;
      Blue  : RGB_Value;
   end record;

   type Image_Data is array (Natural range <>, Natural range <>) of Color_Info with
      Dynamic_Predicate => Image_Data'First (1) = 0 and Image_Data'First (2) = 0;
   -- First dimension is Y/rows; second is X/columns
   -- Y/row 0 is the top row; X/column 0 is the left column

   Invalid_File : exception;

   procedure Write_P3 (File_Name : in String; Image : in Image_Data);
   -- Creates a file named File_Name; writes Image to it in P3 format; and closes the file
   -- The file will have Unix (LF) line terminators

   procedure Write_P6 (File_Name : in String; Image : in Image_Data);
   -- Creates a file named File_Name; writes Image to it in P6 format; and closes the file
   -- The file will have Unix (LF) line terminators

   procedure Write_BMP (File_Name : in String; Image : in Image_Data);
   -- Creates a file named Name; writes Image to it in BMP format; and closes the file

   package Image_Holders is new Ada.Containers.Indefinite_Holders (Element_Type => Image_Data);

   procedure Read (Name : in String; Image : in out Image_Holders.Holder) with
      Pre => Ada.Directories.Exists (Name) and then Ada.Directories.Kind (Name) in Ada.Directories.Ordinary_File;
end Image_IO;
