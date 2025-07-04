-- Image I/O: output in PPM, BMP, PNG, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

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
end Image_IO;
