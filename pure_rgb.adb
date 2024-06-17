-- Demo for Image_IO: Pull all color components to the closest of 0 or 255
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Command_Line;
with Image_IO.Holders;
with Image_IO.Operations;

procedure Pure_RGB is
   procedure Process (Data : in out Image_IO.Image_Data);
   -- Maximize the pixels of Data

   procedure Process (Data : in out Image_IO.Image_Data) is
      use type Image_IO.RGB_Value;

      function Pure (Value : in Image_IO.RGB_Value) return Image_IO.RGB_Value is
         (if Value < 128 then 0 else 255);
      -- Value, "purified"
   begin -- Process
      Rows : for R in Data'Range (1) loop -- Apply filter
         Columns : for C in Data'Range (2) loop
            Data (R, C) := (Red   => Pure (Data (R, C).Red),
                            Green => Pure (Data (R, C).Green),
                            Blue  => Pure (Data (R, C).Blue) );
         end loop Columns;
      end loop Rows;
   end Process;

   Image : Image_IO.Holders.Handle;
begin -- Pure_RGB
   Image_IO.Operations.Read (Name => Ada.Command_Line.Argument (1), Image => Image);
   Image.Update (Process => Process'Access);
   Image_IO.Operations.Write_BMP (File_Name => Ada.Command_Line.Argument (1) & ".bmp", Image => Image.Value);
end Pure_RGB;
