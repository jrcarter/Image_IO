-- Demo for Image_IO: Pull all color components to the closest of 0 or 255
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Command_Line;
with Image_IO;

procedure Pure_RGB is
   procedure Process (Data : in out Image_IO.Image_Data);
   -- Maximize the pixels of Data

   procedure Process (Data : in out Image_IO.Image_Data) is
      use type Image_IO.RGB_Value;
   begin -- Process
      Rows : for R in Data'Range (1) loop -- Apply filter
         Columns : for C in Data'Range (2) loop
            Data (R, C) := (Red   => (if Data (R, C).Red   < 128 then 0 else 255),
                            Green => (if Data (R, C).Green < 128 then 0 else 255),
                            Blue  => (if Data (R, C).Blue  < 128 then 0 else 255) );
         end loop Columns;
      end loop Rows;
   end Process;

   Image : Image_IO.Image_Holders.Holder;
begin -- Pure_RGB
   Image_IO.Read (Name => Ada.Command_Line.Argument (1), Image => Image);
   Image.Update_Element (Process => Process'Access);
   Image_IO.Write_BMP (File_Name => Ada.Command_Line.Argument (1) & ".bmp", Image => Image.Element);
end Pure_RGB;
