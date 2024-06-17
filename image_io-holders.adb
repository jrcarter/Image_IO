-- Image I/O: output in PPM, BMP, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Unchecked_Deallocation;

package body Image_IO.Holders is
   procedure Create (Holder : in out Handle; Width : in Positive; Height : in Positive) is
      -- Empty
   begin -- Create
      Holder.Finalize;
      Holder.Ptr := new Image_Data (0 .. Height - 1, 0 .. Width - 1);
   end Create;

   procedure Update (Holder : in Handle; Process : access procedure (Image : in out Image_Data) ) is
      -- Empty
   begin -- Update
      Process (Image => Holder.Ptr.all);
   end Update;

   overriding procedure Finalize (Object : in out Handle) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Image_Data, Name => Image_Ptr);
   begin -- Finalize
      Free (Object.Ptr);
   exception -- Finalize
   when others =>
      null;
   end Finalize;
end Image_IO.Holders;
