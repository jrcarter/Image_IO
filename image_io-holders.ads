-- Image I/O: output in PPM, BMP, PNG, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

private with Ada.Finalization;

package Image_IO.Holders is
   type Handle is tagged limited private; -- Initially empty

   procedure Create (Holder : in out Handle; Width : in Positive; Height : in Positive);
   -- Makes Holder hold an image Width x Height, default initialized
   -- Any image in Holder is discarded

   function Is_Empty (Holder : in Handle) return Boolean;
   -- Returns True if Create has not been called on Holder; False otherwise

   function Value (Holder : in Handle) return Image_Data with
      Pre => not Holder.Is_Empty;
   -- Returns the image held by Holder

   procedure Update (Holder : in Handle; Process : access procedure (Image : in out Image_Data) ) with
      Pre => not Holder.Is_Empty;
   -- Calls Process with the image in Holder
private -- Image_IO.Holders
   type Image_Ptr is access Image_Data;

   type Handle is new Ada.Finalization.Limited_Controlled with record
      Ptr : Image_Ptr;
   end record;

   overriding procedure Finalize (Object : in out Handle);

   function Value (Holder : in Handle) return Image_Data is
      (Holder.Ptr.all);

   function Is_Empty (Holder : in Handle) return Boolean is
      (Holder.Ptr = null);
end Image_IO.Holders;
