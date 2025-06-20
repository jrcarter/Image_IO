-- Image I/O: output in PPM, BMP, PNG, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Finalization;
with Image_IO.QOI;

private package Image_IO.QOI_Holders is
   type Handle is tagged limited private; -- Initially empty

   procedure Create (Holder : in out Handle; Length : in QOI.Storage_Count);
   -- Makes Holder hold a Storage_Array of length Length, default initialized
   -- Any value in Holder is discarded

   function Is_Empty (Holder : in Handle) return Boolean;
   -- Returns True if Create has not been called on Holder; False otherwise

   function Value (Holder : in Handle) return QOI.Storage_Array with
      Pre => not Holder.Is_Empty;
   -- Returns the data held by Holder

   procedure Update (Holder : in Handle; Process : access procedure (Data : in out QOI.Storage_Array) ) with
      Pre => not Holder.Is_Empty;
   -- Calls Process with the data in Holder
private -- Image_IO.QOI_Holders
   type Data_Ptr is access QOI.Storage_Array;

   type Handle is new Ada.Finalization.Limited_Controlled with record
      Ptr : Data_Ptr;
   end record;

   overriding procedure Finalize (Object : in out Handle);

   function Value (Holder : in Handle) return QOI.Storage_Array is
      (Holder.Ptr.all);

   function Is_Empty (Holder : in Handle) return Boolean is
      (Holder.Ptr = null);
end Image_IO.QOI_Holders;
