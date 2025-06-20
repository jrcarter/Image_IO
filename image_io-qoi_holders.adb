-- Image I/O: output in PPM, BMP, PNG, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Unchecked_Deallocation;

package body Image_IO.QOI_Holders is
   procedure Create (Holder : in out Handle; Length : in QOI.Storage_Count) is
      -- Empty
   begin -- Create
      Finalize (Object => Holder);
      Holder.Ptr := new QOI.Storage_Array (1 .. Length);
   end Create;

   procedure Update (Holder : in Handle; Process : access procedure (Data : in out QOI.Storage_Array) ) is
      -- Empty
   begin -- Update
      Process (Data => Holder.Ptr.all);
   end Update;

   overriding procedure Finalize (Object : in out Handle) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => QOI.Storage_Array, Name => Data_Ptr);
   begin -- Finalize
      Free (Object.Ptr);
   exception -- Finalize
   when others =>
      null;
   end Finalize;
end Image_IO.QOI_Holders;
