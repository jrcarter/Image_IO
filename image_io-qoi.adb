-- Derived from package QOI by Fabien Chouteau
-- Modified for only encoding Image_IO images (RGB) and to compile with ObjectAda V10.5U3
--
-- The following license applies to the original:
--
--  MIT License
--
--  Copyright (c) 2021 Fabien Chouteau
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.

with Ada.Unchecked_Conversion;

package body Image_IO.QOI
   with SPARK_Mode
is
   subtype SE is Storage_Element;

   type Color is record
      R, G, B : SE;
   end record;

   type Index_Range is range 0 .. 63;
   subtype Run_Range is Unsigned_32 range 0 .. 62;

   function Hash (C : Color) return SE is
      (C.R * 3 + C.G * 5 + C.B * 7 + 255 * 11);

   procedure Encode (Pix         :     Storage_Array;
                     Desc        :     QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
   is
      P   : Storage_Count := Output'First;
      Run : Run_Range     := 0;

      function Valid_Parameters return Boolean is
        (Valid_Size (Desc)
         and then Output'First >= 0
         and then Output'Last < Storage_Count'Last
         and then Output'Length >= Encode_Worst_Case (Desc))
      with Ghost;

      procedure Push (D : Unsigned_32)
      with
        Pre  =>
          Valid_Parameters
            and then P in Output'First .. Output'Last - 3,
        Post =>
          P = P'Old + 4;

      generic
         type T is private;
      procedure Gen_Push_8 (D : T)
      with
        Pre  =>
            Valid_Parameters
            and then T'Size = 8
            and then P in Output'Range,
        Post =>
          P = P'Old + 1;

      procedure Push (D : Unsigned_32) is
      begin
         Output (P)     := SE (Shift_Right (D and 16#FF_00_00_00#, 24));
         Output (P + 1) := SE (Shift_Right (D and 16#00_FF_00_00#, 16));
         Output (P + 2) := SE (Shift_Right (D and 16#00_00_FF_00#, 8));
         Output (P + 3) := SE (Shift_Right (D and 16#00_00_00_FF#, 0));

         P := P + 4;
      end Push;

      procedure Gen_Push_8  (D : T) is
         function To_Byte is new Ada.Unchecked_Conversion (T, SE);
      begin
         Output (P) := To_Byte (D);

         P := P + 1;
      end Gen_Push_8;

      procedure Push        is new Gen_Push_8 (SE);
      procedure Push_Run    is new Gen_Push_8 (Run_Tag);
      procedure Push_Index  is new Gen_Push_8 (Index_Tag);
      procedure Push_Diff   is new Gen_Push_8 (Diff_Tag);
      procedure Push_Luma_A is new Gen_Push_8 (LUMA_Tag_A);
      procedure Push_Luma_B is new Gen_Push_8 (LUMA_Tag_B);

      Number_Of_Pixels : constant Storage_Count := Desc.Width * Desc.Height;

      subtype Pixel_Index is Storage_Count range 0 .. Number_Of_Pixels - 1;

      function Read (Index : Pixel_Index) return Color;

      function Read (Index : Pixel_Index) return Color is
         Result : Color;
         Offset : constant Storage_Count := Index * Num_Channels;
         Buffer_Index : constant Storage_Count := Pix'First + Offset;
      begin
         Result.R := Pix (Buffer_Index);
         Result.G := Pix (Buffer_Index + 1);
         Result.B := Pix (Buffer_Index + 2);

         return Result;
      end Read;

      Index   : array (Index_Range) of Color := (others => ((0, 0, 0)));
      Px_Prev : Color := (R => 0, G => 0, B => 0);
      Px      : Color;
   begin
      if Output'Length < Encode_Worst_Case (Desc) then
         Output_Size := 0;
         return;
      end if;

      Push (QOI_MAGIC);
      Push (Unsigned_32 (Desc.Width));
      Push (Unsigned_32 (Desc.Height));
      Push (SE'(Num_Channels));
      Push (SE'(0));

      pragma Assert (P = Output'First + QOI_HEADER_SIZE);
      pragma Assert (Run = 0);
      for Px_Index in Pixel_Index loop
         pragma Loop_Invariant
           (Run in 0 .. Run_Range'Last - 1);
         pragma Loop_Invariant
            (P - Output'First in 0 .. QOI_HEADER_SIZE + (Num_Channels + 1) * (Storage_Count (Px_Index) - Storage_Count (Run)));

         Px := Read (Px_Index);

         if Px = Px_Prev then
            Run := Run + 1;

            if Run = Run_Range'Last or else Px_Index = Pixel_Index'Last
            then
               Push_Run ((Op => QOI_OP_RUN, Run => Uint6 (Run - 1)));
               Run := 0;
            end if;
         else
            if Run > 0 then
               Push_Run ((Op => QOI_OP_RUN, Run => Uint6 (Run - 1)));
               Run := 0;
            end if;

            pragma Assert (Run = 0);
            pragma Assert (P - Output'First In 0 .. QOI_HEADER_SIZE + (Num_Channels + 1) * Storage_Count (Px_Index));

            declare
               Index_Pos : constant Index_Range :=
                 Index_Range (Hash (Px) mod Index'Length);
            begin
               if Index (Index_Pos) = Px then
                  Push_Index ((Op    => QOI_OP_INDEX,
                               Index => Uint6 (Index_Pos)));
               else
                  Index (Index_Pos) := Px;

                  declare
                     VR : constant Integer :=
                            Integer (Px.R) - Integer (Px_Prev.R);
                     VG : constant Integer :=
                            Integer (Px.G) - Integer (Px_Prev.G);
                     VB : constant Integer :=
                            Integer (Px.B) - Integer (Px_Prev.B);

                     VG_R : constant Integer := VR - VG;
                     VG_B : constant Integer := VB - VG;
                  begin
                     if         VR in -2 .. 1
                        and then VG in -2 .. 1
                        and then VB in -2 .. 1
                     then
                        Push_Diff ((Op  => QOI_OP_DIFF,
                                    DR  => Uint2 (VR + 2),
                                    DG  => Uint2 (VG + 2),
                                    DB  => Uint2 (VB + 2)));

                     elsif      VG_R in -8 .. 7
                        and then VG   in -32 .. 31
                        and then VG_B in -8 .. 7
                     then
                        Push_Luma_A ((Op  => QOI_OP_LUMA,
                                      DG  => Uint6 (VG + 32)));
                        Push_Luma_B ((DG_R => Uint4 (VG_R + 8),
                                      DG_B => Uint4 (VG_B + 8)));
                     else
                        Push (QOI_OP_RGB);
                        Push (Px.R);
                        Push (Px.G);
                        Push (Px.B);
                     end if;
                  end;
               end if;
            end;
         end if;

         Px_Prev := Px;
      end loop;

      pragma Assert (P - Output'First in
        0 .. QOI_HEADER_SIZE + (Num_Channels + 1) * Number_Of_Pixels);

      for Index in QOI_PADDING'Range loop
         pragma Loop_Invariant
           (P - Output'First in
              0 .. Encode_Worst_Case (Desc) - QOI_PADDING'Length + Index - 1);
         Push (QOI_PADDING (Index));
      end loop;

      Output_Size := P - Output'First;
   end Encode;
end Image_IO.QOI;
