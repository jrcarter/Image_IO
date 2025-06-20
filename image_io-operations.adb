-- Image I/O: output in PPM, BMP, PNG, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Containers.Vectors;
with Ada.Sequential_IO;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with CRC_32;
with GID;
with Image_IO.QOI;
with Image_IO.QOI_Holders;
with PragmARC.Text_IO;
with System;
with Z_Compression;

package body Image_IO.Operations is
   procedure Write_P3 (File_Name : in String; Image : in Image_Data) is
      Output : PragmARC.Text_IO.File_Handle;
   begin -- Write_P3
      PragmARC.Text_IO.Create (File => Output, Name => File_Name, EOL => PragmARC.Text_IO.Unix_EOL);
      PragmARC.Text_IO.Put_Line (File => Output, Item => "P3");
      PragmARC.Text_IO.Put_Line (File => Output, Item => "# Created by Write_P3");
      PragmARC.Text_IO.Put_Line
         (File => Output, Item => Integer'Image (Image'Length (2) ) & Integer'Image (Image'Length (1) ) & " 255");

      All_Rows : for Y in Image'Range (1) loop
         All_Columns : for X in Image'Range (2) loop
            PragmARC.Text_IO.Put
               (File => Output, Item => Image (Y, X).Red'Image & Image (Y, X).Green'Image & Image (Y, X).Blue'Image);
         end loop All_Columns;

         PragmARC.Text_IO.New_Line (File => Output);
      end loop All_Rows;

      PragmARC.Text_IO.Close (File => Output);
   end Write_P3;

   package RGB_IO is new Ada.Sequential_IO (Element_Type => RGB_Value);

   use type RGB_IO.File_Mode;

   LF : constant := 10;

   procedure Write_P6 (File_Name : in String; Image : in Image_Data) is
      procedure Put_Line (Output : in out RGB_IO.File_Type; Item : in String) with
         Pre => RGB_IO.Is_Open (Output) and then RGB_IO.Mode (Output) = RGB_IO.Out_File;
      -- Writes the bytes of Item to Output, followed by LF

       procedure Put_Line (Output : in out RGB_IO.File_Type; Item : in String) is
         -- Empty
       begin -- Put_Line
         Write : for C of Item loop
            RGB_IO.Write (File => Output, Item => Character'Pos (C) );
         end loop Write;

         RGB_IO.Write (File => Output, Item => LF);
      end Put_Line;

      Output : RGB_IO.File_Type;
   begin -- Write_P6
      RGB_IO.Create (File => Output, Name => File_Name);
      Put_Line (Output => Output, Item => "P6");
      Put_Line (Output => Output, Item => "# Created by Write_P6");
      Put_Line (Output => Output, Item => Integer'Image (Image'Length (2) ) & Integer'Image (Image'Length (1) ) & " 255");

      All_Rows : for Y in Image'Range (1) loop
         All_Columns : for X in Image'Range (2) loop
            RGB_IO.Write (File => Output, Item => Image (Y, X).Red);
            RGB_IO.Write (File => Output, Item => Image (Y, X).Green);
            RGB_IO.Write (File => Output, Item => Image (Y, X).Blue);
         end loop All_Columns;
      end loop All_Rows;

      RGB_IO.Close (File => Output);
   end Write_P6;

   procedure Write_BMP (File_Name : in String; Image : in Image_Data) is
      type Byte_List is array (Positive range <>) of RGB_Value;

      type Word  is mod 2 ** 16;
      type Dword is mod 2 ** 32;

      subtype Word_As_Bytes  is Byte_List (1 .. 2);
      subtype Dword_As_Bytes is Byte_List (1 .. 4);

      function C2B (Color : in Color_Info) return Byte_List is
         (Color.Blue, Color.Green, Color.Red);

      procedure Make_Little_Endian (List : in out Byte_List) with
         Pre => List'First = 1;
      -- If System.Default_Bit_Order /= System.Low_Order_First, reverses the bytes of List

      function W2B (Value : in Word) return Word_As_Bytes;
      -- Returns the bytes of Value in little-endian order

      function D2B (Value : in Dword) return Dword_As_Bytes;
      -- Returns the bytes of Value in little-endian order

      procedure Write (File : in RGB_IO.File_Type; List : in Byte_List);
      -- Writes the bytes of List to File

      Pixel_Bytes_Per_Row : constant Natural := 3 * Image'Length (2);
      Bytes_Per_Row       : constant Natural := 4 * ( (24 * Image'Length (2) + 31) / 32); -- Includes padding bytes
      Image_Bytes         : constant Natural := Image'Length (1) * Bytes_Per_Row;
      Padding_Bytes       : constant Natural := Bytes_Per_Row - Pixel_Bytes_Per_Row;

      use type RGB_Value;

      procedure Make_Little_Endian (List : in out Byte_List) is
         procedure Swap (Left : in out RGB_Value; Right : in out RGB_Value) with
            Post => Left = Right'Old and Right = Left'Old;

         procedure Swap (Left : in out RGB_Value; Right : in out RGB_Value) is
            Temp : RGB_Value := Left;
         begin -- Swap
            Left := Right;
            Right := Temp;
         end Swap;

         use type System.Bit_Order;

         Last : Natural := List'Last;
      begin -- Make_Little_Endian
         if System.Default_Bit_Order = System.Low_Order_First then
            return;
         end if;

         Backwards : for I in 1 .. List'Last / 2 loop
            Swap (List (I), List (Last) );
            Last := Last - 1;
         end loop Backwards;
      end Make_Little_Endian;

      function W2B (Value : in Word) return Word_As_Bytes is
         function Converted is new Ada.Unchecked_Conversion (Source => Word, Target => Word_As_Bytes);

         Result : Word_As_Bytes := Converted (Value);
      begin -- W2B
         Make_Little_Endian (List => Result);

         return Result;
      end W2B;

      function D2B (Value : in Dword) return Dword_As_Bytes is
         function Converted is new Ada.Unchecked_Conversion (Source => Dword, Target => Dword_As_Bytes);

         Result : Dword_As_Bytes := Converted (Value);
      begin -- D2B
         Make_Little_Endian (List => Result);

         return Result;
      end D2B;

      procedure Write (File : in RGB_IO.File_Type; List : in Byte_List) is
         -- Empty
      begin -- Write
         All_Bytes : for B of List loop
            RGB_IO.Write (File => File, Item => B);
         end loop All_Bytes;
      end Write;

      Padding : constant Byte_List := (1 .. Padding_Bytes => 0);
      DPM     : constant Byte_List := D2B (2835); -- Dots per meter (about 72 DPI)
      Zero    : constant Byte_List := (1 .. 4 => 0);

      File : RGB_IO.File_Type;
   begin -- Write_BMP
      RGB_IO.Create (File => File, Name => File_Name);
      Write (File => File, List => (Character'Pos ('B') & Character'Pos ('M') ) ); -- BMP header
      Write (File => File, List => D2B (54 + Dword (Image_Bytes) ) );              -- File size
      Write (File => File, List => Zero);                                          -- Unused
      Write (File => File, List => D2B (54) );                                     -- Offset of image data
      Write (File => File, List => D2B (40) );                   -- DIB header of 40 bytes
      Write (File => File, List => D2B (Image'Length (2) ) );    -- Image width
      Write (File => File, List => D2B (Image'Length (1) ) );    -- Image height
      Write (File => File, List => W2B (1) );                    -- # of planes
      Write (File => File, List => W2B (24) );                   -- Bits/pixel
      Write (File => File, List => Zero);                        -- No compression
      Write (File => File, List => D2B (Dword (Image_Bytes) ) ); -- Image size
      Write (File => File, List => DPM & DPM);                   -- X & Y resolutions
      Write (File => File, List => Zero & Zero);                 -- # of palette colors & all colors important

      All_Rows : for Y in reverse Image'Range (1) loop -- Write image data
         All_Columns : for X in Image'Range (2) loop
            Write (File => File, List => C2B (Image (Y, X) ) );
         end loop All_Columns;

         Write (File => File, List => Padding);
      end loop All_Rows;

      RGB_IO.Close (File => File);
   end Write_BMP;

   procedure Write_QOI (File_Name : in String; Image : in Image_Data) is
      Desc : constant QOI.QOI_Desc := (Width => Image'Length (2), Height => Image'Length (1) );

      Output_Length : constant QOI.Storage_Count := QOI.Encode_Worst_Case (Desc);

      procedure Encode (Data : in out QOI.Storage_Array);
      -- Encodes Image into Data; sets Length to the length of the encoded data

      Data   : QOI_Holders.Handle;
      Length : QOI.Storage_Count;

      procedure Encode (Data : in out QOI.Storage_Array) is
         procedure Convert_And_Encode (List : in out QOI.Storage_Array);
         -- Converts Image to a Storage_Array and passes it to QOI.Encode

         subtype Image_Sub is Image_Data (Image'Range (1), Image'Range (2) );

         use type QOI.Storage_Count;

         subtype Image_List is QOI.Storage_Array (1 .. 3 * Image'Length (1) * Image'Length (2) );

         procedure Convert_And_Encode (List : in out QOI.Storage_Array) is
            function To_List is new Ada.Unchecked_Conversion (Source => Image_Sub, Target => Image_List);
         begin -- Convert_And_Encode
            List := To_List (Image);
            QOI.Encode (Pix => List, Desc => Desc, Output => Data, Output_Size => Length);
         end Convert_And_Encode;

         List : QOI_Holders.Handle;
      begin -- Encode
         List.Create (Length => Image_List'Length);
         List.Update (Process => Convert_And_Encode'Access);
      end Encode;
   begin -- Write_QOI
      Data.Create (Length => Output_Length);
      Data.Update (Process => Encode'Access);

      Write : declare
         procedure Write_File (Data : in out QOI.Storage_Array);
         -- Creates File_Name, writes Data (1 .. Length) to it, and closes the file

         procedure Write_File (Data : in out QOI.Storage_Array) is
            subtype Result_List is QOI.Storage_Array (1 .. Length);

            package Result_IO is new Ada.Sequential_IO (Element_Type => Result_List);

            File : Result_IO.File_Type;
         begin -- Write_File
            Result_IO.Create (File => File, Mode => Result_IO.Out_File, Name => File_Name);
            Result_IO.Write (File => File, Item => Data (1 .. Length) );
            Result_IO.Close (File => File);
         end Write_File;
      begin -- Write
         Data.Update (Process => Write_File'Access);
      end Write;
   end Write_QOI;

   procedure Write_PNG (File_Name : in String; Image : in Image_Data; Grayscale : in Boolean := False) is
      procedure Write (File : in RGB_IO.File_Type; Item : in Interfaces.Unsigned_32);
      -- Writes the 4 bytes of Item to File, MSB first

      procedure Write (File : in RGB_IO.File_Type; Item : in Interfaces.Unsigned_32; CRC : in out CRC_32.CRC_Info);
      -- Writes the 4 bytes of Item to File, MSB first, and updates CRC with those bytes

      BPP : constant Positive := (if Grayscale then 1 else 3); -- Bytes per pixel

      subtype Scanline is CRC_32.Byte_List (1 .. BPP * Image'Length (2) );
      subtype Filtered_Scan is CRC_32.Byte_List (1 .. Scanline'Last + 1);

      package Compressed_Lists is new Ada.Containers.Vectors
         (Index_Type => Positive, Element_Type => Z_Compression.Byte_Value, "=" => Interfaces."=");

      Scan   : Scanline := (others => 0);
      Prev   : Scanline;
      Input  : Filtered_Scan;
      Output : Compressed_Lists.Vector;
      Y      : Natural := 0;
      X      : Positive := Integer'Last;
      CRC    : CRC_32.CRC_Info;

      function Out_Of_Data return Boolean is
         (Y > Image'Last (1) );

      function Next return Z_Compression.Byte_Value with
         Pre => not Out_Of_Data;
      -- Provide the next byte from Input to Compress

      procedure Put (Byte : in Z_Compression.Byte_Value);
      -- Appends a compressed byte to Output and updates CRC with it

      procedure Compress is new Z_Compression.Compress (Out_Of_Data => Out_Of_Data, Next => Next, Put => Put);

      use type Interfaces.Unsigned_32;

      function Next return Z_Compression.Byte_Value is
         function Extracted (Y : in Natural) return Scanline;
         -- Returns the Scanline at Y in Image

         use type CRC_32.Byte_List;

         function Filtered (Scan : in Scanline; Prev : in Scanline) return Filtered_Scan;
         -- Returns Scan, appropriately filtered

         function Filtered (Scan : in Scanline; Prev : in Scanline) return Filtered_Scan is
            use type Interfaces.Integer_8;

            function To_Signed is new Ada.Unchecked_Conversion (Source => CRC_32.Byte_Value, Target => Interfaces.Integer_8);

            function Signed (Byte : in CRC_32.Byte_Value) return Interfaces.Unsigned_32 is
               (Interfaces.Unsigned_32 (abs Integer (To_Signed (Byte) ) ) );

            function Paeth_Byte (Left : in CRC_32.Byte_Value; Up : in CRC_32.Byte_Value; Up_Left : in CRC_32.Byte_Value)
            return CRC_32.Byte_Value;
            -- The Paeth Predictor function

            function Paeth_Byte (Left : in CRC_32.Byte_Value; Up : in CRC_32.Byte_Value; Up_Left : in CRC_32.Byte_Value)
            return CRC_32.Byte_Value is
               P   : constant Integer := Integer (Left) + Integer (Up) - Integer (Up_Left);
               PA  : constant Integer := abs (P - Integer (Left   ) );
               PB  : constant Integer := abs (P - Integer (Up     ) );
               PC  : constant Integer := abs (P - Integer (Up_Left) );
               Min : constant Integer := Integer'Min (Integer'Min (PA, PB), PC);
            begin -- Paeth_Byte
               if PA = Min then
                  return Left;
               end if;

               if PB = Min then
                  return Up;
               end if;

               return Up_Left;
            end Paeth_Byte;

            Sub      : Scanline;
            Up       : Scanline;
            Avg      : Scanline;
            Pth      : Scanline;
            Sum_None : Interfaces.Unsigned_32 := 0;
            Sum_Sub  : Interfaces.Unsigned_32 := 0;
            Sum_Up   : Interfaces.Unsigned_32 := 0;
            Sum_Avg  : Interfaces.Unsigned_32 := 0;
            Sum_Pth  : Interfaces.Unsigned_32 := 0;
            Min      : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;

            use type CRC_32.Byte_Value;
         begin -- Filtered
            All_Bytes : for I in Scan'Range loop
               if I < BPP + 1 then -- First pixel of Scan
                  Sub (I) := Scan (I);
                  Avg (I) := Scan (I) - Prev (I) / 2;
                  Pth (I) := Scan (I) - Prev (I);
               else
                  Sub (I) := Scan (I) - Scan (I - BPP);
                  Avg (I) := Scan (I) - CRC_32.Byte_Value ( (Integer (Scan (I - BPP) ) + Integer (Prev (I) ) ) / 2);
                  Pth (I) := Scan (I) - Paeth_Byte (Scan (I - BPP), Prev (I), Prev (I - BPP) );
               end if;

               Up (I) := Scan (I) - Prev (I);
            end loop All_Bytes;

            Sum : for I in Scan'Range loop
               Sum_None := Sum_None + Signed (Scan (I) );
               Sum_Sub  := Sum_Sub  + Signed (Sub  (I) );
               Sum_Up   := Sum_Up   + Signed (Up   (I) );
               Sum_Avg  := Sum_Avg  + Signed (Avg  (I) );
               Sum_Pth  := Sum_Pth  + Signed (Pth  (I) );
            end loop Sum;

            Min := Interfaces.Unsigned_32'Min
                      (Interfaces.Unsigned_32'Min
                          (Interfaces.Unsigned_32'Min
                             (Interfaces.Unsigned_32'Min (Sum_None, Sum_Sub), Sum_Up), Sum_Avg), Sum_Pth);

            if Sum_None = Min then
               return 0 & Scan;
            end if;

            if Sum_Sub = Min then
               return 1 & Sub;
            end if;

            if Sum_Up = Min then
               return 2 & Up;
            end if;

            if Sum_Avg = Min then
               return 3 & Avg;
            end if;

            return 4 & Pth;
         end Filtered;

         function Extracted (Y : in Natural) return Scanline is
            Result : Scanline;
            To     : Positive := 1;
         begin -- Extracted
            Extract : for X in Image'Range (2) loop
               Result (To) := Image (Y, X).Red;

               if not Grayscale then
                  Result (To + 1) := Image (Y, X).Green;
                  Result (To + 2) := Image (Y, X).Blue;
               end if;

               To := To + BPP;
            end loop Extract;

            return Result;
         end Extracted;
      begin -- Next
         if X > Input'Last then
            Prev := Scan;
            Scan := Extracted (Y);
            Input := Filtered (Scan, Prev);
            X := 1;
         end if;

         X := X + 1;

         if X > Input'Last then
            Y := Y + 1;
         end if;

         return Input (X - 1);
      end Next;

      procedure Put (Byte : in Z_Compression.Byte_Value) is
         -- Empty
      begin -- Put
         Output.Append (New_Item => Byte);
         CRC_32.Update (CRC => CRC, Input => Byte);
      end Put;

      procedure Write (File : in RGB_IO.File_Type; Item : in Interfaces.Unsigned_32) is
         -- Empty
      begin -- Write
         RGB_IO.Write (File => File, Item => RGB_Value (Item / 2 ** 24) );
         RGB_IO.Write (File => File, Item => RGB_Value ( (Item / 2 ** 16) rem 256) );
         RGB_IO.Write (File => File, Item => RGB_Value ( (Item / 256) rem 256) );
         RGB_IO.Write (File => File, Item => RGB_Value (Item rem 256) );
      end Write;

      procedure Write (File : in RGB_IO.File_Type; Item : in Interfaces.Unsigned_32; CRC : in out CRC_32.CRC_Info) is
         -- Empty
      begin -- Write
         Write (File => File, Item => Item);

         CRC_32.Update (CRC => CRC, Input => RGB_Value (Item / 2 ** 24) );
         CRC_32.Update (CRC => CRC, Input => RGB_Value ( (Item / 2 ** 16) rem 256) );
         CRC_32.Update (CRC => CRC, Input => RGB_Value ( (Item / 256) rem 256) );
         CRC_32.Update (CRC => CRC, Input => RGB_Value (Item rem 256) );
      end Write;

      Signature : constant CRC_32.Byte_List := (137, 80, 78, 71, 13, 10, 26, 10);
      IHDR_ID   : constant CRC_32.Byte_List := (73, 72, 68, 82);
      IDAT_ID   : constant CRC_32.Byte_List := (73, 68, 65, 84);
      IEND_ID   : constant CRC_32.Byte_List := (73, 69, 78, 68);

      File : RGB_IO.File_Type;

      use type Ada.Containers.Count_Type;
   begin -- Write_PNG
      RGB_IO.Create (File => File, Mode => RGB_IO.Out_File, Name => File_Name);
      Output.Reserve_Capacity (Capacity => 6 * Image'Length (1) * Image'Length (2) );

      PNG_Signature : for Byte of Signature loop
         RGB_IO.Write (File => File, Item => Byte);
      end loop PNG_Signature;

      -- IHDR chunk
      Write (File => File, Item => 13); -- Length

      IHDR : for Byte of IHDR_ID loop -- Chunk type
         RGB_IO.Write (File => File, Item => Byte);
      end loop IHDR;

      CRC_32.Update (CRC => CRC, List => IHDR_ID);
      Write (File => File, Item => Image'Length (2), CRC => CRC); -- Width
      Write (File => File, Item => Image'Length (1), CRC => CRC); -- Height
      RGB_IO.Write (File => File, Item => 8); -- Bit depth
      CRC_32.Update (CRC => CRC, Input => 8);
      RGB_IO.Write (File => File, Item => (if Grayscale then 0 else 2) ); -- Color type
      CRC_32.Update (CRC => CRC, Input => (if Grayscale then 0 else 2) );
      RGB_IO.Write (File => File, Item => 0); -- Compression method
      CRC_32.Update (CRC => CRC, Input => 0);
      RGB_IO.Write (File => File, Item => 0); -- Filter method
      CRC_32.Update (CRC => CRC, Input => 0);
      RGB_IO.Write (File => File, Item => 0); -- Interlace method
      CRC_32.Update (CRC => CRC, Input => 0);
      Write (File => File, Item => CRC_32.Final (CRC) );

      -- IDAT chunk
      CRC_32.Reset (CRC => CRC);
      CRC_32.Update (CRC => CRC, List => IDAT_ID);
      Compress (Method => Z_Compression.Deflate_3);
      Write (File => File, Item => Interfaces.Unsigned_32 (Output.Length) ); -- Length

      IDAT : for Byte of IDAT_ID loop -- Chunk type
         RGB_IO.Write (File => File, Item => Byte);
      end loop IDAT;

      Data : for I in 1 .. Output.Last_Index loop
         RGB_IO.Write (File => File, Item => Output.Element (I) );
      end loop Data;

      Write (File => File, Item => CRC_32.Final (CRC) );

      -- IEND chunk
      CRC_32.Reset (CRC => CRC);
      Write (File => File, Item => 0); -- Length

      IEND : for Byte of IEND_ID loop -- Chunk type
         RGB_IO.Write (File => File, Item => Byte);
      end loop IEND;

      CRC_32.Update (CRC => CRC, List => IEND_ID);
      Write (File => File, Item => CRC_32.Final (CRC) );

      RGB_IO.Close (File => File);
   end Write_PNG;

   procedure Read (Name : in String; Image : in out Holders.Handle) is
      procedure Load (Image : in out Image_Data);
      -- Load the image into Image

      Header : GID.Image_Descriptor;

      procedure Load (Image : in out Image_Data) is
         procedure Set_X_Y (X : in Natural; Y : in Natural);
         -- Sets Row and Column to Height - Y - 1 and X, respectively

         procedure Put (Red : in RGB_Value; Green : in RGB_Value; Blue : in RGB_Value; Alpha : in RGB_Value);
         -- Sets Data (Row, Column) to the provided color, and increments Column

         procedure Feedback (I : in Natural) is null;

         Row    : Natural := 0;
         Column : Natural := 0;

         procedure Set_X_Y (X : in Natural; Y : in Natural) is
            -- Empty
         begin -- Set_X_Y
            Row := GID.Pixel_Height (Header) - Y - 1;
            Column := X;
         end Set_X_Y;

         procedure Put (Red : in RGB_Value; Green : in RGB_Value; Blue : in RGB_Value; Alpha : in RGB_Value) is
            -- Empty
         begin -- Put
            Image (Row, Column) := (Red => Red, Green => Green, Blue => Blue);
            Column := Column + 1;
         end Put;

         procedure Load_Image is new GID.Load_Image_Contents (Primary_Color_Range => RGB_Value,
                                                              Set_X_Y             => Set_X_Y,
                                                              Put_Pixel           => Put,
                                                              Feedback            => Feedback,
                                                              Mode                => GID.Fast);

         Next_Frame : Duration;
      begin -- Load
         Load_Image (Image => Header, Next_Frame => Next_Frame);
      end Load;

      File : Ada.Streams.Stream_IO.File_Type;
   begin -- Read
      Ada.Streams.Stream_IO.Open (File => File, Mode => Ada.Streams.Stream_IO.In_File, Name => Name);
      GID.Load_Image_Header (Image => Header, From => Ada.Streams.Stream_IO.Stream (File).all, Try_TGA => True);
      Image.Create (Width => GID.Pixel_Width (Header), Height => GID.Pixel_Height (Header) );
      Image.Update (Process => Load'Access);
      Ada.Streams.Stream_IO.Close (File => File);
   end Read;
end Image_IO.Operations;
