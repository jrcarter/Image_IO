-- Image I/O: output in PPM, BMP, and QOI formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Sequential_IO;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GID;
with Image_IO.QOI;
with Image_IO.QOI_Holders;
with PragmARC.Text_IO;
with System;

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
