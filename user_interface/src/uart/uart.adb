with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Serial_Communications;
with Ada.Float_Text_IO;
with Ada.IO_Exceptions;
with Ada.Streams;
with Globals;

package body Uart is

   type Received_Bytes is array (Integer range <>) of Min_Ada.Bytes;

   function Get_Data (
      Number_Of_Samples : Integer
   ) return Boolean is
      Data : Received_Bytes;
   begin
      for I in 1 .. Number_Of_Samples loop
         Data (I) := Min_Ada.Rx_Bytes;
         I        := I + 1;
      end loop;
      return True;
   end Get_Data;

   procedure Process_Data (
      Data              : Min_Ada.Data_Array;
      Trigger_Level     : Float;
      Number_Of_Samples : Integer
   ) is
      Triggered     : Boolean  := False;
      Capture_Start : Positive := 1;
      Capture_End   : Positive := 1;
   begin
      for I in Data'Range loop
         if Float'Val (I) > Trigg and not Triggered then
            null;
            --  Trigger condition met
            Triggered := True;

            --  Find trigger point and collect data before and after
            Capture_Start := Integer'Max (1, I - (Number_Of_Samples / 2));
            Capture_End   := Integer'Max (1, I - (Number_Of_Samples / 2));
            --  TODO Exit the loop
         end if;
      end loop;

      if Triggered then
         Get_Triggered_Data (Data, Capture_Start, Capture_End);
      else
         Get_Triggered_Data (Data, 1, Number_Of_Samples);
      end if;
   end Process_Data;

   function Get_Triggered_Data (
      Data          : Integer;
      Capture_Start : Positive;
      Capture_End   : Positive
   ) return Integer is
      New_Data      : Integer;
   begin
      New_Data := Data (Capture_Start .. Capture_End);
      return New_Data;
   end Get_Triggered_Data;

   function Read (
      Number_Of_Samples : Integer
   ) return Readings_Array is

      --  Initialize the variables for the read
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
      Offset : Ada.Streams.Stream_Element_Offset := 1;

      --  For storing one reading
      Line       : String (1 .. 16);
      Line_Index : Natural := Line'First;
      Char       : Character;

      --  To count the number of character readings done
      Counter    : Integer := 1;

      --  For storing all the readings
      Readings   : Uart.Readings_Array (1 .. Number_Of_Samples);

   begin

      --  We initialize the string to eliminate warnings
      for I in Line'Range loop
         Line (I) := '0';
      end loop;

      --  Make sure to only start collecting data at start of new line
      loop
         GNAT.Serial_Communications.Read (Globals.Port, Buffer, Offset);
         exit when Character'Val (Buffer (1)) = ASCII.LF;
      end loop;

      --  Run until the we gathered the required number of samples
      while Counter < Number_Of_Samples + 1 loop
         begin

            GNAT.Serial_Communications.Read (Globals.Port, Buffer, Offset);

            --  Store the reading in the Char variable
            Char := Character'Val (Buffer (1));

            --  If we read the end of the line
            --  and we are not a the beginning of a line
            if Char = ASCII.LF and then Line_Index /= 1 then

               --  We save the reading to an array
               Ada.Float_Text_IO.Get
                 (From => Line (1 .. Line_Index - 1),
                  Item => Readings (Counter),
                  Last => Line_Index);

               --  We reset the line index and increment the counter
               Line_Index := Line'First;
               Counter := Counter + 1;

            --  If we are not a the end of the line
            elsif Char /= ASCII.LF then

               --  We write the current character to
               --  the current index of our line and increment the line
               Line (Line_Index) := Char;
               Line_Index := Line_Index + 1;
            end if;
         exception
            when Ada.IO_Exceptions.Data_Error =>
               Put_Line ("Data error");
               Line_Index := Line_Index - 1;
         end;
      end loop;
      return Readings;
   end Read;
end Uart;
