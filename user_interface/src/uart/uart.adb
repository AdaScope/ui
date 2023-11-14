with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Serial_Communications;
with Ada.Float_Text_IO;
with Ada.IO_Exceptions;
with Ada.Streams;
with Globals;

package body Uart is

   function Get_Data (
      Number_Of_Samples : Integer
   ) return Readings_Array is
      Readings : Readings_Array (1 .. Number_Of_Samples);
   begin
      Readings := Read (Number_Of_Samples => Number_Of_Samples);
      return Readings;
   end Get_Data;

   function Get_Triggered_Data (
      Data          : Readings_Array;
      Capture_Start : Integer;
      Capture_End   : Integer
   ) return Readings_Array is
      New_Data      : Readings_Array (Capture_Start .. Capture_End);
   begin
      New_Data := Data (Capture_Start .. Capture_End);
      return New_Data;
   end Get_Triggered_Data;

   function Get_Processed_Data (
      Number_Of_Samples : Integer
   ) return Readings_Array is
      Triggered     : Boolean        := False;
      Capture_Start : Integer        := 1;
      Capture_End   : Integer        := Number_Of_Samples / 2;
      Data          : Readings_Array (1 .. Number_Of_Samples);
      Data_Min      : Float          := 5000.0;  --  Oscilloscope max is 3000
      Data_Max      : Float          := 0.0;
      Trigger_Level : Float;
   begin
      Data := Get_Data (Number_Of_Samples);

      for I in Data'Range loop
         Data_Min := Float'Min (Data_Min, Data (I));
         Data_Max := Float'Max (Data_Max, Data (I));
      end loop;

      Trigger_Level := (Data_Min + Data_Max) / 2.0;

      for I in Data'Range loop
         --  Check if can be triggered
         if Data (I) > Trigger_Level - 100.0 and then
            Data (I) < Trigger_Level + 100.0 and then
            not Triggered
         then
            --  Check for correct slope
            if I + 3 <= Number_Of_Samples then
               if Data (I) < Data (I + 3) then
                  Triggered := True;
               end if;
            elsif I - 3 >= 1 then
               if Data (I - 3) < Data (I) then
                  Triggered := True;
               end if;
            end if;

            if Triggered then
               --  Find trigger point and collect data before and after
               Capture_Start :=
                  Integer'Max (1, I - (Number_Of_Samples / 4));
               Capture_End   :=
                  Integer'Min (Data'Last, I + (Number_Of_Samples / 4));

               if Capture_End - Capture_Start /= Number_Of_Samples / 2 then
                  Capture_Start := 1;
                  Capture_End   := Number_Of_Samples / 2;
                  Triggered := False;
               end if;
            end if;

            exit when Triggered;

         end if;
      end loop;

      declare
         Triggered_Data : Readings_Array (Capture_Start .. Capture_End - 1);
      begin
         Triggered_Data := Get_Triggered_Data (
            Data => Data,
            Capture_Start => Capture_Start,
            Capture_End => Capture_End - 1
         );
         return Triggered_Data;
      end;
   end Get_Processed_Data;

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
