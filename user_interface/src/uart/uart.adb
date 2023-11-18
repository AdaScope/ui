with GNAT.Serial_Communications;
with Gtk.Main.Router;
with Ada.Streams;
with Globals;
with Min_Ada;

package body Uart is

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

   procedure Process_Data (
      Channel : Integer;
      Data : Readings_Array;
      Number_Of_Samples : Integer
   ) is
      --  Denotes the start and end of the captured data
      --  This will be a subset of the data comming  in
      --  and will be half its size
      Capture_Start  : Integer := 1;
      Capture_End    : Integer := Number_Of_Samples / 2;

      Data_Min       : Float   := 5000.0;  --  Oscilloscope max is 3000
      Data_Max       : Float   := 0.0;

      Trigger_Level  : Float;

      Positive_Slope : Boolean := False;
   begin

      --  Set the trigger point in the center
      for I in Data'Range loop
         Data_Min := Float'Min (Data_Min, Data (I));
         Data_Max := Float'Max (Data_Max, Data (I));
      end loop;
      Trigger_Level := (Data_Min + Data_Max) / 2.0;

      for I in Data'Range loop

         --  Check if in the correct range for trigger
         if Data (I) > Trigger_Level - 100.0 and then
            Data (I) < Trigger_Level + 100.0
         then

            --  Check if positive slope
            if I + 3 <= Number_Of_Samples then
               if Data (I) < Data (I + 3) then
                  Positive_Slope := True;
               else
                  Positive_Slope := False;
               end if;
            elsif I - 3 >= 1 then
               if Data (I - 3) < Data (I) then
                  Positive_Slope := True;
               else
                  Positive_Slope := False;
               end if;
            end if;

            --  Check if trigger in correct range
            --  to be able to take enough data to its left and its right
            if Positive_Slope then
               if I - (Number_Of_Samples / 4) + 1 >= 1 and then
                  I + (Number_Of_Samples / 4) <= Number_Of_Samples
               then
                  Capture_Start := I - (Number_Of_Samples / 4) + 1;
                  Capture_End := I + (Number_Of_Samples / 4);
               end if;

               if (Capture_End - Capture_Start) /=
                  (Number_Of_Samples / 2) - 1
               then
                  Capture_Start := 1;
                  Capture_End   := Number_Of_Samples / 2;
               end if;
            end if;
         end if;
      end loop;

      Globals.Processed_Data.Set_Data (
         Channel => Channel,
         Data    => Data (Capture_Start .. Capture_End)
      );
   end Process_Data;

   task body Read is --  À modifier pour protocole min

      --  Initialize the variables for the read
      Buffer   : Ada.Streams.Stream_Element_Array (1 .. 1);
      Offset   : Ada.Streams.Stream_Element_Offset := 1;
      Context  : Min_Ada.Min_Context;

   begin

      select -- Waiting for parameters or exit request
         accept Start do

            Min_Ada.Min_Init_Context (Context => Context);

            loop
               GNAT.Serial_Communications.Read (
                  Port   => Globals.Port,
                  Buffer => Buffer,
                  Last   => Offset
               );
               Min_Ada.Rx_Bytes (
                  Context => Context,
                  Data => Min_Ada.Byte (Buffer (1))
               );
            end loop;
         end Start;

      or accept Stop;
         raise Gtk.Main.Router.Quit_Error;
      end select;

      accept Stop;
   end Read;
end Uart;
