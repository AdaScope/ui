with GNAT.Serial_Communications;
with Gtk.Main.Router;
with Ada.Streams;
with Globals;
with Min_Ada;

package body Uart is

   procedure Process_Data (
      Channel : Integer;
      Data : Readings_Array;
      Number_Of_Samples : Integer
   ) is
      --  Denotes the start and end of the captured data
      --  This will be a subset of the data comming  in
      --  and will be half its size
      Capture_Start  : Integer;
      Capture_End    : Integer;

      --  To find the center point of the wave
      Data_Min       : Float   := 5000.0; --  Higher than physical max (3000)
      Data_Max       : Float   := 0.0;    --  Lower or equal to max

      --  The voltage value of the middle of the wave
      Trigger_Level  : Float;

      --  If all the trigger conditions are met
      Triggered      : Boolean := False;
   begin

      --  Set the trigger point in the center
      for I in Data'Range loop
         Data_Min := Float'Min (Data_Min, Data (I));
         Data_Max := Float'Max (Data_Max, Data (I));
      end loop;
      Trigger_Level := (Data_Min + Data_Max) / 2.0;

      --  Loop over all the valid data buffer
      for I in (Number_Of_Samples / 4) + 1 ..
         Number_Of_Samples - (Number_Of_Samples / 4) loop

         --  Check if data in the trigger range
         if Data (I + 1) > Trigger_Level and then
            Data (I) <= Trigger_Level
         then
            --  Take data before and after trigger point
            --  (trigger will be in center)
            Capture_Start := I - (Number_Of_Samples / 4) + 1;
            Capture_End := I + (Number_Of_Samples / 4);

            --  Make sure we have the correct number of samples
            --  (Should be half of the data buffer)
            if (Capture_End - Capture_Start) /=
               (Number_Of_Samples / 2) - 1
            then
               Triggered     := False;
            else
               Triggered     := True;
            end if;
         end if;
      end loop;

      --  Save the processed data in the processed data array
      --  only if we were able to trigger
      if Triggered then
         Globals.Processed_Data.Set_Data (
            Channel => Channel,
            Data    => Data (Capture_Start .. Capture_End)
         );
      end if;
   end Process_Data;

   task body Read is

      --  Variables for the serial read
      Buffer   : Ada.Streams.Stream_Element_Array (1 .. 1);
      Offset   : Ada.Streams.Stream_Element_Offset := 1;

      --  Context for the min protocol
      Context  : Min_Ada.Min_Context;
   begin

      select -- Waiting for parameters or exit request
         accept Start do

            Min_Ada.Min_Init_Context (Context => Context);

            loop
               --  Read data from serial port
               GNAT.Serial_Communications.Read (
                  Port   => Globals.Port,
                  Buffer => Buffer,
                  Last   => Offset
               );
               --  Send data to protocol for processing
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
