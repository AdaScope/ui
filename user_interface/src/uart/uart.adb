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
      Triggered     : Boolean        := False;
      Capture_Start : Integer        := 1;
      Capture_End   : Integer        := Number_Of_Samples / 2;
      Data_Min      : Float          := 5000.0;  --  Oscilloscope max is 3000
      Data_Max      : Float          := 0.0;
      Trigger_Level : Float;
   begin

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
         --  return Triggered_Data;
         --  Faut que cette procedure sache quel channel que c'est
         Globals.Processed_Data.Set_Data (Channel, Triggered_Data);
      end;
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
