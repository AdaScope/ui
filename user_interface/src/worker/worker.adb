with Ada.Calendar;      use Ada.Calendar;
with Ada.Exceptions;    use Ada.Exceptions;
with Gtk.Main.Router;   use Gtk.Main.Router;
with Ada.Text_IO;       use Ada.Text_IO;
with Glib;              use Glib;
with Uart;

--  with Data_structures;

package body Worker is

   --  Variables for data collection
   X : Integer := 0;
   Y : Float := 0.0;
   Number_Of_Samples : constant Integer := 100;

   --  Array for storing the data from the board
   Readings : Uart.Readings_Array (1 .. Number_Of_Samples);

   --  Feeding data to oscilloscope
   procedure Feed_UART_Data (
      Scope : Gtk_Oscilloscope;
      Channel   : Channel_Number) is
   begin

      --  Get data from UART
      Readings := Uart.Read
        (Number_Of_Samples => Number_Of_Samples,
         Port_Location => "/dev/ttyACM0");

      --  Feed data to the graph
      for N in 1 .. Number_Of_Samples loop
         X := N;
         Y := Readings (N);
         
         Scope.Feed
               (Channel => Channel,
                  T     => Gdouble (X),
                  V     => Gdouble (Y)
               );
      end loop;
   end Feed_UART_Data;

   --  Managing pause/play
   task body Process is
      Scope     : Gtk_Oscilloscope;
      Channel1   : Channel_Number;
      Channel2   : Channel_Number;
      Channel3   : Channel_Number;
      Last_Time : Time := Clock;

   begin
      select -- Waiting for parameters or exit request
         accept Start
                (Scope     : Gtk_Oscilloscope;
                  Channel1  : Channel_Number;
                  Channel2  : Channel_Number;
                  Channel3  : Channel_Number
                )
         do
            Process.Scope    := Scope;
            Process.Channel1  := Channel1;
            Process.Channel2  := Channel2;
            Process.Channel3  := Channel3;
            Put_Line ("Start ch " & Channel_Number'Image (Channel1));
            Put_Line ("Start ch " & Channel_Number'Image (Channel2));
            Put_Line ("Start ch " & Channel_Number'Image (Channel3));
         end Start;

      or accept Stop;
         raise Quit_Error;
      end select;

      --  Starting computations
      --  Looping
      while True loop

         --  Updating each 200ms
         if Clock - Last_Time > 0.2 then
            select
               accept Stop; -- Check if existing is requested
               raise Quit_Error;
            else
               Last_Time := Clock;
               Feed_UART_Data (Scope, Channel1);
               Feed_UART_Data (Scope, Channel2);
               Feed_UART_Data (Scope, Channel3);
            end select;
         end if;

      end loop;
      Put_Line ("Invalid loop end channel" & Channel_Number'Image (Channel));
      accept Stop;

   exception
      when Quit_Error | Busy_Error => --  Main loop quitted, we follow
         Put_Line ("Quit channel" & Channel_Number'Image (Channel));
         null;

      when Error : others =>
         Put_Line ("Error channel" & Channel_Number'Image (Channel));
         Say (Exception_Information (Error));
      Put_Line ("Ending process");
   end Process;
end Worker;
