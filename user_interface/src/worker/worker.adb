with Ada.Calendar;      use Ada.Calendar;
with Ada.Exceptions;    use Ada.Exceptions;
with Gtk.Main.Router;   use Gtk.Main.Router;
with Ada.Numerics;     use Ada.Numerics;
with Ada.Text_IO;       use Ada.Text_IO;
with Glib;              use Glib;

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

--  with Data_structures;

package body Worker is
   --
   --  Temporary
   --  X and Y to be replaced with Data_Points objects
   --
   X : Integer := 0;
   Y : Long_Float := 0.0;

   --
   --  Feeding data to oscilloscope
   --
   procedure Feed_UART_Data (
      Scope : Gtk_Oscilloscope;
      Channel   : Channel_Number) is
   begin
      --
      --  Get data from UART
      --
      for n in 0 .. 100 loop
         X := n;
         Y := Sin (Long_Float (Float (2.0 * 0.1 * Pi * X) + Float (Channel)));
         --  Put_Line("Channel " & Channel_Number'Image (Channel) & " - " & Integer'Image (X) & " - " & Long_Float'Image (Y));
         Scope.Feed
               (Channel => Channel,
                  T     => Gdouble (X),
                  V     => Gdouble (Y)
               );
      end loop;

   end Feed_UART_Data;

   --
   --  Process
   --  Managing pause/play
   --

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
         --
         --  Updating each 200ms
         --
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
      Put_Line ("Invalid loop end ch " & Channel_Number'Image (Channel1));
      --  return;
      accept Stop;
   exception
      when Quit_Error | Busy_Error => --  Main loop quitted, we follow
         Put_Line ("Quit ch " & Channel_Number'Image (Channel1));
         null;
      when Error : others =>
         Put_Line ("Error ch " & Channel_Number'Image (Channel1));
         Say (Exception_Information (Error));
      Put_Line ("Ending process");
   end Process;

end Worker;