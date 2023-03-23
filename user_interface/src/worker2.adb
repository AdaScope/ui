with Ada.Calendar;      use Ada.Calendar;
with Ada.Exceptions;    use Ada.Exceptions;
with Glib;              use Glib;
with Gtk.Main.Router;   use Gtk.Main.Router;
with Ada.Numerics.Discrete_Random;

with Ada.Text_IO;       use Ada.Text_IO;
--  with Data_structures;

package body Worker2 is

   Paused : Boolean := True;

   --
   --  Temporary
   --  X and Y to be replaced with Data_Points objects
   --  Z and random generator to be removed
   --
   X : Integer := 0;
   Y : Integer := 0;

   type randRange is new Integer range 1 .. 100;
   package Rand_Int is new Ada.Numerics.Discrete_Random (randRange);
   use Rand_Int;
   gen : Generator;
   Z : randRange;

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
      --  v  Temporary, for testing purposes  v
      X := 0;
      Y := 0;
      --  ^  Temporary, for testing purposes  ^
      for n in 0 .. 100 loop
         Reset (gen);
         Z := Random (gen);
         Scope.Feed
               (Channel => Channel,
                  T     => Gdouble (Float (X) * Float (Z)),
                  V     => Gdouble (Y)
               );
         X := X + 1;
         Y := Y + 1;
      end loop;

   end Feed_UART_Data;

   --
   --  Process
   --  Managing pause/play
   --

   task body Process is
      Scope     : Gtk_Oscilloscope;
      Channel   : Channel_Number;
      Last_Time : Time := Clock;

   begin
      select -- Waiting for parameters or exit request
         accept Start
                (Scope     : Gtk_Oscilloscope;
                  Channel  : Channel_Number
                )
         do
            Process.Scope    := Scope;
            Process.Channel  := Channel;
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
               accept Pause do--  Check if existing is requested
                  Paused := True;
               end Pause;
               or accept Play do
                  Paused := False;
               end Play;
               or accept Stop do
                  raise Quit_Error;
               end Stop;
            else
               Last_Time := Clock;
               if not Paused then
                  Feed_UART_Data(Scope, Channel);
               end if;
            end select;
         end if;

      end loop;
      --  return;
      accept Stop;
   exception
      when Quit_Error | Busy_Error => --  Main loop quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error));
      Put_Line ("Ending process");
   end Process;

end Worker2;
