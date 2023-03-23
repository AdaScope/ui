with Ada.Calendar;     use Ada.Calendar;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Numerics;     use Ada.Numerics;
with GLib;             use GLib;
with Gtk.Main.Router;  use Gtk.Main.Router;

with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with ada.numerics.discrete_random;

with Data_structures;

package body Worker2 is

   Paused : Boolean := True;

   --
   -- Temporary
   -- X and Y to be replaced with Data_Points objects
   -- Z and random generator to be removed
   --
   X : Integer := 0;
   Y : Integer := 0;

   type randRange is new Integer range 1..100;
   package Rand_Int is new ada.numerics.discrete_random(randRange);
   use Rand_Int;
   gen : Generator;
   Z : randRange;

   --
   -- Feeding data to oscilloscope
   --
   procedure Feed_UART_Data (
      Scope : Gtk_Oscilloscope;
      Channel   : Channel_Number) is
   begin
      --
      -- Get data from UART
      --
      X := 0;                                                  -- v  Temporary, for testing purposes  v
      Y := 0;                                                  --
      for n in 0..100 loop                                     --
         reset(gen);                                           --
         Z := random(gen);                                     -- ^  Temporary, for testing purposes  ^
         Scope.Feed
               (  Channel => Channel,
                  T       => GDouble (Float(X)*Float(Z)), -- Feed X data points
                  V       => GDouble (Y)                  -- Feed Y data points
               );
         X := X + 1;                                           -- v  Temporary, for testing purposes  v
         Y := Y + 1;                                           -- ^  Temporary, for testing purposes  ^
      end loop;

   end Feed_UART_Data;

   --
   -- Process -- The task doing actual computations
   --

   task body Process is
      Scope     : Gtk_Oscilloscope;
      Channel   : Channel_Number;
      Last_Time : Time := Clock;
      Start_Time : Time := Clock;

      -- Initial time, final time, no of steps, step size
      A : constant := 0.0;
      B : constant := 10_000.0;  -- To change depending on period
      N : constant := 2_500_000; -- To change depending on buffer size
      H : constant := (B - A) / Long_Float (N);

      -- Minmax algorithm
      Largest      : Long_Float;
      Currentvalue : Long_Float;

   begin
      select -- Waiting for parameters or exit request
         accept Start
                ( Scope    : Gtk_Oscilloscope;
                  Channel  : Channel_Number
                )
         do
            Process.Scope    := Scope;
            Process.Channel  := Channel;
         end;
      or accept Stop;
         raise Quit_Error;
      end select;
      -- Starting computations

      -- Looping
      while True loop
         --
         -- Updating each 200ms
         --
         if Clock - Last_Time > 0.2 then
            select
               accept Pause do-- Check if existing is requested
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
      when Quit_Error | Busy_Error => -- Main loop quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error));
      Put_Line ("Ending process");
   end Process;

end Worker2;
