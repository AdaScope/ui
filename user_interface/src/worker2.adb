--                                                                    --
--  package Worker    Copyright (c) Yogeshwarsing Calleecharan, 2010  --
--  Implementation                   Dmitry A. Kazakov, 2012           --
--                                                                    --
--                                Last revision :  15:58 22 Jan 2012  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Calendar;     use Ada.Calendar;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Numerics;     use Ada.Numerics;
with GLib;             use GLib;
with Gtk.Main.Router;  use Gtk.Main.Router;

with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with ada.numerics.discrete_random;

package body Worker2 is

   X : Integer := 0;
   Y : Integer := 0;
   --  Z : Integer := 1;

   
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
      X := 0;
      Y := 0;
      Put_Line("Bopp ");
      for n in 0..100 loop
         reset(gen);
         Z := random(gen);
         Scope.Feed
               (  Channel => Channel,
                  T       => GDouble (Float(X)*Float(Z)),
                  V       => GDouble (Y)
               );
         X := X + 1;
         Y := Y + 1;
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
      Steps     : Positive;

      -- Initial time, final time, no of steps, step size
      A : constant := 0.0;
      B : constant := 10_000.0;
      N : constant := 2_500_000;
      H : constant := (B - A) / Long_Float (N);

      -- Driving frequencies over whirling range
      Forcing_Frequency_Start : Long_Float;
      Forcing_Frequency_Limit : Long_Float;
      Forcing_Frequency_Step  : Long_Float;

      -- Minmax algorithm
      Largest      : Long_Float;
      Currentvalue : Long_Float;

   begin
      select -- Waiting for parameters or exit request
         accept Start
                (  Data     : Parameters;
                   Scope    : Gtk_Oscilloscope;
                   Channel  : Channel_Number
                )
         do
           Forcing_Frequency_Start := Data.Start;
           Forcing_Frequency_Limit := Data.Stop;
           Forcing_Frequency_Step :=
              (Data.Stop - Data.Start) / Long_Float (Data.Steps);
           Process.Scope    := Scope;
           Process.Channel  := Channel;
           Steps            := Data.Steps;
         end;
      or accept Stop;
         raise Quit_Error;
      end select;
      -- Starting computations

      -- Looping
      while (Clock - Start_Time < 10.0) loop
         --  Put_Line("N: "&Duration'Image(Clock - Start_Time));
         --
         -- Updating each 200ms
         --
         if Clock - Last_Time > 0.2 then
            select
               accept Stop; -- Check if existing is requested
                  raise Quit_Error;
            else
               Last_Time := Clock;
               Put_Line("Boop ");
               --  get_data;
               Feed_UART_Data(Scope, Channel);
            end select;
         end if;

      end loop;
      Put_Line("Ended loop");
      --  return;
      accept Stop;
   exception
      when Quit_Error | Busy_Error => -- Main loop quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error));
      Put_Line("Ending process");
   end Process;

end Worker2;
