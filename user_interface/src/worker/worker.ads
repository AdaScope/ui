
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;

package Worker is
--
--  Channel treads
--
   task type Process is
   --
   --  Start -- Computations with the parameters specified
   --    Scope      - The oscilloscope
   --    Channel    - The number of the channel to feed
   --
      entry Start
            (
               Scope     : Gtk_Oscilloscope;
               Channel_1 : Channel_Number;
               Channel_2 : Channel_Number;
               Channel_3 : Channel_Number
            );
   --
   --  Stop -- Terminate the task prematurely
   --
      entry Stop;
   end Process;
end Worker;
