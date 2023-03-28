
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;

package Worker is
--
-- Process -- Calculation process task
--
   task type Process is
   --
   -- Start -- Computations with the parameters specified
   --
   --    Scope      - The oscilloscope
   --    Channel    - The number of the channel to feed
   --
      entry Start
            (
               scope    : Gtk_Oscilloscope;
               channel  : Channel_Number
            );
   --
   -- Stop -- Terminate the task prematurely
   --
      entry Stop;
   end Process;
end Worker;
