
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;

package Worker2 is
--
-- Process -- Calculation process task
--
   task type Process is
      entry Play;
      entry Pause;
   --
   -- Start -- Computations with the parameters specified
   --
   --    Scope      - The oscilloscope
   --    Channel    - The number of the channel to feed
   --
      entry Start
            (
               Scope    : Gtk_Oscilloscope;
               Channel  : Channel_Number
            );
   --
   -- Stop -- Terminate the task prematurely
   --
      entry Stop;
   end Process;
end Worker2;
