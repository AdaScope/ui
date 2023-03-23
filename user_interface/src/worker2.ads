
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;
with Gtk.Progress_Bar;  use Gtk.Progress_Bar;

package Worker2 is
--
-- Parameters -- Of a calculation session
--
   type Parameters is record
      Start     : Long_Float;
      Stop      : Long_Float;
      Steps     : Positive;
   end record;

--
-- Process -- Calculation process task
--
   task type Process is
      entry Play;
      entry Pause;
   --
   -- Start -- Computations with the parameters specified
   --
   --    Parameters - To use in the computations
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
