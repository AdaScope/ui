
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Glib;                  use Glib;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Oscilloscope;      use Gtk.Oscilloscope;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;
with Ada.Streams;           use Ada.Streams;
with Data_Structures;       use Data_Structures;
with Ada.Numerics.Elementary_Functions;
with Gtk.Layered;
with Gtk.Main;
with Ada.Calendar;
with On_Line_Plotter;

procedure User_Interface is
   Base_Data_Array : Data_Points;
   Buffer1A : Buffer;
   Buffer1B : Buffer;

    -- For the Window
   Window : Gtk_Window;

   --   -- For UART
   --   Port : GNAT.Serial_Communications.Serial_Port;
   --   --Data : GNAT.Serial_Communications.Stream_Element_Array(1..100);
   --   --Data : GNAT.Serial_Communications.Buffer_Type(1..100);
   --   Data : Ada.Streams.Stream_Element_Array (1 .. 10);

   --   Test : Stream_Element_Offset;

begin


On_Line_Plotter;

--     Gtk.Main.Init;
--     Gtk.Window.Gtk_New (Window);
--     Window.Set_Title ("Test plotting");
--     Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
--     Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
--     Base_Data_Array := Set_To_Zero;

--  -- Channel 1, buffers A and B
--     Buffer1A := (Data   => Base_Data_Array,
--                  IsFull => False);
--     Buffer1B := (Data   => Base_Data_Array,
--                  IsFull => False);

--     for I in Buffer1A.Data'Range loop
--        Buffer1A.Data(I) := Float (I*I*I);
--     end loop;

--     declare
--        Curve        : Channel_Number;
--        Oscilloscope : Gtk_Oscilloscope;
--     begin
--        Gtk_New (Oscilloscope);
--        Add (Window, Oscilloscope);
--        Oscilloscope.Set_Manual_Sweep (False);
--         --
--         -- Configuring the lower axis
--         --
--        Oscilloscope.Set_Frozen     (Lower, True);  -- No sweeping
--        Oscilloscope.Set_Time_Scale (Lower, False); -- No scale (slider)
--        Oscilloscope.Set_Time_Grid  (Lower, True);  -- Grid
--        Oscilloscope.Set_Time_Axis
--           (  Lower,
--              True,  -- Visible
--              False  -- As plain numbers
--           );
--        Oscilloscope.Get_Sweeper (Lower).Configure
--           (  Value => 0.0,
--              Lower => 0.0,
--              Upper => 20.0,
--              Step_Increment => 0.1,
--              Page_Increment => 5.0,
--              Page_Size      => GDouble (Buffer1A.Data'Length)
--           );
--          --
--          -- Adding the channel
--          --
--        Curve := 
--           Add_Channel
--              (  Widget  => Oscilloscope,
--                 Mode    => Gtk.Layered.Linear, -- Linear interpolation
--                 Color   => RGB (0.0, 0.0, 0.7),
--                 Sweeper => Lower
--              );
--             --
--             -- Configuring the left axis for this channel (and its group)
--             --
--        Oscilloscope.Set_Group (Left, Oscilloscope.Get_Group (Curve));
--        Oscilloscope.Set_Values_Axis  (Left, True);
--        Oscilloscope.Set_Values_Scale (Left, False);
--        Oscilloscope.Set_Values_Grid  (Left, True);
--        Oscilloscope.Set_Values_Axis_Width (Left, 60);
--           --
--           -- Pushing the data into the channel's buffer
--        declare
--           use Ada.Numerics.Elementary_Functions;
--           X : Float := 0.0;
--           Y : Integer := 1;

--           boo : Float := 1.0; 
--           Period : Duration := 2.0;
--           use Ada.Calendar;
--           Next_Time : Time := Clock + Period;

--        begin
--           for n in 0..5 loop
--              X := 0.0;
--              loop
--                 Oscilloscope.Feed
--                    (  Channel => Curve,
--                       T => GDouble (X),
--                       V => GDouble (Y) -- Buffer1A.Data( Y ) * boo
--                    );
--                 X := X + 1.0;
--                    --X := X + 0.001;
--                 Y := Y + 1;
--                 exit when Buffer1A.Data'Last = Y;
--                 --exit when X > 10.0;
--              end loop;
--              boo := boo * (-1.0);
--           end loop;
--        end;
--     end;
--     Window.Set_Size_Request (400, 300);
--     Show_All (Window);
--     Gtk.Main.Main;
--  exception
--     when Error : others =>
--        Put_Line ("Error: " & Exception_Information (Error));
end User_Interface;
