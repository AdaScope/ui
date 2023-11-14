with AUnit.Test_Cases; use AUnit.Test_Cases;
with Worker; use Worker;
with Gtk.Main.Router; use Gtk.Main.Router;
with Ada.Text_IO; use Ada.Text_IO;

package Worker_Unit_Tests is

   procedure Test_Feed_UART_Data;

end Worker_Unit_Tests;

package body Worker_Unit_Tests is

   procedure Test_Feed_UART_Data is
      Scope : Gtk_Oscilloscope;
      Channel : Channel_Number;
      Expected_X : Gdouble := 42.0;  -- Replace with your expected X value
      Expected_Y : Gdouble := 3.14;  -- Replace with your expected Y value
      Actual_X : Gdouble;
      Actual_Y : Gdouble;
   begin
      -- Set up the test environment

      -- Create a sample Gtk_Oscilloscope instance
      Scope := Gtk_Oscilloscope'Create;
      
      -- Create a sample Channel_Number instance
      Channel := 1;

      -- Call the Feed_UART_Data procedure with your test environment.
      Feed_UART_Data(Scope, Channel);

      -- Retrieve the X and Y values from the Scope object for testing
      Actual_X := Scope.Get_X_Y_Data(Channel).X;
      Actual_Y := Scope.Get_X_Y_Data(Channel).Y;

      -- Add assertions to check the behavior of Feed_UART_Data.

      Assert (Actual_X = Expected_X, "Unexpected X value in the scope");
      Assert (Actual_Y = Expected_Y, "Unexpected Y value in the scope");
   end Test_Feed_UART_Data;

end Worker_Unit_Tests;
