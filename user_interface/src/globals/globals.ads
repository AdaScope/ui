with Gtk.Oscilloscope;      use Gtk.Oscilloscope;
with GNAT.Serial_Communications;
with Uart;

package Globals is

   Port : GNAT.Serial_Communications.Serial_Port; --  The port to connect to

   type Board_State is (Disconnected, Connected);

   for Board_State use (
      Disconnected   => 0,
      Connected      => 1
   );

   Number_Of_Samples : constant Integer := 100;

   protected Board_State_Change is

      procedure Change_State_Connected;
      --  Changes the state of the board to Connected

      procedure Change_State_Disconnected;
      --  Changes the state of the board to Disconnected

      function Get_Board_State return Board_State;
      --  Gets current board state

   private
      Current_Board_State : Board_State := Disconnected;
   end Board_State_Change;

   protected UART_Data_Array is

      procedure Set_Data_Array (
                  Channel : Channel_Number;
                  Data_Array : Uart.Readings_Array);
      --  Sets the data array

      function Get_Data_Array (
                  Channel : Channel_Number)
                  return Uart.Readings_Array;
      --  Gets the data array

      function Get_Data_Point (
                  Channel : Channel_Number;
                  N : Integer)
                  return Float;
      --  Gets the data array

   private
      --  Arrays for storing the data from the board
      Readings_CH_1 : Uart.Readings_Array
         (1 .. Number_Of_Samples) := (others => 0.0);
      Readings_CH_2 : Uart.Readings_Array
         (1 .. Number_Of_Samples) := (others => 0.0);
      Readings_CH_3 : Uart.Readings_Array
         (1 .. Number_Of_Samples) := (others => 0.0);
   end UART_Data_Array;

end Globals;
