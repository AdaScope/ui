with GNAT.Serial_Communications;
with Uart;

package Globals is

   Port : GNAT.Serial_Communications.Serial_Port; --  The port to connect to

   type Board_State is (Disconnected, Connected);

   for Board_State use (
      Disconnected   => 0,
      Connected      => 1
   );

   --  Must be divisible by 4 for triggering
   Number_Of_Samples : constant Integer := 500;

   type Readings_Buffer is record
      Data  : Uart.Readings_Array (1 .. Number_Of_Samples);
      Index : Integer := 1;
   end record;

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

   protected Processed_Data is

      procedure Set_Data (
         Channel : Integer;
         Data    : Uart.Readings_Array
      );
      --  Sets the data array

      function Get_Data (
         Channel : Integer
      ) return Uart.Readings_Array;
      --  Gets the data array

      function Get_Data_Point (
         Channel : Integer;
         N       : Integer
      ) return Float;
      --  Gets the data array

   private
      --  Arrays storing the processed data
      Processed_Data_Channel_1 : Uart.Readings_Array
         (1 .. Number_Of_Samples / 2) := (others => 0.0);

      Processed_Data_Channel_2 : Uart.Readings_Array
         (1 .. Number_Of_Samples / 2) := (others => 0.0);

      Processed_Data_Channel_3 : Uart.Readings_Array
         (1 .. Number_Of_Samples / 2) := (others => 0.0);

   end Processed_Data;

   protected Buffered_Data is

      procedure Set_Data (
         Channel : Integer;
         Data    : Float
      );
      --  Sets the buffer value at the current index

      procedure Reset_Buffer (
         Channel : Integer
      );
      --  Resets the buffer

   private
      --  Arrays storing the raw data used as a buffer
      Readings_Buffer_Channel_1 : Readings_Buffer;

      Readings_Buffer_Channel_2 : Readings_Buffer;

      Readings_Buffer_Channel_3 : Readings_Buffer;

   end Buffered_Data;

end Globals;
