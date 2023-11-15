with Ada.Text_IO;           use Ada.Text_IO;

package body Globals is

   protected body Board_State_Change is

      ----------------------------
      -- Change_State_Connected --
      ----------------------------
      procedure Change_State_Connected is
      begin
         Current_Board_State := Connected;
      end Change_State_Connected;

      -------------------------------
      -- Change_State_Disconnected --
      -------------------------------
      procedure Change_State_Disconnected is
      begin
         Current_Board_State := Disconnected;
      end Change_State_Disconnected;

      ---------------------
      -- Get_Board_State --
      ---------------------
      function Get_Board_State return Board_State is
      begin
         return Current_Board_State;
      end Get_Board_State;

   end Board_State_Change;

   protected body UART_Data_Array is

      ---------------------
      -- Set_Data_Array --
      ---------------------
      procedure Set_Data_Array (
                  Channel : Channel_Number;
                  Data_Array : Uart.Readings_Array) is
      begin
         case Channel is
            when 1 =>
               Readings_CH_1 := Data_Array;
            when 2 =>
               Readings_CH_2 := Data_Array;
            when 3 =>
               Readings_CH_3 := Data_Array;
            when others =>
               Put_Line ("Error - wrong channel entered");
               Put_Line (Channel'Image);
         end case;
      end Set_Data_Array;

      ---------------------
      -- Get_Data_Array --
      ---------------------
      function Get_Data_Array (Channel : Channel_Number) 
                                 return Uart.Readings_Array is
         Default_Array : constant Uart.Readings_Array
            (1 .. Number_Of_Samples) := (others => 0.0);
      begin
         case Channel is
            when 1 =>
               return Readings_CH_1;
            when 2 =>
               return Readings_CH_2;
            when 3 =>
               return Readings_CH_3;
            when others =>
               Put_Line ("Error - wrong channel entered");
               Put_Line (Channel'Image);
               return Default_Array;
         end case;
      end Get_Data_Array;

      ---------------------
      -- Get_Data_Point --
      ---------------------
      function Get_Data_Point (Channel : Channel_Number;
                                 N : Integer)
                                 return Float is
      begin
         case Channel is
            when 1 =>
               return Readings_CH_1 (N);
            when 2 =>
               return Readings_CH_2 (N);
            when 3 =>
               return Readings_CH_3 (N);
            when others =>
               Put_Line ("Error - wrong channel entered");
               Put_Line (Channel'Image);
               return 0.0;
         end case;
      end Get_Data_Point;

   end UART_Data_Array;

end Globals;
