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

   protected body Processed_Data is

      ---------------------
      -- Set_Data_Array --
      ---------------------
      procedure Set_Data (
         Channel : Integer;
         Data_Array : Uart.Readings_Array
      ) is
      begin
         case Channel is
            when 1 =>
               Processed_Data_Channel_1 := Data_Array;
            when 2 =>
               Processed_Data_Channel_2 := Data_Array;
            when 3 =>
               Processed_Data_Channel_3 := Data_Array;
            when others =>
               Put_Line ("Error - wrong channel entered");
               Put_Line (Channel'Image);
         end case;
      end Set_Data;

      ---------------------
      -- Get_Data_Array --
      ---------------------
      function Get_Data (
         Channel : Integer
      ) return Uart.Readings_Array is
         Default_Array : constant Uart.Readings_Array
            (1 .. Number_Of_Samples) := (others => 0.0);
      begin
         case Channel is
            when 1 =>
               return Processed_Data_Channel_1;
            when 2 =>
               return Processed_Data_Channel_2;
            when 3 =>
               return Processed_Data_Channel_3;
            when others =>
               Put_Line ("Error - wrong channel entered");
               Put_Line (Channel'Image);
               return Default_Array;
         end case;
      end Get_Data;

      ---------------------
      -- Get_Data_Point --
      ---------------------
      function Get_Data_Point (
         Channel : Integer;
         N : Integer
      ) return Float is
      begin
         case Channel is
            when 1 =>
               return Processed_Data_Channel_1 (N);
            when 2 =>
               return Processed_Data_Channel_2 (N);
            when 3 =>
               return Processed_Data_Channel_3 (N);
            when others =>
               Put_Line ("Error - wrong channel entered");
               Put_Line (Channel'Image);
               return 0.0;
         end case;
      end Get_Data_Point;
   end Processed_Data;

   protected body Buffered_Data is

      procedure Set_Data (
         Channel : Integer;
         Data    : Float
      ) is
      begin
         case Channel is
            when 1 =>
               Readings_Buffer_Channel_1.Data
                  (Readings_Buffer_Channel_1.Index) := Data;
               if Readings_Buffer_Channel_1.Index < Number_Of_Samples then
                  Readings_Buffer_Channel_1.Index :=
                     Readings_Buffer_Channel_1.Index + 1;
               else
                  Readings_Buffer_Channel_1.Index := 1;
                  Uart.Process_Data (
                     Channel           => 1,
                     Data              => Readings_Buffer_Channel_1.Data,
                     Number_Of_Samples => Number_Of_Samples
                  );
               end if;

            when 2 =>
               Readings_Buffer_Channel_2.Data
                  (Readings_Buffer_Channel_2.Index) := Data;
               if Readings_Buffer_Channel_2.Index < Number_Of_Samples then
                  Readings_Buffer_Channel_2.Index :=
                     Readings_Buffer_Channel_2.Index + 1;
               else
                  Readings_Buffer_Channel_2.Index := 1;
                  Uart.Process_Data (
                     Channel           => 2,
                     Data              => Readings_Buffer_Channel_2.Data,
                     Number_Of_Samples => Number_Of_Samples
                  );
               end if;

            when 3 =>
               Readings_Buffer_Channel_3.Data
                  (Readings_Buffer_Channel_3.Index) := Data;
               if Readings_Buffer_Channel_3.Index < Number_Of_Samples then
                  Readings_Buffer_Channel_3.Index :=
                     Readings_Buffer_Channel_3.Index + 1;
               else
                  Readings_Buffer_Channel_3.Index := 1;
                  Uart.Process_Data (
                     Channel           => 3,
                     Data              => Readings_Buffer_Channel_3.Data,
                     Number_Of_Samples => Number_Of_Samples
                  );
               end if;
            when others =>
               Put_Line ("Error - Wrong channel entered");
               Put_Line (Channel'Image);
         end case;
      end Set_Data;
   end Buffered_Data;

end Globals;
