with Ada.Text_IO; use Ada.Text_IO;

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
         Data    : Uart.Readings_Array
      ) is
      begin
         case Channel is
            when 1 | 5 =>
               Processed_Data_Channel_1 := Data;
            when 2 | 6 =>
               Processed_Data_Channel_2 := Data;
            when 3 | 7 =>
               Processed_Data_Channel_3 := Data;
            when others =>
               Put_Line ("Error Processed_Data.Set_Data");
               Put_Line ("Wrong channel entered:" & Channel'Image);
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
            when 1 | 5 =>
               return Processed_Data_Channel_1;
            when 2 | 6 =>
               return Processed_Data_Channel_2;
            when 3 | 7 =>
               return Processed_Data_Channel_3;
            when others =>
               Put_Line ("Error Processed_Data.Get_Data");
               Put_Line ("Wrong channel entered:" & Channel'Image);
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
            when 1 | 5 =>
               return Processed_Data_Channel_1 (N);
            when 2 | 6 =>
               return Processed_Data_Channel_2 (N);
            when 3 | 7 =>
               return Processed_Data_Channel_3 (N);
            when others =>
               Put_Line ("Error Processed_Data.Get_Data_Point");
               Put_Line ("Wrong channel entered:" & Channel'Image);
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
            when 1 | 5 =>
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

            when 2 | 6 =>
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

            when 3 | 7 =>
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
               Put_Line ("Error Buffered_Data.Set_Data");
               Put_Line ("Wrong channel entered:" & Channel'Image);
         end case;
      end Set_Data;

      procedure Reset_Buffer (
         Channel : Integer
      ) is
      begin
         case Channel is
            when 1 | 5 =>
               Readings_Buffer_Channel_1.Index := 1;
            when 2 | 6 =>
               Readings_Buffer_Channel_2.Index := 1;
            when 3 | 7 =>
               Readings_Buffer_Channel_3.Index := 1;
            when others =>
               Put_Line ("Error Buffered_Data.Reset_Buffer");
               Put_Line ("Wrong channel entered:" & Channel'Image);
         end case;
      end Reset_Buffer;
   end Buffered_Data;

end Globals;
