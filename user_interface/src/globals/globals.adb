package body Globals is

   protected body Board_State_Change is

      ------------
      -- Change_State_Connected --
      ------------
      procedure Change_State_Connected is
      begin
         Current_Board_State := Connected;
      end Change_State_Connected;

      ----------
      -- Change_State_Disconnected --
      ----------
      procedure Change_State_Disconnected is
      begin
         Current_Board_State := Disconnected;
      end Change_State_Disconnected;

      ----------
      -- Get_Board_State_ --
      ----------
      function Get_Board_State return Board_State is
      begin
         return Current_Board_State;
      end Get_Board_State;

   end Board_State_Change;


end Globals;