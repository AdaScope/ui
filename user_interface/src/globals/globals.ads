package Globals is

   type Board_State is (Disconnected, Connected);

   for Board_State use (
      Disconnected   => 0, 
      Connected      => 1);

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

end Globals;
