package Uart is

   --  Type for the values returned by Read function
   type Readings_Array is array (Integer range <>) of Float;

   function Get_Triggered_Data (
      Data          : Readings_Array;
      Capture_Start : Integer;
      Capture_End   : Integer
   ) return Readings_Array;

   procedure Process_Data (
      Channel           : Integer;
      Data              : Readings_Array;
      Number_Of_Samples : Integer
   );

   task type Read is
      entry Start;
      entry Stop;
   end Read;
end Uart;
