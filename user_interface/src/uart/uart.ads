package Uart is

   --  Type for the values returned by Read function
   type Readings_Array is array (Integer range <>) of Float;

   --  Implements triggering to keep the data (mostly) still
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
