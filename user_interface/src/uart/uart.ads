package Uart is

   --  Type for the values returned by Read function
   type Readings_Array is array (Integer range <>) of Float;

   --  Type for I don't know
   D : constant := 0.1;
   type Reading is delta D range 0.0 .. 3000.0;

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
