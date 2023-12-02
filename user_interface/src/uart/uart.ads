package Uart is

   --  Type for the values returned by Read function
   type Readings_Array is array (Integer range <>) of Float;

   task type Read is
      entry Start;
      entry Stop;
   end Read;
end Uart;
