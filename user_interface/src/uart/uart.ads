package Uart is

   --  Type for the values returned by Read function
   type Readings_Array is array (Integer range <>) of Float;

   --  To read the data
   function Read (Number_Of_Samples : Integer)
         return Readings_Array;
end Uart;
