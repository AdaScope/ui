with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Serial_Communications;
with Ada.Streams;

package Uart is

   --  Type for the values returned by Read function
   type Values_Array is array (Integer range <>) of Float;

   --  To read the data from the UART
   function Read (
      Number_Of_Samples : Integer;
      Port_Location : GNAT.Serial_Communications.Port_Name) return Values_Array;

end Uart;
