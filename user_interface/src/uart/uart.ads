with Min_Ada;

package Uart is

   --  Type for the values returned by Read function
   type Readings_Array is array (Integer range <>) of Float;
   type Received_Bytes is array (Integer range <>) of Min_Ada.Byte;

   --  To read the data
   function Read (Number_Of_Samples : Integer)
         return Readings_Array;

   function Get_Data (
      Number_Of_Samples : Integer
   ) return Readings_Array;

   function Get_Triggered_Data (
      Data          : Readings_Array;
      Capture_Start : Integer;
      Capture_End   : Integer
   ) return Readings_Array;

   procedure Get_Processed_Data (
      Number_Of_Samples : Integer
   );
end Uart;
