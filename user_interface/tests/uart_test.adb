with AUnit.Test_Cases; use AUnit.Test_Cases;

with Uart; use Uart;

package Uart_Unit_Tests is

   procedure Test_Read_Function;

end Uart_Unit_Tests;

package body Uart_Unit_Tests is

   procedure Test_Read_Function is
      Num_Samples : Integer := 5;
      Result : Readings_Array;
   begin
      Result := Read(Num_Samples);

      -- Assert that the result array has the expected length
      Assert (Result'Length = Num_Samples, "Unexpected array length");

      -- Loop through the result array to check its values
      for I in Result'Range loop
         case I is
            when 1 =>
               -- Check the first value in the result array
               Assert (Result(I) = 42.0, "Unexpected value in the array at index 1");
            when 2 =>
               -- Check the second value in the result array
               Assert (Result(I) = 3.14, "Unexpected value in the array at index 2");
            -- Add more cases as needed to check other values in the array
            when others =>
               -- Handle unexpected indices (optional)
               Assert (False, "Unexpected index in the result array");
         end case;
      end loop;
   end Test_Read_Function;

end Uart_Unit_Tests;
