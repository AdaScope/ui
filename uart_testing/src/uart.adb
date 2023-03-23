package body Uart is

   function Read (
      Number_Of_Samples : Integer;
      Port_Location : GNAT.Serial_Communications.Port_Name
      ) return Values_Array is
         Port : GNAT.Serial_Communications.Serial_Port;
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
         Offset : Ada.Streams.Stream_Element_Offset := 1;

         Line : String (1 .. 10);
         Line_Index : Natural := Line'First;

         Counter : Integer := 1;
         Values : Uart.Values_Array (1 .. Number_Of_Samples);
   begin

      --  We initialize the string to eliminate warnings
      for I in Line'Range loop
         Line (I) := 'E';
      end loop;

      --  Open the port
      GNAT.Serial_Communications.Open (Port, Port_Location);

      --  Run until the counter is reached
      while Counter < Number_Of_Samples + 1 loop

         --  Read data from the port
         GNAT.Serial_Communications.Read (Port, Buffer, Offset);

         if Character'Val (Buffer (1)) = ASCII.LF then
            --  We write our entire line to the terminal
            Put_Line (Line (Line'First .. Line_Index - 1));

            --  We save the values in an array
            -- Values (Counter) := Float'Value
            --   (Line (Line'First .. Line_Index - 1));
            --  We increment the counter
            --Counter := Counter + 1;

            --  We reset the line index
               Line_Index := Line'First;
         else
            --  We write the current character to
            --  the current index of our line
            Line (Line_Index) := Character'Val (Buffer (1));
            Line_Index := Line_Index + 1;
         end if;

      end loop;
      --  Close the port
      GNAT.Serial_Communications.Close (Port);

      --  Return the values
      return Values;
   end Read;

end Uart;