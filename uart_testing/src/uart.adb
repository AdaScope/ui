package body Uart is

   function Read (
      Number_Of_Samples : Integer;
      Port_Location : GNAT.Serial_Communications.Port_Name
      ) return Readings_Array is

         --  Initialize the variables for the read
         Port : GNAT.Serial_Communications.Serial_Port;
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
         Offset : Ada.Streams.Stream_Element_Offset := 1;

         --  For storing one reading
         Line : String (1 .. 10);
         Line_Index : Natural := Line'First;

         --  To count the number of character readings done
         Counter : Integer := 1;

         --  For storing all the readings
         Readings : Uart.Readings_Array (1 .. Number_Of_Samples);
   begin

      --  We initialize the string to eliminate warnings
      for I in Line'Range loop
         Line (I) := 'E';
      end loop;

      --  Open the port
      GNAT.Serial_Communications.Open (Port, Port_Location);

      --  Make sure to only start collecting data at start of new line
      loop
         GNAT.Serial_Communications.Read (Port, Buffer, Offset);
         exit when Character'Val (Buffer (1)) = ASCII.LF;
      end loop; 

      --  Run until the we gathered the required number of samples
      while Counter < Number_Of_Samples + 1 loop

         --  Read data from the port
         GNAT.Serial_Communications.Read (Port, Buffer, Offset);

         if Character'Val (Buffer (1)) = ASCII.LF and Line_Index /= 1 then
            --  We write our entire line to the terminal
            Put_Line (Line (Line'First .. Line_Index - 1));

            --  We save the reading to a table
            Readings (Counter) := Float'Value (Line (Line'First .. Line_Index - 1));
            Counter := Counter + 1;

            --  We reset the line index to the beginning
            Line_Index := Line'First;
         elsif Character'Val (Buffer (1)) /= ASCII.LF then
            --  We write the current character to
            --  the current index of our line
            Line (Line_Index) := Character'Val (Buffer (1));
            Line_Index := Line_Index + 1;
         end if;
      end loop;
      --  Close the port
      GNAT.Serial_Communications.Close (Port);

      --  Return the readings
      return Readings;
   end Read;

end Uart;
