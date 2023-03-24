with Ada.Text_IO; use Ada.Text_IO;
with Uart;

procedure Uart_Testing is
   Values : Uart.Readings_Array (1 .. 10);
   Values1 : Uart.Readings_Array (1 .. 10);
   Values2 : Uart.Readings_Array (1 .. 10);
   Values3 : Uart.Readings_Array (1 .. 10);
   Values4 : Uart.Readings_Array (1 .. 10);
   Values5 : Uart.Readings_Array (1 .. 10);
   Values6 : Uart.Readings_Array (1 .. 10);
begin
   Values := Uart.Read (10, "/dev/ttyACM0");
   Values1 := Uart.Read (10, "/dev/ttyACM0");
   Values2 := Uart.Read (10, "/dev/ttyACM0");
   Values3 := Uart.Read (10, "/dev/ttyACM0");
   Values4 := Uart.Read (10, "/dev/ttyACM0");
   Values5 := Uart.Read (10, "/dev/ttyACM0");
   Values6 := Uart.Read (10, "/dev/ttyACM0");

   for I of Values loop
      Put_Line (Float'Image (I));
   end loop;
   for I of Values1 loop
      Put_Line (Float'Image (I));
   end loop;
   for I of Values2 loop
      Put_Line (Float'Image (I));
   end loop;
   for I of Values3 loop
      Put_Line (Float'Image (I));
   end loop;
   for I of Values4 loop
      Put_Line (Float'Image (I));
   end loop;
   for I of Values5 loop
      Put_Line (Float'Image (I));
   end loop;
   for I of Values6 loop
      Put_Line (Float'Image (I));
   end loop;
end Uart_Testing;
