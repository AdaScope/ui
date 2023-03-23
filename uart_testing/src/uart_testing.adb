with Uart;

procedure Uart_Testing is
   Values : Uart.Values_Array (1 .. 1000);
begin
   Values := Uart.Read (1000, "/dev/ttyACM0");

end Uart_Testing;
