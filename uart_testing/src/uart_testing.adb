with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Serial_Communications;
with Ada.Streams; use Ada.Streams;

procedure Uart_Testing is
    Port : GNAT.Serial_Communications.Serial_Port;
    Buffer : Stream_Element_Array (1 .. 1);
    Line : String (1 .. 10);
    Line_Index : Natural := Line'First;
    Offset : Stream_Element_Offset := 1;
begin
    -- Open the serial port
    GNAT.Serial_Communications.Open(Port, "/dev/ttyACM0");

    loop
        -- Read data from the port
        GNAT.Serial_Communications.Read(Port, Buffer, Offset);

        if Character'Val(Buffer(1)) = ASCII.LF then
            -- We write our entire line to the terminal
            Put_Line(Line(Line'First .. Line_Index- 1));
            Line_Index := Line'First;
        else
            -- We write the current character to the current index of our line
            Line(Line_Index) := Character'Val(Buffer(1));
            Line_Index := Line_Index + 1;
        end if;
    end loop;

    -- Close the port
    GNAT.Serial_Communications.Close(Port);
end Uart_Testing;
