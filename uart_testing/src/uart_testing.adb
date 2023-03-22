with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Serial_Communications;
with Ada.Streams; use Ada.Streams;

procedure Uart_Testing is
    Port : GNAT.Serial_Communications.Serial_Port;
    Data : Stream_Element_Array (1 .. 1);
    Line : String (1 .. 100);
    Line_Index : Natural := Line'First;
    Offset : Stream_Element_Offset := 1;

begin
    -- Open the serial port
    GNAT.Serial_Communications.Open(Port, "/dev/ttyACM0");

    -- Read data from the port
    loop
        GNAT.Serial_Communications.Read(Port, Data, Offset);
        if Character'Val(Data(1)) = ASCII.LF then
            Put_Line(Line(Line'First .. Line_Index - 1));
            Line_Index := Line'First;
        else
            Line(Line_Index) := Character'Val(Data(1));
            Line_Index := Line_Index + 1;
        end if;
    end loop;

    -- Close the port
    GNAT.Serial_Communications.Close(Port);
end Uart_Testing;
