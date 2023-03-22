with Ada.Text_IO;                   use Ada.Text_IO;
with GNAT.Serial_Communications;
with Ada.Streams;                   use Ada.Streams;

with Ada.Numerics.Elementary_Functions;

procedure Uart_Testing is

    -- For UART
    Port : GNAT.Serial_Communications.Serial_Port;
    --Data : GNAT.Serial_Communications.Stream_Element_Array(1..100);
    --Data : GNAT.Serial_Communications.Buffer_Type(1..100);
    Data : Ada.Streams.Stream_Element_Array (1 .. 10);

    Test : Stream_Element_Offset := 0;
    Char : Character;
begin
    -- Open the serial port
    GNAT.Serial_Communications.Open(Port, "/dev/ttyACM0");

    -- Read data from the porttype
    loop
        GNAT.Serial_Communications.Read(Port, Data, Test);
        for I in Data'Range loop
            if (Character'Val (Data(I))) = ASCII.CR then
                Put_Line ("");
            elsif (Character'Val (Data(I))) = ASCII.LF then
                Put_Line ("");
            else
                Put(Character'Image (Character'Val (Data(I))));
            end if;
        end loop;

        --for I in Data'Range loop
        --    Put_Line (Character'Image (Data(I)));
        --end loop;

    end loop;

    -- Close the port
    GNAT.Serial_Communications.Close(Port);
end Uart_Testing;
