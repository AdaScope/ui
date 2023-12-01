with GNAT.Serial_Communications;
with Gtk.Main.Router;
with Ada.Streams;
with Globals;
with Min_Ada;

package body Uart is

   task body Read is

      --  Variables for the serial read
      Buffer   : Ada.Streams.Stream_Element_Array (1 .. 1);
      Offset   : Ada.Streams.Stream_Element_Offset := 1;

      --  Context for the min protocol
      Context  : Min_Ada.Min_Context;
   begin

      select -- Waiting for parameters or exit request
         accept Start do

            Min_Ada.Min_Init_Context (Context => Context);

            loop
               --  Read data from serial port
               GNAT.Serial_Communications.Read (
                  Port   => Globals.Port,
                  Buffer => Buffer,
                  Last   => Offset
               );
               --  Send data to protocol for processing
               Min_Ada.Rx_Bytes (
                  Context => Context,
                  Data => Min_Ada.Byte (Buffer (1))
               );
            end loop;
         end Start;

      or accept Stop;
         raise Gtk.Main.Router.Quit_Error;
      end select;

      accept Stop;
   end Read;
end Uart;
