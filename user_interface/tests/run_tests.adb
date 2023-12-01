with AUnit.Runner;
with Uart_Unit_Tests;

procedure Run_Tests is
begin
   AUnit.Runner.Run_Test_Suite ("Uart_Unit_Tests");
end Run_Tests;
