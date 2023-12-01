with AUnit.Run;
with Math_Suite; use Math_Suite;
with Aunit.Reporter.Text;

procedure Run_Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : Aunit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
   
end Run_Tests;
