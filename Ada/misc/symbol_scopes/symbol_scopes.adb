with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Symbol_Scopes is
   -- Nested procedure
   procedure Ask_Number is
      My_Num: Integer;
   begin
      loop
         Put("Enter a number: ");
         Get(My_Num);

         -- We can check conditions from the `exit'
         Put_Line("Got number:" & My_Num'Image);
         exit when My_Num = 0;
      end loop;
   end Ask_Number;
begin
   Put("Trying `declare' block. Enter a string: ");

   declare
      -- We can call functions from here. In this case we *need* to call the
      -- function to initialize the String, since it's an unconstrained array.
      User_Input: String := Get_Line;
   begin
      Put_Line("You entered '" & User_Input & "'.");
   end;

   -- NOTE: We can't access User_Input from here

   -- Call nested procedure
   Ask_Number;
end Symbol_Scopes;

-- We can't access Ask_Number from here
