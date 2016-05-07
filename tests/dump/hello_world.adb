with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Hello_World is
   --  Let's create a compilation unit (U) that contains two things: a "main"
   --  function and a "string" data entry.
   U  : Compilation_Unit := Create (Long);
   F1 : constant Function_Ref := Create (U, "main");
   S  : Data_Ref := Create (U, "string");
begin
   --  Make "main" public and make it return a word
   Set_Export (F1, True);
   Set_Return_Type (F1, (Base, Word));

   --  In "main"'s entry basic block, add a call to libc's "puts" and then
   --  return immedialtely 0.
   declare
      B : constant Block_Ref := Entry_Block (F1);
   begin
      Add_Call
        (B         => B,
         Target    => Value (Symbol (U, "puts")),
         Args      => (1 => (Arg_Type  => (Base, Long),
                             Arg_Value => Value (Symbol (S)))),
         Dest_Type => (Base, Long),
         Dest      => Create (F1));
      Set_Ret (B, Value (0));
   end;

   --  Make "string" contain the text we want to print with "puts"
   declare
      S_Data : constant Unbounded_String := To_Unbounded_String
        ("Hello, world!" & ASCII.NUL);
   begin
      Set_Items (S, (1 => (Kind      => Bytes,
                           Item_Kind => Byte,
                           Values    => S_Data)));
   end;

   --  The program is ready! Dump it to the standard output and free resources
   --  allocated for our compilation unit.
   declare
      F : File_Type := Standard_Output;
   begin
      Dump (U, F);
   end;
   Destroy (U);
end Hello_World;
