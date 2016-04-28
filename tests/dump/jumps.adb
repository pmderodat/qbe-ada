with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Jumps is
   F              : File_Type := Standard_Output;
   U              : Compilation_Unit := Create (Long);
   F1             : constant Function_Ref := Create (U, "f1");
   B1, B2, B3, B4 : Block_Ref;
begin
   B1 := Entry_Block (F1);
   B2 := Create (F1);
   B3 := Create (F1);
   B4 := Create (F1);

   Set_Return_Type (F1, (Kind => Base, T => Long));
   Set_Param_Types (F1, (1 => (Kind => Base, T => Long)));
   declare
      PT : constant Temp_Ref_Array := Param_Temps (F1);
      T1 : Temp_Ref renames PT (1);
   begin
      Set_Jump (B1, B2);
      Set_Branch (B2, Value (T1), B3, B4);
      Set_Ret (B3);
      Set_Ret (B3, Value (T1));
   end;

   Dump (U, F);
   Destroy (U);
end Jumps;
