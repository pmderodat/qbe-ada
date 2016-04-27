with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Phis is
   F      : File_Type := Standard_Output;
   U      : Compilation_Unit := Create;
   B1, B2 : Block_Ref;
   F1     : constant Function_Ref := Create (U, "f1");
begin
   B1 := Entry_Block (F1);
   B2 := Create (F1);

   Set_Param_Types (F1, (1 => (Kind => Base, T => Long)));
   declare
      PT : constant Temp_Ref_Array := Param_Temps (F1);
      T1 : Temp_Ref renames PT (1);
      T2 : constant Temp_Ref := Create (F1);
   begin
      Add_Phi (B2, T1, Word, (1 => (B1, Value (T1))));
      Add_Phi
        (B2, T2, Simple,
         ((B1, Value (T1)),
          (B2, Value (Symbol (F1)))));
   end;

   Dump (U, F);
   Destroy (U);
end Phis;
