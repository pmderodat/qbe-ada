--#check_output

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Function_Defs is
   U  : Compilation_Unit := Create (Long);
   A  : constant Aggregate_Type_Ref := Create (U, Opaque, "t0");
   S_Word   : constant Signature_Type := (Kind => Base,      T => Word);
   S_Long   : constant Signature_Type := (Kind => Base,      T => Long);
   S_Double : constant Signature_Type := (Kind => Base,      T => Double);
   S_A      : constant Signature_Type := (Kind => Aggregate, A => A);

   F0 : constant Function_Ref := Create (U, "f0");
   F1 : constant Function_Ref := Create (U, "f1");
   F2 : constant Function_Ref := Create (U, "f2");
   F3 : constant Function_Ref := Create (U, "f3");

   B : Block_Ref;

   F  : File_Type := Standard_Output;
begin
   Set_Alignment (A, 4);
   Set_Size (A, 16);

   Set_Export (F0, True);
   B := Create (F0);

   Set_Return_Type (F1, S_Word);
   Set_Param_Types (F2, (1 => S_Long));
   Set_Param_Types (F3, (S_Double, S_A));

   Dump (U, F);
   Destroy (U);
end Function_Defs;
