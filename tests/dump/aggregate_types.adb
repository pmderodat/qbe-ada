with Ada.Text_IO; use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Aggregate_Types is
   U  : Compilation_Unit := Create;

   T0 : constant Aggregate_Type_Ref := Create (U, Regular, "t0");
   T1 : constant Aggregate_Type_Ref := Create (U, Regular, "t1");
   T2 : constant Aggregate_Type_Ref := Create (U, Regular, "t2");
   T3 : constant Aggregate_Type_Ref := Create (U, Opaque, "t3");
   T4 : constant Aggregate_Type_Ref := Create (U, Opaque, "t4");

   F  : File_Type := Standard_Output;
begin
   Set_Alignment (T1, 8);
   Set_Items (T2, ((Word, 1), (Long, 2)));
   Set_Size (T3, 4);
   Set_Alignment (T4, 1);

   Dump (U, F);
   Destroy (U);
end Aggregate_Types;
