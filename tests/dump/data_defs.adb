with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Data_Defs is
   U  : Compilation_Unit := Create (Long);

   D0 : constant Data_Ref := Create (U, "d0");
   D1 : constant Data_Ref := Create (U, "d1");
   D2 : constant Data_Ref := Create (U, "d2");

   V1 : constant Data_Item :=
     (Kind     => Symbol,
     Item_Kind => Single,
      Name     => Symbol (U, "s1"),
      Offset   => 0);
   V2 : constant Data_Item :=
     (Kind      => Symbol,
      Item_Kind => Double,
      Name      => Symbol (U, "s2"),
      Offset    => 4);
   V3 : constant Data_Item :=
     (Kind      => Number,
      Item_Kind => Word,
      Value     => (Kind => Decimal, Value => 0));
   V4 : constant Data_Item :=
     (Kind      => Number,
      Item_Kind => Word,
      Value     => (Kind => Symbol, Name => Symbol (U, "s3")));
   V5 : constant Data_Item :=
     (Kind => Bytes, Item_Kind => Byte, Values => To_Unbounded_String ("abc"));

   F  : File_Type := Standard_Output;
begin
   Set_Export (D1, True);
   Set_Items (D2, (V1, V2, V3, V4, V5));

   Dump (U, F);
   Destroy (U);
end Data_Defs;
