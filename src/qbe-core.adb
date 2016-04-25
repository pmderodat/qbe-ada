with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Symbols; use GNATCOLL.Symbols;

package body QBE.Core is

   function Symbol (Unit : Compilation_Unit; Name : String) return Symbol_Type
   is (Find (Unit.Symbols, Name));

   --  TODO??? Find a good place to check name unicity and implement it

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Aggregate_Type, Aggregate_Type_Ref);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Aggregate_Item_Array, Aggregate_Item_Array_Access);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Data_Definition, Data_Definition_Ref);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Data_Item_Array, Data_Item_Array_Access);

   ------------
   -- Create --
   ------------

   function Create return Compilation_Unit is
     (new Compilation_Unit_Type'(Symbols => Allocate,
                                 others  => <>));

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : in out Compilation_Unit) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Compilation_Unit_Type, Compilation_Unit);
   begin
      if Unit = null then
         return;
      end if;

      Free (Unit.Symbols);

      for A of Unit.Aggregate_Types loop
         if A.Kind = Regular then
            Destroy (A.Items);
         end if;
         Destroy (A);
      end loop;

      for D of Unit.Data_Defs loop
         Destroy (D.Items);
         Destroy (D);
      end loop;

      Destroy (Unit);
   end Destroy;

   ------------
   -- Create --
   ------------

   function Create
     (Unit : Compilation_Unit;
      Kind : Aggregate_Type_Kind;
      Name : String)
      return Aggregate_Type_Ref
   is
      Result : constant Aggregate_Type_Ref :=
         new Aggregate_Type (Kind => Kind);
   begin
      Result.Unit := Unit;
      Result.Name := Find (Unit.Symbols, Name);
      Result.Alignment := 0;
      case Kind is
         when Regular => null;
         when Opaque  => Result.Size := 0;
      end case;
      Unit.Aggregate_Types.Append (Result);
      return Result;
   end Create;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment (A : Aggregate_Type_Ref; Alignment : Natural) is
   begin
      A.Alignment := Alignment;
   end Set_Alignment;

   ---------------
   -- Set_Items --
   ---------------

   procedure Set_Items (A : Aggregate_Type_Ref; Items : Aggregate_Item_Array)
   is
   begin
      Destroy (A.Items);
      A.Items := new Aggregate_Item_Array'(Items);
   end Set_Items;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (A : Aggregate_Type_Ref; Size : Natural) is
   begin
      A.Size := Size;
   end Set_Size;

   ------------
   -- Create --
   ------------

   function Create
     (Unit : Compilation_Unit;
      Name : String)
      return Data_Definition_Ref
   is
      Result : constant Data_Definition_Ref := new Data_Definition'
        (Unit   => Unit,
         Export => False,
         Name   => Find (Unit.Symbols, Name),
         Items  => null);
   begin
      Unit.Data_Defs.Append (Result);
      return Result;
   end Create;

   ----------------
   -- Set_Export --
   ----------------

   procedure Set_Export (D : Data_Definition_Ref; Export : Boolean) is
   begin
      D.Export := Export;
   end Set_Export;

   ---------------
   -- Set_Items --
   ---------------

   procedure Set_Items (D : Data_Definition_Ref; Items : Data_Item_Array) is
   begin
      Destroy (D.Items);
      D.Items := new Data_Item_Array'(Items);
   end Set_Items;

   ------------
   -- Symbol --
   ------------

   function Symbol (D : Data_Definition_Ref) return Symbol_Type is
   begin
      return D.Name;
   end Symbol;

end QBE.Core;
