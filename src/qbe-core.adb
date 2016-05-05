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
     (Data, Data_Ref);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Data_Item_Array, Data_Item_Array_Access);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Phi_Association_Array, Phi_Association_Array_Access);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Function_Type, Function_Ref);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Signature_Array, Signature_Array_Access);

   procedure Destroy (B : in out Block_Ref);

   ------------
   -- Create --
   ------------

   function Create (Address_Size : Address_Type) return Compilation_Unit is
     (new Compilation_Unit_Type'(Address_Size => Address_Size,
                                 Symbols      => Allocate,
                                 others       => <>));

   -------------
   -- Destroy --
   -------------

   procedure Destroy (B : in out Block_Ref) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Block, Block_Ref);
   begin
      for Phi of B.Phis loop
         Destroy (Phi.Values);
      end loop;
      Destroy (B);
   end Destroy;

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

      for F of Unit.Function_Defs loop
         Destroy (F.Param_Types);
         for B of F.Blocks loop
            Destroy (B);
         end loop;
         Destroy (F);
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
      return Data_Ref
   is
      Result : constant Data_Ref := new Data'
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

   procedure Set_Export (D : Data_Ref; Export : Boolean) is
   begin
      D.Export := Export;
   end Set_Export;

   ---------------
   -- Set_Items --
   ---------------

   procedure Set_Items (D : Data_Ref; Items : Data_Item_Array) is
   begin
      Destroy (D.Items);
      D.Items := new Data_Item_Array'(Items);
   end Set_Items;

   ------------
   -- Symbol --
   ------------

   function Symbol (D : Data_Ref) return Symbol_Type is
   begin
      return D.Name;
   end Symbol;

   ------------
   -- Create --
   ------------

   function Create
     (Unit : Compilation_Unit;
      Name : String)
      return Function_Ref
   is
      Result : constant Function_Ref := new Function_Type'
        (Unit             => Unit,
         Export           => False,
         Name             => Find (Unit.Symbols, Name),
         Has_Return_Type  => False,
         Return_Type      => <>,
         Param_Types      => null,
         Blocks           => <>,
         Next_Block_Index => 1,
         Next_Temp_Index  => 1);
      Entry_Block : constant Block_Ref := Create (Result);
      pragma Unreferenced (Entry_Block);
   begin
      Unit.Function_Defs.Append (Result);
      return Result;
   end Create;

   ----------------
   -- Set_Export --
   ----------------

   procedure Set_Export (F : Function_Ref; Export : Boolean) is
   begin
      F.Export := Export;
   end Set_Export;

   ---------------------
   -- Set_Return_Type --
   ---------------------

   procedure Set_Return_Type (F : Function_Ref; T : Signature_Type) is
   begin
      F.Has_Return_Type := True;
      F.Return_Type := T;
   end Set_Return_Type;

   ---------------------
   -- Set_Param_Types --
   ---------------------

   procedure Set_Param_Types (F : Function_Ref; Param_Types : Signature_Array)
   is
   begin
      Destroy (F.Param_Types);
      F.Param_Types := new Signature_Array'(Param_Types);
      F.Next_Temp_Index := Param_Types'Length + 1;
   end Set_Param_Types;

   ------------
   -- Symbol --
   ------------

   function Symbol (F : Function_Ref) return Symbol_Type is
   begin
      return F.Name;
   end Symbol;

   -----------------
   -- Entry_Block --
   -----------------

   function Entry_Block (F : Function_Ref) return Block_Ref is
   begin
      return F.Blocks.First_Element;
   end Entry_Block;

   ------------
   -- Create --
   ------------

   function Create (F : Function_Ref) return Block_Ref is
      Result : constant Block_Ref := new Block'
        (Func  => F,
         Index => F.Next_Block_Index,
         Phis  => <>,
         Insns => <>,
         Jump  => <>);
   begin
      F.Blocks.Append (Result);
      F.Next_Block_Index := F.Next_Block_Index + 1;
      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (F : Function_Ref) return Temp_Ref is
      Result : constant Temp_Ref := F.Next_Temp_Index;
   begin
      F.Next_Temp_Index := F.Next_Temp_Index + 1;
      return Result;
   end Create;

   -----------------
   -- Param_Temps --
   -----------------

   function Param_Temps (F : Function_Ref) return Temp_Ref_Array is
      Count  : constant Natural :=
        (if F.Param_Types = null
         then 0
         else F.Param_Types.all'Length);
      Result : Temp_Ref_Array (1 .. Count);
   begin
      for I in Result'Range loop
         Result (I) := Temp_Ref (I);
      end loop;
      return Result;
   end Param_Temps;

   -------------
   -- Add_Phi --
   -------------

   procedure Add_Phi
     (B         : Block_Ref;
      Dest      : Temp_Ref;
      Dest_Type : Base_Type;
      Values    : Phi_Association_Array)
   is
      Phi : constant Phi_Type :=
        (Dest      => Dest,
         Dest_Type => Dest_Type,
         Values    => new Phi_Association_Array'(Values));
   begin
      B.Phis.Append (Phi);
   end Add_Phi;

   --------------
   -- Add_Copy --
   --------------

   procedure Add_Copy
     (B         : Block_Ref;
      Value     : Value_Type;
      Dest_Type : Base_Type;
      Dest      : Temp_Ref) is
   begin
      B.Insns.Append ((Kind       => Copy,
                       Dest_Type  => Dest_Type,
                       Dest       => Dest,
                       Copy_Value => Value));
   end Add_Copy;

   ---------------
   -- Add_Arith --
   ---------------

   procedure Add_Arith
     (B           : Block_Ref;
      Kind        : Arith_Insn_Kind;
      Left, Right : Value_Type;
      Dest_Type   : Base_Type;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind       => Arith,
                       Dest_Type  => Dest_Type,
                       Dest       => Dest,
                       Left       => Left,
                       Right      => Right,
                       Arith_Kind => Kind));
   end Add_Arith;

   ---------------
   -- Add_Store --
   ---------------

   procedure Add_Store
     (B          : Block_Ref;
      Store_Type : Extended_Type;
      Value      : Value_Type;
      Address    : Value_Type) is
   begin
      B.Insns.Append ((Kind       => Store,
                       Store_Type => Store_Type,
                       Value      => Value,
                       Store_Addr => Address));
   end Add_Store;

   --------------
   -- Add_Load --
   --------------

   procedure Add_Load
     (B         : Block_Ref;
      Address   : Value_Type;
      Dest_Type : Address_Type;
      Dest      : Temp_Ref) is
   begin
      B.Insns.Append ((Kind => Load,
                       Dest_Type        => Dest_Type,
                       Dest             => Dest,
                       Load_Addr        => Address,
                       Load_Type        => Dest_Type,
                       Load_Sign_Extend => False));
   end Add_Load;

   --------------
   -- Add_Load --
   --------------

   procedure Add_Load
     (B           : Block_Ref;
      Address     : Value_Type;
      Load_Type   : Small_Scalar_Type;
      Sign_Extend : Boolean;
      Dest_Type   : Address_Type;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind             => Load,
                       Dest_Type        => Dest_Type,
                       Dest             => Dest,
                       Load_Addr        => Address,
                       Load_Type        => Load_Type,
                       Load_Sign_Extend => Sign_Extend));
   end Add_Load;

   ---------------
   -- Add_Alloc --
   ---------------

   procedure Add_Alloc
     (B         : Block_Ref;
      Alignment : Alloc_Alignment;
      Size      : Value_Type;
      Dest      : Temp_Ref) is
   begin
      B.Insns.Append ((Kind      => Alloc,
                       Dest_Type => B.Func.Unit.Address_Size,
                       Dest      => Dest,
                       Alignment => Alignment,
                       Size      => Size));
   end Add_Alloc;

   --------------------
   -- Add_Comparison --
   --------------------

   procedure Add_Comparison
     (B            : Block_Ref;
      Kind         : Integer_Comparison_Kind;
      Operand_Type : Address_Type;
      Left, Right  : Value_Type;
      Dest_Type    : Address_Type;
      Dest         : Temp_Ref) is
   begin
      B.Insns.Append ((Kind          => Integer_Comparison,
                       Dest_Type     => Dest_Type,
                       Dest          => Dest,
                       Left          => Left,
                       Right         => Right,
                       Int_Comp_Op   => Operand_Type,
                       Int_Comp_Kind => Kind));
   end Add_Comparison;

   --------------------
   -- Add_Comparison --
   --------------------

   procedure Add_Comparison
     (B            : Block_Ref;
      Kind         : Float_Comparison_Kind;
      Operand_Type : Float_Type;
      Left, Right  : Value_Type;
      Dest_Type    : Address_Type;
      Dest         : Temp_Ref) is
   begin
      B.Insns.Append ((Kind            => Float_Comparison,
                       Dest_Type       => Dest_Type,
                       Dest            => Dest,
                       Left            => Left,
                       Right           => Right,
                       Float_Comp_Op   => Operand_Type,
                       Float_Comp_Kind => Kind));
   end Add_Comparison;

   -------------
   -- Add_Ext --
   -------------

   procedure Add_Ext
     (B           : Block_Ref;
      Source_Type : Small_Scalar_Type;
      Source      : Value_Type;
      Sign_Extend : Boolean;
      Dest_Type   : Address_Type;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind            => Ext,
                       Dest_Type       => Dest_Type,
                       Dest            => Dest,
                       Ext_Src_Type    => Source_Type,
                       Ext_Src         => Source,
                       Ext_Sign_Extend => Sign_Extend));
   end Add_Ext;

   ------------------
   -- Add_Ext_Word --
   ------------------

   procedure Add_Ext_Word
     (B           : Block_Ref;
      Source      : Value_Type;
      Sign_Extend : Boolean;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind            => Ext,
                       Dest_Type       => Long,
                       Dest            => Dest,
                       Ext_Src_Type    => Word,
                       Ext_Src         => Source,
                       Ext_Sign_Extend => Sign_Extend));
   end Add_Ext_Word;

   --------------------
   -- Add_Ext_Single --
   --------------------

   procedure Add_Ext_Single
     (B      : Block_Ref;
      Source : Value_Type;
      Dest   : Temp_Ref) is
   begin
      B.Insns.Append ((Kind            => Ext,
                       Dest_Type       => Double,
                       Dest            => Dest,
                       Ext_Src_Type    => Single,
                       Ext_Src         => Source,
                       Ext_Sign_Extend => True));
   end Add_Ext_Single;

   ----------------------
   -- Add_Trunc_Double --
   ----------------------

   procedure Add_Trunc_Double
     (B      : Block_Ref;
      Source : Value_Type;
      Dest   : Temp_Ref) is
   begin
      B.Insns.Append ((Kind      => Trunc,
                       Dest_Type => Single,
                       Dest      => Dest,
                       Trunc_Src => Source));
   end Add_Trunc_Double;

   -------------------
   -- Add_To_Signed --
   -------------------

   procedure Add_To_Signed
     (B : Block_Ref;
      Source_Type : Float_Type;
      Source      : Value_Type;
      Dest_Type   : Address_Type;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind          => Conversion,
                       Dest_Type     => Dest_Type,
                       Dest          => Dest,
                       Conv_Src_Type => Source_Type,
                       Conv_Src      => Source));
   end Add_To_Signed;

   ------------------
   -- Add_To_Float --
   ------------------

   procedure Add_To_Float
     (B : Block_Ref;
      Source_Type : Address_Type;
      Source      : Value_Type;
      Dest_Type   : Float_Type;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind          => Conversion,
                       Dest_Type     => Dest_Type,
                       Dest          => Dest,
                       Conv_Src_Type => Source_Type,
                       Conv_Src      => Source));
   end Add_To_Float;

   --------------
   -- Add_Cast --
   --------------

   procedure Add_Cast
     (B           : Block_Ref;
      Source      : Value_Type;
      Dest_Type   : Base_Type;
      Dest        : Temp_Ref) is
   begin
      B.Insns.Append ((Kind      => Cast,
                       Dest_Type => Dest_Type,
                       Dest      => Dest,
                       Cast_Src  => Source));
   end Add_Cast;

   --------------
   -- Set_Jump --
   --------------

   procedure Set_Jump (B : Block_Ref; Dest : Block_Ref) is
   begin
      B.Jump := (Kind => Jump, Dest => Dest);
   end Set_Jump;

   ----------------
   -- Set_Branch --
   ----------------

   procedure Set_Branch
     (B                             : Block_Ref;
      Condition                     : Value_Type;
      Branch_Dest, Fallthrough_Dest : Block_Ref)
   is
   begin
      B.Jump := (Kind             => Branch,
                 Condition        => Condition,
                 Branch_Dest      => Branch_Dest,
                 Fallthrough_Dest => Fallthrough_Dest);
   end Set_Branch;

   -------------
   -- Set_Ret --
   -------------

   procedure Set_Ret (B : Block_Ref) is
   begin
      B.Jump := (Kind => Ret);
   end Set_Ret;

   -------------
   -- Set_Ret --
   -------------

   procedure Set_Ret (B : Block_Ref; Value : Value_Type) is
   begin
      B.Jump := (Ret_Value, Value => Value);
   end Set_Ret;

end QBE.Core;
