private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Interfaces;

private with GNATCOLL.Symbols;

--  This package provides an API to create in memory QBE programs. Have a look
--  at the QBE.Core.Dump unit to dump such in memory programs as text.

package QBE.Core is

   --  TODO??? Turn Unsigned_64 occurences into a big integers. Is it
   --  necessary, though?  There is no extended type that is wider than 64
   --  bits...

   -------------------
   -- Main entities --
   -------------------

   type Extended_Type is (Word, Long, Single, Double, Half, Byte);
   --  Scalar types

   subtype Base_Type is Extended_Type range Word .. Double;
   --  Subset of scalar types that can be used as temporaries in the IL

   subtype Address_Type is Extended_Type range Word .. Long;
   --  Subset of scalar types that are candidates to represent addresses in the
   --  generated programs.

   type Compilation_Unit is private;
   --  Container for all entities declared in a compilation unit. This owns all
   --  these entities, so users must not keep references to them when the
   --  corresponding compilation unit is destroyed (see Destroy).
   --
   --  Note that entities created with one compilation unit must not be used
   --  with another unit.

   function Create (Address_Size : Address_Type) return Compilation_Unit
      with Inline;
   --  Create a new compilation unit. Address_Size indicates the type used to
   --  represent addresses in the generated program.
   --
   --  Once done with this unit, its resources must be deallocated using
   --  Destroy.

   procedure Destroy (Unit : in out Compilation_Unit)
      with Inline;
   --  Destroy a compilation unit and all the entities it contains

   type Symbol_Type is private;
   --  Symbol name for data and functions

   function Symbol (Unit : Compilation_Unit; Name : String) return Symbol_Type
      with Inline;
   --  Get the symbol corresponding to some name

   type Constant_Kind is (Decimal, Single, Double, Symbol);
   type Constant_Type (Kind : Constant_Kind := Decimal) is record
      case Kind is
         when Decimal .. Double => Value : Interfaces.Unsigned_64;
         when Symbol            => Name  : Symbol_Type;
      end case;
   end record;
   --  Constant value to be used either to define global data or to be used as
   --  immediate for instructions.

   ---------------------
   -- Aggregate types --
   ---------------------

   type Aggregate_Type_Ref is private;
   --  Reference to an aggregate type

   type Aggregate_Type_Kind is (Regular, Opaque);
   --  Aggregate types can be either regular, in which case we know their
   --  internal structure, or they can be opaque, in which case we only know
   --  their size.

   type Aggregate_Item is record
      Item_Type : Extended_Type;
      --  Type for all the fields

      Count     : Positive;
      --  Number of fields to define
   end record;
   --  For regular aggregate types, this represents a group of consecutive
   --  fields

   type Aggregate_Item_Array is array (Positive range <>) of Aggregate_Item;

   function Get_Kind (A : Aggregate_Type_Ref) return Aggregate_Type_Kind
      with Inline;
   --  Return the kind of an aggregate type

   function Create
     (Unit : Compilation_Unit;
      Kind : Aggregate_Type_Kind;
      Name : String)
      return Aggregate_Type_Ref
      with Post => Get_Kind (Create'Result) = Kind;
   --  Create a new aggregate type in Unit. Kind define whether it's regular or
   --  opaque and Name provides a name for it.
   --
   --  Return a reference to the created type.

   procedure Set_Alignment (A : Aggregate_Type_Ref; Alignment : Natural);
   --  Set the alignment constraint for A (in bytes)

   procedure Set_Items (A : Aggregate_Type_Ref; Items : Aggregate_Item_Array)
      with Pre => Get_Kind (A) = Regular,
           Inline;
   --  Define the fields for A. If called multiple times on the same type, only
   --  the last sequence of fields is considered.

   procedure Set_Size (A : Aggregate_Type_Ref; Size : Natural)
      with Pre => Get_Kind (A) = Opaque,
           Inline;
   --  Define the size of A (in bytes)

   -----------------------------
   -- Global Data definitions --
   -----------------------------

   type Data_Ref is private;
   --  Reference to global data

   type Data_Item_Kind is (Symbol, Number, Bytes, Zero_Bytes);

   type Data_Item (Kind : Data_Item_Kind := Symbol) is record
      case Kind is
         when Symbol .. Bytes =>
            Item_Kind : Extended_Type;
            --  Type for this data item

            case Kind is
               when Symbol =>
                  Name   : Symbol_Type;
                  Offset : Interfaces.Unsigned_64;
                  --  The value of this item is the address of the Name symbol
                  --  plus the Offset constant.

               when Number =>
                  Value : Constant_Type;
                  --  The value of this item Value

               when Bytes =>
                  Values : Ada.Strings.Unbounded.Unbounded_String;
                  --  This data represents one item per byte in Values

               when others => null; -- This is "unreachable"
            end case;
         when Zero_Bytes =>
            Count     : Positive;
            --  This data item introduces Count padding bytes
      end case;
   end record;

   type Data_Item_Array is array (Positive range <>) of Data_Item;

   function Create
     (Unit : Compilation_Unit;
      Name : String)
      return Data_Ref;
   --  Create an entry for global data

   procedure Set_Export (D : Data_Ref; Export : Boolean)
      with Inline;
   --  Define whether this data entry is exported to other compilation units

   procedure Set_Items (D : Data_Ref; Items : Data_Item_Array)
      with Inline;
   --  Assign actual data to D. If called multiple times on the same data
   --  definition , only the last data is considered.

   function Symbol (D : Data_Ref) return Symbol_Type
      with Inline;
   --  Get the symbol name for D

   ---------------
   -- Functions --
   ---------------

   type Function_Ref is private;
   --  Reference to a function

   type Signature_Type_Kind is (Base, Aggregate);

   type Signature_Type (Kind : Signature_Type_Kind := Base) is record
      case Kind is
         when Base      => T : Base_Type;
         when Aggregate => A : Aggregate_Type_Ref;
      end case;
   end record;
   --  Type to be used for function arguments and return values

   type Signature_Array is array (Positive range <>) of Signature_Type;

   function Create
     (Unit : Compilation_Unit;
      Name : String)
      return Function_Ref;
   --  Create a new function to be defined in Unit and assign its Name. return
   --  a reference to the defined function.

   procedure Set_Export (F : Function_Ref; Export : Boolean)
      with Inline;
   --  Define whether this function is exported to other compilation units

   procedure Set_Return_Type (F : Function_Ref; T : Signature_Type)
      with Inline;
   --  Set a return type for F. If not called, the function is assumed not to
   --  return anything.

   procedure Set_Param_Types (F : Function_Ref; Param_Types : Signature_Array)
      with Inline;
   --  Define the list of parameters that F accepts. If not called, the
   --  function is assumed to accept no parameter.

   function Symbol (F : Function_Ref) return Symbol_Type
      with Inline;
   --  Get the symbol name for F

   ------------------
   -- Basic blocks --
   ------------------

   type Block_Ref is private;
   --  Reference to a basic block in a function

   function Entry_Block (F : Function_Ref) return Block_Ref
      with Inline;
   --  Return the entry basic block for function F

   function Create (F : Function_Ref) return Block_Ref;
   --  Create a new basic block for function F and return a reference to it

   -----------------
   -- Temporaries --
   -----------------

   type Temp_Ref is private;
   --  Reference to a temporary in a function

   No_Temp : constant Temp_Ref;
   --  Special value to mean "no temporary"

   type Temp_Ref_Array is array (Positive range <>) of Temp_Ref;

   function Create (F : Function_Ref) return Temp_Ref;
   --  Create a new temporary in function F and return a reference to it

   function Param_Temps (F : Function_Ref) return Temp_Ref_Array;
   --  For each parameter F has, return the corresponding temporary to use in
   --  F's body.

   ------------------
   -- Instructions --
   ------------------

   type Value_Kind is (Constant_Value, Temp_Value);
   type Value_Type (Kind : Value_Kind := Constant_Value) is record
      case Kind is
         when Constant_Value =>
            C : Constant_Type;
         when Temp_Value =>
            T : Temp_Ref;
      end case;
   end record;
   --  Value to be used as an instruction operand

   function Value (C : Constant_Type) return Value_Type is
     ((Kind => Constant_Value, C => C));
   function Value (S : Symbol_Type) return Value_Type is
     ((Kind => Constant_Value, C => (Kind => Symbol, Name => S)));
   function Value (T : Temp_Ref) return Value_Type is
     ((Kind => Temp_Value, T => T));

   type Phi_Association is record
      Block : Block_Ref;
      Value : Value_Type;
   end record;
   --  Basic block/value association for PHI nodes input

   type Phi_Association_Array is array (Positive range <>) of Phi_Association;

   procedure Add_Phi
     (B         : Block_Ref;
      Dest      : Temp_Ref;
      Dest_Type : Base_Type;
      Values    : Phi_Association_Array);
   --  Add a new PHI node to B. This node computes the value in Values that
   --  matches the previously executed basic block. It stores this value in the
   --  Dest temporary with the Dest_Type basic type.

   procedure Set_Jump (B : Block_Ref; Dest : Block_Ref);
   --  Make B end with an unconditional jump to Dest

   procedure Set_Branch
     (B                             : Block_Ref;
      Condition                     : Value_Type;
      Branch_Dest, Fallthrough_Dest : Block_Ref);
   --  Make B end with a conditional jump. If Condition is non-zero, it will
   --  jump to Branch_Dest, otherwise it will jump to Fallthrough_Dest.

   procedure Set_Ret (B : Block_Ref);
   --  Make B end with a return instruction with no value

   procedure Set_Ret (B : Block_Ref; Value : Value_Type);
   --  Make B en with a return instruction with Value

private

   type Symbol_Type is new GNATCOLL.Symbols.Symbol;

   type Compilation_Unit_Type;
   type Compilation_Unit is access Compilation_Unit_Type;

   type Aggregate_Item_Array_Access is access Aggregate_Item_Array;

   type Aggregate_Type (Kind : Aggregate_Type_Kind) is record
      Unit      : Compilation_Unit;
      Name      : Symbol_Type;
      Alignment : Natural;
      case Kind is
         when Regular => Items : Aggregate_Item_Array_Access;
         when Opaque  => Size  : Natural;
      end case;
   end record;

   type Aggregate_Type_Ref is access Aggregate_Type;

   type Data_Item_Array_Access is access Data_Item_Array;

   type Data is record
      Unit   : Compilation_Unit;
      Export : Boolean;
      Name   : Symbol_Type;
      Items  : Data_Item_Array_Access;
   end record;

   type Data_Ref is access Data;

   type Signature_Array_Access is access Signature_Array;

   type Phi_Association_Array_Access is access Phi_Association_Array;

   type Temp_Ref is new Natural;
   --  A temporary is currently just an unique identifier with no information
   --  associated.

   No_Temp : constant Temp_Ref := 0;

   type Phi_Type is record
      Dest      : Temp_Ref;
      Dest_Type : Base_Type;
      Values    : Phi_Association_Array_Access;
   end record;

   package Phi_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Phi_Type);

   type Jump_Kind is (Jump, Branch, Ret, Ret_Value);
   type Jump_Type (Kind : Jump_Kind := Ret) is record
      case Kind is
         when Jump =>
            Dest                          : Block_Ref;
         when Branch =>
            Condition                     : Value_Type;
            Branch_Dest, Fallthrough_Dest : Block_Ref;
         when Ret =>
            null;
         when Ret_Value =>
            Value                         : Value_Type;
      end case;
   end record;

   type Block is record
      Func  : Function_Ref;
      --  Function in which this basic block is defined

      Index : Positive;
      --  Function-specific index for this basic block. We use it to define a
      --  name for this block.

      Phis  : Phi_Vectors.Vector;
      --  Collection of PHI nodes this block contains

      Jump  : Jump_Type;
      --  Control-flow behavior for this block at the end of its execution
   end record;

   type Block_Ref is access Block;

   package Block_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Block_Ref);

   type Function_Type is record
      Unit             : Compilation_Unit;
      Export           : Boolean;
      Name             : Symbol_Type;
      Has_Return_Type  : Boolean;
      Return_Type      : Signature_Type;
      Param_Types      : Signature_Array_Access;

      Blocks           : Block_Vectors.Vector;
      Next_Block_Index : Positive;

      Next_Temp_Index  : Temp_Ref;
      --  Index for the next temporary we will create for this function,
      --  incremented everytime we create a new temporary. Each parameter has
      --  its own temporary starting from 1.
   end record;

   type Function_Ref is access Function_Type;

   function Get_Kind (A : Aggregate_Type_Ref) return Aggregate_Type_Kind
   is (A.Kind);

   package Aggregate_Type_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Aggregate_Type_Ref);

   package Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Data_Ref);

   package Function_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Function_Ref);

   type Compilation_Unit_Type is limited record
      Address_Size    : Address_Type;
      Symbols         : GNATCOLL.Symbols.Symbol_Table_Access;
      Aggregate_Types : Aggregate_Type_Vectors.Vector;
      Data_Defs       : Data_Vectors.Vector;
      Function_Defs   : Function_Vectors.Vector;
   end record;

end QBE.Core;
