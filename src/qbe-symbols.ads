private with Ada.Containers.Hashed_Sets;
private with Ada.Strings.Hash;

--  Simple hashed set based symbol table. A symbol table associates unique
--  words for each equal strings.
--
--  Symbol tables are widely used in compilers share the dynamically allocated
--  strings used to represent names. Doing so both reduces memory usage and
--  makes name comparison a constant time operation.

package QBE.Symbols is

   type Symbol_Table is limited private;
   --  Symbol table. Create one with the Create function and invoke Destroy on
   --  it when done with it.

   type Symbol_Type is private;
   --  A symbol, representing a name. Symbols are owned by the symbol table
   --  that created them, so make sure you do not keep a symbol after its
   --  symbol table has been destroyed.

   No_Symbol : constant Symbol_Type;

   function Create return Symbol_Table;
   --  Create a symbol table

   procedure Destroy (T : in out Symbol_Table);
   --  Destroy a symbol table. This invalidates all the Symbol_Type values
   --  created with T.

   function Get (T : in out Symbol_Table; Name : String) return Symbol_Type
      with Post => Get'Result /= No_Symbol;
   --  If this is the first time Get is called for this T and this Name, return
   --  an unique value. Otherwise, return the previous value returned for these
   --  parameters.

   function Value (S : Symbol_Type) return String
      with Pre => S /= No_Symbol;
   --  Return the name associated to a symbol

private

   type Symbol_Type is access String;

   No_Symbol : constant Symbol_Type := null;

   function Hash (S : Symbol_Type) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (S.all));

   function Equivalent_Elements (Left, Right : Symbol_Type) return Boolean is
     (Left = Right);

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Symbol_Type,
      Hash                => Hash,
      Equivalent_Elements => Equivalent_Elements,
      "="                 => "=");

   type Symbol_Table is new String_Sets.Set with null record;

   function Create return Symbol_Table is (String_Sets.Set with others => <>);

   function Value (S : Symbol_Type) return String is (S.all);

end QBE.Symbols;
