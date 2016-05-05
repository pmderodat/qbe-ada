with Ada.Unchecked_Deallocation;

package body QBE.Symbols is

   procedure Destroy is new Ada.Unchecked_Deallocation
     (String, Symbol_Type);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (T : in out Symbol_Table) is
      use String_Sets;

      Cur    : Cursor := T.First;
      Symbol : Symbol_Type;
   begin
      while Has_Element (Cur) loop
         Symbol := Element (Cur);
         Next (Cur);
         Destroy (Symbol);
      end loop;
      T.Clear;
   end Destroy;

   ---------
   -- Get --
   ---------

   function Get (T : in out Symbol_Table; Name : String) return Symbol_Type is
      use String_Sets;

      Cur : constant Cursor := T.Find (Name'Unrestricted_Access);
      S   : Symbol_Type;
   begin
      if Has_Element (Cur) then
         return Element (Cur);
      else
         S := new String'(Name);
         T.Include (S);
         return S;
      end if;
   end Get;

end QBE.Symbols;
