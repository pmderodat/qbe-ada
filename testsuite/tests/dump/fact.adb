--#check_output

with Ada.Text_IO; use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Fact is
   F           : File_Type := Standard_Output;
   U           : Compilation_Unit := Create (Long);
   F1          : constant Function_Ref := Create (U, "fact");
   Params      : Temp_Ref_Array (1 .. 1);
   N, R        : Temp_Ref;
   E_Block     : Block_Ref;
   Loop_Header : Block_Ref;
   Loop_Body   : Block_Ref;
   Ret_Block   : Block_Ref;
begin
   Set_Export (F1, True);
   Set_Return_Type (F1, (Base, Word));
   Set_Param_Types (F1, (1 => (Base, Word)));
   Params := Param_Temps (F1);

   N := Create (F1);
   R := Create (F1);

   E_Block := Entry_Block (F1);
   Loop_Header := Create (F1);
   Loop_Body := Create (F1);
   Ret_Block := Create (F1);

   --  Entry: initialize N to the parameter and R to 1

   Add_Arith (E_Block, Add, Value (Params (1)), Value (0), Word, N);
   Add_Arith (E_Block, Add, Value (1), Value (0), Word, R);
   Set_Jump (E_Block, Loop_Header);

   --  Loop header: if N > 1, enter body otherwise go to return

   declare
      Cond : constant Temp_Ref := Create (F1);
   begin
      Add_Comparison
        (Loop_Header, SGE, Word, Value (N), Value (1), Word, Cond);
      Set_Branch (Loop_Header, Value (Cond), Loop_Body, Ret_Block);
   end;

   --  Loop body: perform one iteration

   Add_Arith (Loop_Body, Mul, Value (N), Value (R), Word, R);
   Add_Arith (Loop_Body, Sub, Value (N), Value (1), Word, N);
   Set_Jump (Loop_Body, Loop_Header);

   --  Return block: just return R

   Set_Ret (Ret_Block, Value (R));

   --  We are done!

   Dump (U, F);
   Destroy (U);
end Fact;
