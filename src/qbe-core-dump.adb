with Interfaces;
use type Interfaces.Unsigned_64;

procedure QBE.Core.Dump (Unit : Compilation_Unit; File : in out File_Type) is

   function "+" (S : Symbol_Type) return String is
     (QBE.Symbols.Value (QBE.Symbols.Symbol_Type (S)));

   procedure Put_Comma (Is_First : in out Boolean);
   procedure Put_Label (Prefix : String; Id : Positive);
   procedure Put (B : Block_Ref);
   procedure Put (T : Temp_Ref);

   procedure Dump (C : Constant_Type);
   procedure Dump (ET : Extended_Type);
   procedure Dump (S : Signature_Type);
   procedure Dump (A : Aggregate_Type_Ref);
   procedure Dump (D : Data_Ref);
   procedure Dump (V : Value_Type);

   procedure Dump (F : Function_Ref);
   procedure Dump (B : Block_Ref);
   procedure Dump (I : Instruction_Type);

   ---------------
   -- Put_Comma --
   ---------------

   procedure Put_Comma (Is_First : in out Boolean) is
   begin
      if Is_First then
         Is_First := False;
      else
         Put (File, ", ");
      end if;
   end Put_Comma;

   ---------------
   -- Put_Label --
   ---------------

   procedure Put_Label (Prefix : String; Id : Positive) is
      Id_Image : constant String := Positive'Image (Id);
   begin
      Put
        (File, Prefix & Id_Image (Id_Image'First + 1 .. Id_Image'Last));
   end Put_Label;

   ---------
   -- Put --
   ---------

   procedure Put (B : Block_Ref) is
   begin
      Put_Label ("@b", B.Index);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (T : Temp_Ref) is
   begin
      Put_Label ("%l", Positive (T));
   end Put;

   ----------
   -- Dump --
   ----------

   procedure Dump (C : Constant_Type) is
   begin
      case C.Kind is
         when Decimal .. Double =>
            --  TODO??? Output human readable images for floating-point values
            Put (File, Interfaces.Unsigned_64'Image (C.Value));
         when Symbol =>
            Put (File, '$');
            Put (File, +C.Name);
      end case;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (ET : Extended_Type) is
   begin
      Put (File, (case ET is
                  when Word   => 'w',
                  when Long   => 'l',
                  when Single => 's',
                  when Double => 'd',
                  when Half   => 'h',
                  when Byte   => 'b'));
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (S : Signature_Type) is
   begin
      case S.Kind is
         when Base =>
            Dump (S.T);
         when Aggregate =>
            Put (File, ':');
            Put (File, +S.A.Name);
      end case;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (A : Aggregate_Type_Ref) is
      Is_First : Boolean := True;
   begin
      Put (File, "type :" & (+A.Name) & " =");
      if A.Kind = Opaque or else A.Alignment /= 0 then
         Put (File, " align" & Natural'Image (A.Alignment));
      end if;

      case A.Kind is
         when Regular =>
            Put (File, " {");
            if A.Items /= null then
               for Item of A.Items.all loop
                  Put_Comma (Is_First);
                  Dump (Item.Item_Type);
                  if Item.Count /= 1 then
                     Put (Natural'Image (Item.Count));
                  end if;
               end loop;
            end if;
            Put_Line (File, "}");

         when Opaque =>
            Put_Line (File, " {" & Natural'Image (A.Size) & " }");
      end case;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (D : Data_Ref) is
      Is_First : Boolean := True;
   begin
      if D.Export then
         Put (File, "export ");
      end if;
      Put (File, "data $" & (+D.Name) & " = {");
      if D.Items /= null then
         for Item of D.Items.all loop
            Put_Comma (Is_First);

            case Item.Kind is
               when Symbol .. Bytes =>
                  Dump (Item.Item_Kind);
                  Put (File, ' ');

                  case Item.Kind is
                     when Symbol =>
                        Put (File, "$" & (+Item.Name));
                        if Item.Offset /= 0 then
                           Put (File,
                                " +"
                                & Interfaces.Unsigned_64'Image (Item.Offset));
                        end if;

                     when Number =>
                        Dump (Item.Value);

                     when Bytes =>
                        --  TODO??? Output a string literal instead for
                        --  printable characters.

                        for C of Ada.Strings.Unbounded.To_String (Item.Values)
                        loop
                           Put (File, Natural'Image (Character'Pos (C)));
                        end loop;

                     when others => raise Program_Error;
                  end case;

               when Zero_Bytes =>
                  Put (File, "z" & Natural'Image (Item.Count));
            end case;
         end loop;
      end if;
      Put_Line (File, "}");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (V : Value_Type) is
   begin
      case V.Kind is
         when Constant_Value => Dump (V.C);
         when Temp_Value     => Put (V.T);
      end case;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (F : Function_Ref) is
   begin
      if F.Export then
         Put (File, "export ");
      end if;

      Put (File, "function ");
      if F.Has_Return_Type then
         Dump (F.Return_Type);
         Put (File, ' ');
      end if;

      Put (File, '$');
      Put (File, +F.Name);

      Put (File, '(');
      declare
         PT       : constant Temp_Ref_Array := Param_Temps (F);
         Is_First : Boolean := True;
      begin
         for I in PT'Range loop
            Put_Comma (Is_First);
            Dump (F.Param_Types (I));
            Put (File, ' ');
            Put (PT (I));
         end loop;
      end;
      Put_Line (File, ")");

      Put_Line (File, "{");
      for B of F.Blocks loop
         Dump (B);
      end loop;
      Put_Line (File, "}");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (B : Block_Ref) is
   begin
      Put (B);
      New_Line (File);

      for Phi of B.Phis loop
         Put (Phi.Dest);
         Put (File, " =");
         Dump (Phi.Dest_Type);
         Put (File, ' ');
         declare
            Is_First : Boolean := True;
         begin
            for Assoc of Phi.Values.all loop
               Put_Comma (Is_First);
               Put (Assoc.Block);
               Put (File, ' ');
               Dump (Assoc.Value);
            end loop;
         end;
         New_Line (File);
      end loop;

      for I of B.Insns loop
         Dump (I);
         New_Line (File);
      end loop;

      case B.Jump.Kind is
         when Jump =>
            Put (File, "jmp ");
            Put (B.Jump.Dest);
         when Branch =>
            Put (File, "jnz ");
            Dump (B.Jump.Condition);
            Put (File, ", ");
            Put (B.Jump.Branch_Dest);
            Put (File, ", ");
            Put (B.Jump.Fallthrough_Dest);
         when Ret =>
            Put (File, "ret");
         when Ret_Value =>
            Put (File, "ret ");
            Dump (B.Jump.Value);
      end case;
      New_Line (File);
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (I : Instruction_Type) is
      Type_Char : constant array (Extended_Type) of Character := "wlsdhb";
   begin
      case I.Kind is
         when Store =>
            Put (File, "store" & Type_Char (I.Store_Type) & ' ');
            Dump (I.Value);
            Put (File, ", ");
            Dump (I.Store_Addr);

         when Call =>
            Put (I.Call_Dest);
            Put (File, " =");
            Dump (I.Call_Dest_Type);
            Put (File, " call ");
            Dump (I.Call_Target);
            Put (File, '(');
            declare
               Is_First : Boolean := True;
            begin
               for Arg of I.Call_Args.all loop
                  Put_Comma (Is_First);
                  Dump (Arg.Arg_Type);
                  Put (File, ' ');
                  Dump (Arg.Arg_Value);
               end loop;
            end;
            Put (File, ')');

         when others =>
            Put (I.Dest);
            Put (File, " =" & Type_Char (I.Dest_Type) & ' ');

            case I.Kind is
               when Store | Call => raise Program_Error;

               when Load =>
                  Put (File, "load" & Type_Char (I.Load_Type));
                  if I.Load_Type in Word | Half | Byte then
                     Put (File, (if I.Load_Sign_Extend then 's' else 'u'));
                  end if;
                  Put (File, ' ');
                  Dump (I.Load_Addr);

               when Copy =>
                  Put (File, "copy ");
                  Dump (I.Copy_Value);

               when Alloc =>
                  declare
                     A : constant String :=
                       (case I.Alignment is
                        when Alloc4 => "4",
                        when Alloc8 => "8",
                        when Alloc16 => "16");
                  begin
                     Put (File, "alloc" & A & ' ');
                  end;
                  Dump (I.Size);

               when Arith | Integer_Comparison | Float_Comparison =>
                  case I.Kind is
                     when Arith =>
                        Put (File, (case I.Arith_Kind is
                                 when Add  => "add",
                                 when Sub  => "sub",
                                 when Div  => "div",
                                 when Mul  => "mul",
                                 when Udiv => "udiv",
                                 when Srem => "srem",
                                 when Urem => "urem",
                                 when Bor  => "or",
                                 when Bxor => "xor",
                                 when Band => "and",
                                 when Sar  => "sar",
                                 when Shr  => "shr",
                                 when Shl  => "shl"));
                     when Integer_Comparison =>
                        declare
                           C : constant String :=
                             (case I.Int_Comp_Kind is
                              when EQ  => "eq",
                              when NE  => "ne",
                              when SLE => "sle",
                              when SLT => "slt",
                              when SGE => "sge",
                              when SGT => "sgt",
                              when ULE => "ule",
                              when ULT => "ult",
                              when UGE => "uge",
                              when UGT => "ugt");
                        begin
                           Put (File, 'c' & C & Type_Char (I.Int_Comp_Op));
                        end;
                     when Float_Comparison =>
                        declare
                           C : constant String :=
                             (case I.Float_Comp_Kind is
                              when EQ => "eq",
                              when NE => "ne",
                              when LE => "sle",
                              when LT => "slt",
                              when GE => "sge",
                              when GT => "sgt",
                              when O  => "o",
                              when UO => "uo");
                        begin
                           Put (File, 'c' & C & Type_Char (I.Float_Comp_Op));
                        end;
                     when others => raise Program_Error;
                  end case;
                  Put (File, ' ');
                  Dump (I.Left);
                  Put (File, ", ");
                  Dump (I.Right);

               when Ext =>
                  Put (File, "ext");
                  case I.Ext_Src_Type is
                     when Word | Half | Byte =>
                        Put (File, (if I.Ext_Sign_Extend then 's' else 'u'));
                        Put (File, Type_Char (I.Ext_Src_Type));
                     when Single =>
                        Put (File, 's');
                     when others =>
                        raise Program_Error;
                  end case;
                  Put (File, ' ');
                  Dump (I.Ext_Src);

               when Trunc =>
                  Put (File, "truncd ");
                  Dump (I.Trunc_Src);

               when Conversion =>
                  declare
                     To_C : Character := 'i';
                  begin
                     if I.Conv_Src_Type in Word | Long then
                        Put (File, 's');
                        To_C := 'f';
                     end if;
                     Put
                       (File, Type_Char (I.Conv_Src_Type) & "to" & To_C & ' ');
                     Dump (I.Conv_Src);
                  end;

               when Cast =>
                  Put (File, "cast ");
                  Dump (I.Cast_Src);
            end case;
      end case;
      New_Line (File);
   end Dump;

begin
   for A of Unit.Aggregate_Types loop
      Dump (A);
   end loop;

   for D of Unit.Data_Defs loop
      Dump (D);
   end loop;

   for F of Unit.Function_Defs loop
      Dump (F);
   end loop;
end QBE.Core.Dump;
