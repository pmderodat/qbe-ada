with Interfaces;
use type Interfaces.Unsigned_64;

with GNATCOLL.Symbols;

procedure QBE.Core.Dump (Unit : Compilation_Unit; File : in out File_Type) is

   function "+" (S : Symbol_Type) return String is
     (GNATCOLL.Symbols.Get (GNATCOLL.Symbols.Symbol (S)).all);
   procedure Put_Comma (Is_First : in out Boolean);

   procedure Dump (C : Constant_Type);
   procedure Dump (ET : Extended_Type);
   procedure Dump (A : Aggregate_Type_Ref);
   procedure Dump (D : Data_Ref);

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
                  when Simple => 's',
                  when Double => 'd',
                  when Half   => 'h',
                  when Byte   => 'b'));
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

begin
   for A of Unit.Aggregate_Types loop
      Dump (A);
   end loop;

   for D of Unit.Data_Defs loop
      Dump (D);
   end loop;
end QBE.Core.Dump;