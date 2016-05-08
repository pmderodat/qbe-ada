QBE-Ada
=======

This repository contains an library to easily create
[QBE](http://c9x.me/compile/) programs in Ada. Note that this is in the early
stages of its development.

Here is an example that generates a "Hello, world!" program:

```ada
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with QBE.Core; use QBE.Core;
with QBE.Core.Dump;

procedure Hello_World is
   --  Let's create a compilation unit (U) that contains two things: a "main"
   --  function and a "string" data entry.
   U  : Compilation_Unit := Create (Long);
   F1 : constant Function_Ref := Create (U, "main");
   S  : Data_Ref := Create (U, "string");
begin
   --  Make "main" public and make it return a word
   Set_Export (F1, True);
   Set_Return_Type (F1, (Base, Word));

   --  In "main"'s entry basic block, add a call to libc's "puts" and then
   --  return immedialtely 0.
   declare
      B : constant Block_Ref := Entry_Block (F1);
   begin
      Add_Call
        (B         => B,
         Target    => Value (Symbol (U, "puts")),
         Args      => (1 => (Arg_Type  => (Base, Long),
                             Arg_Value => Value (Symbol (S)))),
         Dest_Type => (Base, Long),
         Dest      => Create (F1));
      Set_Ret (B, Value (0));
   end;

   --  Make "string" contain the text we want to print with "puts"
   declare
      S_Data : constant Unbounded_String := To_Unbounded_String
        ("Hello, world!" & ASCII.NUL);
   begin
      Set_Items (S, (1 => (Kind      => Bytes,
                           Item_Kind => Byte,
                           Values    => S_Data)));
   end;

   --  The program is ready! Dump it to the standard output and free resources
   --  allocated for our compilation unit.
   declare
      F : File_Type := Standard_Output;
   begin
      Dump (U, F);
   end;
   Destroy (U);
end Hello_World;
```

Here is how to use this on a `x86_64` based Linux machine:

```sh
# Build the Ada program
gprbuild hello_world.adb

# Run it, transfer its output (QBE IR) to QBE. QBE will produce assembly code.
./hello_world | qbe - > hw_gen.s

# Assemble the generated program and link it with the libc.
gcc -o hw_gen hw_gen.s

# Run it!
./hw_gen
```

If all works as expected, the last command should print: "Hello, world!".


Build
-----

In order to build this project, you need to install:

* an Ada 2012 compiler, a recent `gnat` Debian package will do the job, for
  instance;
* [GPRbuild](https://github.com/AdaCore/gprbuild).

Once they are available, just run from this directory:

```sh
gprbuild -Pqbe -p
```

The library will get generated in the `lib` subdirectory.


Testing
-------

Testcases are present in the [`tests`](tests/) subdirectory. Each testcase is a
mere Ada main procedure using the library that prints a QBE program on the
standard output. There is no automated testsuite yet, though.
