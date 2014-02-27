What is it?
===========

Arbuste is toy langage where all instructions are binary operators. A program is thus a binary tree.

The compiler is written in Ocaml with ocamllex and ocamlyacc.

Compile it!
===========

As bytecode:

  ocamlbuild src/arbuste.byte
  
As native code:

  ocamlbuild src/arbiste.native
  
Run!
====

Run

  ./arbuste.byte examples/helloworld.arb
  
or

  ./arbuste.native examples/helloworld.arb
