%{
  open Ast
  open Ast.Operator
  open Ast.Operand
%}

%token EOF
%token ADD SUB MUL DIV
%token EQUAL
%token BEGIN_PAR END_PAR
%token PRINT STDOUT
%token SEQ LET IN

%token <int> INT
%token <string> STRING
%token <string> IDENT

%start main
%type <Ast.t> main

%%

main:
  | operation EOF {$1}
  | EOF {Operand Void}
;

operation:
  | BEGIN_PAR operation END_PAR {$2}
  | operator operation operation {Operation ($1, $2, $3)}
  | operand {Operand $1}
;

operator:
  | ADD {Add}
  | SUB {Sub}
  | MUL {Mul}
  | DIV {Div}
  | PRINT {Print}
  | SEQ {Seq}
  | LET {Let}
  | IN {In}

operand:
  | STDOUT {Stdout}
  | STRING {String $1}
  | INT {Int $1}
  | IDENT {Ident ($1, symbol_start_pos ())}
;
