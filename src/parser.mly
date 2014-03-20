%{
  open Ast
  open Ast.Operator
  open Ast.Operand
%}

%token EOF
%token ADD SUB MUL DIV
%token OR AND
%token LESSER GREATER EQUAL
%token BEGIN_PAR END_PAR
%token PRINT READ STDOUT STDIN
%token SEQ LET IN IF BRANCH
%token FUNC PARAM END_PARAM EVAL
%token CAST TO_STRING TO_INT TO_BOOL

%token <int> INT
%token <bool> BOOL
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
  | ADD     {Add}
  | SUB     {Sub}
  | MUL     {Mul}
  | DIV     {Div}
  | LESSER  {Lesser}
  | GREATER {Greater}
  | EQUAL   {Equal}
  | OR      {Or}
  | AND     {And}
  | PRINT   {Print}
  | READ    {Read}
  | SEQ     {Seq}
  | LET     {Let}
  | IN      {In}
  | IF      {If}
  | BRANCH  {Branch}
  | FUNC    {Func}
  | PARAM   {Param}
  | EVAL    {Eval}
  | CAST    {Cast}

operand:
  | STDOUT       {Stdout}
  | STDIN        {Stdin}
  | STRING       {String $1}
  | INT          {Int $1}
  | BOOL         {Bool $1}
  | IDENT        {Ident ($1, symbol_start_pos ())}
  | END_PARAM    {EndParam}
  | TO_STRING    {ToString}
  | TO_INT       {ToInt}
  | TO_BOOL      {ToBool}
;
