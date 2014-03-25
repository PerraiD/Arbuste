module Operator = struct
  type t = 
    | Add| Sub | Mul | Div | Or | And
    | Lesser | Greater | Equal
    | Print | Read
    | Seq | Let | In | If | Branch
    | Func | Param | Eval
    | Cast
  let to_string = function
    | Add     -> "+"
    | Sub     -> "-"
    | Mul     -> "*"
    | Div     -> "/"
    | Equal   -> "="
    | Lesser  -> "<"
    | Greater -> ">"
    | Or      -> "or"
    | And     -> "and"
    | Print   -> "print"
    | Read    -> "read"
    | Seq     -> "seq"
    | Let     -> "let"
    | In      -> "in"
    | If      -> "if"
    | Branch  -> "branch"
    | Func    -> "func"
    | Param   -> "param"
    | Eval    -> "eval"
    | Cast    -> "cast"
  let print opr = print_string (to_string opr)
  let print_endline opr = print opr; print_newline ()  
end

module Operand = struct
  type t =
    | Void
    | EndParam
    | Stdout
    | Stdin
    | ToString
    | ToInt
    | ToBool
    | String of string
    | Int of int
    | Bool of bool
    | Ident of string * Lexing.position
  let to_string = function
    | Void         -> "void"
    | EndParam     -> "end_param"
    | Stdout       -> "stdout"
    | Stdin        -> "stdin"
    | ToString     -> "string"
    | ToInt        -> "int"
    | ToBool       -> "Bool"
    | String x     -> x
    | Int x        -> string_of_int x
    | Bool b       -> string_of_bool b
    | Ident (x, _) -> x
  let print opd = print_string (to_string opd)
  let print_endline opd = print opd; print_newline ()
end

type t = {contents :ast; position : Lexing.position}
and ast = Operand of Operand.t | Operation of Operator.t * t * t

(** Gets a string from an ast. *)
let to_string ast = match ast.contents with
  | Operand opd -> Operand.to_string opd
  | Operation (opn, _, _) -> Operator.to_string opn

(** Prints the [ast] with only one operator or operand per line. *)
let print ast =
  let rec print' tab ast = match (tab, ast.contents) with
    | (tab, Operand x) -> print_string tab; Operand.print_endline x
    | (tab, Operation (x, y, z)) ->
      print_string tab; Operator.print_endline x;
      print' (tab ^ "  ") y;
      print' (tab ^ "  ") z
  in
  print' "" ast

(** Prints AST list [l]. *)
let print_list l = List.iter print l
