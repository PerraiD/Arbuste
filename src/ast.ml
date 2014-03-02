module Operator = struct
  type t = 
    | Add| Sub | Mul | Div
    | Print
    | Seq | Let | In
  let to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Print -> "print"
    | Seq -> "seq"
    | Let -> "let"
    | In -> "in"
  let print opr = print_string (to_string opr)
  let print_endline opr = print opr; print_newline ()  
end

module Operand = struct
  type t =
    | Void
    | Stdout
    | String of string
    | Int of int
    | Ident of string * Lexing.position
  let to_string = function
    | Void -> "void"
    | Stdout -> "stdout"
    | String x -> x
    | Int x -> string_of_int x
    | Ident (x, _) -> x
  let print opd = print_string (to_string opd)
  let print_endline opd = print opd; print_newline ()
end

type t = Operand of Operand.t | Operation of Operator.t * t * t

exception EvaluationError of string

let print ast = 
  let rec print' = function
    | (tab, Operand x) -> print_string tab; Operand.print_endline x
    | (tab, Operation (x, y, z)) ->
        print_string tab; Operator.print_endline x;
        print' (tab ^ "  ", y);
        print' (tab ^ "  ", z)
  in
print' ("", ast)
