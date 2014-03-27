module Operator :
  sig
    type t =
        Add
      | Sub
      | Mul
      | Div
      | Or
      | And
      | Lesser
      | Greater
      | Equal
      | Print
      | Read
      | Seq
      | Let
      | In
      | If
      | Branch
      | Func
      | Param
      | Eval
      | Cast
    val to_string : t -> string
    val print : t -> unit
    val print_endline : t -> unit
  end

module Operand :
  sig
    type t =
        Void
      | EndParam
      | Stdout
      | Stdin
      | ToString
      | ToInt
      | ToBool
      | String of string
      | Int of int
      | Bool of bool
      | Ident of string
    val to_string : t -> string
    val print : t -> unit
    val print_endline : t -> unit
  end

type t = {data : ast; position : Lexing.position}
and ast = Operand of Operand.t | Operation of Operator.t * t * t

val to_string : t -> string

val print : t -> unit

val print_list : t list -> unit
