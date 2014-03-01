open Ast
open Ast.Operator
open Ast.Operand

(** Interprets the given AST with the given environment. *)
let rec evaluate ast env = match ast with
  | Operand _ as leaf
    -> leaf

  (* Variable assignement *)

  | Operation (In, Operation (Let, Operand (Ident id), Operand x), op)
    -> evaluate op (Symbols.add env id x)
  | Operation (In, _, _)
  | Operation (Let, _, _)
    -> Error.raise_simple "Invalid in ... let ... construction"

  (* Reduction rules *)

  | Operation (opr, (Operation (o, x, y)), opd)
    -> (* left reduce *)
       let in_eval = evaluate (Operation (o, x, y)) env in
       evaluate (Operation (opr, in_eval, opd)) env
  | Operation (opr, opd, (Operation (o, x, y)))
    -> (* right reduce *)
       let in_eval = evaluate (Operation (o, x, y)) env in
       evaluate (Operation (opr, opd, in_eval)) env

  (* Identifier *)

  | Operation (opr, Operand Ident i, opd)
    -> let v = Symbols.find env i in
       evaluate (Operation (opr, Operand v, opd)) env

  (* Sequence *)

  | Operation (Seq, Operand Void, Operand x) 
    -> evaluate (Operand x) env
  | Operation (Seq, Operand _, _)
    -> Error.raise_simple "Invalid seq ... construction"

  (* Print *)

  | Operation (Print, Operand Stdout, Operand v)
    -> Operand.print v; Operand Void
  | Operation (Print, _, _)
    -> Error.raise_simple "Print error"
   
  (* Arithmetic operations *)

  | Operation (Add, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 + x2))
  | Operation (Sub, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 - x2))
  | Operation (Mul, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 * x2))
  | Operation (Div, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 / x2))
  | Operation (Add, _, _)
  | Operation (Sub, _, _)
  | Operation (Mul, _, _)
  | Operation (Div, _, _)
    -> Error.raise_simple "Arithmetic operations only accept int values"

(** Interprets the given AST. *)
let run ast =
  let env = Symbols.create () in
  match evaluate ast env with
    | Operand Void -> ()
    | _ -> Error.raise_simple "The program should return void"
