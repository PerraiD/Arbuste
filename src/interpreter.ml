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
    -> raise (EvaluationError "Invalid in ... let ... construction")

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
    -> raise (EvaluationError "Invalid seq ... construction")

  (* Print *)

  | Operation (Print, Operand Stdout, Operand v)
    -> Operand.print_endline v; Operand Void
  | Operation (Print, _, _)
    -> raise (EvaluationError "Print error")
   
  (* Valid arithmetic operations *)

  | Operation (Add, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 + x2))
  | Operation (Sub, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 - x2))
  | Operation (Mul, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 * x2))
  | Operation (Div, Operand (Int x1), Operand (Int x2))
    -> Operand (Int (x1 / x2))

  (* Invalid arithmetic operations *)

  | Operation (Add, _, _)
  | Operation (Sub, _, _)
  | Operation (Mul, _, _)
  | Operation (Div, _, _)
    -> raise (EvaluationError "Arithmetic operations only accept int values")

(** Interprets the given AST. *)
let run ast =
  let env = Symbols.create () in
  match evaluate ast env with
    | Operand Void -> ()
    | _ -> raise (EvaluationError "The program should return void")
