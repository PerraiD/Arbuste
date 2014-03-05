open Ast
open Ast.Operator
open Ast.Operand

let print_ident i env = match Symbols.find env i with
  | String s -> print_string s
  | Int i -> print_int i
  | Bool b -> print_string (string_of_bool b)
  | _ -> Error.raise_simple "Print error, invalid parameter"

let print_operand opd env = match opd with
  | String s -> print_string s
  | Int i -> print_int i
  | Bool b -> print_string (string_of_bool b)
  | Ident (i, _) -> print_ident i env
  | _ -> Error.raise_simple "Print error, invalid parameter"

(** Interprets the given [ast] with the given environment. *)
let rec evaluate ast env = match ast with
  | Operand (Ident (i, _))
    -> Operand (Symbols.find env i)
  | Operand _ as leaf
    -> leaf

  (* Variable assignement *)

  | Operation (In, Operation (Let, Operand (Ident (id, p)), Operand x), op)
    -> if Symbols.mem env id then Error.warn_shadowed id p;
       evaluate op (Symbols.add env id x)
  | Operation (In, _, _)
  | Operation (Let, _, _)
    -> Error.raise_simple "Invalid in ... let ... construction"

  (* Branching *)

  | Operation (If, cond, Operation (Branch, t, f))
    -> begin match evaluate cond env with
         | Operand (Bool true)  -> evaluate t env
         | Operand (Bool false) -> evaluate f env
         | _ -> Error.raise_simple "Condition must be a boolean value"
       end
  | Operation (If, _, _)
  | Operation (Branch, _, _)
    -> Error.raise_simple "Invalid if ... branch ... construction"

  (* Reduction rules *)

  | Operation (opr, (Operation (o, x, y)), opd)
    -> let in_eval = evaluate (Operation (o, x, y)) env in
       evaluate (Operation (opr, in_eval, opd)) env
  | Operation (opr, (Operand (Ident (i, p))), opd)
    -> let in_eval = evaluate (Operand (Ident (i, p))) env in
       evaluate (Operation (opr, in_eval, opd)) env
  | Operation (opr, opd, (Operation (o, x, y)))
    -> let in_eval = evaluate (Operation (o, x, y)) env in
       evaluate (Operation (opr, opd, in_eval)) env
  | Operation (opr, opd, Operand (Ident (i, p)))
    -> let in_eval = evaluate (Operand (Ident (i, p))) env in
       evaluate (Operation (opr, opd, in_eval)) env

  (* Sequence *)

  | Operation (Seq, Operand Void, Operand x) 
    -> evaluate (Operand x) env
  | Operation (Seq, Operand _, _)
    -> Error.raise_simple "Invalid seq ... construction"

  (* Print *)

  | Operation (Print, Operand Stdout, Operand v)
    -> print_operand v env; Operand Void        
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
    -> Error.raise_simple "Arithmetic operations only accept integer values"

  (* Boolean operations *)
  | Operation (Or, Operand (Bool x), Operand (Bool y))
    -> Operand (Bool (x || y))
  | Operation (And, Operand (Bool x), Operand (Bool y))
    -> Operand (Bool (x && y))
  | Operation (Or, _, _)
  | Operation (And, _, _)
    -> Error.raise_simple "Boolean operations only accept boolean values"
  | Operation (Equal, Operand x, Operand y)
    -> Operand (Bool (x = y))

(** Interprets the given [ast]. *)
let run ast =
  let env = Symbols.create () in
  match evaluate ast env with
    | Operand Void -> ()
    | _ -> Error.raise_simple "The program should return void"
