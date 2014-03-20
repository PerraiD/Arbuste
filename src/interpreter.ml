open Ast
open Ast.Operator
open Ast.Operand

let print_ident i env = match Environment.find env i with
  | Operand (String s) -> print_string s
  | Operand (Int i) -> print_int i
  | Operand (Bool b) -> print_string (string_of_bool b)
  | _ -> Error.raise_simple "Print error, invalid parameter"

let print_operand opn env = match opn with
  | Operand opd -> begin match opd with
    | String s -> print_string s
    | Int i -> print_int i
    | Bool b -> print_string (string_of_bool b)
    | Ident (i, _) -> print_ident i env
    | _ -> Error.raise_simple "Print error, invalid parameter"
    end
  | _ -> Error.raise_simple "Print error, invalid parameter"

let rec get_params_idents = function
  | Operand EndParam -> []
  | Operation (Param, Operand opd, next)
    -> (Operand opd)::(get_params_idents next)
  | _ -> Error.raise_simple "Param definition error"

let rec get_params_values plist env = match plist with
  | Operand EndParam -> []
  | Operation (Param, Operand (Ident (i, _)), next)
    -> let opd = Environment.find env i in
       opd::(get_params_values next env)
  | Operation (Param, Operand opd, next)
    -> (Operand opd)::(get_params_values next env)
  | _ -> Error.raise_simple "Param definition error"

let warn_on_ignore = function
  | Operand Void -> ()
  | _ -> Error.warn "Ignored value"

(** Interprets the given [ast] with the given [env]ironment. *)
let rec evaluate ast env = match ast with
  | Operand (Ident (i, _))
    -> (Environment.find env i), env
  | Operand _ as leaf
    -> leaf, env

 (* Sequences *)

  | Operation (In, Operand Void, opn)
  | Operation (Seq, Operand Void, opn) 
    -> evaluate opn env
  | Operation (In, left, right)
    -> let eval, left_env = evaluate left env in
       warn_on_ignore eval;
       evaluate right left_env
  | Operation (Seq, left, right)
    -> let eval, _ = evaluate left env in
       warn_on_ignore eval;
       evaluate right env

  (* Function declaration *)

  | Operation (Let, Operand (Ident (id, pos)), Operation (Func, p, f))
    -> if Environment.mem env id then Error.warn_shadowed id pos;
       let params = get_params_idents p in
       (Operand Void), (Environment.add_fun env id f params)
  | Operation (Func, _, _)
  | Operation (Param, _, _)
    -> Error.raise_simple "Invalid func ... param ... construction"

  (* Variable assignement *)

  | Operation (Let, Operand (Ident (id, p)), opn)
    -> if Environment.mem env id then Error.warn_shadowed id p;
       let (eval, _) = evaluate opn env in
       (Operand Void), (Environment.add env id eval)     
  | Operation (Let, _, _)
    -> Error.raise_simple "Invalid in ... let ... construction"

  (* Function evaluation *)

  | Operation (Eval, Operand (Ident (i, pos)), params)
    -> if Environment.mem env i
         then
           let (f, idents) = Environment.find_func env i in
           let p =  get_params_values params env in
           let func_env = Environment.make_env idents p in
           evaluate f (Environment.add_fun func_env i f idents)
         else Error.raise_positioned ("Unknown function " ^ i) pos
  | Operation (Eval, _, _)
    -> Error.raise_simple "Invalid eval ... param ... construction"

  (* Branching *)

  | Operation (If, cond, Operation (Branch, t, f))
    -> begin match evaluate cond env with
         | (Operand (Bool true)), _  -> evaluate t env
         | (Operand (Bool false)), _ -> evaluate f env
         | _ -> Error.raise_simple "Condition must be a boolean value"
       end
  | Operation (If, _, _)
  | Operation (Branch, _, _)
    -> Error.raise_simple "Invalid if ... branch ... construction"

  (* Read a string *)

  | Operation (Read, Operand Stdin, Operand (Ident (i,_)))
    -> let s = input_line stdin in
       let new_env = Environment.add env i (Operand (String s)) in
       (Operand Void), new_env  
  | Operation (Read, _, _)
    -> Error.raise_simple "Read error"

  (* Reduction rules *)

  | Operation (opr, (Operation (o, x, y)), opd)
    -> let in_eval, _ = evaluate (Operation (o, x, y)) env in
       evaluate (Operation (opr, in_eval, opd)) env
  | Operation (opr, (Operand (Ident (i, p))), opd)
    -> let in_eval, _ = evaluate (Operand (Ident (i, p))) env in
       evaluate (Operation (opr, in_eval, opd)) env
  | Operation (opr, opd, (Operation (o, x, y)))
    -> let in_eval, _ = evaluate (Operation (o, x, y)) env in
       evaluate (Operation (opr, opd, in_eval)) env
  | Operation (opr, opd, Operand (Ident (i, p)))
    -> let in_eval, _ = evaluate (Operand (Ident (i, p))) env in
       evaluate (Operation (opr, opd, in_eval)) env

  (* Print a string *)

  | Operation (Print, Operand Stdout, v)
    -> print_operand v env; (Operand Void), env  
  | Operation (Print, _, _)
    -> Error.raise_simple "Print error"

  (* String concatenation *)

  | Operation (Add, Operand (String s1), Operand (String s2))
    -> (Operand (String (s1 ^ s2))), env
   
  (* Arithmetic operations *)

  | Operation (Add, Operand (Int x1), Operand (Int x2))
    -> (Operand (Int (x1 + x2))), env
  | Operation (Sub, Operand (Int x1), Operand (Int x2))
    -> (Operand (Int (x1 - x2))), env
  | Operation (Mul, Operand (Int x1), Operand (Int x2))
    -> (Operand (Int (x1 * x2))), env
  | Operation (Div, Operand (Int x1), Operand (Int x2))
    -> (Operand (Int (x1 / x2))), env
  | Operation (Add, _, _)
  | Operation (Sub, _, _)
  | Operation (Mul, _, _)
  | Operation (Div, _, _)
    -> Ast.print ast; Error.raise_simple "Arithmetic operations only accept integer values"

  (* Boolean operations *)

  | Operation (Or, Operand (Bool x), Operand (Bool y))
    -> (Operand (Bool (x || y))), env
  | Operation (And, Operand (Bool x), Operand (Bool y))
    -> (Operand (Bool (x && y))), env
  | Operation (Or, _, _)
  | Operation (And, _, _)
    -> Error.raise_simple "Boolean operations only accept boolean values"
  | Operation (Equal, Operand x, Operand y)
    -> (Operand (Bool (x = y))), env
  | Operation (Lesser, Operand (Int x), Operand (Int y))
    -> (Operand (Bool (x < y))), env
  | Operation (Greater, Operand (Int x), Operand (Int y))
    -> (Operand (Bool (x > y))), env
  | Operation (Lesser, _, _)
  | Operation (Greater, _, _)
    -> Error.raise_simple "Comparison operators can only be used between integer values"

  (* Type casting *)

  | Operation (Cast, Operand ToString, Operand (String s))
    -> (Operand (String s)), env
  | Operation (Cast, Operand ToString, Operand (Int i))
    -> (Operand (String (string_of_int i))), env
  | Operation (Cast, Operand ToString, Operand (Bool b))
    -> (Operand (String (string_of_bool b))), env
  | Operation (Cast, Operand ToInt, Operand (String s))
    -> begin
         try let i = int_of_string s in (Operand (Int i)), env
         with _ -> Error.raise_simple ("Cannot convert " ^ s ^ " to an int")
       end
  | Operation (Cast, Operand ToInt, Operand (Int i))
    -> (Operand (Int i)), env
  | Operation (Cast, Operand ToInt, Operand (Bool b))
    -> if b
         then (Operand (Int 1)), env
         else (Operand (Int 0)), env
  | Operation (Cast, Operand ToBool, Operand (String s))
    -> begin
         try let b = bool_of_string s in (Operand (Bool b)), env
         with _ -> Error.raise_simple ("Cannot convert " ^ s ^ " to a bool")
       end
  | Operation (Cast, Operand ToBool, Operand (Int i))
    -> begin match i with
         | 0 -> (Operand (Bool false)), env
         | 1 -> (Operand (Bool true)), env
         | _ -> Error.raise_simple ("Cannot convert " ^ (string_of_int i)
                                  ^ "to a bool")
       end
  | Operation (Cast, Operand ToBool, Operand (Bool b))
    -> (Operand (Bool b)), env
  | Operation (Cast, _, _)
    -> Error.raise_simple "Cast error"

(** Interprets the given [ast]. *)
let run ast =
  let env = Environment.create () in
  match evaluate ast env with
    | (Operand Void), _ -> ()
    | _ -> Error.raise_simple "The program should return void"
