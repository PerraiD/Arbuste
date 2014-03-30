open Ast
open Ast.Operator
open Ast.Operand

let rec get_params ast = match ast.data with
  | Operand EndParam -> []
  | Operation (Param, opd, next)
    -> begin match opd.data with
         | (Operand _)  -> opd :: (get_params next)
         | _ -> Error.error "Param definition error" opd.position
       end
  | _ -> Error.error "Param definition error" ast.position

let rec get_args plist env = match plist.data with
  | Operand EndParam -> []
  | Operation (Param, {data = Operand (Ident i); position}, next)
    -> begin match Environment.find env i with
         | Some opd -> opd :: (get_args next env)
         | None -> Error.error ("Could not find identifier " ^ i) position
       end
  | Operation (Param, {data = Operand opd; position}, next)
    -> {data = (Operand opd); position}::(get_args next env)
  | _ -> Error.error "Param definition error" plist.position

let warn_on_ignore ast = match ast.data with
  | Operand Void -> ()
  | _ -> Error.warn "Ignored value"

(** Interprets the given [ast] with the given [env]ironment. *)
let rec evaluate ast env = match ast.data with
  | Operand (Ident id)
    -> begin match Environment.find env id with
         | Some opd -> opd, env
         | None -> Error.error ("Could not find identifier " ^ id) ast.position
       end
  | Operand _  -> ast, env

  (* Sequences *)

  | Operation (In, left, right)
    -> let eval, left_env = evaluate left env in
       warn_on_ignore eval;
       evaluate right left_env
  | Operation (Seq, left, right)
    -> let eval, _ = evaluate left env in
       warn_on_ignore eval;
       evaluate right env

  (* Function declaration *)

  | Operation (Let, {data = Operand (Ident id); position}, {data = Operation (Func, params, f)})
    -> if Environment.mem env id then Error.warn_shadowed id position;
       let param_list = get_params params in
       let new_env = Environment.add_fun env id f param_list in
       {ast with data = Operand Void}, new_env
  | Operation (Func, _, _)
  | Operation (Param, _, _)
    -> Error.error "Invalid func ... param ... construction" ast.position

  (* Variable assignement *)

  | Operation (Let, {data = Operand (Ident id); position}, opn)
    -> if Environment.mem env id then Error.warn_shadowed id position;
       let (eval, _) = evaluate opn env in
       {ast with data = Operand Void}, (Environment.add env id eval)     
  | Operation (Let, _, _)
    -> Error.error "Invalid in ... let ... construction" ast.position

  (* Function evaluation *)

  | Operation (Eval, {data = Operand (Ident id); position}, params)
    -> if Environment.mem env id
         then
           match Environment.find_func env id with
             | None -> Error.error ("Could not find identifier " ^ id) position
             | Some (f, idents) ->
                 let args =  get_args params env in
                 let func_env = Environment.make_env idents args in
                 evaluate f (Environment.add_fun func_env id f idents)
         else Error.error ("Unknown function " ^ id) position
  | Operation (Eval, _, _)
    -> Error.error "Invalid eval ... param ... construction" ast.position

  (* Branching *)

  | Operation (If, cond, {data = Operation (Branch, t, f)})
    -> let eval, _ = evaluate cond env in
       begin match eval.data with
         | Operand (Bool true)  -> evaluate t env
         | Operand (Bool false) -> evaluate f env
         | _ -> Error.error "Condition must be a boolean value" cond.position
       end
  | Operation (If, _, _)
  | Operation (Branch, _, _)
    -> Error.error "Invalid if ... branch ... construction" ast.position

  (* Read a string *)

  | Operation (Read, {data = Operand Stdin}, {data = Operand (Ident id); position})
    -> let s = input_line stdin in
       let new_env = Environment.add env id {data = Operand (String s); position} in
       {ast with data = Operand Void}, new_env  
  | Operation (Read, _, _)
    -> Error.error "Read error" ast.position

  (* Reduction rules *)

  | Operation (opr, ({data = Operation _} as left), right)
    -> let in_eval, _ = evaluate left env in
       evaluate {ast with data = Operation (opr, in_eval, right)} env
  | Operation (opr, ({data = Operand (Ident _)} as left), right)
    -> let in_eval, _ = evaluate left env in
       evaluate {ast with data = Operation (opr, in_eval, right)} env
  | Operation (opr, left, ({data = Operation _} as right))
    -> let in_eval, _ = evaluate right env in
       evaluate {ast with data = Operation (opr, left, in_eval)} env
  | Operation (opr, left, ({data = Operand (Ident _)} as right))
    -> let in_eval, _ = evaluate right env in
       evaluate {ast with data = Operation (opr, left, in_eval)} env

  (* Print a string *)

  | Operation (Print, {data = Operand Stdout}, {data = Operand (String s)})
    -> print_string s; {ast with data = Operand Void}, env  
  | Operation (Print, _, _)
    -> Error.error "Print error" ast.position

  (* String concatenation *)

  | Operation (Add, {data = Operand (String s1)}, {data = Operand (String s2)})
    -> {ast with data = Operand (String (s1 ^ s2))}, env
   
  (* Arithmetic operations *)

  | Operation (Add, {data = Operand (Int x1)}, {data = Operand (Int x2)})
    -> {ast with data = Operand (Int (x1 + x2))}, env
  | Operation (Sub, {data = Operand (Int x1)}, {data = Operand (Int x2)})
    -> {ast with data = Operand (Int (x1 - x2))}, env
  | Operation (Mul, {data = Operand (Int x1)}, {data = Operand (Int x2)})
    -> {ast with data = Operand (Int (x1 * x2))}, env
  | Operation (Div, {data = Operand (Int x1)}, {data = Operand (Int x2)})
    -> {ast with data = Operand (Int (x1 / x2))}, env
  | Operation (Add, _, _)
  | Operation (Sub, _, _)
  | Operation (Mul, _, _)
  | Operation (Div, _, _)
    -> Error.error "Arithmetic operations only accept integer values" ast.position

  (* Boolean operations *)

  | Operation (Or, {data = Operand (Bool x)}, {data = Operand (Bool y)})
    -> {ast with data = Operand (Bool (x || y))}, env
  | Operation (And, {data = Operand (Bool x)}, {data = Operand (Bool y)})
    -> {ast with data = Operand (Bool (x && y))}, env
  | Operation (Or, _, _)
  | Operation (And, _, _)
    -> Error.error "Boolean operations only accept boolean values" ast.position
  | Operation (Equal, {data = Operand x}, {data = Operand y})
    -> {ast with data = Operand (Bool (x = y))}, env
  | Operation (Lesser, {data = Operand (Int x)}, {data = Operand (Int y)})
    -> {ast with data = Operand (Bool (x < y))}, env
  | Operation (Greater, {data = Operand (Int x)}, {data = Operand (Int y)})
    -> {ast with data = Operand (Bool (x > y))}, env
  | Operation (Lesser, _, _)
  | Operation (Greater, _, _)
    -> Error.error "Comparison operators can only be used between integer values" ast.position

  (* Type casting *)

  | Operation (Cast, {data = Operand ToString}, {data = Operand (String s)})
    -> {ast with data = Operand (String s)}, env
  | Operation (Cast, {data = Operand ToString}, {data = Operand (Int i)})
    -> {ast with data = Operand (String (string_of_int i))}, env
  | Operation (Cast, {data = Operand ToString}, {data = Operand (Bool b)})
    -> {ast with data = Operand (String (string_of_bool b))}, env
  | Operation (Cast, {data = Operand ToInt}, {data = Operand (String s)})
    -> begin
         try let i = int_of_string s in {ast with data = Operand (Int i)}, env
         with _ -> Error.error ("Cannot convert " ^ s ^ " to an int") ast.position
       end
  | Operation (Cast, {data = Operand ToInt}, {data = Operand (Int i)})
    -> {ast with data = Operand (Int i)}, env
  | Operation (Cast, {data = Operand ToInt}, {data = Operand (Bool b)})
    -> if b
         then {ast with data = Operand (Int 1)}, env
         else {ast with data = Operand (Int 0)}, env
  | Operation (Cast, {data = Operand ToBool}, {data = Operand (String s)})
    -> begin
         try let b = bool_of_string s in {ast with data = Operand (Bool b)}, env
         with _ -> Error.error ("Cannot convert " ^ s ^ " to a bool") ast.position
       end
  | Operation (Cast, {data = Operand ToBool}, {data = Operand (Int i)})
    -> begin match i with
         | 0 -> {ast with data = Operand (Bool false)}, env
         | 1 -> {ast with data = Operand (Bool true)}, env
         | _ -> Error.error ("Cannot convert " ^ (string_of_int i)
                                  ^ "to a bool") ast.position
       end
  | Operation (Cast, {data = Operand ToBool}, {data = Operand (Bool b)})
    -> {ast with data = Operand (Bool b)}, env
  | Operation (Cast, _, _)
    -> Error.error "Cast error" ast.position

(** Interprets the given [ast]. *)
let run ast =
  let env = Environment.create () in
  let eval, _ = evaluate ast env in
  match eval.data with
    | Operand Void -> ()
    | _ -> Error.warn "The program should return void"
