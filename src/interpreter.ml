open Ast
open Ast.Operator
open Ast.Operand

let rec get_params_idents (ast:Ast.t) = match ast.contents with
  | Operand EndParam -> []
  | Operation (Param, opd, next)
    -> begin match opd.contents with
         | (Operand _)  -> opd :: (get_params_idents next)
         | _ -> Error.error "Param definition error" opd.position
       end
  | _ -> Error.error "Param definition error" ast.position

let rec get_params_values plist env = match plist.contents with
  | Operand EndParam -> []
  | Operation (Param, {contents = Operand (Ident i); position = p}, next)
    -> begin match Environment.find env i with
         | Some opd -> opd :: (get_params_values next env)
         | None -> Error.error ("Could not find identifier " ^ i) p
       end
  | Operation (Param, {contents = Operand opd; position = p}, next)
    -> {contents = (Operand opd); position = p}::(get_params_values next env)
  | _ -> Error.error "Param definition error" plist.position

let warn_on_ignore ast = match ast.contents with
  | Operand Void -> ()
  | _ -> Error.warn "Ignored value"

(** Interprets the given [ast] with the given [env]ironment. *)
let rec evaluate (ast:Ast.t) (env:Environment.t) = match ast.contents with
  | Operand (Ident i)
    -> begin match Environment.find env i with
         | Some opd -> opd, env
         | None -> Error.error ("Could not find identifier " ^ i) ast.position
       end
  | Operand _ as leaf
    -> {ast with contents = leaf}, env

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

  | Operation (Let, {contents = Operand (Ident id); position = pos}, {contents = Operation (Func, p, f)})
    -> if Environment.mem env id then Error.warn_shadowed id pos;
       let params = get_params_idents p in
       {ast with contents = Operand Void}, (Environment.add_fun env id f params)
  | Operation (Func, _, _)
  | Operation (Param, _, _)
    -> Error.error "Invalid func ... param ... construction" ast.position

  (* Variable assignement *)

  | Operation (Let, {contents = Operand (Ident id); position = p}, opn)
    -> if Environment.mem env id then Error.warn_shadowed id p;
       let (eval, _) = evaluate opn env in
       {ast with contents = Operand Void}, (Environment.add env id eval)     
  | Operation (Let, _, _)
    -> Error.error "Invalid in ... let ... construction" ast.position

  (* Function evaluation *)

  | Operation (Eval, {contents = Operand (Ident i); position = pos}, params)
    -> if Environment.mem env i
         then
           match Environment.find_func env i with
             | None -> Error.error ("Could not find identifier " ^ i) pos
             | Some (f, idents) ->
                 let p =  get_params_values params env in
                 let func_env = Environment.make_env idents p in
                 evaluate f (Environment.add_fun func_env i f idents)
         else Error.error ("Unknown function " ^ i) pos
  | Operation (Eval, _, _)
    -> Error.error "Invalid eval ... param ... construction" ast.position

  (* Branching *)

  | Operation (If, cond, {contents = Operation (Branch, t, f)})
    -> let eval, _ = evaluate cond env in
       begin match eval.contents with
         | Operand (Bool true)  -> evaluate t env
         | Operand (Bool false) -> evaluate f env
         | _ -> Error.error "Condition must be a boolean value" cond.position
       end
  | Operation (If, _, _)
  | Operation (Branch, _, _)
    -> Error.error "Invalid if ... branch ... construction" ast.position

  (* Read a string *)

  | Operation (Read, {contents = Operand Stdin}, {contents = Operand (Ident i); position = p})
    -> let s = input_line stdin in
       let new_env = Environment.add env i {contents = Operand (String s); position = p} in
       {ast with contents = Operand Void}, new_env  
  | Operation (Read, _, _)
    -> Error.error "Read error" ast.position

  (* Reduction rules *)

  | Operation (opr, {contents = Operation (o, x, y); position = p}, opd)
    -> let in_eval, _ = evaluate {contents = Operation (o, x, y); position = p} env in
       evaluate {ast with contents = Operation (opr, in_eval, opd)} env
  | Operation (opr, {contents = Operand (Ident i); position = p}, opd)
    -> let in_eval, _ = evaluate {contents = Operand (Ident i); position = p} env in
       evaluate {ast with contents = Operation (opr, in_eval, opd)} env
  | Operation (opr, opd, {contents = Operation (o, x, y); position = p})
    -> let in_eval, _ = evaluate {contents = Operation (o, x, y); position = p} env in
       evaluate { ast with contents = Operation (opr, opd, in_eval)} env
  | Operation (opr, opd, {contents = Operand (Ident i); position = p})
    -> let in_eval, _ = evaluate {contents = Operand (Ident i); position = p} env in
       evaluate {ast with contents = Operation (opr, opd, in_eval)} env

  (* Print a string *)

  | Operation (Print, {contents = Operand Stdout}, {contents = Operand (String s)})
    -> print_string s; {ast with contents = Operand Void}, env  
  | Operation (Print, _, _)
    -> Error.error "Print error" ast.position

  (* String concatenation *)

  | Operation (Add, {contents = Operand (String s1)}, {contents = Operand (String s2)})
    -> {ast with contents = Operand (String (s1 ^ s2))}, env
   
  (* Arithmetic operations *)

  | Operation (Add, {contents = Operand (Int x1)}, {contents = Operand (Int x2)})
    -> {ast with contents = Operand (Int (x1 + x2))}, env
  | Operation (Sub, {contents = Operand (Int x1)}, {contents = Operand (Int x2)})
    -> {ast with contents = Operand (Int (x1 - x2))}, env
  | Operation (Mul, {contents = Operand (Int x1)}, {contents = Operand (Int x2)})
    -> {ast with contents = Operand (Int (x1 * x2))}, env
  | Operation (Div, {contents = Operand (Int x1)}, {contents = Operand (Int x2)})
    -> {ast with contents = Operand (Int (x1 / x2))}, env
  | Operation (Add, _, _)
  | Operation (Sub, _, _)
  | Operation (Mul, _, _)
  | Operation (Div, _, _)
    -> Error.error "Arithmetic operations only accept integer values" ast.position

  (* Boolean operations *)

  | Operation (Or, {contents = Operand (Bool x)}, {contents = Operand (Bool y)})
    -> {ast with contents = Operand (Bool (x || y))}, env
  | Operation (And, {contents = Operand (Bool x)}, {contents = Operand (Bool y)})
    -> {ast with contents = Operand (Bool (x && y))}, env
  | Operation (Or, _, _)
  | Operation (And, _, _)
    -> Error.error "Boolean operations only accept boolean values" ast.position
  | Operation (Equal, {contents = Operand x}, {contents = Operand y})
    -> {ast with contents = Operand (Bool (x = y))}, env
  | Operation (Lesser, {contents = Operand (Int x)}, {contents = Operand (Int y)})
    -> {ast with contents = Operand (Bool (x < y))}, env
  | Operation (Greater, {contents = Operand (Int x)}, {contents = Operand (Int y)})
    -> {ast with contents = Operand (Bool (x > y))}, env
  | Operation (Lesser, _, _)
  | Operation (Greater, _, _)
    -> Error.error "Comparison operators can only be used between integer values" ast.position

  (* Type casting *)

  | Operation (Cast, {contents = Operand ToString}, {contents = Operand (String s)})
    -> {ast with contents = Operand (String s)}, env
  | Operation (Cast, {contents = Operand ToString}, {contents = Operand (Int i)})
    -> {ast with contents = Operand (String (string_of_int i))}, env
  | Operation (Cast, {contents = Operand ToString}, {contents = Operand (Bool b)})
    -> {ast with contents = Operand (String (string_of_bool b))}, env
  | Operation (Cast, {contents = Operand ToInt}, {contents = Operand (String s)})
    -> begin
         try let i = int_of_string s in {ast with contents = Operand (Int i)}, env
         with _ -> Error.error ("Cannot convert " ^ s ^ " to an int") ast.position
       end
  | Operation (Cast, {contents = Operand ToInt}, {contents = Operand (Int i)})
    -> {ast with contents = Operand (Int i)}, env
  | Operation (Cast, {contents = Operand ToInt}, {contents = Operand (Bool b)})
    -> if b
         then {ast with contents = Operand (Int 1)}, env
         else {ast with contents = Operand (Int 0)}, env
  | Operation (Cast, {contents = Operand ToBool}, {contents = Operand (String s)})
    -> begin
         try let b = bool_of_string s in {ast with contents = Operand (Bool b)}, env
         with _ -> Error.error ("Cannot convert " ^ s ^ " to a bool") ast.position
       end
  | Operation (Cast, {contents = Operand ToBool}, {contents = Operand (Int i)})
    -> begin match i with
         | 0 -> {ast with contents = Operand (Bool false)}, env
         | 1 -> {ast with contents = Operand (Bool true)}, env
         | _ -> Error.error ("Cannot convert " ^ (string_of_int i)
                                  ^ "to a bool") ast.position
       end
  | Operation (Cast, {contents = Operand ToBool}, {contents = Operand (Bool b)})
    -> {ast with contents = Operand (Bool b)}, env
  | Operation (Cast, _, _)
    -> Error.error "Cast error" ast.position

(** Interprets the given [ast]. *)
let run ast =
  let env = Environment.create () in
  let eval, _ = evaluate ast env in
  match eval.contents with
    | Operand Void -> ()
    | _ -> Error.warn "The program should return void"
