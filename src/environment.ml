open Ast
open Ast.Operand

type symbol = Ast.t * (Ast.t list)

type t = (string * symbol) list

let create () = []

let print env =
  let f = function
    | (k, (v, [])) -> Printf.printf "var: %s = %s\n" k (Ast.to_string v)
    | (k, (v, p)) -> Printf.printf "func: %s()\n" k in
  List.iter f env

let make_env ~keys ~values =
  let f x y = match x.data with
    | Operand (Ident i) -> (i, (y, []))
    | _ -> Error.error "Bad arguments" x.position
  in
  List.map2 f keys values
 
let mem ~env ~key = List.mem_assoc key env 

let add_fun ~env ~key ~value ~params =
  if mem env key
    then
      let l = List.remove_assoc key env in
      (key,(value, params)) :: l
    else (key, (value, params)) :: env

let add ~env ~key ~value = add_fun env key value []

let find ~env ~key =
  try Some (fst (List.assoc key env))
  with Not_found -> None

let find_func ~env ~key =
  try Some (List.assoc key env)
  with Not_found -> None
