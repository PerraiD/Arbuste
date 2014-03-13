open Ast
open Ast.Operand

type symbol = Ast.t * Ast.Operand.t list

type t = (string * symbol) list

let create () = []

let make_env idents values =
  let f x y = match x with
    | Operand (Ident (i, _)) -> (i, (y, []))
    | _ -> Error.raise_simple "Bad arguments"
  in
  List.map2 f idents values
  

(** Tells whether [key] belongs to [env]. *)
let mem env key = List.mem_assoc key env 

let add_fun env key (value:Ast.t) params =
  if mem env key
    then
      let l = List.remove_assoc key env in
      (key,(value, params)) :: l
    else (key, (value, params)) :: env

let add env key (value:Ast.t) = add_fun env key value []

let find env key =
  try fst (List.assoc key env)
  with Not_found -> Error.raise_simple ("Could not find identifier " ^ key)

let find_func env key =
  try List.assoc key env
  with Not_found -> Error.raise_simple ("Could not find identifier " ^ key)
  

