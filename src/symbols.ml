open Ast
open Ast.Operand

type symbol = Ast.t * Ast.Operand.t list

type t = (string * symbol) list

(** Creates a new environment. *)
let create () = []

(** Creates an environment with the given identifiers associated to the given
    values. *)
let make_env idents values =
  let f x y = match x with
    | Operand (Ident (i, _)) -> (i, (y, []))
    | _ -> Error.raise_simple "Bad arguments"
  in
  List.map2 f idents values
  

(** Tells whether [key] belongs to [env]. *)
let mem env key = List.mem_assoc key env 

(** Adds a functionction definition with its parameters to an environment. *)
let add_fun env key (value:Ast.t) params =
  if mem env key
    then
      let l = List.remove_assoc key env in
      (key,(value, params)) :: l
    else (key, (value, params)) :: env

(** Adds a variable to the environment. *)
let add env key (value:Ast.t) = add_fun env key value []

(** Finds a variable value in the environment. *)
let find env key =
  try fst (List.assoc key env)
  with Not_found -> Error.raise_simple ("Could not find identifier " ^ key)

(** Finds a function definition in the environment. *)
let find_func env key =
  try List.assoc key env
  with Not_found -> Error.raise_simple ("Could not find identifier " ^ key)
  

