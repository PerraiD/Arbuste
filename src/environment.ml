open Ast
open Ast.Operand

type symbol = Ast.t * (Ast.t list)

type t = (string * symbol) list

(** Creates a new environment. *)
let create () = []

(** Prints an [env]ironment. *)
let print env =
  let f = function
    | (k, (v, [])) -> Printf.printf "var: %s = %s\n" k (Ast.to_string v)
    | (k, (v, p)) -> Printf.printf "func: %s()\n" k in
  List.iter f env

(** Creates an environment with the given identifiers associated to the given
    values. *)
let make_env idents values =
  let f x y = match x.data with
    | Operand (Ident i) -> (i, (y, []))
    | _ -> Error.error "Bad arguments" x.position
  in
  List.map2 f idents values
 
(** Tells whether [key] belongs to [env]. *)
let mem (env:t) (key:string) = List.mem_assoc key env 

(** Adds a functionction definition with its parameters to an environment. *)
let add_fun (env:t) key (value:Ast.t) (params:Ast.t list) =
  if mem env key
    then
      let l = List.remove_assoc key env in
      (key,(value, params)) :: l
    else (key, (value, params)) :: env

(** Adds a variable to the environment. *)
let add env key (value:Ast.t) = add_fun env key value []

(** Finds a variable value in the environment. *)
let find env key =
  try Some (fst (List.assoc key env))
  with Not_found -> None

(** Finds a function definition in the environment. *)
let find_func env key =
  try Some (List.assoc key env)
  with Not_found -> None
