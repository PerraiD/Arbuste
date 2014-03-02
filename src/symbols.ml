open Ast

type t = (string * Ast.t) list

let create () = []

(** Tells whether [key] belongs to [env]. *)
let mem env key = List.mem_assoc key env 

let add env key value =
  if mem env key
    then
      let l = List.remove_assoc key env in
      (key,value) :: l
    else (key, value) :: env

let find env key = List.assoc key env
  

