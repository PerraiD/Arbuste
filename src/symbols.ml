open Ast

type t = (string * Ast.t) list

let create () = []

let mem env key = List.mem_assoc key env 

let add env key value =
  if mem env key
    then env
    else (key, value) :: env

let find env key = List.assoc key env
  

