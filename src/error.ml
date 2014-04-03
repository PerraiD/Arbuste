open Lexing

type arbuste_exception = string * Lexing.position

exception ArbusteError of arbuste_exception

let error message pos = raise (ArbusteError (message, pos))

let print (m,p) =
  Printf.eprintf "Error line %d character %d: %s\n" p.pos_lnum (p.pos_bol + 1) m

let warn_shadowed ~id ~pos =
   Printf.eprintf "Warning line %d character %d: previous definition of %s is shadowed\n"
     pos.pos_lnum (pos.pos_bol + 1) id

let warn s = Printf.eprintf s
