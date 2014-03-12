open Lexing

type arbuste_exception =
  | Simple of string
  | Positioned of string * Lexing.position

exception ArbusteError of arbuste_exception

let raise_simple message = raise (ArbusteError (Simple message))

let raise_positioned s p =
  raise (ArbusteError (Positioned (s, p)))

let print = function
  | Simple m -> Printf.eprintf "Error: %s\n" m
  | Positioned (m, p) -> Printf.eprintf "Error line %d character %d: %s\n"
                         p.pos_lnum (p.pos_bol + 1) m

let warn_shadowed id p =
   Printf.eprintf "Warning line %d character %d: previous definition of %s is shadowed\n"
     p.pos_lnum (p.pos_bol + 1) id
