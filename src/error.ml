open Lexing

type arbuste_exception =
  | Simple of string
  | Positioned of string * Lexing.position

exception ArbusteError of arbuste_exception

let raise_simple message = raise (ArbusteError (Simple message))

let raise_positioned s p =
  let l_pos = string_of_int p.pos_lnum in
  let c_pos = string_of_int (p.pos_bol + 1) in
  let message = "Error line " ^ l_pos ^ " character " ^ c_pos ^ ": " ^ s in
  raise (ArbusteError (Positioned (message, p)))

let print = function
  | Simple m -> Printf.eprintf "Error: %s\n" m
  | Positioned (m, p) -> Printf.eprintf "Error line %d character %d: %s\n"
                         p.pos_lnum (p.pos_bol + 1) m
