open Lexing

(** Prints a lexing error for anunrecognized character. *)
let unrecognized_char position character =
    Printf.printf "Error line %d character %d: Unrecognized character '%c'\n"
                  position.pos_lnum (position.pos_bol + 1) character

(** Prints a parsing warning for an uninitialized variable. *)
let not_initialized variable position =
    let line_number = position.pos_lnum in
    Printf.printf "Warning line %d: variable '%s' has not been initialized\n"
                  line_number variable

(** Prints a parsing warning for an already initializied variable. *)
let already_initialized variable position =
    let line_number = position.pos_lnum in
    Printf.printf "Warning line %d: variable '%s' has already been initialized\n"
                  line_number variable
