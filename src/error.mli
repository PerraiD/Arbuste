type arbuste_exception

exception ArbusteError of arbuste_exception

(** Raises an error with the given message and position. *)
val error : string -> Lexing.position -> 'a

(** Prints the given [arbuste_exception]. *)
val print : arbuste_exception -> unit

(** Warns that an identifier is shadowed. *)
val warn_shadowed : id:string -> pos:Lexing.position -> unit

(** Prints a warning. *)
val warn : ('a, out_channel, unit) format -> 'a
