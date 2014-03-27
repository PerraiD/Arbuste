type arbuste_exception

exception ArbusteError of arbuste_exception

val error : string -> Lexing.position -> 'a

val print : arbuste_exception -> unit

val warn_shadowed : string -> Lexing.position -> unit

val warn : ('a, out_channel, unit) format -> 'a
