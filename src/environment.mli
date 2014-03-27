type t

(** Creates a new environment. *)
val create : unit -> t

(** Prints an [env]ironment. *)
val print : t -> unit

(** Creates an environment with the given identifiers associated to the given
    values. *)
val make_env : Ast.t list -> Ast.t list -> t
 
(** Tells whether [key] belongs to [env]. *)
val mem : t -> string -> bool 

(** Adds a functionction definition with its parameters to an environment. *)
val add_fun : t -> string -> Ast.t -> Ast.t list -> t

(** Adds a variable to the environment. *)
val add : t -> string -> Ast.t -> t

(** Finds a variable value in the environment. *)
val find : t -> string -> Ast.t option

(** Finds a function definition in the environment. *)
val find_func : t -> string -> (Ast.t * Ast.t list) option
