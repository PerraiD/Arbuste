type t

(** Creates a new environment. *)
val create : unit -> t

(** Prints an environment. *)
val print : t -> unit

(** Creates an environment with the given identifiers associated to the given
    values. *)
val make_env : keys:Ast.t list -> values:Ast.t list -> t
 
(** Tells whether key belongs to environment. *)
val mem : env:t -> key:string -> bool 

(** Adds a functionction definition with its parameters to an environment. *)
val add_fun : env:t -> key:string -> value:Ast.t -> params:Ast.t list -> t

(** Adds a variable to the environment. *)
val add : env:t -> key:string -> value:Ast.t -> t

(** Finds a variable value in the environment. *)
val find : env:t -> key:string -> Ast.t option

(** Finds a function definition in the environment. *)
val find_func : env:t -> key:string -> (Ast.t * Ast.t list) option
