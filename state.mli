
(** The abstract type of values representing the git state *)
type t

(** [init_state dir] is the state of the current working tree belonging to the
    repo rooted at [dir]. *)
val init_state : string -> t

(** [commit_history st] is the commit history of the current state [st] *)
val commit_history : t -> Porcelain.commit_t list

(** [head st] is the commit pointed to by the head in the current state [st] *)
val head : t -> Porcelain.commit_t

(** [merge st] is the commit pointed to by the upstream branch of the 
    current branch *) 
val merge : t -> Porcelain.commit_t option

(** [push st] is the branch that the current branch is being pushed to *)
val push : t -> Porcelain.commit_t option

(** [exec st c] is the state after executing command [c] from state [st]. *)
val exec : t -> Command.t -> t

(** TODO: magit also has support for stashed changes *)


