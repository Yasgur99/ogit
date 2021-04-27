(** The abstract type of values representing the git state *)
type t

(** The variant type representing the differences in the VIM when
    various commands are called *)
type render_mode =
  | Normal
  | CommitMode
  | CommitDone of string
  | DiffMode of string

(** The representation type that specifies what [color] [text] should be
    printed as. *)
type printable = {
  text : string;
  color : string;
}

(** [init_state dir] is the state of the current working tree belonging
    to the repo rooted at [dir]. *)
val init_state : string -> t

(** [commit_history st] is the commit history of the current state [st] *)
val commit_history : t -> Porcelain.commit_t list

(** [head st] is current HEAD *)
val head : t -> string

(** [merge st] is the commit pointed to by the upstream branch of the
    current branch *)
val merge : t -> Porcelain.commit_t option

(** [push st] is the branch that the current branch is being pushed to *)
val push : t -> Porcelain.commit_t option

(** [exec st c] is the state after executing command [c] from state
    [st]. *)
val exec : t -> Command.t -> t

(** [printable_of_state st] is a printable represnation of the state *)
val printable_of_state : t -> printable list

(** [get_curs st] is the y index of the position into state. *)
val get_curs : t -> int

(** [set_curs st y] is the state whose cursor is [y]. The rest of the
    state says the same. *)
val set_curs : t -> int -> t

(** [get_mode st] is the rendering mode for [st]. *)
val get_mode : t -> render_mode

(** [set_mode st new_mode] manually changes the mode of [st] to
    [new_mode]. *)
val set_mode : t -> render_mode -> t

(** [update_mode st cmd] automatically changes the mode of [st] to the
    appropriate mode based on the command [cmd]. The rest of the state
    stays the same. *)
val update_mode : t -> Command.t -> t
