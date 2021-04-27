(** [init ()] is a curses window. It's side effects include
    initialization of colors, enabling cbreak, enabling noecho, disables
    the curser, and clearning the window *)
val init : unit -> Curses.window

(** [cleanup ()] ends the window and cleans up the side effects created
    by [init ()]*)
val cleanup : unit -> unit

val render : State.t -> Curses.window -> unit

val render_commit_mode : State.t -> Curses.window -> string

val render_diff_mode : State.t -> Curses.window -> unit

val get_color : string -> int
