(** Parsing and execution of user commands. *)

(** The type [file_name] represents the file name that can be part of a
    user command. *)
type file_name = string

(** The type [commit_msg] represents the commit message that can be part
    of a user command. *)
type commit_msg = string

(** The type [branch_name] represents the branch name that can be part
    of a user command. *)
type branch_name = string

(** The type that represents a key. The interpretation of a key is
    equivalent to the definition's in [Curses.Key]. [Curses.Key] does
    not expose a type key, and is instead implemented using an integer
    and some functions that map the name of a key to an integer. *)
type key = int

(** The type [t] represents a git command that is decomposed into a
    verb and possibly a file_name, commit_msg, or branch_name. *)
type t =
  | Add of file_name
  | Remove of file_name
  | Commit of commit_msg
  | Branch of branch_name
  | Checkout of branch_name
  | Fetch
  | Push
  | Pull
  | Status
  | Init
  | Quit

(** Raised when an invalid command is parsed. *)
exception Invalid_cmd of string

(** Raised when an empty command is parsed. *)
exception Empty_cmd of string

(** Raised when program should be terminated *)
exception Program_terminate

(** [check_err err] is unit if [err] is [true]. Raises
    [Command.Program_terminate] if [err] is [false].*)
val check_err : Curses.err -> unit

(** [parse_key key] parses a player's keystroke input into a [cmd] *)
val parse_key : key -> t 

(** [string_of_cmd cmd] is the string representation of [cmd] *)
val string_of_cmd : t -> string
