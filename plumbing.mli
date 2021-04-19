(** Representation of raw git commands.

    This module represents the calling of calling git commands with any
    command line arguments *)

(** The representation of the result of executing a git command *)
type result

(* [make result o e] is a result where [o] is the lines of stdout and
   [e] is the lines of stderr *)
val make_result : string list -> string list -> string list -> result

(* [get_stdout r] is the lines of stdout *)
val get_stdout : result -> string list

(* [get_stdout r] is the lines of stderr *)
val get_stderr : result -> string list

(* [get_out r] is the lines of of both stdout and stderr in the order
   that they were sent to their respective streams.

   For example, if data was written to stdout, then stderr, and then
   stdout, again, then [get_out r] follows that same order. *)
val get_out : result -> string list

(** [init args] calls git init with arguments [args] *)
val init : string array -> result

(** [hash_object args] calls git hash-object with arguments [args] and
    is the output to standard output *)
val hash_object : string array -> result

(** [cat_file args] calls git cat-file with arguments [args] *)
val cat_file : string array -> result

(** [update_index args] calls git update-index with arguments [args] *)
val update_index : string array -> result

(** [write_tree args] calls git write-tree with arguments [args] *)
val write_tree : string array -> result

(** [read_tree args] calls git read-tree with arguments [args] *)
val read_tree : string array -> result

(** [commit_tree args] calls git commit-tree with arguments [args] *)
val commit_tree : string array -> result

(** [log args] calls git update-index with arguments [args] *)
val log : string array -> result

(** [add args] calls git add with arguments [args] *)
val add : string array -> result

(** [restore args] calls git restore with arguments [args] *)
val restore: string array -> result

(** [commit] calls git commit with arguments [args] *)
val commit : string array -> result

(** [show args] calls git show with arguments [args] *)
val show : string array -> result

(** [diff args] calls git diff with arguments [args] *)
val diff : string array -> result

(** [status args] calls git status with arguments [args] *)
val status : string array -> result

(** [git args] calls git with arguments [args] *)
val git : string array -> result
