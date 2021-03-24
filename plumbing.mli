(** Representation of raw git commands.

    This module represents the calling of calling git commands with
    any command line arguments *)

(** [init args] calls git init with arguments [args] *)
val init : string array -> unit

(** [hash_object args] calls git hash-object with arguments [args] and is the output to standard output *)
val hash_object : string array -> string

(** [cat_file args] calls git cat-file with arguments [args] *)
val cat_file : string array -> unit

(** [update_index args] calls git update-index with arguments [args] *)
val update_index  : string array ->  unit

(** [write_tree args] calls git write-tree with arguments [args] *)
val write_tree : string array -> unit

(** [read_tree args] calls git read-tree  with arguments [args] *)
val read_tree : string array -> unit

(** [commit_tree args] calls git commit-tree with arguments [args] *)
val commit_tree : string array -> unit

(** [log args] calls git update-index with arguments [args] *)
val log : string array -> unit 

(** [add args] calls git add with arguments [args] *)
val add : string array -> unit

(** [commit] calls git commit with arguments [args] *)
val commit : string array -> unit

(** [show args] calls git show with arguments [args] *)
val show : string array  -> unit

(** [diff args] calls git diff with arguments [args] *)
val diff : string array -> unit

(** [status args] calls git status with arguments [args] *)
val status: string array -> unit
