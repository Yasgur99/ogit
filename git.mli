(** Representation of git internals.

    This module represents the calling of git plumbing commands *)

(** The abstract type of a git commit object *)
type commit_object

type object_id = string

(** [init n] initializes a git repository in the current working directory if [n] is [None], otherwise initializes a git repository with name [n] as a subdirectory in the current working directory *)
val init : string option -> ()

(** [hash_object f] calls git hash-object with command line arguments [-w] which stores data in file with filename [f] in a repositorie's .git/objects directory and is the object that refers to that data object *)
val hash_object : string : -> commit_object

(** [cat_file h o] calls git cat-file with command line argumet [o] on object [h] which displays the content of an object in the repository if [o] = \"-p\" or the type of the object if [o] = \"-t"" *)
val cat_file : args list -> t

(** [update_index h f] adds the object [h] with filename [f] to the staging area *)
val update_index : object_id -> string -> t

(** [write_tree] writes the staging area out to a tree object and genereates a tree object from the state of the index if the tree does not yet exist *)
val write_tree : () -> t

(** [read_tree h p] reads tree object [h] into the staging area with prefix [p] *)
val read_tree : object_id -> string -> list -> t

(** [commit_tree h m] creates and is a commit object from tree object [h] with commit message [m] *)
val commit_tree : object_id -> string -> object_id

(** [log h] is the list of commit objects that are reachable by following parents of commit [h] in reverse chronological order *)
val log : object_id -> commit_object list
