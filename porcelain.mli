(** Representation of porcelain git commands.

    This module represents the calling of calling git commands that the
    user interface will find more useful than using the raw plumbing
    commands *)

(** The abstract type of a git commit object *)
type commit_object

type object_id = string

type object_content

type object_type

(** [init n] initializes a git repository in the current working directory if [n] is [None], otherwise initializes a git repository with name [n] as a subdirectory in the current working directory *)
val init : string option -> unit

(** [hash_object f] calls [Plumbing.hash_object] with command line argument -w,
    which stores data in file with filename [f] in a repository's .git/objects
    directory and is the object that refers to that data object *)
val hash_object : string -> object_id

(** [cat_file h] calls git cat-file on object [h] with option -p, which displays the content of an object in the repository  *)
val cat_file : object_id -> object_content

(** [cat_file_type h] calls git cat-file on object [h] with option -t, which displays the type of the object if [o] = -t *)
val cat_file_type : object_id -> object_type

(** [update_index h f] adds the object [h] with filename [f] to the staging area *)
val update_index : object_id -> string -> unit

(** [write_tree] writes the staging area out to a tree object and genereates a tree object from the state of the index if the tree does not yet exist *)
val write_tree : unit -> object_id 

(** [read_tree h] reads tree object [h] into the staging area *)
val read_tree : object_id -> unit

(** [read_tree h p] reads tree object [h] into the staging area with prefix [p] *)
val read_tree_prefix : object_id -> string -> unit

(** [commit_tree h m] creates and is a commit object from tree object [h] with commit message [m] *)
val commit_tree : object_id -> string -> object_id

(** [log] is the list of commit objects that are reachable from HEAD in reverse chronological order *)
val log : unit -> commit_object list

(** [log h] is the list of commit objects that are reachable by following parents of commit [h] in reverse chronological order *)
val log : object_id -> commit_object list
