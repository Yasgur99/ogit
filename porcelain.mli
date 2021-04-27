(** Representation of porcelain git commands.

    This module represents the calling of calling git commands that the
    user interface will find more useful than using the raw plumbing
    commands *)

(** The abstract type of a git commit object *)
type commit_t

type object_id = string

type object_content

type object_type

type status_t

(** [init d] initializes a git repository in the current working
    directory if [n] is [None], otherwise initializes a git repository
    with name [n] as a subdirectory in the current working directory *)
val init : string option -> unit

(** [pull] pulls files from repository *)
(* val pull : unit -> unit *)

(** [hash_object f] calls [Plumbing.hash_object] with command line
    argument -w, which stores data in file with filename [f] in a
    repository's .git/objects directory and is the object that refers to
    that data object *)
val hash_object : string -> object_id

(** [cat_file h] calls git cat-file on object [h] with option -p, which
    displays the content of an object in the repository *)
val cat_file : object_id -> object_content

(** [cat_file_type h] calls git cat-file on object [h] with option -t,
    which displays the type of the object if [o] = -t *)
val cat_file_type : object_id -> object_type

(** [update_index h f] adds the object [h] with filename [f] to the
    staging area *)
val update_index : object_id -> string -> unit

(** [write_tree] writes the staging area out to a tree object and
    genereates a tree object from the state of the index if the tree
    does not yet exist *)
val write_tree : unit -> object_id

(** [read_tree h] reads tree object [h] into the staging area *)
val read_tree : object_id -> unit

(** [read_tree h p] reads tree object [h] into the staging area with
    prefix [p] *)
val read_tree_prefix : object_id -> string -> unit

(** [commit_tree h m] creates and is a commit object from tree object
    [h] with commit message [m] *)
val commit_tree : object_id -> string -> object_id

(** [log h] is the list of commit objects that are reachable from HEAD
    in reverse chronological order if [h] is [None], otherwise the
    commit objects that are reachable by following parents of commit [h]
    in reverse chronological order *)
val log : object_id option -> commit_t list

(** [get_head] is the value of HEAD *)
val get_head : string

(** [string_of_commit c] is a commit in the form [hash msg] *)
val string_of_commit_t : commit_t -> string

(** [add fnames] adds the files with filenames [fnames] to the staging
    area *)
val add : string list -> unit

(** [restore_staged fnames] restores staged files [fnames] from the
    staging area *)
val restore_staged : string list -> unit

(** [commit msg] commits the changes in the staging area with commit
    message [msg] and returns any errors that were produced. *)
val commit : string -> string

(** [show] shows the staged, unstaged, and untracked files *)
val show : unit -> unit

(** [diff] shows the diffs of tracked files *)
val diff : unit -> string

(** [status] shows the status of the working tree *)
val status : unit -> status_t

(** [get_untracked s] is the untracked filenames in the status [s] *)
val get_untracked : status_t -> string list

(** [get_tracked s] is the untracked filenames in the status [s] *)
val get_tracked : status_t -> string list

(** [get_staged s] is the staged filenames in the status [s] *)
val get_staged : status_t -> string list
