open Plumbing

module type Porcelain = sig
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

  (** [hash_object f] calls [Plumbing.hash_object] with command line
      argument -w, which stores data in file with filename [f] in a repository's .git/objects directory and is the object that refers to
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

  (** [string_of_commit c] is a commit in the form [hash msg] *)
  val string_of_commit_t : commit_t -> string

  (** [add fnames] adds the files with filenames [fnames] to the staging
      area *)
  val add : string list -> unit

  (** [restore_staged fnames] restores staged files [fnames] from the staging
      area *)
  val restore_staged : string list -> unit

  (** [commit msg] commits the changes in the staging area with commit
      message [msg] *)
  val commit : string -> unit

  (** [show] shows the staged, unstaged, and untracked files *)
  val show : unit -> unit

  (** [diff] shows the diffs of tracked files *)
  val diff : unit -> unit

  (** [status] shows the status of the working tree *)
  val status : unit -> status_t

  (** [get_untracked s] is the untracked filenames in the status [s] *)
  val get_untracked : status_t -> string list

  (** [get_tracked s] is the untracked filenames in the status [s] *)
  val get_tracked : status_t -> string list

  (** [get_staged s] is the staged filenames in the status [s] *)
  val get_staged : status_t -> string list
end

module PorcelainImpl (P : Plumbing) = struct

  type commit_t = {
    tree : string;
    (*parents : string;*)
    (*author : string;*)
    (*committer : string;*)
    msg : string;
  }

  type object_id = string

  type object_content = unit

  type object_type =
    | Blob of { contents : string }
    | Tree of {
        entry : int;
        ob_type : object_type;
        sha1 : string;
        name : string;
      }
    | Commit of { com_ob : commit_t }
    | Tag of {
        obj_name : string;
        (*?*)
        ob_type : object_type;
        tagger : string;
        msg : string;
      }

  type status_t = {
    untracked : string list;
    tracked : string list;
    staged : string list;
  }

  let init (dir : string option) : unit = failwith "Unimplemented"

  (* match dir with | None -> Plumbing.init [||] | Some _ -> Plumbing.init
     [| dir |] *)

  let hash_object file : object_id = failwith "unimplemented"

  (* Plumbing.hash_object [| "-w"; file |] *)

  let cat_file hash = failwith "unimplemented"

  (* Plumbing.cat_file [| "-p"; hash |]*)

  let cat_file_type hash = failwith "unimplemented"

  (* Plumbing.cat_file [|"-t"; hash|] *)

  let update_index hash file = failwith "unimplemented"

  (* Plumbing.update_index [| hash; file|] (* not sure of this one *) *)

  let write_tree () = failwith "Unimplemented"

  (* Plumbing.write_tree [||] *)

  let read_tree hash = failwith "unimplemented"

  (* Plumbing.read_tree [| hash |] *)

  let read_tree_prefix hash prefix = failwith "unimplemented"

  (* Plumbing.read_tree [| prefix; hash |] *)

  let commit_tree hash message = failwith "unimplemented"

  (* Plumbing.commit_tree [| "-m"; message; hash|] *)
  (* not sure if -m "msg" comes before or after hash I see both in doc *)

  let commit_t_of_commit_oneline line =
    let hash = String.sub line 0 7 in

    (*print_endline (string_of_int (String.length line));*)
    let msg = "  " ^ String.sub line 9 (String.length line - 22) in
    { tree = hash; msg }

  let commit_t_list_of_res res =
    let lines = P.get_out res in
    List.map commit_t_of_commit_oneline lines

  let log hash =
    match hash with
    | None ->
        let res = P.log [| "-10" |] in
        commit_t_list_of_res res
    | Some h ->
        let res = P.log [| h; "-10" |] in
        commit_t_list_of_res res

  let add files =
    let args_arr = Array.of_list files in
    ignore (P.add args_arr)

  let restore_staged files =
    let args_lst = "--staged" :: files in
    let args_arr = Array.of_list args_lst in
    ignore (P.restore args_arr)

  let commit msg = ignore (P.commit [| "-m"; msg |])

  let show () = failwith "unimplemented" (*Plumbing.show [||]*)

  let diff () = failwith "unimplemented" (*Plumbing.diff [||]*)

  let empty_status_t = { untracked = []; tracked = []; staged = [] }

  let add_to_untracked status filename =
    {
      tracked = status.tracked;
      untracked = filename :: status.untracked;
      staged = status.staged;
    }

  let add_to_tracked status filename =
    {
      tracked = filename :: status.tracked;
      untracked = status.untracked;
      staged = status.staged;
    }

  let add_to_staged status filename =
    {
      tracked = status.tracked;
      untracked = status.untracked;
      staged = filename :: status.staged;
    }

  let add_to_staged_and_tracked status filename =
    let status' = add_to_staged status filename in
    add_to_tracked status' filename

  let add_to_status_t status line =
    let filename = String.sub line 2 (String.length line - 2) in
    let filename = String.trim filename in
    match String.sub line 0 2 with
    | "??" -> add_to_untracked status filename
    | " M" -> add_to_tracked status filename
    | "M " -> add_to_staged status filename
    | "MM" -> add_to_staged_and_tracked status filename
    | "MD" -> add_to_staged_and_tracked status filename
    | " A" -> add_to_tracked status filename
    | "A " -> add_to_staged status filename
    | "AM" -> add_to_staged_and_tracked status filename
    | "AD" -> add_to_staged_and_tracked status filename
    | " D" -> add_to_tracked status filename
    | "D " -> add_to_staged status filename
    | " R" -> add_to_tracked status filename
    | "R " -> add_to_staged status filename
    | "RM" -> add_to_staged_and_tracked status filename
    | "RD" -> add_to_staged_and_tracked status filename
    | " C" -> add_to_tracked status filename
    | "C " -> add_to_staged status filename
    | "CM" -> add_to_staged_and_tracked status filename
    | "CD" -> add_to_staged_and_tracked status filename
    | "DR" -> add_to_staged_and_tracked status filename
    | "DC" -> add_to_staged_and_tracked status filename
    | "DD" -> add_to_staged_and_tracked status filename
    | "AU" -> add_to_staged_and_tracked status filename
    | "UD" -> add_to_staged_and_tracked status filename
    | "UA" -> add_to_staged_and_tracked status filename
    | "DU" -> add_to_staged_and_tracked status filename
    | "AA" -> add_to_staged_and_tracked status filename
    | "UU" -> add_to_staged_and_tracked status filename
    | _ -> failwith "TODO throw some failure exception"

  let status_t_of_string_list lines =
    List.fold_left add_to_status_t empty_status_t lines

  let status () =
    let status = P.status [| "--porcelain" |] in
    let lines = P.get_out status in
    status_t_of_string_list lines

  let get_untracked status = status.untracked

  let get_tracked status = status.tracked

  let get_staged status = status.staged

  let string_of_commit_t c = c.tree ^ " " ^ c.msg
                                              
end
