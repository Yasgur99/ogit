open Plumbing

module type Porcelain = sig
  (** Representation of porcelain git commands.

      This module represents the calling of calling git commands that
      the user interface will find more useful than using the raw
      plumbing commands *)

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
  val pull : string -> string -> string -> string

  (** [push] pulls files from repository *)
  val push : string -> string -> string -> string

  (** [log h] is the list of commit objects that are reachable from HEAD
      in reverse chronological order if [h] is [None], otherwise the
      commit objects that are reachable by following parents of commit
      [h] in reverse chronological order *)
  val log : object_id option -> commit_t list

  (** [add fnames] adds the files with filenames [fnames] to the staging
      area *)
  val add : string list -> unit

  (** [branch_msg n] is the message of the last commit in the branch
      named [n] *)
  val branch_msg : string -> string

  (** [restore_staged fnames] restores staged files [fnames] from the
      staging area *)
  val restore_staged : string list -> unit

  (** [commit msg] commits the changes in the staging area with commit
      message [msg] *)
  val commit : string -> string

  (** [diff] shows the diffs of tracked files *)
  val diff : unit -> string

  (** [status] shows the status of the working tree *)
  val status : unit -> status_t

  (** [checkout b] switches to branch named [b] *)
  val checkout : string -> string

  val create_branch : string -> string

  val delete_branch : string -> string

  val stash_apply : unit -> string

  val stash_pop : unit -> string

  (** [string_of_commit c] is a commit in the form [hash msg] *)
  val string_of_commit_t : commit_t -> string

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

  let rec rm_leading_spaces str =
    match String.split_on_char ' ' str with
    | [] -> str
    | [ "" ] -> str
    | "" :: t ->
        rm_leading_spaces (String.sub str 0 (String.length str - 1))
    | h :: t -> str

  let pull u p b =
    match b with
    | "remote" ->
        P.pull [||]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"
    | branch ->
        P.pull [| branch |]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"

  let push u p b =
    match b with
    | "remote" ->
        P.push [||]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"
    | branch ->
        P.push [| branch |]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"

  let commit_t_of_commit_oneline line =
    let hash = String.sub line 0 7 in
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

  let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false

  let branch_msg name =
    if contains name "fatal:" then ""
    else
      let res = P.log [| "--graph"; name; "-1"; "--format=%s" |] in
      let msg =
        P.get_out res |> List.fold_left (fun acc x -> acc ^ x) ""
      in
      let start = String.index msg '*' in
      String.sub msg (start + 2) (String.length msg - start - 2)

  let get_head () =
    let long_ref =
      match P.get_out (P.head [||]) with [] -> "" | h :: t -> h
    in
    let start =
      match long_ref with
      | "" -> 0
      | _ ->
          Str.search_backward (Str.regexp "heads") long_ref
            (String.length long_ref - 1)
          + 6
    in
    String.sub long_ref start (String.length long_ref - start)

  let get_last_msg =
    P.get_out (P.log [| "-1"; "--format=%s" |])
    |> List.fold_left (fun acc x -> acc ^ x) ""

  let get_upstream () =
    P.get_out
      (P.revparse
         [| "--abbrev-ref"; "--symbolic-full-name"; "@{upstream}" |])
    |> List.fold_left (fun acc x -> acc ^ x) ""

  let get_push () =
    P.get_out
      (P.revparse
         [| "--abbrev-ref"; "--symbolic-full-name"; "@{push}" |])
    |> List.fold_left (fun acc x -> acc ^ x) ""

  let add files =
    let args_arr = Array.of_list files in
    ignore (P.add args_arr)

  let restore_staged files =
    let args_lst = "--staged" :: files in
    let args_arr = Array.of_list args_lst in
    ignore (P.restore args_arr)

  let commit msg =
    P.commit [| "-m"; msg |]
    |> P.get_out
    |> List.map rm_leading_spaces
    |> List.rev |> String.concat "\n"

  let diff () =
    P.diff [||]
    |> P.get_out
    |> List.map rm_leading_spaces
    |> List.rev |> String.concat "\n"

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

  let init (dir : string option) : unit =
    match dir with
    | None -> ignore (P.init [||])
    | Some d -> ignore (P.init [| d |])

  let status_t_of_string_list lines =
    List.fold_left add_to_status_t empty_status_t lines

  let status () =
    let status = P.status [| "--porcelain" |] in
    let lines = P.get_out status in
    status_t_of_string_list lines

  let checkout branch =
    let res = P.checkout [| branch |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  let create_branch branch =
    let res = P.checkout [| "-b"; branch |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  let delete_branch branch =
    let res = P.checkout [| "-d"; branch |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  let stash_apply () =
    let res = P.stash [| "apply" |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  let stash_pop () =
    let res = P.stash [| "pop" |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  let get_untracked status = status.untracked

  let get_tracked status = status.tracked

  let get_staged status = status.staged

  let string_of_commit_t c = c.tree ^ " " ^ c.msg
end
