type commit_object = {
  tree : string;
  (* is this the SHA-1? *)
  parents : string;
  (* previous SHA-1 *)
  author : string;
  committer : string;
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
  | Commit of { com_ob : commit_object }
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

let cat_file_type hash = failwith "unimplemented"

let update_index hash file = failwith "unimplemented"
(* Plumbing.update_index [| hash; file|] (* not sure of this one *) *)

let write_tree () = failwith "Unimplemented"

let read_tree hash = failwith "unimplemented"
(* Plumbing.read_tree [| hash |] *)

let read_tree_prefix hash prefix = failwith "unimplemented"
(* Plumbing.read_tree [| prefix; hash |] *)

let commit_tree hash message = failwith "unimplemented"
(* Plumbing.commit_tree [| hash; message |] *)

let log hash = failwith "unimplemented" (*Plumbing.log [| hash |] *)

let add files = failwith "unimplemented" (*Plumbing.add [| files |]*)

let commit msg = failwith "unimplemented"
(*Plumbing.commit [| "-m"; msg |]*)

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

let add_to_status_t status line =
  let filename = String.sub line 2 (String.length line - 2) in
  match String.sub line 0 2 with
  | "??" -> add_to_untracked status filename
  | " M" -> add_to_tracked status filename
  | "M " -> add_to_staged status filename
  | "MM" -> failwith "todo"
  | "MD" -> failwith "todo"
  | " A" -> add_to_tracked status filename (* not in documentation *)
  | "A " -> add_to_staged status filename
  | "AM" -> failwith "todo"
  | "AD" -> failwith "todo"
  | " D" -> add_to_tracked status filename
  | "D " -> add_to_staged status filename (* not in documentation *)
  | " R" -> add_to_tracked status filename
  | "R " -> add_to_staged status filename
  | "RM" -> failwith "todo"
  | "RD" -> failwith "todo"
  | " C" -> add_to_tracked status filename
  | "C " -> add_to_staged status filename
  | "CM" -> failwith "todo"
  | "CD" -> failwith "todo"
  | "DR" -> failwith "todo"
  | "DC" -> failwith "todo"
  | "DD" -> failwith "todo"
  | "AU" -> failwith "todo"
  | "UD" -> failwith "todo"
  | "UA" -> failwith "todo"
  | "DU" -> failwith "todo"
  | "AA" -> failwith "todo"
  | "UU" -> failwith "TODO throw some failure exception"
  | _ -> failwith "TODO throw some failure exception"

let status_t_of_string_list lines =
  List.fold_left add_to_status_t empty_status_t lines

let status () =
  let status = Plumbing.status [| "--porcelain" |] in
  let lines = Plumbing.get_out status in
  status_t_of_string_list lines

let get_untracked status = status.untracked

let get_tracked status = status.tracked

let get_staged status = status.staged
