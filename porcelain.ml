
type commit_object = {
  tree : string;
  (* is this the SHA-1? *)
  parents : string;
  (* previous SHA-1 *)
  author : string;
  committer : string;
  msg : string;
}

type object_id = { header_hash : string }

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
 

let init dir : unit = failwith "unimplemented"
  (* match dir with
  | None -> Plumbing.init [||]
  | Some _ -> Plumbing.init [| dir |] *)

  let hash_object file : object_id = failwith "unimplemented" (* Plumbing.hash_object [| "-w"; file |] *)

let cat_file hash = failwith "unimplemented"

let cat_file_type hash = failwith "unimplemented"

let update_index hash file : unit = failwith "unimplemented"
  (* Plumbing.update_index [| hash; file|] (* not sure of this one *) *)

let write_tree = failwith "Unimplemented"

let read_tree hash = failwith "unimplemented" 
(* Plumbing.read_tree [| hash |] *)

let read_tree_prefix hash prefix = failwith "unimplemented"
(* Plumbing.read_tree [| prefix; hash |] *)

let commit_tree hash message = failwith "unimplemented"
(* Plumbing.commit_tree [| hash; message |] *)
let log hash = failwith "unimplemented"
(* Plumbing.log [| hash |] *)

let add files = failwith "unimplemented"
(* Plumbing.add [| files |] *)

let commit msg = failwith "unimplemented"
(* Plumbing.commit [| "-m"; msg |] *)

let show () = failwith "unimplemented"
(* Plumbing.show [||] *)

let diff () = failwith "unimplemented"
(* Plumbing.diff [||] *)

let status () = {
  (** TODO: This impl is just for mocking since it wasnt ready.
      Be sure to change it *)
  untracked = [];
  tracked = [];
  staged = [];
}

let get_untracked status = status.untracked

let get_tracked status = status.tracked

let get_staged status = status.staged 


