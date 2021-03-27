
type commit_object = unit

type object_id = string

type object_content = unit

type object_type = unit

type status_t = {
  untracked : string list;
  tracked : string list;
  staged : string list;
}

let init dir = failwith "unimplemented"

let hash_object file = failwith "unimplemented"

let cat_file hash = failwith "unimplemented"

let cat_file_type hash = failwith "unimplemented"

let update_index hash file = failwith "unimplemented"

let write_tree () = failwith "unimplemented"

let read_tree hash = failwith "unimplemented"

let read_tree_prefix hash prefix = failwith "unimplemented"

let commit_tree hash message = failwith "unimplemented"

let log hash = failwith "unimplemented"

let add files = failwith "unimplemented"

let commit msg = failwith "unimplemented"

let show () = failwith "unimplemented"

let diff () = failwith "unimplemented"

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
