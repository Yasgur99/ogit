
type t = {
  commit_history : Porcelain.commit_t list;
  head : Porcelain.commit_t;
  merge : Porcelain.commit_t option;
  push : Porcelain.commit_t option;
}

let init_state dir = failwith "Unimplemented"

let commit_history st = failwith "Unimplemented"

let head st = failwith "Unimplemented"

let merge st = failwith "Unimplemented"

let push st = failwith "Unimplemented"

let exec st cmd = failwith "Unimplemented"
