
type t = {
  commit_history : Porcelain.commit_t list;
  (*head : Porcelain.commit_t;
  merge : Porcelain.commit_t option;
  push : Porcelain.commit_t option;*)
}

type printable = {
  text : string;
  color : string;
}

let get_head () = failwith "unimplemented"

let init_state dir = 
  {
    commit_history = Porcelain.log None;
    (*head = get_head ();
    merge = None;
    push = None;*)
  }

let commit_history st = 
  st.commit_history

let head st = failwith "Unimplemented"

let merge st = failwith "Unimplemented"

let push st = failwith "Unimplemented"

let exec st cmd = failwith "Unimplemented"

let printable_of_commit_t c =
  {
    text = Porcelain.string_of_commit_t c;
    color = "white";
  }

let printable_of_state st =
  let commits = commit_history st in 
  List.map printable_of_commit_t commits


