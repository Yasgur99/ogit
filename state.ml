type t = {
  commit_history : Porcelain.commit_t list;
  (*head : Porcelain.commit_t; merge : Porcelain.commit_t option; push :
    Porcelain.commit_t option;*)
  user_curs_y : int;
}

type printable = {
  text : string;
  color : string;
}

let get_head () = failwith "unimplemented"

let init_state dir =
  {
    commit_history = Porcelain.log None;
    (*head = get_head (); merge = None; push = None;*)
    user_curs_y = 0;
  }

let commit_history st = st.commit_history

let head st = failwith "Unimplemented"

let merge st = failwith "Unimplemented"

let push st = failwith "Unimplemented"

let get_user_curs_y st = st.user_curs_y

let set_user_curs_y st i =
  let new_y = if i < 0 then 0 else i in
  { commit_history = st.commit_history; user_curs_y = new_y }

let exec_add st f =
  Porcelain.add [ f ];
  st

let exec_unstage st f =
  Porcelain.restore_staged [ f ];
  st

let exec st = function
  | Command.Quit -> raise Command.Program_terminate
  | Command.NavUp -> set_user_curs_y st (get_user_curs_y st - 1)
  | Command.NavDown -> set_user_curs_y st (get_user_curs_y st + 1)
  | Command.Stage f -> exec_add st f
  | Command.Unstage f -> exec_unstage st f
  | _ -> st

let printable_of_commit_t c =
  { text = Porcelain.string_of_commit_t c; color = "white" }

let commit_header = { text = "Recent Commits"; color = "yellow" }

let printable_of_state st =
  let commits = commit_history st in
  let commits_printable = List.map printable_of_commit_t commits in
  commit_header :: commits_printable
