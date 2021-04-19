type t = {
  commit_history : Porcelain.commit_t list;
  (*head : Porcelain.commit_t; merge : Porcelain.commit_t option; push :
    Porcelain.commit_t option;*)
  untracked : string list;
  tracked : string list;
  staged : string list;
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
    untracked = [];
    tracked = [];
    staged = [];
    user_curs_y = 0;
  }

let commit_history st = st.commit_history

let head st = failwith "Unimplemented"

let merge st = failwith "Unimplemented"

let push st = failwith "Unimplemented"

let untracked st = st.untracked

let tracked st = st.tracked

let staged st = st.staged

let get_user_curs_y st = st.user_curs_y

let set_user_curs_y st i =
<<<<<<< HEAD
  let new_y = if get_user_curs_y st = 0 then 0 else i in
  {
    commit_history = st.commit_history;
    untracked = st.untracked;
    tracked = st.tracked;
    staged = st.staged;
    user_curs_y = new_y;
  }
=======
  let new_y = if i < 0 then 0 else i in
  { commit_history = st.commit_history; user_curs_y = new_y }
>>>>>>> ed78d72af219191911f0aa4c8863d09341366994

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

let printable_of_file f = { text = f; color = "white" }

let commit_header = { text = "Recent Commits"; color = "yellow" }

let untracked_header = { text = "Untracked"; color = "yellow" }

let tracked_header = { text = "Tracked"; color = "yellow" }

let staged_header = { text = "Staged"; color = "yellow" }

let printable_of_state st =
  let commits = commit_history st in
  let commits_printable = List.map printable_of_commit_t commits in
  let untracked_printable = List.map printable_of_file (untracked st) in
  let tracked_printable = List.map printable_of_file (tracked st) in
  let staged_printable = List.map printable_of_file (staged st) in
  (commit_header :: commits_printable)
  @ (untracked_header :: untracked_printable)
  @ (tracked_header :: tracked_printable)
  @ (staged_header :: staged_printable)
