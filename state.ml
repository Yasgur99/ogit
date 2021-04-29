type render_mode =
  | Normal
  | CommitMode
  | CommitDone of string
  | DiffMode of string

(** The representation type for state. *)
type t = {
  commit_history : Porcelain.commit_t list;
  head : string;
  merge : string;
  push : string;
  untracked : string list;
  tracked : string list;
  staged : string list;
  curs : int;
  mode : render_mode;
}

type printable = {
  text : string;
  color : string;
}

(** [init_state dir] is the state of the directory [dir]. The cursor
    points to the first line of the terminal. Requires [dir] is a
    directory containing a valid .git directory *)
let init_state dir =
  {
    commit_history = Porcelain.log None;
    head = Porcelain.get_head;
    merge = Porcelain.get_upstream;
    push = Porcelain.get_push;
    untracked = Porcelain.get_untracked (Porcelain.status ());
    tracked = Porcelain.get_tracked (Porcelain.status ());
    staged = Porcelain.get_staged (Porcelain.status ());
    curs = 0;
    mode = Normal;
  }

(** [update_git_state st] updates commit_history, untracked, tracked and
    staged files according to the git directory *)
let update_git_state st =
  {
    commit_history = Porcelain.log None;
    head = Porcelain.get_head;
    merge = Porcelain.get_upstream;
    push = Porcelain.get_push;
    untracked = Porcelain.get_untracked (Porcelain.status ());
    tracked = Porcelain.get_tracked (Porcelain.status ());
    staged = Porcelain.get_staged (Porcelain.status ());
    curs = st.curs;
    mode = Normal;
  }

(*********************************************************)
(* Access/Mutate state *)
(*********************************************************)
let head st = st.head

let merge st = st.merge

let push st = st.push

let untracked st = st.untracked

let tracked st = st.tracked

let staged st = st.staged

let commit_history st = st.commit_history

let get_curs st = st.curs

let get_mode st = st.mode

let get_max_y st =
  List.length st.commit_history
  + List.length st.untracked
  + List.length st.tracked + List.length st.staged + 6

let set_curs st i =
  let new_y =
    if i < 0 then 0 else if i > get_max_y st then get_max_y st else i
  in
  {
    commit_history = st.commit_history;
    head = st.head;
    merge = st.merge;
    push = st.push;
    untracked = st.untracked;
    tracked = st.tracked;
    staged = st.staged;
    curs = new_y;
    mode = st.mode;
  }

let set_mode st new_mode =
  {
    commit_history = st.commit_history;
    head = st.head;
    push = st.push;
    merge = st.merge;
    untracked = st.untracked;
    tracked = st.tracked;
    staged = st.staged;
    curs = st.curs;
    mode = new_mode;
  }

let update_mode st cmd =
  let new_mode =
    match cmd with Command.Commit _ -> CommitMode | _ -> st.mode
  in
  set_mode st new_mode

(*********************************************************)
(* Printable *)
(*********************************************************)
let printable_of_file c f = { text = f; color = c }

let commit_header = { text = "Recent Commits"; color = "yellow" }

let head_header = { text = "Head"; color = "yellow" }

let merge_header = { text = "Merge"; color = "yellow" }

let push_header = { text = "Push"; color = "yellow" }

let untracked_header = { text = "Untracked"; color = "yellow" }

let tracked_header = { text = "Tracked"; color = "yellow" }

let staged_header = { text = "Staged"; color = "yellow" }

let blank_line = { text = " "; color = "white" }

let printable_of_commit_t c =
  { text = Porcelain.string_of_commit_t c; color = "white" }

let printable_of_state st =
  let commits_printable =
    List.map printable_of_commit_t (commit_history st) |> List.rev
  in
  let head_printable =
    { text = head st ^ "   " ^ Porcelain.get_last_msg; color = "white" }
  in
  let merge_printable =
    {
      text = merge st ^ "   " ^ Porcelain.branch_msg (merge st);
      color = "white";
    }
  in
  let push_printable =
    {
      text = push st ^ "   " ^ Porcelain.branch_msg (push st);
      color = "white";
    }
  in
  let untracked_printable = List.map (printable_of_file "red") (untracked st) in
  let tracked_printable = List.map (printable_of_file "red") (tracked st) in
  let staged_printable = List.map (printable_of_file "green") (staged st) in
  untracked_header :: untracked_printable
  @ tracked_header :: tracked_printable
  @ staged_header :: staged_printable
  @ [ blank_line ]
  @ [ head_header; head_printable ]
  @ [ merge_header; merge_printable ]
  @ [ push_header; push_printable ]
  @ [ blank_line ]
  @ commit_header :: commits_printable

(*********************************************************)
(* Exec *)
(*********************************************************)

let get_curs_content st =
  let printables = printable_of_state st in
  let printable = List.nth printables st.curs in
  printable.text

let exec_add st =
  let curs_content = get_curs_content st in
  Porcelain.add [ curs_content ];
  update_git_state st

let exec_unstage st =
  let curs_content = get_curs_content st in
  Porcelain.restore_staged [ curs_content ];
  update_git_state st

let exec_commit st msg =
  let output = Porcelain.commit msg in
  set_mode (update_git_state st) (CommitDone output)

let exec_diff st =
  Porcelain.add st.untracked;
  Porcelain.add st.tracked;
  let out = Porcelain.diff () in
  Porcelain.restore_staged st.untracked;
  Porcelain.restore_staged st.tracked;
  set_mode st (DiffMode out)

let exec_pull st =
  Porcelain.pull ();
  update_git_state st

let exec_push st =
  Porcelain.push ();
  update_git_state st

let exec st = function
  | Command.NavUp -> set_curs st (get_curs st - 1)
  | Command.NavDown -> set_curs st (get_curs st + 1)
  | Command.Stage -> exec_add st
  | Command.Unstage -> exec_unstage st
  | Command.Commit msg -> if msg = "" then st else exec_commit st msg
  | Command.Diff -> exec_diff st
  | Command.Clear -> set_mode st Normal
  | Command.Pull -> exec_pull st
  | Command.Push -> exec_push st
  | Command.Quit -> raise Command.Program_terminate
  | Command.Nop -> st
