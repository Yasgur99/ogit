type render_mode =
  | Normal
  | CommitMode
  | CommitDone of string
  | DiffMode of string

(** The representation type for state. *)
type t = {
  commit_history : Porcelain.commit_t list;
  head : Porcelain.commit_t;
  (*merge : Porcelain.commit_t option; push : Porcelain.commit_t option;*)
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
    head = Porcelain.get_head (Porcelain.log None |> List.rev);
    (* merge = None; push = None;*)
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
    head = Porcelain.get_head (Porcelain.log None);
    (* merge = None; push = None;*)
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

let merge st = failwith "Unimplemented"

let push st = failwith "Unimplemented"

let untracked st = st.untracked

let tracked st = st.tracked

let staged st = st.staged

let commit_history st = st.commit_history

let get_curs st = st.curs

let get_mode st = st.mode

let get_max_y st =
  List.length st.commit_history
  + List.length st.untracked
  + List.length st.tracked + List.length st.staged + 4

let set_curs st i =
  let new_y =
    if i < 0 then 0 else if i > get_max_y st then get_max_y st else i
  in
  {
    commit_history = st.commit_history;
    head = st.head;
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
let printable_of_file f = { text = f; color = "white" }

let commit_header = { text = "Recent Commits"; color = "yellow" }

let head_header = { text = "Head"; color = "yellow" }

let untracked_header = { text = "Untracked"; color = "yellow" }

let tracked_header = { text = "Tracked"; color = "yellow" }

let staged_header = { text = "Staged"; color = "yellow" }

let blank_line = { text = " "; color = "white" }

let printable_of_commit_t c =
  { text = Porcelain.string_of_commit_t c; color = "white" }

let printable_of_state st =
  let commits_printable =
    List.map printable_of_commit_t (commit_history st)
  in
  let head_printable = printable_of_commit_t (head st) in
  let untracked_printable = List.map printable_of_file (untracked st) in
  let tracked_printable = List.map printable_of_file (tracked st) in
  let staged_printable = List.map printable_of_file (staged st) in
  (untracked_header :: untracked_printable)
  @ (tracked_header :: tracked_printable)
  @ (staged_header :: staged_printable)
  @ [ blank_line ]
  @ [ head_header; head_printable ]
  @ (commit_header :: commits_printable)

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

let exec st = function
  | Command.NavUp -> set_curs st (get_curs st - 1)
  | Command.NavDown -> set_curs st (get_curs st + 1)
  | Command.Stage -> exec_add st
  | Command.Unstage -> exec_unstage st
  | Command.Commit msg -> if msg = "" then st else exec_commit st msg
  | Command.Diff -> exec_diff st
  | Command.Quit -> raise Command.Program_terminate
  | Command.Nop -> st
