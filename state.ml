open Plumbing
open Porcelain

module type State = sig
  module MPorcelain : Porcelain

  (** The abstract type of values representing the git state *)
  type t

  (** The variant type representing the differences in the VIM when
      various commands are called *)
  type render_mode =
    | Normal
    | CommitMode
    | CommitDone of string
    | DiffMode of string
    | PushMode
    | PushElsewhereMode
    | PushElsewhereDone of string
    | PullMode
    | PullElsewhereMode
    | PullElsewhereDone of string

  (** The representation type that specifies what [color] [text] should
      be printed as. *)
  type printable = {
    text : string;
    color : string;
  }

  (** [init_state dir] is the state of the current working tree
      belonging to the repo rooted at [dir]. *)
  val init_state : string -> t

  (** [commit_history st] is the commit history of the current state
      [st] *)
  val commit_history : t -> MPorcelain.commit_t list

  (** [head st] is the commit pointed to by the head in the current
      state [st] *)
  val head : t -> string

  (** [merge st] is the commit pointed to by the upstream branch of the
      current branch *)
  val merge : t -> string

  (** [push st] is the branch that the current branch is being pushed to *)
  val push : t -> string

  (** [exec st c] is the state after executing command [c] from state
      [st]. *)
  val exec : t -> Command.t -> t

  (** [printable_of_state st] is a printable represnation of the state *)
  val printable_of_state : t -> printable list

  (** [get_curs st] is the y index of the position into state. *)
  val get_curs : t -> int

  (** [set_curs st y] is the state whose cursor is [y]. The rest of the
      state says the same. *)
  val set_curs : t -> int -> t

  (** [get_mode st] is the rendering mode for [st]. *)
  val get_mode : t -> render_mode

  (** [set_mode st new_mode] manually changes the mode of [st] to
      [new_mode]. *)
  val set_mode : t -> render_mode -> t

  (** [update_mode st cmd] automatically changes the mode of [st] to the
      appropriate mode based on the command [cmd]. The rest of the state
      stays the same. *)
  val update_mode : t -> Command.t -> t
end

module StateImpl (P : Plumbing) : State = struct
  module MPorcelain = PorcelainImpl (P)

  type render_mode =
    | Normal
    | CommitMode
    | CommitDone of string
    | DiffMode of string
    | PushMode
    | PushElsewhereMode
    | PushElsewhereDone of string
    | PullMode
    | PullElsewhereMode
    | PullElsewhereDone of string

  (** The representation type for state. *)
  type t = {
    commit_history : MPorcelain.commit_t list;
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
      commit_history = MPorcelain.log None;
      head = MPorcelain.get_head;
      merge = MPorcelain.get_upstream;
      push = MPorcelain.get_push;
      untracked = MPorcelain.get_untracked (MPorcelain.status ());
      tracked = MPorcelain.get_tracked (MPorcelain.status ());
      staged = MPorcelain.get_staged (MPorcelain.status ());
      curs = 0;
      mode = Normal;
    }

  (** [update_git_state st] updates commit_history, untracked, tracked
      and staged files according to the git directory *)
  let update_git_state st =
    {
      commit_history = MPorcelain.log None;
      head = MPorcelain.get_head;
      merge = MPorcelain.get_upstream;
      push = MPorcelain.get_push;
      untracked = MPorcelain.get_untracked (MPorcelain.status ());
      tracked = MPorcelain.get_tracked (MPorcelain.status ());
      staged = MPorcelain.get_staged (MPorcelain.status ());
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
    + List.length st.tracked + List.length st.staged + 11

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
    { text = MPorcelain.string_of_commit_t c; color = "white" }

  let printable_of_state st =
    let commits_printable =
      List.map printable_of_commit_t (commit_history st) |> List.rev
    in
    let head_printable =
      {
        text = head st ^ "   " ^ MPorcelain.get_last_msg;
        color = "white";
      }
    in
    let merge_printable =
      {
        text = merge st ^ "   " ^ MPorcelain.branch_msg (merge st);
        color = "white";
      }
    in
    let push_printable =
      {
        text = push st ^ "   " ^ MPorcelain.branch_msg (push st);
        color = "white";
      }
    in
    let untracked_printable =
      List.map (printable_of_file "red") (untracked st)
    in
    let tracked_printable =
      List.map (printable_of_file "red") (tracked st)
    in
    let staged_printable =
      List.map (printable_of_file "green") (staged st)
    in
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
    MPorcelain.add [ curs_content ];
    update_git_state st

  let exec_unstage st =
    let curs_content = get_curs_content st in
    MPorcelain.restore_staged [ curs_content ];
    update_git_state st

  let exec_commit st msg =
    let output = MPorcelain.commit msg in
    set_mode (update_git_state st) (CommitDone output)

  let exec_diff_tracked st =
    let out = MPorcelain.diff () in
    MPorcelain.restore_staged st.untracked;
    MPorcelain.restore_staged st.tracked;
    set_mode st (DiffMode out)

  let exec_diff_staged st =
    MPorcelain.add st.tracked;
    MPorcelain.restore_staged st.staged;
    let out = MPorcelain.diff () in
    MPorcelain.restore_staged st.tracked;
    MPorcelain.add st.staged;
    set_mode st (DiffMode out)

  let exec_diff_all st =
    MPorcelain.restore_staged st.staged;
    let out = MPorcelain.diff () in
    MPorcelain.add st.staged;
    set_mode st (DiffMode out)

  let exec_diff_file st =
    MPorcelain.add st.tracked;
    let curs_content = get_curs_content st in
    MPorcelain.restore_staged [ curs_content ];
    let out = MPorcelain.diff () in
    MPorcelain.restore_staged st.tracked;
    if List.mem curs_content st.staged then
      MPorcelain.add [ curs_content ]
    else ();
    set_mode st (DiffMode out)

  let exec_pull_remote st =
    MPorcelain.pull None;
    set_mode (update_git_state st) Normal

  let exec_pull_origin_master st =
    MPorcelain.pull None;
    (* TODO *)
    set_mode (update_git_state st) Normal

  let exec_pull_elsewhere st msg =
    MPorcelain.pull msg;
    update_git_state st

  let exec_push_remote st =
    MPorcelain.push None;
    set_mode (update_git_state st) Normal

  let exec_push_origin_master st =
    MPorcelain.push None;
    (* TODO *)
    set_mode (update_git_state st) Normal

  let exec_push_elsewhere st msg =
    MPorcelain.push msg;
    update_git_state st

  let exec st = function
    | Command.NavUp -> set_curs st (get_curs st - 1)
    | Command.NavDown -> set_curs st (get_curs st + 1)
    | Command.Stage -> exec_add st
    | Command.Unstage -> exec_unstage st
    | Command.Commit msg -> if msg = "" then st else exec_commit st msg
    | Command.DiffMenu -> set_mode st (DiffMode "MENU")
    | Command.DiffTracked -> exec_diff_tracked st
    | Command.DiffStaged -> exec_diff_staged st
    | Command.DiffAll -> exec_diff_all st
    | Command.DiffFile -> exec_diff_file st
    | Command.Clear -> set_mode st Normal
    | Command.PullMenu -> set_mode st PullMode
    | Command.PullRemote -> exec_pull_remote st
    | Command.PullOriginMaster -> exec_pull_origin_master st
    | Command.PullElsewhere msg ->
        if msg = "" then st else exec_pull_elsewhere st (Some msg)
    | Command.PushMenu -> set_mode st PushMode
    | Command.PushRemote -> exec_push_remote st
    | Command.PushOriginMaster -> exec_push_origin_master st
    | Command.PushElsewhere msg ->
        if msg = "" then st else exec_push_elsewhere st (Some msg)
    | Command.Quit -> raise Command.Program_terminate
    | Command.Nop -> st
end
