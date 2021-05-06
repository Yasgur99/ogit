type file_name = string

type commit_msg = string

type branch_name = string

type key = int

type t =
  | Stage
  | Unstage
  | Quit
  | NavUp
  | NavDown
  | Commit of string
  | Diff
  | PullMenu
  | PullRemote
  | PullOriginMaster
  | PullElsewhere
  | PushMenu
  | PushRemote
  | PushOriginMaster
  | PushElsewhere
  | Clear
  | Nop

exception Program_terminate

let parse_key key =
  if key = int_of_char 's' then Stage
  else if key = int_of_char 'u' then Unstage
  else if key = int_of_char 'k' || key = Curses.Key.up then NavUp
  else if key = int_of_char 'j' || key = Curses.Key.down then NavDown
  else if key = int_of_char 'q' then Quit
  else if key = int_of_char 'c' then Commit ""
  else if key = int_of_char 'd' then Diff
  else if key = int_of_char 'L' then PullMenu
  else if key = int_of_char 'P' then PushMenu
  else if key = int_of_char ' ' then Clear
  else Nop

let parse_key_pull_mode key =
  if key = int_of_char 'p' then PullRemote
  else if key = int_of_char 'u' then PullOriginMaster
  else if key = int_of_char 'e' then PullElsewhere
  else Nop

let parse_key_push_mode key =
  if key = int_of_char 'p' then PushRemote
  else if key = int_of_char 'u' then PushOriginMaster
  else if key = int_of_char 'e' then PushElsewhere
  else Nop

let string_of_cmd cmd =
  match cmd with
  | Stage -> "stage"
  | Unstage -> "unstage"
  | NavUp -> "navup"
  | NavDown -> "navdown"
  | Commit _ -> "commit"
  | Diff -> "diff"
  | PullMenu -> "pull"
  | PullRemote -> "pull"
  | PullOriginMaster -> "pull"
  | PullElsewhere -> "pull"
  | PushMenu -> "push"
  | PushRemote -> "push"
  | PushOriginMaster -> "push"
  | PushElsewhere -> "push"
  | Clear -> "clear"
  | Quit -> "quit"
  | Nop -> "nop"
