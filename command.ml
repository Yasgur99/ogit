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
  | DiffMenu
  | DiffFile
  | DiffTracked
  | DiffStaged
  | DiffAll
  | PullMenu
  | PullRemote
  | PullOriginMaster
  | PullElsewhere of string
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
  else if key = int_of_char 'D' then DiffMenu
  else if key = int_of_char 'L' then PullMenu
  else if key = int_of_char 'P' then PushMenu
  else if key = int_of_char ' ' then Clear
  else Nop

let parse_key_diff_mode key =
  if key = int_of_char 's' then DiffStaged
  else if key = int_of_char 't' then DiffTracked
  else if key = int_of_char 'a' then DiffAll
  else if key = int_of_char 'f' then DiffFile
  else if key = int_of_char 'k' || key = Curses.Key.up then NavUp
  else parse_key key

let parse_key_pull_mode key =
  if key = int_of_char 'p' then PullRemote
  else if key = int_of_char 'u' then PullOriginMaster
  else if key = int_of_char 'e' then PullElsewhere ""
  else parse_key key

let parse_key_push_mode key =
  if key = int_of_char 'p' then PushRemote
  else if key = int_of_char 'u' then PushOriginMaster
  else if key = int_of_char 'e' then PushElsewhere
  else parse_key key

let string_of_cmd cmd =
  match cmd with
  | Stage -> "stage"
  | Unstage -> "unstage"
  | NavUp -> "navup"
  | NavDown -> "navdown"
  | Commit _ -> "commit"
  | DiffMenu -> "diff"
  | DiffStaged -> "diff"
  | DiffTracked -> "diff"
  | DiffFile -> "diff"
  | DiffAll -> "diff"
  | PullMenu -> "pull"
  | PullRemote -> "pull"
  | PullOriginMaster -> "pull"
  | PullElsewhere _ -> "pullelsewhere"
  | PushMenu -> "push"
  | PushRemote -> "push"
  | PushOriginMaster -> "push"
  | PushElsewhere -> "push"
  | Clear -> "clear"
  | Quit -> "quit"
  | Nop -> "nop"
