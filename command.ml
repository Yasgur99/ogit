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
  else Nop

let string_of_cmd cmd =
  match cmd with
  | Stage -> "stage"
  | Unstage -> "unstage"
  | NavUp -> "navup"
  | NavDown -> "navdown"
  | Commit _ -> "commit"
  | Diff -> "diff"
  | Quit -> "quit"
  | Nop -> "nop"
