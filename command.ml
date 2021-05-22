type file_name = string

type commit_msg = string

type branch_name = string

type key = int

type t =
  | Stage
  | Unstage
  | Quit
  | NavUp of bool
  | NavDown of bool
  | Commit of string
  | DiffMenu
  | DiffFile
  | DiffTracked
  | DiffStaged
  | DiffAll
  | PullMenu
  | Pull of string * string * string
  | PushMenu
  | Push of string * string * string
  | BranchMenu
  | CheckoutBranchPrompt
  | CreateBranchPrompt
  | DeleteBranchPrompt
  | CheckoutBranch of string
  | CreateBranch of string
  | DeleteBranch of string
  | Clear
  | Nop

exception Program_terminate

let parse_key key =
  if key = int_of_char 's' then Stage
  else if key = int_of_char 'u' then Unstage
  else if key = int_of_char 'k' || key = Curses.Key.up then NavUp true
  else if key = int_of_char 'j' || key = Curses.Key.down then
    NavDown true
  else if key = int_of_char 'q' then Quit
  else if key = int_of_char 'c' then Commit ""
  else if key = int_of_char 'D' then DiffMenu
  else if key = int_of_char 'L' then PullMenu
  else if key = int_of_char 'P' then PushMenu
  else if key = int_of_char ' ' then Clear
  else if key = int_of_char 'b' then BranchMenu
  else Nop

let parse_key_diff_mode key =
  if key = int_of_char 's' then DiffStaged
  else if key = int_of_char 't' then DiffTracked
  else if key = int_of_char 'a' then DiffAll
  else if key = int_of_char 'f' then DiffFile
  else parse_key key

let parse_key_pull_mode key =
  if key = int_of_char 'p' then Pull ("", "", "remote")
  else if key = int_of_char 'u' then Pull ("", "", "origin/master")
  else if key = int_of_char 'e' then Pull ("", "", "")
  else parse_key key

let parse_key_push_mode key =
  if key = int_of_char 'p' then Push ("", "", "remote")
  else if key = int_of_char 'u' then Push ("", "", "origin/master")
  else if key = int_of_char 'e' then Pull ("", "", "")
  else parse_key key

let parse_key_branch_mode key =
  if key = int_of_char 'b' then CheckoutBranchPrompt
  else if key = int_of_char 'c' then CreateBranchPrompt
  else if key = int_of_char 'x' then DeleteBranchPrompt
  else parse_key key
