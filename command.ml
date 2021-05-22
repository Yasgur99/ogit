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
  | PullRemote
  | PullOriginMaster
  | PullElsewhere of string
  | PushMenu
  | PushRemote
  | PushOriginMaster
  | BranchMenu
  | CheckoutBranchPrompt
  | CreateBranchPrompt
  | DeleteBranchPrompt
  | CheckoutBranch of string
  | CreateBranch of string
  | DeleteBranch of string
  | PushElsewhere of string
  | NormalTutorial
  | DiffTutorial
  | PullTutorial
  | PushTutorial
  | BranchTutorial
  | BackNormal
  | BackDiff
  | BackPull
  | BackPush
  | BackBranch
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
  else if key = int_of_char 'i' then NormalTutorial
  else Nop

let parse_key_diff_mode key =
  if key = int_of_char 's' then DiffStaged
  else if key = int_of_char 't' then DiffTracked
  else if key = int_of_char 'a' then DiffAll
  else if key = int_of_char 'f' then DiffFile
  else if key = int_of_char 'i' then DiffTutorial
  else parse_key key

let parse_key_pull_mode key =
  if key = int_of_char 'p' then PullRemote
  else if key = int_of_char 'u' then PullOriginMaster
  else if key = int_of_char 'e' then PullElsewhere ""
  else if key = int_of_char 'i' then PullTutorial
  else parse_key key

let parse_key_push_mode key =
  if key = int_of_char 'p' then PushRemote
  else if key = int_of_char 'u' then PushOriginMaster
  else if key = int_of_char 'e' then PushElsewhere ""
  else if key = int_of_char 'i' then PushTutorial
  else parse_key key

let parse_key_branch_mode key =
  if key = int_of_char 'b' then CheckoutBranchPrompt
  else if key = int_of_char 'c' then CreateBranchPrompt
  else if key = int_of_char 'x' then DeleteBranchPrompt
  else if key = int_of_char 'i' then BranchTutorial
  else parse_key key

let parse_key_normal_tutorial key =
  if key = int_of_char 'i' then BackNormal else parse_key key

let parse_key_diff_tutorial key =
  if key = int_of_char 'i' then BackDiff else parse_key_diff_mode key

let parse_key_pull_tutorial key =
  if key = int_of_char 'i' then BackPull else parse_key_pull_mode key

let parse_key_push_tutorial key =
  if key = int_of_char 'i' then BackPush else parse_key_push_mode key

let parse_key_branch_tutorial key =
  if key = int_of_char 'i' then BackBranch
  else parse_key_branch_mode key

let string_of_cmd cmd =
  match cmd with
  | Stage -> "stage"
  | Unstage -> "unstage"
  | NavUp _ -> "navup"
  | NavDown _ -> "navdown"
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
  | BranchMenu -> "branch"
  | CheckoutBranchPrompt -> "checkout branch prompt"
  | CreateBranchPrompt -> "create branch prompt"
  | DeleteBranchPrompt -> "delete branch prompt"
  | CheckoutBranch _ -> "checkout branch"
  | CreateBranch _ -> "create branch"
  | DeleteBranch _ -> "delete branch"
  | PushElsewhere _ -> "push"
  | Clear -> "clear"
  | Quit -> "quit"
  | NormalTutorial -> "tutorial"
  | DiffTutorial -> "tutorial"
  | PullTutorial -> "tutorial"
  | PushTutorial -> "tutorial"
  | BranchTutorial -> "tutorial"
  | BackNormal -> "back"
  | BackDiff -> "back"
  | BackPull -> "back"
  | BackPush -> "back"
  | BackBranch -> "back"
  | Nop -> "nop"
