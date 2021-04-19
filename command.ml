type file_name = string

type commit_msg = string

type branch_name = string

type key = int

type t =
  | Stage of file_name
  | Unstage of file_name
  | Commit of commit_msg
  | Branch of branch_name
  | Checkout of branch_name
  | Fetch
  | Push
  | Pull
  | Status
  | Init
  | Quit
  | NavUp
  | NavDown

exception Invalid_cmd of string

exception Empty_cmd of string

exception Program_terminate

let check_err err = if err = true then () else raise Program_terminate

let parse_key key =
  if key = int_of_char 's' then Status
  else if key = int_of_char 'q' then Quit
  else if key = int_of_char 'u' then NavUp
  else if key = int_of_char 'd' then NavDown
  else raise (Invalid_cmd ("Invalid command " ^ string_of_int key))

let string_of_cmd cmd =
  match cmd with
  | Stage _ -> "stage"
  | Unstage _ -> "unstage"
  | Commit _ -> "commit"
  | Branch _ -> "branch"
  | Checkout _ -> "checkout"
  | Fetch -> "fetch"
  | Push -> "push"
  | Pull -> "pull"
  | Status -> "status"
  | Init -> "init"
  | NavUp -> "up"
  | NavDown -> "down"
  | Quit -> "quit"
