type file_name = string

type commit_msg = string

type branch_name = string

type cmd =
  | Add of file_name
  | Remove of file_name
  | Commit of commit_msg 
  | Branch of branch_name
  | Checkout of branch_name
  | Fetch
  | Push
  | Pull
  | Status
  | Init

exception Invalid_cmd of string

exception Empty_cmd of string

let parse (str : string) =
  try
    let cmd_string =
      if String.contains str ' ' then
        String.sub str 0 (String.index str ' ')
      else str
    in
    match cmd_string with
    | "" -> raise (Empty_cmd "Empty command")
    | "add" -> Add (String.sub str 4 (String.length str - 4))
    | "rm" -> Remove (String.sub str 3 (String.length str - 3))
    | "commit" -> Commit (String.sub str 7 (String.length str - 7))
    | "branch" -> Branch (String.sub str 7 (String.length str - 7))
    | "checkout" -> Checkout (String.sub str 9 (String.length str - 9))
    | "push" -> Push
    | "pull" -> Pull
    | "status" -> Status
    | "init" -> Init
    | "fetch" -> Fetch
    | _ -> raise (Invalid_cmd "Invalid command")
  with _ -> raise (Invalid_cmd "Invalid command")

let parse_key k =
  if k = int_of_char 's' then Status
  else raise (Invalid_cmd "Invalid command")

let exec c =
  match c with
  | Add f -> Porcelain.add f
  | Remove f -> Porcelain.remove f
  | Commit msg -> Porcelain.commit msg
  | Branch b -> Porcelain.branch b
  | Checkout b -> Porcelain.checkout b
  | Fetch -> Porcelain.fetch
  | Push -> Porcelain.push
  | Pull -> Porcelain.pull
  | Status -> Porcelain.status
  | Init -> Porcelain.init 

