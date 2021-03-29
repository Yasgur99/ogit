type file_name = string

type commit_msg = string

type branch_name = string

type key = int

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
  | Quit

exception Invalid_cmd of string

exception Empty_cmd of string

exception Program_terminate

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

let check_err err = if err = true then () else raise Program_terminate

let parse_key key =
  if key = int_of_char 's' then Status
  else if key = int_of_char 'q' then Quit
  else raise (Invalid_cmd "Invalid command")

let incr_e1 pair =
  pair := (fst !pair + 1, snd !pair);
  pair

let red =
  check_err (Curses.init_pair 1 Curses.Color.red Curses.Color.black)

let green =
  check_err (Curses.init_pair 2 Curses.Color.green Curses.Color.black)

let enable_color color = Curses.attron (Curses.A.color_pair color)

let disable_color color = Curses.attroff (Curses.A.color_pair color)

let exec_status win =
  let status = Porcelain.status () in
  let untracked = Porcelain.get_untracked status in
  let tracked = Porcelain.get_tracked status in
  let staged = Porcelain.get_staged status in

  let yx = ref (Curses.getyx win) in
  check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 "untracked: ");
  enable_color 1;
  List.iter
    (fun f -> check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 f))
    untracked;
  check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 "tracked: ");
  List.iter
    (fun f -> check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 f))
    tracked;
  check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 "staged: ");
  disable_color 1;
  enable_color 2;
  List.iter
    (fun f ->
      check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) (snd !yx) f))
    staged;
  disable_color 2;
  ()

let exec c win =
  match c with
  (*| Add f -> Porcelain.add f | Remove f -> Porcelain.remove f | Commit
    msg -> Porcelain.commit msg | Branch b -> Porcelain.branch b |
    Checkout b -> Porcelain.checkout b | Fetch -> Porcelain.fetch | Push
    -> Porcelain.push | Pull -> Porcelain.pull | Init -> Porcelain.init *)
  | Status -> exec_status win
  | Quit -> raise Program_terminate
  | _ -> failwith "unimplemented command"
