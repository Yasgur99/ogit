type file_name = string

type commit_msg = string

type branch_name = string

type key = int

type t =
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

let check_err err = if err = true then () else raise Program_terminate

let parse_key key =
  if key = int_of_char 's' then Status
  else if key = int_of_char 'q' then Quit
  else raise (Invalid_cmd "Invalid command")

let incr_e1 pair =
  pair := (fst !pair + 1, snd !pair);
  pair

let get_color color =
  let colors =
    [
      "red";
      "green";
      "yellow";
      "blue";
      "magenta";
      "cyan";
      "white";
      "red_back";
      "green_back";
      "yellow_back";
      "blue_back";
      "magenta_back";
      "cyan_back";
      "white_back";
    ]
  in
  let rec r_color color n =
    if List.nth colors n = color then n + 1 else r_color color (n + 1)
  in
  r_color color 0

let enable_color color =
  Curses.attron (Curses.A.color_pair (get_color color))

let disable_color color =
  Curses.attroff (Curses.A.color_pair (get_color color))

let exec_status win =
  let status = Porcelain.status () in
  let untracked = Porcelain.get_untracked status in
  let tracked = Porcelain.get_tracked status in
  let staged = Porcelain.get_staged status in

  let yx = ref (Curses.getyx win) in
  enable_color "yellow";
  check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 "untracked: ");
  disable_color "yellow";
  enable_color "red_back";
  List.iter
    (fun f -> check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 f))
    untracked;
  disable_color "red_back";
  enable_color "yellow";
  check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 "tracked: ");
  disable_color "yellow";
  enable_color "cyan_back";
  List.iter
    (fun f -> check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 f))
    tracked;
  disable_color "cyan_back";
  enable_color "yellow";
  check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 "staged: ");
  disable_color "yellow";
  enable_color "green_back";
  List.iter
    (fun f -> check_err (Curses.mvwaddstr win (fst !(incr_e1 yx)) 1 f))
    staged;
  disable_color "green_back";
  ()

let string_of_cmd cmd =
  match cmd with
  | Add _ -> "add"
  | Remove _ -> "remove"
  | Commit _ -> "commit"
  | Branch _ -> "branch"
  | Checkout _ -> "checkout"
  | Fetch -> "fetch"
  | Push -> "push"
  | Pull -> "pull"
  | Status -> "status"
  | Init -> "init"
  | Quit -> "quit"
