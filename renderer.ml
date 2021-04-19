open Curses

let check_err err =
  if err = true then () else raise Command.Program_terminate

let init_colors () =
  check_err (Curses.start_color ());
  check_err (Curses.init_pair 1 Curses.Color.red Curses.Color.black);
  check_err (Curses.init_pair 2 Curses.Color.green Curses.Color.black);
  check_err (Curses.init_pair 3 Curses.Color.yellow Curses.Color.black);
  check_err (Curses.init_pair 4 Curses.Color.blue Curses.Color.black);
  check_err (Curses.init_pair 5 Curses.Color.magenta Curses.Color.black);
  check_err (Curses.init_pair 6 Curses.Color.cyan Curses.Color.black);
  check_err (Curses.init_pair 7 Curses.Color.white Curses.Color.black);
  check_err (Curses.init_pair 8 Curses.Color.black Curses.Color.red);
  check_err (Curses.init_pair 9 Curses.Color.black Curses.Color.green);
  check_err (Curses.init_pair 10 Curses.Color.black Curses.Color.yellow);
  check_err (Curses.init_pair 11 Curses.Color.black Curses.Color.blue);
  check_err
    (Curses.init_pair 12 Curses.Color.black Curses.Color.magenta);
  check_err (Curses.init_pair 13 Curses.Color.black Curses.Color.cyan);
  check_err (Curses.init_pair 14 Curses.Color.black Curses.Color.white)

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

let init () : Curses.window =
  let win = Curses.initscr () in
  init_colors ();
  check_err (Curses.cbreak ());
  check_err (Curses.noecho ());
  check_err (Curses.curs_set 0);
  Curses.clear ();
  win

let cleanup () =
  check_err (Curses.curs_set 1);
  check_err (Curses.echo ());
  check_err (Curses.nocbreak ());
  Curses.endwin ()

let cursor_nextline win =
  let yx = Curses.getyx win in
  let new_yx = (fst yx + 1, 0) in
  check_err (Curses.wmove win (fst new_yx) (snd new_yx))

let cursor_reset win = check_err (Curses.wmove win 0 0)

let render_user_line win (line : State.printable) =
  enable_color "cyan_back";
  try
    let fst_char = String.sub line.text 0 1 in
    let rest = String.sub line.text 1 (String.length line.text - 1) in
    check_err (Curses.waddstr win fst_char);
    disable_color "cyan_back";
    enable_color line.color;
    check_err (Curses.waddstr win rest);
    disable_color line.color;
    cursor_nextline win
  with _ ->
    enable_color "cyan_back";
    check_err (Curses.waddstr win line.text);
    disable_color "cyan_back"

let render_line win user_curs_y (line : State.printable) =
  let yx = Curses.getyx win in
  if fst yx = user_curs_y then render_user_line win line
  else (
    enable_color line.color;
    check_err (Curses.waddstr win line.text);
    disable_color line.color;
    cursor_nextline win)

let render_lines win lines user_curs_y =
  List.iter (render_line win user_curs_y) lines

let render state win =
  let lines = State.printable_of_state state in
  render_lines win lines (State.get_user_curs_y state);
  check_err (Curses.wrefresh win);
  check_err (Curses.wmove win 0 0)
