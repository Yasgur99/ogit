open Curses 

let init_colors () =
  Command.check_err (Curses.start_color ());
  Command.check_err
    (Curses.init_pair 1 Curses.Color.red Curses.Color.black);
  Command.check_err
    (Curses.init_pair 2 Curses.Color.green Curses.Color.black);
  Command.check_err
    (Curses.init_pair 3 Curses.Color.yellow Curses.Color.black);
  Command.check_err
    (Curses.init_pair 4 Curses.Color.blue Curses.Color.black);
  Command.check_err
    (Curses.init_pair 5 Curses.Color.magenta Curses.Color.black);
  Command.check_err
    (Curses.init_pair 6 Curses.Color.cyan Curses.Color.black);
  Command.check_err
    (Curses.init_pair 7 Curses.Color.white Curses.Color.black);
  Command.check_err
    (Curses.init_pair 8 Curses.Color.black Curses.Color.red);
  Command.check_err
    (Curses.init_pair 9 Curses.Color.black Curses.Color.green);
  Command.check_err
    (Curses.init_pair 10 Curses.Color.black Curses.Color.yellow);
  Command.check_err
    (Curses.init_pair 11 Curses.Color.black Curses.Color.blue);
  Command.check_err
    (Curses.init_pair 12 Curses.Color.black Curses.Color.magenta);
  Command.check_err
    (Curses.init_pair 13 Curses.Color.black Curses.Color.cyan);
  Command.check_err
    (Curses.init_pair 14 Curses.Color.black Curses.Color.white)

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
  Command.check_err (Curses.cbreak ());
  Command.check_err (Curses.noecho ());
  Command.check_err (Curses.curs_set 0);
  Curses.clear ();
  win

let cleanup () =
  Command.check_err (Curses.curs_set 1);
  Command.check_err (Curses.echo ());
  Command.check_err (Curses.nocbreak ());
  Curses.endwin ()

let cursor_nextline win =
  let yx = Curses.getyx win in
  let new_yx = ((fst yx + 1) , 0) in 
  Command.check_err (Curses.wmove win (fst new_yx) (snd new_yx))

let render_line win (line : State.printable)=
  enable_color line.color;
  Command.check_err (Curses.waddstr win line.text);
  disable_color line.color;
  cursor_nextline win 

let render_lines win lines =
  List.iter (render_line win) lines

let render state win = 
  let lines = State.printable_of_state state in
  render_lines win lines;
  Command.check_err (Curses.wrefresh win)

  
