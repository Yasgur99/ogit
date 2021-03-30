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

let init_curses () : Curses.window =
  let win = Curses.initscr () in
  init_colors ();
  Command.check_err (Curses.cbreak ());
  Command.check_err (Curses.noecho ());
  Command.check_err (Curses.curs_set 0);
  Curses.clear ();
  win

let run win () =
  Command.check_err (Curses.waddstr win "Welcome to ogit");
  Command.check_err (Curses.wrefresh win);

  while true do
    let key = Curses.wgetch win in
    let cmd = Command.parse_key key in
    Command.exec cmd win;
    Command.check_err (Curses.wrefresh win)
  done

let cleanup () =
  Command.check_err (Curses.curs_set 1);
  Command.check_err (Curses.echo ());
  Command.check_err (Curses.nocbreak ());
  Curses.endwin ()

let () =
  try
    let win = init_curses () in
    run win ()
  with Command.Program_terminate ->
    cleanup ();
    exit 0
