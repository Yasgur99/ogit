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

let render state win = ()
