let init_curses () : Curses.window =
  let win = Curses.initscr () in
  Command.check_err (Curses.cbreak ());
  Command.check_err (Curses.noecho ());
  Command.check_err (Curses.curs_set 0);
  Curses.clear ();
  win 

let run win () = 
    Command.check_err (Curses.waddstr win "Welcome to ogit");
    Command.check_err (Curses.wrefresh win);

    while true do (
      let key = Curses.wgetch win in
      let cmd = Command.parse_key key in
      Command.exec cmd win;
      Command.check_err (Curses.wrefresh win);
    ) done

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

    
