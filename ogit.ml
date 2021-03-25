let init_curses () =
  let win = Curses.initscr () in
  let r = Curses.cbreak in
  let r = Curses.noecho in
  let r = Curses.curs_set 0 in
  Curses.clear ();
  win


let enable_color color =
  Curses.attron color

let disable_color color =
  Curses.attroff color

let () = 
(*    let args = Array.sub Sys.argv 1 (Array.length Sys.argv -1) in
    let result = Plumbing.git args in
    List.iter print_endline (Plumbing.get_out result)  *)

    (*try
      run;
    with
      cleanup; *)

    let win = init_curses in

    (* Setup useful constants *)
    let max_y, max_x = Curses.getmaxyx win in 
    let red = Curses.init_pair Curses.Colors.red Curses.Colors.red in
    let green = Curses.init_pair Curses.Colors.green Curses.Colors.green in
    
    while true do
      let key = Curses.getch in
      let r = Curses.waddstr win "Welcome to ogit" in
      let r = Curses.wrefresh win;
    done
    
        (* Clean up curses *)
    let r = Curses.curs_set 1 in
    let r = Curses.echo in
    let r = Curses.nocbreak in
    Curses.endwin () 
