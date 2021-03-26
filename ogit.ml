(** [check_err f] check_errutes function [f] and raises check_errption if [f] is [true] *)
let check_err err  =
    if err  = true then
      ()
    else
      failwith "error"


let init_curses () : Curses.window =
  let win = Curses.initscr () in
  check_err (Curses.cbreak ());
  check_err (Curses.noecho ());
  check_err (Curses.curs_set 0);
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

    let win = init_curses () in

    (* Setup useful constants *)
(*    let max_y, max_x = Curses.getmaxyx win in 
    let red = Curses.init_pair Curses.Colors.red Curses.Colors.black in
    let green = Curses.init_pair Curses.Colors.green Curses.Colors.black in
 *)   

    check_err (Curses.waddstr win "Welcome to ogit");
    check_err (Curses.wrefresh win);

    while true do (
      let key = Curses.wgetch win in
      let cmd = Command.parse_key key in
      let res = Command.exec cmd in
        ()
    ) done;
    
    (* Clean up curses *)
    check_err (Curses.curs_set 1); 
    check_err (Curses.echo ());
    check_err (Curses.nocbreak ()); 
    Curses.endwin () 
