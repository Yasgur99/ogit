let () = 
  let c = Command.parse Sys.argv in
    Command.execute c
