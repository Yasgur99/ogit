let fork_and_execv (exe : string) (args : string array) : string list = 
  let inp, out = Unix.pipe() in
  let pid = Unix.fork () in
  if pid = 0 
  then
    Unix.close inp;
    Unix.dup2 out Unix.stdout;
    Unix.dup2 out Unix.stderr;
    Unix.close inp; 
    Unix.execvp exe args
  else 
    let lines = ref [] in
    Unix.close out;
    try 
      while true do
        lines:= input_line inp :: !lines
      done; !lines
    with End_of_file ->
      close_in

let init (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "init"|] args)

let hash_object (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "hash-object"|] args)

let cat_file (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "cat-file"|] args)

let update_index (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "update-index"|] args)

let write_tree (args : string array) = 
  fork_and_execv "git" (Array.append [|"git"; "write-tree"|] args)

let read_tree (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "read-tree"|] args)

let commit_tree (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "commit-tree"|] args)

let log (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "--no-pager"; "log"|] args)

let add (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "add"|] args)
  
let commit (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "commit"|] args)

let show (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "--no-pager"; "show"|] args)

let diff (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "--no-pager"; "diff"|] args)
 
let status (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "status"|] args)
 
