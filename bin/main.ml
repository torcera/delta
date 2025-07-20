open Delta.Compile

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let () =
  if Array.length Sys.argv = 1 then repl ()
  else
    let filename = Sys.argv.(1) in
    let source = read_file filename in
    let verbose = true in
    compile source verbose
