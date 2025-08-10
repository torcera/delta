open Parsing
open Resolution
open Lexing
open Typing
open Codegen

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Printf.sprintf "line %d, column %d" line col

let compile source verbose =
  print_endline "[*] Lexing...";
  let lexbuf = Lexing.from_string source in
  try
    print_endline "[*] Parsing...";
    let parsed_ast = Parser.program Lexer.read_token lexbuf in
    if verbose then print_endline (Parsed_ast.show_program parsed_ast);

    print_endline "[*] Resolving imports...";
    let parsed_ast = Resolve.resolve_imports parsed_ast in
    if verbose then print_endline (Parsed_ast.show_program parsed_ast);

    print_endline "[*] Typechecking...";
    let typed_ast = Annotate.annotate_program parsed_ast in
    if verbose then print_endline (Typed_ast.show_program typed_ast);

    print_endline "[*] Compiling...";
    ignore (Direct.codegen_program typed_ast);
    Direct.dump_ir ();
    Direct.save_ir_to_file "llvm_bin/output.ll"
  with
  | Lexer.SyntaxError msg ->
      Printf.eprintf "Lexer error at %s: %s\n" (print_position lexbuf) msg
  | Parser.Error ->
      Printf.eprintf "Parser error at %s\n" (print_position lexbuf)
  | Annotate.TypeError msg -> Printf.eprintf "Type error at: %s\n" msg
  | Direct.LLVMError msg -> Printf.eprintf "LLVM error: %s\n" msg

let repl () =
  let rec loop () =
    print_string "> ";
    let line = read_line () in
    compile line false;
    loop ()
  in
  loop ()
