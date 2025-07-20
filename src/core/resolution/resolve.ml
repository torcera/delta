open Parsing

exception ImportError of string

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let rec resolve_imports (program : Parsed_ast.program) : Parsed_ast.program =
  let (Parsed_ast.Program decls) = program in
  let decls =
    List.concat_map
      (function
        | Parsed_ast.Statement (Parsed_ast.Import module_name) ->
            load_module module_name
        | decl -> [ decl ])
      decls
  in
  Program decls

and load_module (module_name : string) : Parsed_ast.decl list =
  match module_name with
  | "math" ->
      let content = read_file "src/stdlib/math/math.dx" in
      let lexbuf = Lexing.from_string content in
      let parsed_ast = Parser.program Lexer.read_token lexbuf in
      let resolved_ast = resolve_imports parsed_ast in
      let decls = match resolved_ast with Parsed_ast.Program decls -> decls in
      decls
  | "net" ->
      let content = read_file "src/stdlib/net/net.dx" in
      let lexbuf = Lexing.from_string content in
      let parsed_ast = Parser.program Lexer.read_token lexbuf in
      let resolved_ast = resolve_imports parsed_ast in
      let decls = match resolved_ast with Parsed_ast.Program decls -> decls in
      decls
  | "socket" ->
      let content = read_file "src/stdlib/net/http.dx" in
      let lexbuf = Lexing.from_string content in
      let parsed_ast = Parser.program Lexer.read_token lexbuf in
      let resolved_ast = resolve_imports parsed_ast in
      let decls = match resolved_ast with Parsed_ast.Program decls -> decls in
      decls
  | _ -> raise (ImportError ("Module not found: " ^ module_name))
