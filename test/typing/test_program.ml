open Parsing
open Typing
open Alcotest
open Syntax.Ast_types

let type_and_check_program ~name ~input ~expected =
  let typed_program = Annotate.annotate_program input in
  let testable =
    testable (Fmt.of_to_string Typed_ast.show_program) Typed_ast.equal_program
  in
  check testable name expected typed_program

let test_var_decl () =
  type_and_check_program ~name:"VarDecl"
    ~input:(Parsed_ast.Program [ Parsed_ast.VarDecl ("x", Parsed_ast.Int 1) ])
    ~expected:
      (Typed_ast.Program [ Typed_ast.VarDecl ("x", Typed_ast.Int 1, TInt) ])

let test_stmt_decl () =
  type_and_check_program ~name:"StmtDecl"
    ~input:
      (Parsed_ast.Program
         [ Parsed_ast.Statement (Parsed_ast.ExprStmt (Parsed_ast.Int 1)) ])
    ~expected:
      (Typed_ast.Program
         [ Typed_ast.Statement (Typed_ast.ExprStmt (Typed_ast.Int 1)) ])

let test_func_decl () =
  type_and_check_program ~name:"FuncDecl"
    ~input:
      (Parsed_ast.Program
         [
           Parsed_ast.FuncDecl
             ( "add",
               [ ("x", TInt); ("y", TInt) ],
               [
                 Parsed_ast.Statement (Parsed_ast.ReturnStmt (Parsed_ast.Int 1));
               ],
               TInt );
         ])
    ~expected:
      (Typed_ast.Program
         [
           Typed_ast.FuncDecl
             ( "add",
               [ ("x", TInt); ("y", TInt) ],
               [ Typed_ast.Statement (Typed_ast.ReturnStmt (Typed_ast.Int 1)) ],
               TInt );
         ])

let test_struct_decl () =
  type_and_check_program ~name:"StructDecl"
    ~input:
      (Parsed_ast.Program
         [ Parsed_ast.StructDecl ("Point", [ ("x", TInt); ("y", TInt) ]) ])
    ~expected:
      (Typed_ast.Program
         [ Typed_ast.StructDecl ("Point", [ ("x", TInt); ("y", TInt) ]) ])

let tests =
  [
    test_case "VarDecl" `Quick test_var_decl;
    test_case "Statement" `Quick test_stmt_decl;
    test_case "FuncDecl" `Quick test_func_decl;
    test_case "StructDecl" `Quick test_struct_decl;
  ]
