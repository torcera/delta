open Parsing
open Typing
open Alcotest
open Syntax.Ast_types

let type_and_check_stmt ~name ~input ~expected =
  let function_ret_ty = TVoid in
  (* Assume the function that contains the statement returns void *)
  let typed_stmt =
    Annotate.annotate_stmt Annotate.Env.empty_env Annotate.TypeEnv.empty_env
      function_ret_ty input
  in
  let testable =
    testable (Fmt.of_to_string Typed_ast.show_stmt) Typed_ast.equal_stmt
  in
  check testable name expected typed_stmt

let test_expr_stmt () =
  type_and_check_stmt ~name:"ExprStmt"
    ~input:(Parsed_ast.ExprStmt (Parsed_ast.Int 1))
    ~expected:(Typed_ast.ExprStmt (Typed_ast.Int 1))

let test_if_stmt () =
  type_and_check_stmt ~name:"IfStmt"
    ~input:
      (Parsed_ast.IfStmt
         (Parsed_ast.Bool true, Parsed_ast.Block [], Parsed_ast.Block []))
    ~expected:
      (Typed_ast.IfStmt
         (Typed_ast.Bool true, Typed_ast.Block [], Typed_ast.Block []))

let test_while_stmt () =
  type_and_check_stmt ~name:"WhileStmt"
    ~input:(Parsed_ast.WhileStmt (Parsed_ast.Bool true, Parsed_ast.Block []))
    ~expected:(Typed_ast.WhileStmt (Typed_ast.Bool true, Typed_ast.Block []))

let test_return_stmt () =
  type_and_check_stmt ~name:"ReturnStmt"
    ~input:(Parsed_ast.ReturnStmt Parsed_ast.Void)
    ~expected:(Typed_ast.ReturnStmt Typed_ast.Void)

let test_block_stmt () =
  type_and_check_stmt ~name:"Block" ~input:(Parsed_ast.Block [])
    ~expected:(Typed_ast.Block [])

let tests =
  [
    test_case "ExprStmt" `Quick test_expr_stmt;
    test_case "IfStmt" `Quick test_if_stmt;
    test_case "WhileStmt" `Quick test_while_stmt;
    test_case "ReturnStmt" `Quick test_return_stmt;
    test_case "Block" `Quick test_block_stmt;
  ]
