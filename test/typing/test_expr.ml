open Parsing
open Typing
open Alcotest
open Syntax.Ast_types

let type_and_check_expr ~name ~input ~expected ~env ~type_env =
  let typed_expr, _expr_ty = Annotate.annotate_expr env type_env input in
  let testable =
    testable (Fmt.of_to_string Typed_ast.show_expr) Typed_ast.equal_expr
  in
  check testable name expected typed_expr

let test_int_expr () =
  type_and_check_expr ~name:"Int" ~input:(Parsed_ast.Int 1)
    ~expected:(Typed_ast.Int 1) ~env:Annotate.Env.empty_env
    ~type_env:Annotate.TypeEnv.empty_env

let test_float_expr () =
  type_and_check_expr ~name:"Float" ~input:(Parsed_ast.Float 1.0)
    ~expected:(Typed_ast.Float 1.0) ~env:Annotate.Env.empty_env
    ~type_env:Annotate.TypeEnv.empty_env

let test_bool_expr () =
  type_and_check_expr ~name:"Bool" ~input:(Parsed_ast.Bool true)
    ~expected:(Typed_ast.Bool true) ~env:Annotate.Env.empty_env
    ~type_env:Annotate.TypeEnv.empty_env

let test_char_expr () =
  type_and_check_expr ~name:"Char" ~input:(Parsed_ast.Char 'a')
    ~expected:(Typed_ast.Char 'a') ~env:Annotate.Env.empty_env
    ~type_env:Annotate.TypeEnv.empty_env

let test_string_expr () =
  type_and_check_expr ~name:"String" ~input:(Parsed_ast.String "hello")
    ~expected:(Typed_ast.String "hello") ~env:Annotate.Env.empty_env
    ~type_env:Annotate.TypeEnv.empty_env

let test_void_expr () =
  type_and_check_expr ~name:"Void" ~input:Parsed_ast.Void
    ~expected:Typed_ast.Void ~env:Annotate.Env.empty_env
    ~type_env:Annotate.TypeEnv.empty_env

let test_identifier () =
  let env = Annotate.Env.empty_env in
  let env' = Annotate.Env.extend env "x" TInt in
  type_and_check_expr ~name:"Identifier" ~input:(Parsed_ast.Identifier "x")
    ~expected:(Typed_ast.Identifier ("x", TInt))
    ~env:env' ~type_env:Annotate.TypeEnv.empty_env

let test_un_op () =
  type_and_check_expr ~name:"UnOp"
    ~input:(Parsed_ast.UnOp (Syntax.Ast_types.UnOpNegate, Parsed_ast.Int 1))
    ~expected:(Typed_ast.UnOp (Syntax.Ast_types.UnOpNegate, Typed_ast.Int 1))
    ~env:Annotate.Env.empty_env ~type_env:Annotate.TypeEnv.empty_env

let test_bin_op () =
  type_and_check_expr ~name:"BinOp"
    ~input:
      (Parsed_ast.BinOp
         (Syntax.Ast_types.BinOpPlus, Parsed_ast.Int 1, Parsed_ast.Int 2))
    ~expected:
      (Typed_ast.BinOp
         (Syntax.Ast_types.BinOpPlus, Typed_ast.Int 1, Typed_ast.Int 2))
    ~env:Annotate.Env.empty_env ~type_env:Annotate.TypeEnv.empty_env

let test_assign () =
  let env = Annotate.Env.empty_env in
  let env' = Annotate.Env.extend env "x" TInt in
  type_and_check_expr ~name:"Assign"
    ~input:(Parsed_ast.Assign ("x", Parsed_ast.Int 1))
    ~expected:(Typed_ast.Assign ("x", Typed_ast.Int 1))
    ~env:env' ~type_env:Annotate.TypeEnv.empty_env

let test_call () =
  let env = Annotate.Env.empty_env in
  let env' = Annotate.Env.extend env "function" (TFunction ([ TInt ], TInt)) in
  type_and_check_expr ~name:"Call"
    ~input:
      (Parsed_ast.Call (Parsed_ast.Identifier "function", [ Parsed_ast.Int 2 ]))
    ~expected:
      (Typed_ast.Call
         ( Typed_ast.Identifier ("function", TFunction ([ TInt ], TInt)),
           [ Typed_ast.Int 2 ],
           TInt ))
    ~env:env' ~type_env:Annotate.TypeEnv.empty_env

let test_struct_init () =
  let type_env = Annotate.TypeEnv.empty_env in
  let type_env' =
    Annotate.TypeEnv.extend type_env "Point"
      (StructDef [ ("x", TInt); ("y", TInt) ])
  in
  type_and_check_expr ~name:"StructInit"
    ~input:
      (Parsed_ast.StructInit
         ("Point", [ ("x", Parsed_ast.Int 1); ("y", Parsed_ast.Int 2) ]))
    ~expected:
      (Typed_ast.StructInit
         ( "Point",
           [ ("x", Typed_ast.Int 1, TInt); ("y", Typed_ast.Int 2, TInt) ] ))
    ~env:Annotate.Env.empty_env ~type_env:type_env'

let test_array_init () =
  type_and_check_expr ~name:"ArrayInit"
    ~input:(Parsed_ast.ArrayInit [ Parsed_ast.Int 1; Parsed_ast.Int 2 ])
    ~expected:
      (Typed_ast.ArrayInit ([ Typed_ast.Int 1; Typed_ast.Int 2 ], TArray TInt))
    ~env:Annotate.Env.empty_env ~type_env:Annotate.TypeEnv.empty_env

let test_field_access () =
  let env = Annotate.Env.empty_env in
  let env' = Annotate.Env.extend env "point" (TNamed "Point") in
  let type_env = Annotate.TypeEnv.empty_env in
  let type_env' =
    Annotate.TypeEnv.extend type_env "Point"
      (StructDef [ ("x", TInt); ("y", TInt) ])
  in
  type_and_check_expr ~name:"FieldAccess"
    ~input:(Parsed_ast.FieldAccess (Parsed_ast.Identifier "point", "x"))
    ~expected:
      (Typed_ast.FieldAccess
         ( Typed_ast.Identifier ("point", TNamed "Point"),
           "x",
           0,
           TNamed "Point",
           TInt ))
    ~env:env' ~type_env:type_env'

let test_array_access () =
  let env = Annotate.Env.empty_env in
  let env' = Annotate.Env.extend env "array" (TArray TInt) in
  type_and_check_expr ~name:"ArrayAccess"
    ~input:
      (Parsed_ast.ArrayAccess (Parsed_ast.Identifier "array", Parsed_ast.Int 0))
    ~expected:
      (Typed_ast.ArrayAccess
         ( Typed_ast.Identifier ("array", TArray TInt),
           Typed_ast.Int 0,
           TArray TInt,
           TInt ))
    ~env:env' ~type_env:Annotate.TypeEnv.empty_env

let tests =
  [
    test_case "Int" `Quick test_int_expr;
    test_case "Float" `Quick test_float_expr;
    test_case "Bool" `Quick test_bool_expr;
    test_case "Char" `Quick test_char_expr;
    test_case "String" `Quick test_string_expr;
    test_case "Void" `Quick test_void_expr;
    test_case "Identifier" `Quick test_identifier;
    test_case "UnOp" `Quick test_un_op;
    test_case "BinOp" `Quick test_bin_op;
    test_case "Assign" `Quick test_assign;
    test_case "Call" `Quick test_call;
    test_case "StructInit" `Quick test_struct_init;
    test_case "ArrayInit" `Quick test_array_init;
    test_case "FieldAccess" `Quick test_field_access;
    test_case "ArrayAccess" `Quick test_array_access;
  ]
