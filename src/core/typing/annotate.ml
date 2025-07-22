open Parsing
open Syntax.Ast_types

exception TypeError of string

module Env = struct
  module StringMap = Map.Make (String)

  type env = ty StringMap.t

  let empty_env = StringMap.empty
  let extend env name ty = StringMap.add name ty env
  let lookup env name = StringMap.find name env

  let string_of_env env =
    StringMap.fold
      (fun name ty acc -> name ^ ": " ^ string_of_type ty ^ "\n" ^ acc)
      env ""
end

let rec annotate_expr (env : ty Env.StringMap.t) (expr : Parsed_ast.expr) :
    Typed_ast.expr * ty =
  match expr with
  | Parsed_ast.Int i -> (Typed_ast.Int i, TInt)
  | Parsed_ast.Float f -> (Typed_ast.Float f, TFloat)
  | Parsed_ast.Bool b -> (Typed_ast.Bool b, TBool)
  | Parsed_ast.Char c -> (Typed_ast.Char c, TChar)
  | Parsed_ast.String s -> (Typed_ast.String s, TString)
  | Parsed_ast.Void -> (Typed_ast.Void, TVoid)
  | Parsed_ast.Identifier name -> (
      try
        let ty = Env.lookup env name in
        (Typed_ast.Identifier (name, ty), ty)
      with Not_found -> raise (TypeError ("Variable " ^ name ^ " not found")))
  | Parsed_ast.UnOp (op, e) ->
      let typed_expr, expr_ty = annotate_expr env e in
      let un_op_ty = match op with UnOpNegate -> expr_ty | UnOpNot -> TBool in
      (Typed_ast.UnOp (op, typed_expr), un_op_ty)
  | Parsed_ast.BinOp (op, lhs_expr, rhs_expr) ->
      let typed_lhs, lhs_ty = annotate_expr env lhs_expr in
      let typed_rhs, rhs_ty = annotate_expr env rhs_expr in
      ignore
        (if lhs_ty = rhs_ty then TBool
         else raise (TypeError "Operands of BinOp must be of same type"));
      let bin_op_ty =
        match op with
        | BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpRem -> lhs_ty
        | BinOpLessThan | BinOpGreaterThan | BinOpLessThanEqual
        | BinOpGreaterThanEqual | BinOpAnd | BinOpOr | BinOpEqual
        | BinOpNotEqual ->
            TBool
      in
      (Typed_ast.BinOp (op, typed_lhs, typed_rhs), bin_op_ty)
  | Parsed_ast.Assign (name, expr) ->
      let var_ty =
        try Env.lookup env name
        with Not_found ->
          raise (TypeError ("Variable " ^ name ^ " not found"))
      in
      let typed_expr, expr_ty = annotate_expr env expr in
      if var_ty = expr_ty then (Typed_ast.Assign (name, typed_expr), var_ty)
      else raise (TypeError "Type mismatch in assignment")
  | Parsed_ast.Call (name, args) -> (
      let func =
        try Env.lookup env name
        with Not_found ->
          raise (TypeError ("Function " ^ name ^ " not found"))
      in
      match func with
      | TFunction (param_types, return_type) ->
          if List.length args = List.length param_types then
            let args, arg_types =
              List.split
                (List.map
                   (fun e ->
                     let e, e_ty = annotate_expr env e in
                     (e, e_ty))
                   args)
            in
            if List.for_all2 ( = ) arg_types param_types then
              ( Typed_ast.Call (name, args, TFunction (param_types, return_type)),
                return_type )
            else raise (TypeError "Arguments of Call must be of same type")
          else
            raise
              (TypeError "Number of arguments does not match function signature")
      | _ -> raise (TypeError (name ^ " is not a function")))
  | Parsed_ast.StructInit (name, fields) ->
      let struct_ty =
        try Env.lookup env name
        with Not_found -> raise (TypeError ("Struct " ^ name ^ " not found"))
      in
      let expected_fields =
        match struct_ty with
        | TStruct (_, fields) -> fields
        | _ -> raise (TypeError (name ^ " is not a struct type"))
      in
      let typed_fields =
        List.map
          (fun (field_name, expr) ->
            match List.assoc_opt field_name expected_fields with
            | None ->
                raise
                  (TypeError
                     ("Field " ^ field_name ^ " not found in struct " ^ name))
            | Some expected_ty ->
                let typed_expr, expr_ty = annotate_expr env expr in
                if expr_ty = expected_ty then (field_name, typed_expr, expr_ty)
                else
                  raise
                    (TypeError
                       ("Type mismatch in field " ^ field_name ^ ": expected "
                      ^ show_ty expected_ty ^ ", got " ^ show_ty expr_ty)))
          fields
      in
      (* Optional: ensure no extra or missing fields *)
      let field_names_provided = List.map fst fields in
      let expected_field_names = List.map fst expected_fields in
      if
        List.sort compare field_names_provided
        = List.sort compare expected_field_names
      then (Typed_ast.StructInit (name, typed_fields), struct_ty)
      else
        raise (TypeError "Struct initialization does not match expected fields")
| Parsed_ast.FieldAccess (struct_expr, field_name) ->
    let typed_struct_expr, struct_expr_ty = annotate_expr env struct_expr in
    (match struct_expr_ty with
    | TStruct (_, fields) -> (
        match List.assoc_opt field_name fields with
        | Some field_ty ->
            (* find index of field_name in fields *)
            let field_index =
              let rec find_index lst idx =
                match lst with
                | [] -> raise (TypeError ("Field " ^ field_name ^ " not found in struct type"))
                | (fname, _) :: rest ->
                    if fname = field_name then idx else find_index rest (idx + 1)
              in
              find_index fields 0
            in
            ( Typed_ast.FieldAccess (typed_struct_expr, field_name, field_index, struct_expr_ty, field_ty),
              field_ty )
        | None ->
            raise (TypeError ("Field " ^ field_name ^ " not found in struct type")))
    | _ ->
        raise (TypeError ("Attempted field access on non-struct type")))

let rec annotate_stmt (env : ty Env.StringMap.t) (ret_ty : ty)
    (stmt : Parsed_ast.stmt) : Typed_ast.stmt =
  match stmt with
  | Parsed_ast.ExprStmt e ->
      let typed_expr, _ = annotate_expr env e in
      Typed_ast.ExprStmt typed_expr
  | Parsed_ast.IfStmt (cond_expr, then_stmt, else_stmt) ->
      let typed_cond_expr, cond_expr_ty = annotate_expr env cond_expr in
      if not (cond_expr_ty = TBool) then
        raise (TypeError "Condition expression must be of type bool");
      let typed_then_stmt = annotate_stmt env ret_ty then_stmt in
      let typed_else_stmt = annotate_stmt env ret_ty else_stmt in
      Typed_ast.IfStmt (typed_cond_expr, typed_then_stmt, typed_else_stmt)
  | Parsed_ast.WhileStmt (cond_expr, body) ->
      let typed_cond_expr, cond_expr_ty = annotate_expr env cond_expr in
      if not (cond_expr_ty = TBool) then
        raise (TypeError "Condition expression must be of type bool");
      let typed_body_stmt = annotate_stmt env ret_ty body in
      Typed_ast.WhileStmt (typed_cond_expr, typed_body_stmt)
  | Parsed_ast.Block decls ->
      let typed_decls, _ = check_block env ret_ty decls in
      Typed_ast.Block typed_decls
  | Parsed_ast.ReturnStmt e ->
      let typed_return_expr, return_expr_ty = annotate_expr env e in
      if not (return_expr_ty = ret_ty) then
        raise (TypeError "Return type mismatch");
      Typed_ast.ReturnStmt typed_return_expr
  | Parsed_ast.Import _ ->
      raise (TypeError "Import statement not allowed in block")

and check_block (env : ty Env.StringMap.t) (ret_ty : ty)
    (decls : Parsed_ast.decl list) : Typed_ast.decl list * ty Env.StringMap.t =
  let typed_decls, env' =
    List.fold_left
      (fun (typed_decls, env) decl ->
        match decl with
        | Parsed_ast.VarDecl (name, expr) ->
            let typed_expr, expr_ty = annotate_expr env expr in
            let env' = Env.extend env name expr_ty in
            (Typed_ast.VarDecl (name, typed_expr, expr_ty) :: typed_decls, env')
        | Parsed_ast.Statement stmt ->
            let typed_stmt = annotate_stmt env ret_ty stmt in
            (Typed_ast.Statement typed_stmt :: typed_decls, env)
        | Parsed_ast.StructDecl (name, fields) ->
            let env' = Env.extend env name (TStruct (name, fields)) in
            (Typed_ast.StructDecl (name, fields) :: typed_decls, env')
        | Parsed_ast.FuncDecl _ | Parsed_ast.ExternDecl _ ->
            raise (TypeError "Function declaration not allowed in block"))
      ([], env) decls
  in
  (List.rev typed_decls, env')

let build_top_level_declarations (decls : Parsed_ast.decl list) :
    ty Env.StringMap.t =
  List.fold_left
    (fun env decl ->
      match decl with
      | Parsed_ast.FuncDecl (name, params, _, ret_ty) ->
          let sig_ = TFunction (List.map snd params, ret_ty) in
          Env.extend env name sig_
      | Parsed_ast.ExternDecl (name, params, ret_ty) ->
          let sig_ = TFunction (List.map snd params, ret_ty) in
          Env.extend env name sig_
      | Parsed_ast.VarDecl (name, expr) ->
          let _typed_expr, expr_ty = annotate_expr env expr in
          Env.extend env name expr_ty
      | Parsed_ast.StructDecl (name, fields) ->
          Env.extend env name (TStruct (name, fields))
      | _ -> env)
    Env.empty_env decls

let annotate_program (Parsed_ast.Program decls) : Typed_ast.program =
  let env = build_top_level_declarations decls in
  let typed_decls =
    List.map
      (function
        | Parsed_ast.FuncDecl (name, params, body, ret_ty) ->
            let param_env =
              List.fold_left
                (fun env (param_name, param_ty) ->
                  Env.extend env param_name param_ty)
                env params
            in
            let typed_body, _ = check_block param_env ret_ty body in
            Typed_ast.FuncDecl (name, params, typed_body, ret_ty)
        | Parsed_ast.ExternDecl (name, params, ret_ty) ->
            Typed_ast.ExternDecl (name, params, ret_ty)
        | Parsed_ast.VarDecl (name, expr) ->
            let typed_expr, expr_ty = annotate_expr env expr in
            Typed_ast.VarDecl (name, typed_expr, expr_ty)
        | Parsed_ast.Statement stmt ->
            let typed_stmt = annotate_stmt env TVoid stmt in
            Typed_ast.Statement typed_stmt
        | Parsed_ast.StructDecl (name, fields) ->
            Typed_ast.StructDecl (name, fields))
      decls
  in
  Typed_ast.Program typed_decls
