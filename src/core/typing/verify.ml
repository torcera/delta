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

let rec infer_expr (env : ty Env.StringMap.t) (expr : Parsed_ast.expr) : ty =
  match expr with
  | Parsed_ast.Int _ -> TInt
  | Parsed_ast.Float _ -> TFloat
  | Parsed_ast.Bool _ -> TBool
  | Parsed_ast.Char _ -> TChar
  | Parsed_ast.String _ -> TString
  | Parsed_ast.Void -> TVoid
  | Parsed_ast.Identifier name -> (
      try Env.lookup env name
      with Not_found -> failwith ("Variable " ^ name ^ " not found"))
  | Parsed_ast.UnOp (op, e) ->
      let e_ty = infer_expr env e in
      let un_op_ty = match op with UnOpNegate -> e_ty | UnOpNot -> TBool in
      un_op_ty
  | Parsed_ast.BinOp (op, e1, e2) ->
      let e1_ty = infer_expr env e1 in
      let e2_ty = infer_expr env e2 in
      ignore
        (if e1_ty = e2_ty then TBool
         else failwith "Operands of BinOp must be of same type");
      let bin_op_ty =
        match op with
        | BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpRem -> e1_ty
        | BinOpLessThan | BinOpGreaterThan | BinOpLessThanEqual
        | BinOpGreaterThanEqual | BinOpAnd | BinOpOr | BinOpEqual
        | BinOpNotEqual ->
            TBool
      in
      bin_op_ty
  | Parsed_ast.Assign (name, expr) ->
      let tvar =
        try Env.lookup env name
        with Not_found -> failwith ("Variable " ^ name ^ " not found")
      in
      let texpr = infer_expr env expr in
      if tvar = texpr then tvar else failwith "Type mismatch in assignment"
  | Parsed_ast.Call (name, args) -> (
      let func =
        try Env.lookup env name
        with Not_found -> failwith ("Function " ^ name ^ " not found")
      in
      match func with
      | TFunction (param_types, return_type) ->
          if List.length args = List.length param_types then
            let arg_types = List.map (infer_expr env) args in
            if List.for_all2 ( = ) arg_types param_types then return_type
            else failwith ("Parameter type mismatch in call to " ^ name)
          else failwith "Number of arguments does not match function signature"
      | _ -> failwith (name ^ " is not a function"))

let rec check_stmt (env : ty Env.StringMap.t) (ret_ty : ty)
    (stmt : Parsed_ast.stmt) =
  match stmt with
  | Parsed_ast.ExprStmt e -> ignore (infer_expr env e)
  | Parsed_ast.PrintStmt e -> ignore (infer_expr env e)
  | Parsed_ast.IfStmt (cond_expr, then_stmt, else_stmt) ->
      let t = infer_expr env cond_expr in
      if not (t = TBool) then
        raise (TypeError "Condition expression must be of type bool");
      check_stmt env ret_ty then_stmt;
      check_stmt env ret_ty else_stmt
  | Parsed_ast.WhileStmt (cond_expr, body) ->
      let t = infer_expr env cond_expr in
      if not (t = TBool) then
        raise (TypeError "Condition expression must be of type bool");
      check_stmt env ret_ty body
  | Parsed_ast.Block decls -> ignore (check_block env ret_ty decls)
  | Parsed_ast.ReturnStmt e ->
      let t = infer_expr env e in
      if not (t = ret_ty) then raise (TypeError "Return type mismatch")
  | Parsed_ast.Import _ ->
      raise (TypeError "Import statement not allowed in block")

and check_block (env : ty Env.StringMap.t) (ret_ty : ty)
    (decls : Parsed_ast.decl list) =
  List.fold_left
    (fun env decl ->
      match decl with
      | Parsed_ast.VarDecl (name, expr) ->
          let t = infer_expr env expr in
          Env.extend env name t
      | Parsed_ast.Statement stmt ->
          check_stmt env ret_ty stmt;
          env
      | Parsed_ast.FuncDecl _ ->
          raise (TypeError "Function declaration not allowed in block"))
    env decls

let build_func_declarations (decls : Parsed_ast.decl list) =
  List.fold_left
    (fun env decl ->
      match decl with
      | Parsed_ast.FuncDecl (name, params, _, ret_ty) ->
          let sig_ = TFunction (List.map snd params, ret_ty) in
          Env.extend env name sig_
      | Parsed_ast.VarDecl (name, expr) ->
          let t = infer_expr env expr in
          Env.extend env name t
      | _ -> env)
    Env.empty_env decls

let type_check_program (Parsed_ast.Program decls) =
  let env = build_func_declarations decls in
  List.iter
    (function
      | Parsed_ast.FuncDecl (_name, params, body, ret_ty) ->
          let param_env =
            List.fold_left
              (fun env (name, ty) -> Env.extend env name ty)
              env params
          in
          ignore (check_block param_env ret_ty body)
      | Parsed_ast.VarDecl (_name, expr) -> ignore (infer_expr env expr)
      | Parsed_ast.Statement stmt -> check_stmt env TVoid stmt)
    decls
