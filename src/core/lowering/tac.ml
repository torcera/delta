open Parsing

let fresh_var =
  let counter = ref 0 in
  fun () ->
    let v = Printf.sprintf "t%d" !counter in
    incr counter;
    v

let fresh_label =
  let counter = ref 0 in
  fun () ->
    let l = Printf.sprintf "L%d" !counter in
    incr counter;
    l

let rec ssa_of_expr (expr : Parsed_ast.expr) :
    Lowered_ast.instr list * Lowered_ast.operand =
  match expr with
  | Parsed_ast.Int i -> ([], Int i)
  | Parsed_ast.Float f -> ([], Float f)
  | Parsed_ast.Bool b -> ([], Bool b)
  | Parsed_ast.Char c -> ([], Char c)
  | Parsed_ast.String s -> ([], String s)
  | Parsed_ast.Void -> ([], Void)
  | Parsed_ast.Identifier name -> ([], Var name)
  | Parsed_ast.UnOp (op, e) ->
      let instrs, v = ssa_of_expr e in
      let tmp = fresh_var () in
      (instrs @ [ Assign (tmp, UnOp (op, v)) ], Var tmp)
  | Parsed_ast.BinOp (op, e1, e2) ->
      let i1, v1 = ssa_of_expr e1 in
      let i2, v2 = ssa_of_expr e2 in
      let tmp = fresh_var () in
      (i1 @ i2 @ [ Assign (tmp, BinOp (op, v1, v2)) ], Var tmp)
  | Parsed_ast.Assign (name, expr) ->
      let instrs, v = ssa_of_expr expr in
      (instrs @ [ Assign (name, Copy v) ], Var name)
  | Parsed_ast.Call (fname, args) ->
      let instrs, values =
        List.fold_left
          (fun (acc_i, acc_v) arg ->
            let i, v = ssa_of_expr arg in
            (acc_i @ i, acc_v @ [ v ]))
          ([], []) args
      in
      let tmp = fresh_var () in
      (instrs @ [ Assign (tmp, Call (fname, values)) ], Var tmp)

let rec ssa_of_stmt (s : Parsed_ast.stmt) : Lowered_ast.instr list =
  match s with
  | ExprStmt e ->
      let instrs, _ = ssa_of_expr e in
      instrs
  | ReturnStmt e ->
      let instrs, v = ssa_of_expr e in
      instrs @ [ Return v ]
  | PrintStmt e ->
      let instrs, v = ssa_of_expr e in
      instrs @ [ Print v ]
  | IfStmt (cond, then_stmt, else_stmt) ->
      let cond_instrs, v = ssa_of_expr cond in
      let then_label = fresh_label () in
      let else_label = fresh_label () in
      let end_label = fresh_label () in
      let then_instrs = ssa_of_stmt then_stmt in
      let else_instrs = ssa_of_stmt else_stmt in
      cond_instrs
      @ [ Lowered_ast.If (v, then_label, else_label) ]
      @ [ Lowered_ast.Label then_label ]
      @ then_instrs
      @ [ Lowered_ast.Goto end_label ]
      @ [ Lowered_ast.Label else_label ]
      @ else_instrs
      @ [ Lowered_ast.Goto end_label ]
      @ [ Lowered_ast.Label end_label ]
  | WhileStmt (cond, body) ->
      let start_label = fresh_label () in
      let body_label = fresh_label () in
      let end_label = fresh_label () in
      let cond_instrs, v = ssa_of_expr cond in
      let body_instrs = ssa_of_stmt body in
      [ Lowered_ast.Label start_label ]
      @ cond_instrs
      @ [ Lowered_ast.If (v, body_label, end_label) ]
      @ [ Lowered_ast.Label body_label ]
      @ body_instrs
      @ [ Lowered_ast.Goto start_label ]
      @ [ Lowered_ast.Label end_label ]
  | Block decls -> List.concat_map ssa_of_decl decls
  | Import _ -> []

and ssa_of_decl (d : Parsed_ast.decl) : Lowered_ast.instr list =
  match d with
  | VarDecl (name, expr) ->
      let instrs, v = ssa_of_expr expr in
      instrs @ [ Assign (name, Copy v) ]
  | Statement s -> ssa_of_stmt s
  | FuncDecl (_name, _args, _body_decls, _ret_ty) ->
      (* This case is handled at top-level below *)
      failwith "nested FuncDecl not supported in SSA"

let ssa_of_func (d : Parsed_ast.decl) : Lowered_ast.func option =
  match d with
  | FuncDecl (name, args, body, _) ->
      let arg_names = List.map fst args in
      let instrs = List.concat_map ssa_of_decl body in
      Some { name; args = arg_names; body = instrs }
  | _ -> None

let ssa_of_program (Parsed_ast.Program decls) : Lowered_ast.program =
  List.filter_map ssa_of_func decls

let string_of_ssa (ssa : Lowered_ast.instr list) : string =
  String.concat "\n" (List.map Lowered_ast.show_instr ssa)
