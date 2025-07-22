open Typing
open Syntax.Ast_types
open Llvm

exception LLVMError of string

let context = global_context ()
let delta_module = create_module context "DeltaModule"
let builder = builder context

let named_value_scopes : (string, llvalue) Hashtbl.t list ref =
  ref [ Hashtbl.create 10 ]

let custom_types : (string, lltype) Hashtbl.t = Hashtbl.create 10

let enter_scope () =
  named_value_scopes := Hashtbl.create 10 :: !named_value_scopes

let exit_scope () =
  match !named_value_scopes with
  | [] -> raise (LLVMError "Scope stack underflow")
  | _ :: tl -> named_value_scopes := tl

let add_local name value =
  match !named_value_scopes with
  | scope :: _ -> Hashtbl.add scope name value
  | [] -> raise (LLVMError "No scope to add variable")

let add_global name value =
  match !named_value_scopes with
  | scope :: _ -> Hashtbl.add scope name value
  | [] -> raise (LLVMError "No global scope to add variable")

let find_variable name =
  let rec find = function
    | [] -> raise (LLVMError ("Variable " ^ name ^ " not found"))
    | scope :: rest -> (
        match Hashtbl.find_opt scope name with
        | Some value -> value
        | None -> find rest)
  in
  find !named_value_scopes

let create_entry_block_alloca (func : llvalue) (var_name : string) (ty : lltype)
    : llvalue =
  let builder = builder_at context (instr_begin (entry_block func)) in
  build_alloca ty var_name builder

let i32_t = i32_type context
let float_t = float_type context
let bool_t = i1_type context
let i8_t = i8_type context
let void_t = void_type context

let llvm_type ty =
  match ty with
  | TInt -> i32_t
  | TFloat -> float_t
  | TBool -> bool_t
  | TString -> pointer_type context
  | TChar -> i8_t
  | TVoid -> void_type context
  | TStruct (name, _fields) -> (
      match Hashtbl.find_opt custom_types name with
      | Some ty -> ty
      | None -> raise (LLVMError ("Custom type " ^ name ^ " not found")))
  | ty -> raise (LLVMError ("Unsupported type " ^ string_of_type ty))

let rec codegen_expr (expr : Typed_ast.expr) : llvalue =
  match expr with
  | Int i -> const_int i32_t i
  | Float f -> const_float float_t f
  | Bool b -> const_int bool_t (if b then 1 else 0)
  | Char c -> const_int i8_t (Char.code c)
  | String s -> build_global_stringptr s "str" builder
  | Void -> const_null void_t
  | Identifier (name, ty) -> (
      let var = find_variable name in
      match ty with
      | TStruct _ -> var
      | _ -> build_load (llvm_type ty) var name builder)
  | BinOp (op, lhs, rhs) -> (
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      match op with
      | BinOpPlus -> build_add lhs_val rhs_val "addtmp" builder
      | BinOpMinus -> build_sub lhs_val rhs_val "subtmp" builder
      | BinOpMult -> build_mul lhs_val rhs_val "multmp" builder
      | BinOpDiv -> build_sdiv lhs_val rhs_val "divtmp" builder
      | BinOpRem -> build_srem lhs_val rhs_val "modtmp" builder
      | BinOpLessThan -> build_icmp Icmp.Slt lhs_val rhs_val "lttmp" builder
      | BinOpGreaterThan -> build_icmp Icmp.Sgt lhs_val rhs_val "gttmp" builder
      | BinOpLessThanEqual ->
          build_icmp Icmp.Sle lhs_val rhs_val "letmp" builder
      | BinOpGreaterThanEqual ->
          build_icmp Icmp.Sge lhs_val rhs_val "getmp" builder
      | BinOpAnd -> build_and lhs_val rhs_val "andtmp" builder
      | BinOpOr -> build_or lhs_val rhs_val "ortmp" builder
      | BinOpEqual -> build_icmp Icmp.Eq lhs_val rhs_val "eqtmp" builder
      | BinOpNotEqual -> build_icmp Icmp.Ne lhs_val rhs_val "netmp" builder)
  | UnOp (op, operand) -> (
      let operand_val = codegen_expr operand in
      match op with
      | UnOpNegate -> build_neg operand_val "negtmp" builder
      | UnOpNot -> build_not operand_val "nottmp" builder)
  | Assign (name, expr) ->
      let expr_val = codegen_expr expr in
      let var = find_variable name in
      build_store expr_val var builder
  | Call (func_name, args, func_type) ->
      let callee =
        match lookup_function func_name delta_module with
        | Some callee -> callee
        | None ->
            raise (LLVMError ("Unknown function referenced: " ^ func_name))
      in
      let params = params callee in
      if Array.length params <> List.length args then
        raise
          (LLVMError
             (Printf.sprintf "Expected %d arguments, got %d"
                (Array.length params) (List.length args)));
      let args = Array.of_list (List.map codegen_expr args) in
      let func_type =
        match func_type with
        | TFunction (param_types, return_type) ->
            function_type (llvm_type return_type)
              (Array.of_list (List.map llvm_type param_types))
        | _ -> raise (LLVMError "Invalid function type")
      in
      let call_inst = build_call func_type callee args "" builder in
      if type_of call_inst |> classify_type = TypeKind.Void then
        call_inst
      else (
        set_value_name "calltmp" call_inst;
        call_inst
      )
  | StructInit (name, fields) ->
      (* Note: name is struct name, not variable name! *)
      let struct_ty =
        try Hashtbl.find custom_types name
        with Not_found -> raise (Failure ("Unknown struct type: " ^ name))
      in
      let struct_ptr = Llvm.build_alloca struct_ty "struct_tmp" builder in
      List.iteri
        (fun i (_, expr, _) ->
          let field_val = codegen_expr expr in
          let field_ptr =
            build_struct_gep struct_ty struct_ptr i
              ("field" ^ string_of_int i)
              builder
          in
          ignore (Llvm.build_store field_val field_ptr builder))
        fields;
      struct_ptr
  | FieldAccess (struct_expr, field_name, field_index, struct_ty, field_ty) ->
    let struct_ptr = codegen_expr struct_expr in
    let field_ptr = build_struct_gep (llvm_type struct_ty) struct_ptr field_index field_name builder in
    build_load (llvm_type field_ty) field_ptr field_name builder

let rec codegen_stmt (stmt : Typed_ast.stmt) : llvalue option =
  match stmt with
  | ExprStmt expr ->
      ignore (codegen_expr expr);
      None
  | IfStmt (cond, then_stmt, else_stmt) ->
      let cond_val = codegen_expr cond in
      let zero = const_int bool_t 0 in
      let cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" builder in
      let start_bb = insertion_block builder in
      let func = block_parent start_bb in
      let then_bb = append_block context "then" func in
      let else_bb = append_block context "else" func in
      let merge_bb = append_block context "ifcont" func in
      ignore (build_cond_br cond_bool then_bb else_bb builder);

      (* Then block *)
      position_at_end then_bb builder;
      enter_scope ();
      let then_val = codegen_stmt then_stmt in
      exit_scope ();
      ignore (build_br merge_bb builder);
      let then_bb_end = insertion_block builder in

      (* Else block *)
      position_at_end else_bb builder;
      enter_scope ();
      let else_val = codegen_stmt else_stmt in
      exit_scope ();
      ignore (build_br merge_bb builder);
      let else_bb_end = insertion_block builder in

      (* Merge block *)
      position_at_end merge_bb builder;
      let then_val = Option.value then_val ~default:(const_null i32_t) in
      let else_val = Option.value else_val ~default:(const_null i32_t) in
      ignore
        (build_phi
           [ (then_val, then_bb_end); (else_val, else_bb_end) ]
           "iftmp" builder);
      (* TODO: Check if then_val or else_val returns *)
      None
  | ReturnStmt expr ->
      let return_val = codegen_expr expr in
      ignore (build_ret return_val builder);
      Some return_val
  | WhileStmt (cond, body) ->
      let start_bb = insertion_block builder in
      let func = block_parent start_bb in

      let cond_bb = append_block context "while.cond" func in
      let body_bb = append_block context "while.body" func in
      let after_bb = append_block context "while.after" func in

      (* Jump to condition block *)
      ignore (build_br cond_bb builder);

      (* Condition block *)
      position_at_end cond_bb builder;
      let cond_val = codegen_expr cond in
      let zero = const_int bool_t 0 in
      let cond_bool = build_icmp Icmp.Ne cond_val zero "whilecond" builder in
      ignore (build_cond_br cond_bool body_bb after_bb builder);

      (* Body block *)
      position_at_end body_bb builder;
      enter_scope ();
      let return_val = codegen_stmt body in
      exit_scope ();
      (* After executing body, jump back to condition *)
      ignore (build_br cond_bb builder);

      (* Continue building after the loop *)
      position_at_end after_bb builder;

      return_val
  | Block decls ->
      enter_scope ();

      let rec codegen_decls decls =
        match decls with
        | [] -> None
        | decl :: rest -> (
            match codegen_decl ~global:false decl with
            | Some v -> Some v
            | None -> codegen_decls rest)
      in
      let ret_val = codegen_decls decls in
      exit_scope ();
      ret_val

and codegen_decl ~global (decl : Typed_ast.decl) : llvalue option =
  match decl with
  | Statement stmt ->
      print_endline "--- Generating code for statement";
      codegen_stmt stmt
  | VarDecl (name, expr, expr_ty) ->
      (let expr_val = codegen_expr expr in
       if global then (
         print_endline ("--- Generating code for global variable: " ^ name);
         let global_var = define_global name expr_val delta_module in
         add_global name global_var)
       else (
         print_endline ("--- Generating code for local variable: " ^ name);
         let func = block_parent (insertion_block builder) in
         let alloca = create_entry_block_alloca func name (llvm_type expr_ty) in
         let value_to_store =
           match expr_ty with
             | TStruct _ -> build_load (llvm_type expr_ty) expr_val "tmp" builder
             | _ -> expr_val
         in
         ignore (build_store value_to_store alloca builder);
         add_local name alloca));
      None
  | StructDecl (name, fields) ->
      print_endline ("--- Generating code for struct: " ^ name);
      let field_types =
        List.map (fun (_name, field_ty) -> llvm_type field_ty) fields
      in
      let struct_type = named_struct_type context name in
      struct_set_body struct_type (Array.of_list field_types) false;
      Hashtbl.add custom_types name struct_type;
      None
  | FuncDecl (name, params, body, ret_ty) ->
      print_endline ("--- Generating code for function: " ^ name);
      let llvm_ret_type = llvm_type ret_ty in
      let llvm_param_types =
        Array.of_list (List.map (fun (_pname, ty) -> llvm_type ty) params)
      in
      let func_type = function_type llvm_ret_type llvm_param_types in
      let func = define_function name func_type delta_module in
      let entry_bb = entry_block func in
      position_at_end entry_bb builder;

      enter_scope ();

      List.iteri
        (fun i (pname, ty) ->
          let param = param func i in
          set_value_name pname param;
          let alloca = create_entry_block_alloca func pname (llvm_type ty) in
          ignore (build_store param alloca builder);
          add_local pname alloca)
        params;

      let return_value = codegen_stmt (Block body) in

      exit_scope ();

      print_endline "--- Verifying function...";

      ignore
        (match return_value with
        | Some _ -> ()
        | None -> ignore (build_ret_void builder));

      Llvm_analysis.assert_valid_function func;
      None
  | ExternDecl (name, params, ret_ty) ->
      print_endline ("--- Generating code for extern function: " ^ name);
      let llvm_ret_type = llvm_type ret_ty in
      let llvm_param_types =
        Array.of_list (List.map (fun (_pname, ty) -> llvm_type ty) params)
      in
      let func_type = function_type llvm_ret_type llvm_param_types in
      ignore (declare_function name func_type delta_module);
      None

let codegen_program (program : Typed_ast.program) : unit =
  match program with
  | Typed_ast.Program decls ->
      ignore (List.map (codegen_decl ~global:true) decls)

let dump_ir () : unit = Llvm.dump_module delta_module

let save_ir_to_file filename =
  let oc = open_out filename in
  output_string oc (Llvm.string_of_llmodule delta_module);
  close_out oc
