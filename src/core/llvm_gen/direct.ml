open Typing
open Syntax.Ast_types
open Llvm
open Llvm_target

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

let dyn_array_type () =
  struct_type context
    [|
      i32_t; (* length *)
      i32_t; (* capacity *)
      pointer_type context (* data *);
    |]

let size_t = i64_type context
let malloc_ty = function_type (pointer_type context) [| size_t |]
let malloc_decl = declare_function "malloc" malloc_ty delta_module
let _ = Llvm_X86.initialize ()
let triple = Llvm_target.Target.default_triple ()
let target = Llvm_target.Target.by_triple triple

let target_machine =
  Llvm_target.TargetMachine.create ~triple ~cpu:"generic" ~features:"" target

let data_layout = Llvm_target.TargetMachine.data_layout target_machine

let _ =
  Llvm.set_data_layout
    (Llvm_target.DataLayout.as_string data_layout)
    delta_module

let _ = Llvm.set_target_triple triple delta_module

let llvm_type ty =
  match ty with
  | TInt -> i32_t
  | TFloat -> float_t
  | TBool -> bool_t
  | TString -> pointer_type context
  | TChar -> i8_t
  | TVoid -> void_type context
  | TNamed name -> (
      match Hashtbl.find_opt custom_types name with
      | Some ty -> ty
      | None -> raise (LLVMError ("Custom type " ^ name ^ " not found"))
  )
  (* | TStruct (name, _fields) -> (
      match Hashtbl.find_opt custom_types name with
      | Some ty -> ty
      | None -> raise (LLVMError ("Custom type " ^ name ^ " not found"))) *)
  (* | TArray ty -> array_type (llvm_type ty) 10 *)
  | TArray _ -> dyn_array_type ()
  | TFunction (_, _) -> pointer_type context

let size_of_llvm_type ty =
  match classify_type ty with
  | TypeKind.Integer -> 4
  | TypeKind.Float -> 4
  | TypeKind.Double -> 8
  | TypeKind.Pointer -> 8
  | _ -> raise (LLVMError "Unsupported type size computation")

let rec codegen_expr (expr : Typed_ast.expr) : llvalue =
  match expr with
  | Int i -> const_int i32_t i
  | Float f -> const_float float_t f
  | Bool b -> const_int bool_t (if b then 1 else 0)
  | Char c -> const_int i8_t (Char.code c)
  | String s -> build_global_stringptr s "str" builder
  | Void -> const_null void_t
  | Identifier (name, ty) -> (
      try
        let var = find_variable name in
        match ty with
        | TNamed _ | TArray _ -> var
        | _ -> build_load (llvm_type ty) var name builder
      with LLVMError _ -> (
        match lookup_function name delta_module with
        | Some func -> func
        | None -> raise (LLVMError ("Function " ^ name ^ " not found"))))
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
  | Call (func_expr, args, func_type) ->
      (* let callee =
        match lookup_function func_name delta_module with
        | Some callee -> callee
        | None ->
            raise (LLVMError ("Unknown function referenced: " ^ func_name))
      in *)
      let func_val = codegen_expr func_expr in
      (* let arg_vals = List.map codegen_expr args in *)
      let param_types, return_type =
        match func_type with
        | TFunction (param_types, return_type) -> (param_types, return_type)
        | _ -> raise (LLVMError "Invalid function type")
      in

      let arg_vals =
        List.map2
          (fun arg param_ty ->
            let arg_val = codegen_expr arg in
            match param_ty with
            | TNamed _ ->
              (* This loads struct from pointer to pass by copy instead of reference *)
              build_load (llvm_type param_ty) arg_val "named_arg" builder
            | _ -> arg_val)
          args param_types
      in

      let expected_arg_count = List.length param_types in
      if List.length args <> expected_arg_count then
        raise
          (LLVMError
             (Printf.sprintf "Expected %d arguments, got %d" expected_arg_count
                (List.length args)));

      let llvm_func_ty =
        function_type (llvm_type return_type)
          (Array.of_list (List.map llvm_type param_types))
      in
      (* 
      let params = params func_val in
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
      in *)
      let call_inst =
        build_call llvm_func_ty func_val (Array.of_list arg_vals) "" builder
      in
      if type_of call_inst |> classify_type = TypeKind.Void then call_inst
      else (
        set_value_name "calltmp" call_inst;
        call_inst)
  | StructInit (name, fields) ->
      (* Note: name is struct name, not variable name! *)
      let struct_ty =
        try Hashtbl.find custom_types name
        with Not_found -> raise (Failure ("Unknown struct type: " ^ name))
      in
      let struct_ptr = Llvm.build_alloca struct_ty "struct_tmp" builder in
      List.iteri
        (fun i (_, expr, field_ty) ->
          let field_val = codegen_expr expr in
          let field_val =
            match classify_type (Llvm.type_of field_val) with
            | TypeKind.Pointer ->
                (* Load struct/array values to store into outer struct *)
                build_load (llvm_type field_ty) field_val "tmpfield" builder
            | _ -> field_val
          in
          let field_ptr =
            build_struct_gep struct_ty struct_ptr i
              ("field" ^ string_of_int i)
              builder
          in
          ignore (Llvm.build_store field_val field_ptr builder))
        fields;
      let struct_val = build_load struct_ty struct_ptr "struct_val" builder in
      struct_val
| FieldAccess (struct_expr, field_name, field_index, struct_ty, field_ty) ->
    let struct_val = codegen_expr struct_expr in
    let struct_ptr =
      if Llvm.classify_type (Llvm.type_of struct_val) = TypeKind.Pointer then
        struct_val
      else
        (* Allocate space and store the struct to get a pointer *)
        let ptr = build_alloca (llvm_type struct_ty) "struct_tmp" builder in
        ignore (build_store struct_val ptr builder);
        ptr
    in
    let field_ptr =
      build_struct_gep (llvm_type struct_ty) struct_ptr field_index field_name
        builder
    in
    (* For pointer types like array or nested struct, return the pointer directly *)
    (match field_ty with
    | TArray _ | TNamed _ ->
        (* Avoid loading so the caller can decide how to use the pointer *)
        field_ptr
    | _ ->
        (* Load primitive field values (int, float, bool, etc.) *)
        build_load (llvm_type field_ty) field_ptr field_name builder)
  | ArrayInit (exprs, array_ty) ->
      ignore (Printf.printf "ArrayInit: %s\n" (string_of_type array_ty));
      let num_elems = List.length exprs in
      let array_llvm_ty = llvm_type array_ty in
      let first_elem_ty =
        match array_ty with
        | TArray first_elem_ty -> first_elem_ty
        | _ -> raise (LLVMError "Invalid array type")
      in
      ignore (Printf.printf "elem_llvm_ty: %s\n" (string_of_type first_elem_ty));
      let array_struct_ty = dyn_array_type () in

      let array_ptr = build_alloca array_struct_ty "array_tmp" builder in

      (* Allocate heap for data *)
      let num_elem_64 = Int64.of_int num_elems in
      let elem_size_64 = DataLayout.abi_size array_llvm_ty data_layout in
      let size_64 = Int64.mul num_elem_64 elem_size_64 in
      let total_size = const_of_int64 size_t size_64 true in
      let malloc_raw =
        build_call malloc_ty malloc_decl [| total_size |] "malloc_tmp" builder
      in

      (* Store fields *)
      let length_ptr =
        build_struct_gep array_struct_ty array_ptr 0 "len_ptr" builder
      in
      let capacity_ptr =
        build_struct_gep array_struct_ty array_ptr 1 "cap_ptr" builder
      in
      let data_ptr_ptr =
        build_struct_gep array_struct_ty array_ptr 2 "data_ptr" builder
      in

      ignore (build_store (const_int i32_t num_elems) length_ptr builder);
      ignore (build_store (const_int i32_t num_elems) capacity_ptr builder);
      ignore (build_store malloc_raw data_ptr_ptr builder);

      (* Store elements: cast raw malloc ptr to typed pointer locally *)
      let typed_ptr =
        build_bitcast malloc_raw (pointer_type context) "typed_data_ptr" builder
      in
      List.iteri
        (fun i expr ->
          let value = codegen_expr expr in
          let actual_value =
            match first_elem_ty with
            | TNamed _ | TArray _ ->
                (* If it's a pointer, load the struct/array value it points to *)
                if classify_type (Llvm.type_of value) = TypeKind.Pointer then
                  build_load (llvm_type first_elem_ty) value "tmp_value" builder
                else
                  value
            | _ -> value
          in
          let idx = const_int i32_t i in
          let elem_ptr =
            build_in_bounds_gep (llvm_type first_elem_ty) typed_ptr [| idx |]
              "elem_ptr" builder
          in
          ignore (build_store actual_value elem_ptr builder))
        exprs;

      array_ptr
  | ArrayAccess (array_expr, index_expr, _array_ty, elem_ty) ->
      let elem_llvm_ty = llvm_type elem_ty in
      let array_struct_ty = dyn_array_type () in

      let array_ptr = codegen_expr array_expr in
      let index_val = codegen_expr index_expr in

      (* Load the opaque data pointer from struct *)
      let data_ptr_ptr =
        build_struct_gep array_struct_ty array_ptr 2 "data_ptr_ptr" builder
      in
      let raw_data_ptr =
        build_load (pointer_type context) data_ptr_ptr "raw_data_ptr" builder
      in

      (* GEP to element *)
      let elem_ptr =
        build_in_bounds_gep elem_llvm_ty raw_data_ptr [| index_val |] "elem_ptr"
          builder
      in
      build_load elem_llvm_ty elem_ptr "elem" builder

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
      (* If then block does NOT end with a terminator, add branch to merge *)
      if Llvm.block_terminator then_bb = None then
        ignore (build_br merge_bb builder);
      let then_bb_end = insertion_block builder in

      (* Else block *)
      position_at_end else_bb builder;
      enter_scope ();
      let else_val = codegen_stmt else_stmt in
      exit_scope ();
      (* If else block does NOT end with a terminator, add branch to merge *)
      if Llvm.block_terminator else_bb = None then
        ignore (build_br merge_bb builder);
      let else_bb_end = insertion_block builder in

      (* Merge block *)
      position_at_end merge_bb builder;

      (* Determine which blocks actually branch to merge_bb *)
      let incoming = [] in
      let incoming =
        if
          match Llvm.block_terminator then_bb with
          | Some term when Llvm.instr_opcode term = Llvm.Opcode.Br ->
              Array.exists (( = ) merge_bb) (Llvm.successors term)
          | _ -> false
        then
          (Option.value then_val ~default:(const_null i32_t), then_bb_end)
          :: incoming
        else incoming
      in
      let incoming =
        if
          match Llvm.block_terminator else_bb with
          | Some term when Llvm.instr_opcode term = Llvm.Opcode.Br ->
              Array.exists (( = ) merge_bb) (Llvm.successors term)
          | _ -> false
        then
          (Option.value else_val ~default:(const_null i32_t), else_bb_end)
          :: incoming
        else incoming
      in

      (* Build PHI only if there's at least one incoming edge *)
      if incoming <> [] then ignore (build_phi incoming "iftmp" builder);

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
           | TNamed _ | TArray _ ->
            (* Only load if expr_val is a pointer *)
            if classify_type (Llvm.type_of expr_val) = TypeKind.Pointer then
              build_load (llvm_type expr_ty) expr_val "tmp" builder
            else
              expr_val
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
