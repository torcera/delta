let context = Llvm.global_context ()
let the_module = Llvm.create_module context "StructModule"
let builder = Llvm.builder context

(* Define the struct type *)
let my_struct_type = Llvm.named_struct_type context "MyStruct"
let i32_t = Llvm.i32_type context
let float_t = Llvm.float_type context
let _ = Llvm.struct_set_body my_struct_type [| i32_t; float_t |] false

(* Define main function *)
let main_type = Llvm.function_type i32_t [||]
let main_func = Llvm.define_function "main" main_type the_module
let entry = Llvm.append_block context "entry" main_func
let _ = Llvm.position_at_end entry builder

let build () =
  (* Allocate the struct and manipulate it *)
  let struct_ptr = Llvm.build_alloca my_struct_type "myStruct" builder in
  let field0 =
    Llvm.build_struct_gep my_struct_type struct_ptr 0 "field0" builder
  in
  ignore (Llvm.build_store (Llvm.const_int i32_t 123) field0 builder);

  let field1 =
    Llvm.build_struct_gep my_struct_type struct_ptr 1 "field1" builder
  in
  ignore (Llvm.build_store (Llvm.const_float float_t 4.56) field1 builder);

  ignore (Llvm.build_ret (Llvm.const_int i32_t 0) builder)

let dump_ir () : unit = Llvm.dump_module the_module
