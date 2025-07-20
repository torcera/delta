open Syntax.Ast_types

type operand =
  | Var of name
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Void
[@@deriving show, eq]

type instr =
  | Assign of name * rvalue (* %x = un_op or %y = bin_op *)
  (* | Phi of name * ty * (operand * label) list (* %x = phi [v1, bb1], [v2, bb2], ... *)
  | Call of name option * name * operand list (* %x = call f(...) or call f(...) *)
  | Jump of label                             (* br label *)
  | CondJump of operand * label * label       (* br i1 %cond, label %then, label %else *)
  | Alloca of name * ty                       (* %x = alloca i32 *)
  | Load of name * operand                    (* %x = load i32, i32* %y *)
  | Store of name * operand * operand         store i32 %x, i32* %y *)
  | Return of operand (* ret i32 %x or ret void *)
  | Print of operand
  | Goto of string
  | If of operand * string * string
  | Label of string
[@@deriving show, eq]

and rvalue =
  | BinOp of bin_op * operand * operand
  | UnOp of un_op * operand
  | Call of name * operand list
  | Copy of operand
[@@deriving show, eq]

and label = string [@@deriving show, eq]

type basic_block = { label : label; instrs : instr list } [@@deriving show, eq]

type func = { name : string; args : name list; body : instr list }
[@@deriving show, eq]

type program = func list [@@deriving show, eq]
