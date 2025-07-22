open Syntax.Ast_types

type expr =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Void
  | Identifier of name
  | BinOp of bin_op * expr * expr
  | UnOp of un_op * expr
  | Assign of name * expr
  | Call of name * expr list
  | StructInit of name * (name * expr) list
  | FieldAccess of expr * name
[@@deriving show, eq]

type stmt =
  | ExprStmt of expr
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt
  | ReturnStmt of expr
  | PrintStmt of expr
  | Block of decl list
  | Import of string
[@@deriving show, eq]

and decl =
  | FuncDecl of name * (name * ty) list * decl list * ty
  | ExternDecl of name * (name * ty) list * ty
  | VarDecl of name * expr
  | StructDecl of name * (name * ty) list
  | Statement of stmt
[@@deriving show, eq]

type program = Program of decl list [@@deriving show, eq]
