type name = string [@@deriving show, eq]
type id = int [@@deriving show, eq]
type level = int [@@deriving show, eq]

type ty =
  | TInt
  | TFloat
  | TBool
  | TChar
  | TString
  | TVoid
  | TFunction of ty list * ty
  | TStruct of name * (name * ty) list
[@@deriving show, eq]

type param = name * ty [@@deriving show, eq]

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpRem
  | BinOpLessThan
  | BinOpGreaterThan
  | BinOpLessThanEqual
  | BinOpGreaterThanEqual
  | BinOpAnd
  | BinOpOr
  | BinOpEqual
  | BinOpNotEqual
[@@deriving show, eq]

type un_op = UnOpNot | UnOpNegate [@@deriving show, eq]

let string_of_bin_op = function
  | BinOpPlus -> "+"
  | BinOpMinus -> "-"
  | BinOpMult -> "*"
  | BinOpDiv -> "/"
  | BinOpRem -> "%"
  | BinOpLessThan -> "<"
  | BinOpGreaterThan -> ">"
  | BinOpLessThanEqual -> "<="
  | BinOpGreaterThanEqual -> ">="
  | BinOpAnd -> "&&"
  | BinOpOr -> "||"
  | BinOpEqual -> "=="
  | BinOpNotEqual -> "!="

let string_of_un_op = function UnOpNegate -> "-" | UnOpNot -> "!"

let string_of_type (t : ty) : string =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TChar -> "Char"
  | TString -> "String"
  | TVoid -> "Unit"
  | TFunction _ -> "Function"
  | TStruct _ -> "Struct"
