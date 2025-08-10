%{
    open Parsed_ast
    open Syntax.Ast_types
%}

%token <string> ID
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE
%token RANGLE
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token COLON
%token COMMA
%token ARROW
%token EQUAL_EQUAL
%token EQUAL
%token PLUS
%token MINUS
%token MULT
%token DIV
%token REM
%token AND
%token OR
%token BANG
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_BOOL
%token TYPE_CHAR
%token TYPE_STRING
%token TYPE_VOID
%token TRUE
%token FALSE
%token VOID
%token IF
%token ELSE
%token VAR
%token FUNCTION
%token RETURN
%token WHILE
%token STRUCT
%token TYPE
%token MAP
%token IMPORT
%token DOT
%token EXTERN
%token EOF

%right EQUAL
%left PLUS MINUS LANGLE RANGLE
%left MULT DIV REM
%left AND OR
%nonassoc BANG EQUAL_EQUAL

%start <Parsed_ast.program> program

%%

expr:
    | LPAREN; e=expr; RPAREN { e }
    | i=INT { Int i }
    | f=FLOAT { Float f }
    | c=CHAR { Char c }
    | s=STRING { String s }
    | VOID { Void }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | id=ID { Identifier id }
    | op=un_op e=expr { UnOp(op, e) }
    | e1=expr op=bin_op e2=expr { BinOp(op, e1, e2) }
    | name=ID; EQUAL; value_expr=expr { Assign(name, value_expr) }
    | name=expr; LPAREN; args=separated_list(COMMA, expr); RPAREN { Call(name, args) }
    | struct_name=ID; LBRACE; fields=separated_list(COMMA, struct_field); RBRACE { StructInit(struct_name, fields) }
    | struct_expr=expr; DOT; field_name=ID { FieldAccess(struct_expr, field_name) }
    | LBRACKET; elements=separated_list(COMMA, expr); RBRACKET { ArrayInit(elements) }
    | container_expr=expr; LBRACKET; key_expr=expr; RBRACKET { ArrayAccess(container_expr, key_expr) }
    // | MAP; LANGLE; key_ty=ty; COMMA; value_ty=ty; RANGLE; LBRACE; fields=separated_list(COMMA, map_field); RBRACE { MapInit(key_ty, value_ty, fields) }
    // | map_expr=expr; LBRACKET; key_expr=expr; RBRACKET { MapAccess(map_expr, key_expr) }
    // | map_expr=expr; LBRACKET; key_expr=expr; RBRACKET; EQUAL; value_expr=expr { MapAssign(map_expr, key_expr, value_expr) }
    ;

stmt:
    | e=expr; SEMICOLON { ExprStmt(e) }
    | IF; LPAREN; cond_expr=expr; RPAREN; then_stmt=stmt; ELSE; else_stmt=stmt { IfStmt(cond_expr, then_stmt, else_stmt) }
    | WHILE; LPAREN; cond_expr=expr; RPAREN; body=stmt { WhileStmt(cond_expr, body) }
    | RETURN; expr=expr; SEMICOLON { ReturnStmt(expr) }
    | IMPORT; module_name=ID; SEMICOLON { Import(module_name) }
    | b=block { Block(b) }
    ;

block:
    | LBRACE; decls=list(decl); RBRACE { decls }
    ;

param:
    | name=ID; COLON; ty=ty { (name, ty) }
    ;

decl:
    | FUNCTION; name=ID; LPAREN; params=separated_list(COMMA, param); RPAREN; ARROW; return_type=ty; body=block { FuncDecl (name, params, body, return_type) }
    | EXTERN; FUNCTION; name=ID; LPAREN; params=separated_list(COMMA, param); RPAREN; ARROW; return_type=ty; SEMICOLON { ExternDecl (name, params, return_type) }
    | VAR; name=ID; EQUAL; e=expr; SEMICOLON { VarDecl(name, e) }
    | TYPE; name=ID; STRUCT; LBRACE; fields=list(struct_field_decl); RBRACE { StructDecl (name, fields) }
    | s=stmt { Statement(s) }
    ;

struct_field:
    | name=ID; COLON; e=expr { (name, e) }
    ;

struct_field_decl:
    | name=ID; COLON; ty=ty; SEMICOLON { (name, ty) }

map_field:
    | key_expr=expr; COLON; value_expr=expr { (key_expr, value_expr) }

program:
    | decls=list(decl); EOF { Program(decls) }
    ;

ty:
    | TYPE_INT { TInt }
    | TYPE_FLOAT { TFloat }
    | TYPE_BOOL { TBool }
    | TYPE_CHAR { TChar }
    | TYPE_STRING { TString }
    | TYPE_VOID { TVoid }
    | ty=ty; LBRACKET; RBRACKET { TArray(ty) }
    | FUNCTION; LPAREN; params=separated_list(COMMA, ty); RPAREN; ARROW; ret=ty { TFunction(params, ret) }
    | name=ID; { TNamed(name) }
    ;

%inline un_op:
    | BANG { UnOpNot }
    | MINUS { UnOpNegate }
    ;

%inline bin_op:
    | PLUS { BinOpPlus }
    | MINUS { BinOpMinus }
    | MULT { BinOpMult }
    | DIV { BinOpDiv }
    | REM { BinOpRem }
    | LANGLE { BinOpLessThan }
    | RANGLE { BinOpGreaterThan }
    | LANGLE EQUAL { BinOpLessThanEqual }
    | RANGLE EQUAL { BinOpGreaterThanEqual }
    | AND { BinOpAnd }
    | OR { BinOpOr }
    | EQUAL_EQUAL { BinOpEqual }
    | BANG EQUAL { BinOpNotEqual }
    ;
