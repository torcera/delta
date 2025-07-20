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
%token SEMICOLON
%token COLON
%token COMMA
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
%token PRINT
%token WHILE
%token IMPORT
%token EOF

%right EQUAL
%left PLUS MINUS LANGLE RANGLE
%left MULT DIV REM
%left AND OR
%nonassoc BANG

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
    | name=ID; LPAREN; args=separated_list(COMMA, expr); RPAREN { Call(name, args) }
    ;

stmt:
    | e=expr; SEMICOLON { ExprStmt(e) }
    | IF; LPAREN; cond_expr=expr; RPAREN; then_stmt=stmt; ELSE; else_stmt=stmt { IfStmt(cond_expr, then_stmt, else_stmt) }
    | WHILE; LPAREN; cond_expr=expr; RPAREN; body=stmt { WhileStmt(cond_expr, body) }
    | RETURN; expr=expr; SEMICOLON { ReturnStmt(expr) }
    | PRINT; expr=expr; SEMICOLON { PrintStmt(expr) }
    | IMPORT; module_name=ID; { Import(module_name) }
    | b=block { Block(b) }
    ;

block:
    | LBRACE; decls=list(decl); RBRACE { decls }
    ;

param:
    | name=ID; ty=ty { (name, ty) }
    ;

decl:
    | FUNCTION; name=ID; LPAREN; params=separated_list(COMMA, param); RPAREN; COLON; return_type=ty; body=block { FuncDecl (name, params, body, return_type) }
    | VAR; name=ID; EQUAL; e=expr; SEMICOLON { VarDecl(name, e) }
    | s=stmt { Statement(s) }
    ;

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
    | EQUAL EQUAL { BinOpEqual }
    | BANG EQUAL { BinOpNotEqual }
    ;
