{
    open Parser

    exception SyntaxError of string
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = (alpha) (alpha|digit|'_')*
let integer = '-'? digit+
let float = '-'? digit+ '.' digit+ (['e' 'E'] '-'? digit+)? 
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

rule read_token =
    parse
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | ";" { SEMICOLON }
    | ":" { COLON }
    | "," { COMMA }
    | "->" { ARROW }
    | "<" { LANGLE }
    | ">" { RANGLE }
    | "=" { EQUAL }
    | "==" { EQUAL_EQUAL }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULT }
    | "/" { DIV }
    | "%" { REM }
    | "and" { AND }
    | "or" { OR }
    | "!" { BANG }
    | "true" { TRUE }
    | "false" { FALSE }
    | "if" { IF }
    | "else" { ELSE }
    | "nil" { VOID }
    | "int" { TYPE_INT }
    | "float" { TYPE_FLOAT }
    | "bool" { TYPE_BOOL }
    | "char" { TYPE_CHAR }
    | "str" { TYPE_STRING }
    | "void" { TYPE_VOID }
    | "var" { VAR }
    | "fn" { FUNCTION }
    | "while" { WHILE }
    | "import" { IMPORT }
    | "return" { RETURN }
    | "extern" { EXTERN }
    | "struct" { STRUCT }
    | "." { DOT }
    | whitespace { read_token lexbuf }
    | "#" { read_comment lexbuf }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | identifier { ID (Lexing.lexeme lexbuf) }
    | "'" { read_char lexbuf }
    | '"' { read_string (Buffer.create 16) lexbuf }
    | newline { Lexing.new_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_char = parse
    | '\\' 'n' '\''    { CHAR '\n' }
    | '\\' 't' '\''    { CHAR '\t' }
    | '\\' 'b' '\''    { CHAR '\b' }
    | '\\' 'r' '\''    { CHAR '\r' }
    | '\\' 'f' '\''    { CHAR '\012' }
    | '\\' '\\' '\''   { CHAR '\\' }
    | '\\' '\'' '\''   { CHAR '\'' }
    | [^ '\'' '\\'] '\'' { CHAR (Lexing.lexeme_char lexbuf 0) }
    | '\\' _           { raise (SyntaxError "Unterminated or invalid escape character") }
    | _                { raise (SyntaxError ("Malformed character literal: " ^ Lexing.lexeme lexbuf)) }
    | eof              { raise (SyntaxError "Unterminated character literal") }

and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
        }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment =
    parse
    | newline { Lexing.new_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { read_comment lexbuf }
