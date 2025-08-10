# Delta

## TODO

- [ ] Struct access
- [ ] String allocation / array of chars
- [ ] If statement early return function type matching
- [ ] Array slices?
- [ ] GC

## Grammar

```
<program> ::= <decl>* EOF

<decl> ::= "fn" <ID> "(" <param_list>? ")" ":" <type> <block>
         | "extern" "fn" <ID> "(" <param_list>? ")" ":" <type> ";"
         | "var" <ID> "=" <expr> ";"
         | "struct" <ID> "{" <struct_field_decl>* "}"
         | <stmt>

<param_list> ::= <param> ("," <param>)*
<param> ::= <ID> <type>

<stmt> ::= <expr> ";"
         | "if" "(" <expr> ")" <stmt> "else" <stmt>
         | "while" "(" <expr> ")" <stmt>
         | "return" <expr> ";"
         | "import" <ID> ";"
         | <block>

<block> ::= "{" <decl>* "}"

<expr> ::= "(" <expr> ")"
         | <INT>
         | <FLOAT>
         | <CHAR>
         | <STRING>
         | "nil"
         | "true"
         | "false"
         | <ID>
         | <unary_op> <expr>
         | <expr> <binary_op> <expr>
         | <ID> "=" <expr>
         | <ID> "(" <arg_list>? ")"
         | <ID> "{" <struct_field_list>? "}"
         | <expr> "." <ID>
         | "[" <expr_list>? "]"
         | <expr> "[" <expr> "]"

<arg_list> ::= <expr> ("," <expr>)*
<expr_list> ::= <expr> ("," <expr>)*

<unary_op> ::= "!" | "-"
<binary_op> ::= "+" | "-" | "*" | "/" | "%"
              | "<" | ">" | "<=" | ">="
              | "==" | "!="
              | "and" | "or"

<struct_field> ::= <ID> ":" <expr>
<struct_field_list> ::= <struct_field> ("," <struct_field>)*

<struct_field_decl> ::= <ID> ":" <type> ";"

<type> ::= "int" | "float" | "bool" | "char" | "str" | "void"
```
