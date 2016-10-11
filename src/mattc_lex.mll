{
    open Mattc_par
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = ['\n' '\r']+
let int = ['0'-'9']+
let single_line_comment = "//" [^ '\n' '\r']*
let multi_line_comment = "/*" ([^ '*']* ("*" [^ '/'])?)* "*/"
let multi_line_comment_err = "/*" ([^ '*']* ("*" [^ '/'])?)*
let not_eq = "!=" | "<>"

rule read = parse
    | single_line_comment   { read lexbuf } (* This ignores comments *)
    | multi_line_comment    { read lexbuf }
    | whitespace            { read lexbuf } (* This ignores whitespace *)
    | newline               { read lexbuf } (* This ignores newlines *)
    | int as value          { INT_LITERAL (int_of_string value) }
    | "true"                { BOOL_LITERAL (true) }
    | "false"               { BOOL_LITERAL (false) }
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "*"                   { MULTIPLY }
    | "/"                   { DIVIDE }
    | "AND"                 { AND }
    | "NAND"                { NAND }
    | "OR"                  { OR }
    | "XOR"                 { XOR }
    | "NOR"                 { NOR }
    | "NXOR"                { NXOR }
    | "NOT"                 { NOT }
    | "<"                   { L_THAN }
    | ">"                   { G_THAN }
    | "<="                  { L_THAN_EQ }
    | ">="                  { G_THAN_EQ }
    | "=="                  { EQ }
    | not_eq                { NOT_EQ }
    | ";"                   { EOE }
    | "("                   { OPBRACKET }
    | ")"                   { CLBRACKET }
    | eof                   { EOF }
    | multi_line_comment_err    { raise(SyntaxError("Missing end of multi-line comment. Did you miss a '*/'?")) }
    | _                         { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }