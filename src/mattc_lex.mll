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

rule read = parse
    | single_line_comment   { read lexbuf } (* This ignores comments *)
    | multi_line_comment    { read lexbuf }
    | whitespace            { read lexbuf } (* This ignores whitespace *)
    | newline               { read lexbuf } (* This ignores newlines *)
    | int as value          { INT_LITERAL (int_of_string value) }
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "*"                   { MULTIPLY }
    | "/"                   { DIVIDE }
    | ";"                   { EOE }
    | "("                   { OPBRACKET }
    | ")"                   { CLBRACKET }
    | eof                   { EOF }
    | multi_line_comment_err    { raise(SyntaxError("Missing end of multi-line comment. Did you miss a '*/'?")) }
    | _                         { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }