{
    open Mattc_par
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = ['\n' '\r']+
let int = ['0'-'9']+
let single_line_comment = "//" [^ '\n' '\r']* ['\n' '\r']
let multi_line_comment_open = "/*"
let multi_line_comment_close = "*/"

rule read = parse
    | single_line_comment   { read lexbuf } (* This ignores comments *)
    | multi_line_comment_open   { read_comment lexbuf }
    | whitespace            { read lexbuf } (* This ignores whitespace *)
    | newline               { read lexbuf } (* This ignores newlines *)
    | int as value          { INT_LITERAL (int_of_string value) }
    | "+"               { PLUS }
    | "-"               { MINUS }
    | "*"               { MULTIPLY }
    | "/"               { DIVIDE }
    | ";"               { EOE }
    | "("               { OPBRACKET }
    | ")"               { CLBRACKET }
    | _                 { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }
    | eof               { EOF }
and read_comment = parse
    | multi_line_comment_close  { read lexbuf }
    | _                         { read_comment lexbuf }
