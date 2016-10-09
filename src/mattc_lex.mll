{
    open Mattc_par
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = ['\n' '\r']+
let int = ['0'-'9']+
let comment = "//" [^ '\n' '\r']* ['\n' '\r']

rule read = parse
    | comment           { read lexbuf } (* This ignores comments *)
    | whitespace        { read lexbuf } (* This ignores whitespace *)
    | newline           { read lexbuf } (* This ignores newlines *)
    | int as value      { INT_LITERAL (int_of_string value)}
    | "+"               { PLUS }
    | "-"               { MINUS }
    | "*"               { MULTIPLY }
    | "/"               { DIVIDE }
    | ";"               { EOE }
    | "("               { OPBRACKET }
    | ")"               { CLBRACKET }
    | _                 { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }
    | eof               { EOF }