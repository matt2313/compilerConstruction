{
    open Mattc_par
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = ['\n' '\r']+
let int = ['0'-'9']+

rule read = parse
    | whitespace        { read lexbuf } (* This ignores whitespace *)
    | newline           { read lexbuf } (* This ignores newlines *)
    | int as value      { INT (int_of_string value)}
    | "+"               { PLUS }
    | "-"               { MINUS }
    | "*"               { MULTIPLY }
    | "/"               { DIVIDE }
    | ";"               { EOE }
    | _                 { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }
    | eof               { EOF }