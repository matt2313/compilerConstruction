{
    open Mattc_par
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = ['\n' '\r']
let int = ['0'-'9']+
let float_a = ['0'-'9']+ "." ['0'-'9']*
let float_b = ['0'-'9']+ ("." ['0'-'9']*)? "f"
let string = "\"" [^ '"']* "\""
(* (['a'-'z']|['A'-'Z'])+ (['a'-'z']|['A'-'Z']|['0'-'9'])* *)
let single_line_comment = "//" [^ '\n' '\r']*
let not_eq = "!=" | "<>"

rule read = parse
    | single_line_comment   { read lexbuf } (* This ignores comments *)
    | "/*"                  { read_multi_line_comment lexbuf }
    | whitespace            { read lexbuf } (* This ignores whitespace *)
    | newline               { Lexing.new_line lexbuf; read lexbuf } (* We need to keep track of when we go to a new line *)
    | int as value          { INT_LITERAL (int_of_string value) }
    | float_a as value      { FLOAT_LITERAL (float_of_string value) }
    | float_b as value      { FLOAT_LITERAL (float_of_string (String.sub value 0 ((String.length value) - 1))) } (* The f isn't part of the float value *)
    | string as value       { STRING_LITERAL (String.sub value 1 ((String.length value) - 2)) } (* Don't count the opening and closing quotes as part of the value *)
    | "read_int"            { READ_INT }
    | "read_string"         { READ_STRING }
    | "print_int"           { PRINT_INT }
    | "print_string"        { PRINT_STRING }
    | "while"               { WHILE }
    | "do"                  { DO }
    | "if"                  { IF }
    | "else"                { ELSE }
    | "true"                { BOOL_LITERAL (true) }
    | "false"               { BOOL_LITERAL (false) }
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "*"                   { MULTIPLY }
    | "/"                   { DIVIDE }
    | "^"                   { CONCAT }
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
    | "{"                   { OPBRACE }
    | "}"                   { CLBRACE }
    | eof                   { EOF }
    | _                     { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }

(* Can't do this with a simple regular expression since we have to keep track of lines *)
and read_multi_line_comment = parse
    | newline               { Lexing.new_line lexbuf; read_multi_line_comment lexbuf }
    | [^ '*']               { read_multi_line_comment lexbuf }
    | "*" newline           { Lexing.new_line lexbuf; read_multi_line_comment lexbuf }
    | "*" [^ '/']#newline   { read_multi_line_comment lexbuf }
    | "*/"                  { read lexbuf } (* Go back to normal *)
    | eof                   { raise(SyntaxError("Missing end of multi-line comment. Did you miss a '*/'?")) }