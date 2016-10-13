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
let identifier = (['a'-'z' 'A'-'Z' '_']) (['0'-'9' 'a'-'z' 'A'-'Z' '_'])*
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
    | "true"                { BOOL_LITERAL (true) }
    | "false"               { BOOL_LITERAL (false) }
    | "int"                 { INT_TYPENAME }
    | "float"               { FLOAT_TYPENAME }
    | "bool"                { BOOL_TYPENAME }
    | "string"              { STRING_TYPENAME }
    | "as"                  { CAST }
    | "let"                 { LET }
    | "new"                 { NEW }
    | "in"                  { IN }
    | "read_int"            { READ_INT }
    | "read_float"          { READ_FLOAT }
    | "read_bool"           { READ_BOOL }
    | "read_string"         { READ_STRING }
    | "print_int"           { PRINT_INT }
    | "print_float"         { PRINT_FLOAT }
    | "print_bool"          { PRINT_BOOL }
    | "print_string"        { PRINT_STRING }
    | "substring"           { SUBSTRING }
    | "length"              { LENGTH }
    | "while"               { WHILE }
    | "do"                  { DO }
    | "if"                  { IF }
    | "else"                { ELSE }
    | "return"              { RETURN }
    | ":="                  { ASSIGN }
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "*"                   { MULTIPLY }
    | "/"                   { DIVIDE }
    | "^"                   { CONCAT }
    | ","                   { SEPERATOR }
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
    | identifier as name    { IDENTIFIER name }
    | _                     { raise(SyntaxError("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }

(* Can't do this with a simple regular expression since we have to keep track of lines *)
and read_multi_line_comment = parse
    | newline               { Lexing.new_line lexbuf; read_multi_line_comment lexbuf }
    | [^ '*']               { read_multi_line_comment lexbuf }
    | "*" newline           { Lexing.new_line lexbuf; read_multi_line_comment lexbuf }
    | "*" [^ '/']#newline   { read_multi_line_comment lexbuf }
    | "*/"                  { read lexbuf } (* Go back to normal *)
    | eof                   { raise(SyntaxError("Missing end of multi-line comment. Did you miss a '*/'?")) }