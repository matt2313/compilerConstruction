open Mattc_par
open Mattc_lex
open Lexing

(* At the moment we assume we've been given a single input file *)

let getPosition lexbuf =
    let pos = lexbuf.lex_curr_p in
    (* pos_bol is offset of the current line, pos_cnum is the offset of the current char *)
    (* The difference between these 2 values is the offset from the start of the line *)
    "line: " ^ string_of_int pos.pos_lnum ^ ", char: " ^ string_of_int (abs (pos.pos_bol - pos.pos_cnum))

let rec printAll arr = match arr with
    | [] -> ()
    | hd::tl -> print_endline hd; printAll tl
    
let parseWithErrors lexbuf =
    try Mattc_par.start Mattc_lex.read lexbuf with
    | SyntaxError message -> prerr_string("ERROR: " ^ message ^ " at " ^ getPosition lexbuf);
                             exit (-1)
    | Parsing.Parse_error -> prerr_string("Parse error at " ^ getPosition lexbuf);
                             exit (-1)
                             
let parseWithoutErrors = (Mattc_par.start Mattc_lex.read)
    
let _ =
    let argc = Array.length Sys.argv in
    (* Note that the 1st argument is the name of the program itself, which we ignore *)
    if argc = 2
    then let fileIn = open_in (Array.get Sys.argv 1) in
        Lexing.from_channel fileIn
        |> parseWithErrors
        (* This only works if we know every line evaluates to a string *)
        |> String.concat "\n"
        |> print_endline;
        close_in fileIn;
        print_endline "File parsed correctly!";
    else print_endline ("Error! Expected 1 argument but got " ^ string_of_int (argc - 1))