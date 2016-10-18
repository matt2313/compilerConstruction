open Mattc_par
open Mattc_lex
open Lexing

let verbose = ref false

let getPosition lexbuf =
    let pos = lexbuf.lex_curr_p in
    (* pos_bol is offset of the current line, pos_cnum is the offset of the current char *)
    (* The difference between these 2 values is the offset from the start of the line *)
    "line: " ^ string_of_int pos.pos_lnum ^ ", char: " ^ string_of_int (abs (pos.pos_bol - pos.pos_cnum))
    
let parseWithErrors filename lexbuf =
    try Mattc_par.start Mattc_lex.read lexbuf with
    | SyntaxError message -> prerr_string("ERROR: " ^ message ^ " at " ^ getPosition lexbuf);
                             exit (-1)
    | Parsing.Parse_error -> prerr_string("Parse error in '" ^ filename ^ "' at " ^ getPosition lexbuf);
                             exit (-1)
                             
let parseWithoutErrors = Mattc_par.start Mattc_lex.read

let parseFile_quiet filename =
    let fileIn = open_in filename in
    Lexing.from_channel fileIn
    |> parseWithErrors filename
    |> ignore;  (* We don't want to do anything with the string we got from parsing *)
    close_in fileIn;
    print_endline ("File '" ^ filename ^ "' parsed correctly with value: ?")

(*
let parseFile_verbose filename =
    let fileIn = open_in filename in
    Lexing.from_channel fileIn
    |> parseWithErrors filename
    |> String.concat "\n"
    |> print_endline;
    close_in fileIn;
    print_endline ("File '" ^ filename ^ "' parsed correctly with value: ?")
*)
let parseFile_verbose = parseFile_quiet
    
let parseFile filename =
    if !verbose
    then parseFile_verbose filename
    else parseFile_quiet filename

let _ =
    let specList = [("-v", Arg.Set verbose, "Prints evaluations of lines as they are parsed")] in
    let usageMessage = "Compiles MattC Programs" in
    Arg.parse specList parseFile usageMessage;
    print_endline "All files parsed correctly"