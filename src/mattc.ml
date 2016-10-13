open Mattc_par
open Mattc_lex
open Lexing

let verbose = ref false

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

let parseFile filename =
    let fileIn = open_in filename in
    Lexing.from_channel fileIn
    |> parseWithErrors
    |> ignore;  (* We don't want to do anything with the string we got from parsing *)
    close_in fileIn;
    print_endline ("File '" ^ filename ^ "' parsed correctly")

let parseFile_verbose filename =
    let fileIn = open_in filename in
    Lexing.from_channel fileIn
    |> parseWithErrors
    |> String.concat "\n"
    |> print_endline;
    close_in fileIn;
    print_endline ("File '" ^ filename ^ "' parsed correctly")

let _ =
    let specList = [("-v", Arg.Set verbose, "Prints evaluations of lines as they are parsed")] in
    let anon_func = if !verbose then parseFile_verbose else parseFile in
    let usageMessage = "Compiles MattC Programs" in
    Arg.parse specList anon_func usageMessage
