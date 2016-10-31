open ParseTreeType
open ParseTreeEvaluator
open Mattc_par
open Mattc_lex
open Lexing

let verbose = ref false
let evaluateFile = ref false
let optimise = ref false

let getPosition lexbuf =
    let pos = lexbuf.lex_curr_p in
    (* pos_bol is offset of the current line, pos_cnum is the offset of the current char *)
    (* The difference between these 2 values is the offset from the start of the line *)
    "line: " ^ string_of_int pos.pos_lnum ^ ", char: " ^ string_of_int (abs (pos.pos_bol - pos.pos_cnum))
    
let parseWithErrors filename lexbuf =
    try Mattc_par.start Mattc_lex.read lexbuf with
    | SyntaxError message     -> prerr_string("Syntax error (" ^ message ^ ") in '" ^ filename ^ "' at " ^ getPosition lexbuf);
                                 exit (-1)
    | Parsing.Parse_error     -> prerr_string("Parse error in '" ^ filename ^ "' at " ^ getPosition lexbuf);
                                 exit (-1)

let parseWithoutErrors = Mattc_par.start Mattc_lex.read

let printFileResult x filename =
    if !evaluateFile then
        try let eval = parseTree_eval x emptyStore in print_endline ""; print_endline ("File '" ^ filename ^ "' parsed correctly with value: " ^ (valueToString eval.evaluation)) with
        | EvaluationError message -> prerr_string("Evaluation error in '" ^ filename ^ "' (" ^ message ^ ")");
                                     exit(-1)
    else
        print_endline ("File '" ^ filename ^ "' parsed correctly.")

let parseFile_quiet filename =
    let fileIn = open_in filename in
    let tree = Lexing.from_channel fileIn
    |> parseWithErrors filename in
    close_in fileIn;
    printFileResult tree filename

let parseFile_verbose filename =
    let fileIn = open_in filename in
    let tree = Lexing.from_channel fileIn
    |> parseWithErrors filename in
    print_endline (string_of_parseTree tree);
    close_in fileIn;
    printFileResult tree filename
    
let parseFile filename =
    if !verbose
    then parseFile_verbose filename
    else parseFile_quiet filename

let _ =
    let specList = [("-v", Arg.Set verbose, "Prints evaluations of lines as they are parsed");
                    ("-e", Arg.Set evaluateFile, "Evaluates the program after it has been parsed");
                    ("-o", Arg.Set optimise, "Optimises the program before evaluation and compilation")] in
    let usageMessage = "Compiles MattC Programs" in
    Arg.parse specList parseFile usageMessage;
    print_endline "All files parsed correctly"