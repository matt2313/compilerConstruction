open ParseTreeType
open ParseTreeEvaluator
open ParseTreeOptimiser
open Mattc_par
open Mattc_lex
open Lexing

type inputType =
    | TerminalInput
    | FileInput of string
    
let verbose = ref false
let verboseAfterOptimisation = ref false
let evaluateFile = ref false
let optimise = ref false
let input = ref TerminalInput

let setInputFilename name = input := FileInput(name)
let setTerminalInput () = input := TerminalInput

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
        try let eval = (match !input with
                              | TerminalInput            -> evaluateParseTree x (read_int) (read_float) (read_bool) (read_line)
                              | FileInput(inputFileName) -> let inputFileIn = open_in inputFileName in
                                                            let intFromFile () = int_of_string (input_line inputFileIn) in
                                                            let floatFromFile () = float_of_string (input_line inputFileIn) in
                                                            let boolFromFile () = bool_of_string (input_line inputFileIn) in
                                                            let stringFromFile () = input_line inputFileIn in
                                                            evaluateParseTree x (intFromFile) (floatFromFile) (boolFromFile) (stringFromFile)
                       ) in print_endline ("File '" ^ filename ^ "' parsed correctly with value: " ^ (valueToString eval.evaluation))
            with
        | EvaluationError message -> prerr_string("Evaluation error in '" ^ filename ^ "' (" ^ message ^ ")");
                                     exit(-1)
    else
        print_endline ("File '" ^ filename ^ "' parsed correctly.")
    
let parseFile filename =
    let fileIn = open_in filename in
    let tree = Lexing.from_channel fileIn
    |> parseWithErrors filename in
    if !verbose then print_endline ("Parse tree for raw '" ^ filename ^ "':"); print_endline (string_of_parseTree tree);
    close_in fileIn;
    let tree = if !optimise then optimiseParseTree tree else tree in
    if !verboseAfterOptimisation && !optimise then print_endline ("Parse tree for optimised '" ^ filename ^ "':"); print_endline (string_of_parseTree tree);
    printFileResult tree filename;
    print_endline "";
    print_endline ""

let _ =
    let specList = [("-v", Arg.Set verbose, "Prints the parse tree before optimisation");
                    ("-e", Arg.Set evaluateFile, "Evaluates the program after it has been parsed");
                    ("-i", Arg.String setInputFilename, "Sets the file to use as input");
                    ("-terminalInput", Arg.Unit setTerminalInput, "Sets the terminal to be used as input");
                    ("-o", Arg.Set optimise, "Optimises the program before evaluation and compilation");
                    ("-ov", Arg.Set verboseAfterOptimisation, "Prints the parse tree after optimisation")] in
    let usageMessage = "Compiles MattC Programs" in
    Arg.parse specList parseFile usageMessage;
    print_endline "All files parsed correctly"