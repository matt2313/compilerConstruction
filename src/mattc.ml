open ParseTreeType
open ParseTreeEvaluator
open ParseTreeOptimiser

open InstructionSetType
open InstructionSetConvert
open InstructionSetEvaluate
open InstructionSetX86

open Mattc_par
open Mattc_lex
open Lexing

type inputType =
    | TerminalInput
    | FileInput of string

type compileType =
    | CompileNone
    | CompileAbstract
    | CompileX86 of string
    
let verbose = ref false
let verboseAfterOptimisation = ref false
let evaluateFile = ref false
let optimise = ref false
let input = ref TerminalInput
let compile = ref CompileNone
let outFilename = ref ""
let simulate = ref false

let setInputFilename name = input := FileInput(name)
let setTerminalInput () = input := TerminalInput

let setOutFilename newName = outFilename := newName
let getOutFilename () = if !outFilename == ""
                        then (match !input with
                                    | FileInput(name) -> name ^ ".program"
                                    | TerminalInput   -> "out.program"
                             )
                        else !outFilename

let setCompileNone () = compile := CompileNone
let setCompileAbstract () = compile := CompileAbstract
let setCompileX86 templateFilename = compile := CompileX86 templateFilename

let expectedValue = ref ""
let setExpectedValue s = expectedValue := s
let checkAgainstExpected v =
    let gotValue = valueToString v in
    if !expectedValue = "" || !expectedValue = gotValue
    then ()
    else (prerr_string ("Expected " ^ !expectedValue ^ " but got " ^ gotValue); exit(-1))

let getPosition lexbuf =
    let pos = lexbuf.lex_curr_p in
    (* pos_bol is offset of the current line, pos_cnum is the offset of the current char *)
    (* The difference between these 2 values is the offset from the start of the line *)
    "line: " ^ string_of_int pos.pos_lnum ^ ", char: " ^ string_of_int (abs (pos.pos_bol - pos.pos_cnum))
    
let parseWithErrors filename lexbuf =
    try Mattc_par.start Mattc_lex.read lexbuf with
    | SyntaxError message     -> prerr_string("Syntax error (" ^ message ^ ") in '" ^ filename ^ "' at " ^ getPosition lexbuf);
                                 exit (1)
    | Parsing.Parse_error     -> prerr_string("Parse error in '" ^ filename ^ "' at " ^ getPosition lexbuf);
                                 exit (2)

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
                       ) in (checkAgainstExpected eval.evaluation; print_endline ("File '" ^ filename ^ "' parsed correctly with value: " ^ (valueToString eval.evaluation)))
            with
        | EvaluationError message -> prerr_string("Evaluation error in '" ^ filename ^ "' (" ^ message ^ ")");
                                     exit(3)
    else
        print_endline ("File '" ^ filename ^ "' parsed correctly.")

let rec printInstructionsToFile stream instructions = match instructions with
    | hd::tl -> output_string stream ((instruction_toString hd) ^ "\n");
                printInstructionsToFile stream tl
    | []     -> flush stream
    
let rec printX86InstructionsToFile stream instructions = match instructions with
    | hd::tl -> output_string stream ("\t" ^ (instructionX86_toString hd) ^ "\n");
                printX86InstructionsToFile stream tl
    | []     -> flush stream

let rec printX86InstructionsToFileWithTemplate template destination instructions =
    try
        while true do
            let currLine = input_line template in
            (* Copy the template file to the destination file, but replacing a specific line with the generated code *)
            if currLine = "##### START_GENERATED_CODE"
            then (output_string destination ("# Start of generated code" ^ "\n");
                  printX86InstructionsToFile destination instructions;
                  output_string destination ("# End of generated code" ^ "\n")
                 )
            else output_string destination (currLine ^ "\n")
        done
    with End_of_file -> ()
    
let parseFile filename =
    let fileIn = open_in filename in
    let tree = Lexing.from_channel fileIn
    |> parseWithErrors filename in
    if !verbose then (print_endline ("Parse tree for raw '" ^ filename ^ "':"); print_endline (string_of_parseTree tree));
    close_in fileIn;
    let tree = if !optimise then optimiseParseTree tree else tree in
    if !verboseAfterOptimisation && !optimise then (print_endline ("Parse tree for optimised '" ^ filename ^ "':"); print_endline (string_of_parseTree tree));
    (match !compile with
           | CompileNone                  -> ()
           | CompileAbstract              -> let instructions = instructionList_of_parseTree tree numRegisters in
                                             let newFilename = getOutFilename () in
                                             let fileOut = open_out newFilename in
                                             printInstructionsToFile fileOut instructions;
                                             close_out fileOut;
                                             print_endline ("Generated '" ^ newFilename ^ "'");
                                             if !simulate then let returnValue = IntValue(evaluateInstructionSet instructions) in (checkAgainstExpected returnValue; print_endline ("Simulated '" ^ newFilename ^ "' with return value: '" ^ (valueToString returnValue) ^ "'"))
           | CompileX86(templateFileName) -> let instructions = instructionList_of_parseTree tree numRegisters in
                                             let newFilename = getOutFilename () in
                                             let fileOut = open_out newFilename in
                                             let fileIn = open_in templateFileName in
                                             printX86InstructionsToFileWithTemplate fileIn fileOut (instructionListToX86List instructions);
                                             close_out fileOut;
                                             close_in fileIn;
                                             print_endline ("Generated '" ^ newFilename ^ "'")
    );
    printFileResult tree filename;
    print_endline "";
    print_endline ""

let _ =
    let specList = [("-v", Arg.Set verbose, "Prints the parse tree before optimisation");
                    ("-q", Arg.Unit (fun () -> verbose := false; verboseAfterOptimisation := false), "Removes the '-v' and '-ov' options");
                    ("-e", Arg.Set evaluateFile, "Evaluates the program after it has been parsed");
                    ("-i", Arg.String setInputFilename, "Sets the file to use as input");
                    ("-terminalInput", Arg.Unit setTerminalInput, "Sets the terminal to be used as input");
                    ("-o", Arg.Set optimise, "Optimises the program before evaluation and compilation");
                    ("-no", Arg.Clear optimise, "Stops the code from being optimised");
                    ("-ov", Arg.Set verboseAfterOptimisation, "Prints the parse tree after optimisation");
                    ("-c", Arg.Unit setCompileAbstract, "Compiles the program into an abstract machine language");
                    ("-x86", Arg.String setCompileX86, "Compiles the program into x86 code with the given template");
                    ("-out", Arg.String setOutFilename, "Sets the name of the compiled file");
                    ("-s", Arg.Set simulate, "Simulates the compiled instruction set");
                    ("-exp", Arg.String setExpectedValue, "Sets the expected simulated or evaluated value")] in
    let usageMessage = "Compiles MattC Programs" in
    Arg.parse specList parseFile usageMessage;
    print_endline "All files parsed correctly"