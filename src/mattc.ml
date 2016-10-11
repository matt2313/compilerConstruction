open Mattc_par
open Mattc_lex

(* At the moment we assume we've been given a single input file *)

let rec printAll arr = match arr with
    | [] -> ()
    | hd::tl -> print_endline hd; printAll tl
    
let _ =
    let argc = Array.length Sys.argv in
    (* Note that the 1st argument is the name of the program itself, which we ignore *)
    if argc = 2
    then let fileIn = open_in (Array.get Sys.argv 1) in
        Lexing.from_channel fileIn
        |> Mattc_par.start Mattc_lex.read
        (* This only works if we know every line evaluates to a string *)
        |> String.concat "\n"
        |> print_endline;
        close_in fileIn;
        print_endline "File parsed correctly!"
    else (print_endline ("Error! Expected 1 argument but got " ^ string_of_int (argc - 1)))