(* This module contains the type that is used to convert the generic instructions into actual X86 assembly code *)
open InstructionSetType

exception X86GenerationError of string

type addressX86 =
    | NoAddress

type instructionX86 =
    | X86_MoveData of addressX86 * addressX86


let addressX86_toString x = match x with
    | NoAddress -> "noaddress"

let instructionX86_toString x = match x with
    | X86_MoveData(lhs, rhs) -> "movd " ^ (addressX86_toString lhs) ^ " " ^ (addressX86_toString rhs)
    

(*
type instruction =
    | Add of address * address      (* Add value of 2 addresses and store the result in $acc *)
    | Subtract of address * address (* Subtract value of 2 addresses and store the result in $acc *)
    | Multiply of address * address (* Multiply value of 2 addresses and store the result in $acc *)
    | Divide of address * address   (* Divide value of 2 addresses and store the result in $acc *)
    
    | StoreValue of address         (* Take the value in $acc and store it in memory *)
    | StoreValueIn of address       (* Take the value in $acc and store it in the memory address referenced by the given address *)
    | LoadConstant of int           (* Place a literal value into $acc *)
    | LoadAddress of address        (* Place an address into $acc (NOT the value the address holds) *)
    | PushStack of int              (* Push the stack by the given ammount *)
    | PopStack of int               (* Pop the stack by the given ammount *)
    
    | Label of string               (* Label used for jumping *)
    
    | BlankLine                     (* This isn't an instruction, it's used to render the output nicely *)
*)