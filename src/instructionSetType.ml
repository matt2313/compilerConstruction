(* This module contains the type that is used to generare the assembly code *)

type address =
    | RegisterNum of int            (* Generic register *)
    | StackAddress of int           (* Offset from the current stack pointer *)

type instruction =
    | Add of address * address      (* Add 2 addresses and store the result in $acc *)
    | Subtract of address * address (* Subtract 2 addresses and store the result in $acc *)
    | StoreValue of address         (* Take the value in $acc and store it in memory *)
    | LoadConstant of int           (* Place a literal value into $acc *)
    | PushStack of int              (* Push the stack by the given ammount *)
    | PopStack of int               (* Pop the stack by the given ammount *)
  (*| GetStackPointer               (* Places the stack pointer into $acc *)*)
    | Label of string               (* Label used for jumping *)

type program = instruction list



let address_toString x = match x with
    | RegisterNum(iVal)  -> "$" ^ (string_of_int iVal)
    | StackAddress(iVal) -> string_of_int iVal

let instruction_toString x = match x with
    | Add(lhs, rhs)       -> "add  " ^ (address_toString lhs) ^ " " ^ (address_toString rhs)
    | Subtract(lhs, rhs)  -> "sub  " ^ (address_toString lhs) ^ " " ^ (address_toString rhs)
    | StoreValue(address) -> "str  " ^ (address_toString address)
    | LoadConstant(iVal)  -> "ldc  " ^ (string_of_int iVal)
    | PushStack(iVal)     -> "psh  " ^ (string_of_int iVal)
    | PopStack(iVal)      -> "pop  " ^ (string_of_int iVal)
  (*| GetStackPointer     -> "stck "*)
    | Label(name)         -> "\n" ^ name ^ ":"

let rec instructionList_toString x = match x with
    | hd::tl -> (instruction_toString hd) ^ "\n" ^ (instructionList_toString tl)
    | []     -> ""