(* This module contains the type that is used to generare the assembly code *)

type address =
    | RegisterNum of int            (* Generic register *)
    | StackAddress of int           (* Pointer to value on the stack (RAM) *)
    | RegisterAcc                   (* $acc, the acculminator register *)
    | RegisterStackPtr              (* Register for the stack pointer *)
    | RegisterBasePtr               (* Register for the base pointer *)
    | NoAddress                     (* Error value *)

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
    | MoveData of address * address (* Copy data from one address to another one *)
    
    | Label of string               (* Label used for jumping *)
    
    | BlankLine                     (* This isn't an instruction, it's used to render the output nicely *)



let address_toString x = match x with
    | RegisterNum(iVal)  -> "$" ^ (string_of_int iVal)
    | StackAddress(iVal) -> "&" ^ string_of_int iVal
    | RegisterAcc        -> "$acc"
    | RegisterStackPtr   -> "$stck"
    | RegisterBasePtr    -> "$base"
    | NoAddress          -> "noAddress"

let instruction_toString x = match x with
    | Add(lhs, rhs)       -> "add  " ^ (address_toString lhs) ^ " " ^ (address_toString rhs)
    | Subtract(lhs, rhs)  -> "sub  " ^ (address_toString lhs) ^ " " ^ (address_toString rhs)
    | Multiply(lhs, rhs)  -> "mul  " ^ (address_toString lhs) ^ " " ^ (address_toString rhs)
    | Divide(lhs, rhs)    -> "div  " ^ (address_toString lhs) ^ " " ^ (address_toString rhs)
    
    | StoreValue(address)   -> "str  " ^ (address_toString address)
    | StoreValueIn(address) -> "stri " ^ (address_toString address)
    | LoadConstant(iVal)    -> "ldc  " ^ (string_of_int iVal)
    | LoadAddress(addr)     -> "ldc  " ^ (address_toString addr)
    | PushStack(iVal)       -> "psh  " ^ (string_of_int iVal)
    | PopStack(iVal)        -> "pop  " ^ (string_of_int iVal)
    | MoveData(src, dest)   -> "mov  " ^ (address_toString src) ^ " " ^ (address_toString dest)
    
    | Label(name)         -> name ^ ":"
    | BlankLine           -> ""