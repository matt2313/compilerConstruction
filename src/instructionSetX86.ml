(* This module contains the type that is used to convert the generic instructions into actual X86 assembly code *)
open InstructionSetType

exception X86GenerationError of string

let variableSize = 8    (* Number of bytes in an int *)
let stackStart = -8     (* Position of the first element of the stack we can write to *)
let stackDirection = -1 (* The stack grows down *)

type addressX86 =
    | GeneralRegisterX86 of int (* general registers eax to e15 *)
    | BasePointerX86
    | StackPointerX86
    | StackAddressX86 of int

type instructionX86 =
    | X86_Add      of addressX86 * addressX86
    | X86_Sub      of addressX86 * addressX86
    | X86_Mul      of addressX86                    (* imul and idiv assume the lhs is the 1st general register *)
    | X86_Div      of addressX86
    
    | X86_MoveAddress of addressX86 * addressX86    (* Moves the value of one address into another address *)
    | X86_MoveConstant of int * addressX86          (* Moves a constant value into an address *)
    | X86_MoveValueOf of addressX86 * addressX86    (* Moves a value into the address another address points to *)
    | X86_MoveValueOf_In of addressX86 * addressX86 (* Like X86_MoveValueOf, but treats the 2nd address as a pointer *)
    
    | X86_Push of addressX86                        (* Pushes the value in the given address onto the stack *)
    | X86_Pop of addressX86                         (* Pops the top value from the stack into the given address *)
    | X86_AddAddress of addressX86 * int            (* Add a constant to the value in an address *)
    
    | X86_Label of string                           (* Label that can be jumped to *)
    | X86_Jump of string                            (* Unconditional jump *)
    | X86_JumpIfEqual of string                     (* Conditional jumps based on the value of the flag register *)
    | X86_JumpIfNotEqual of string
    | X86_JumpIfGreaterThan of string
    | X86_JumpIfGreaterOrEqual of string
    | X86_Call of string
    | X86_Return
    

let isRegisterAddress x = match x with
    | StackAddressX86(_) -> false
    | _                  -> true
    
let rec addressX86_toString x = match x with
    | GeneralRegisterX86(0) -> "%rax"
    | GeneralRegisterX86(1) -> "%rbx"
    | GeneralRegisterX86(2) -> "%rcx"
    | GeneralRegisterX86(n) -> if n <= 10 then "%r" ^ (string_of_int (n + 5)) else raise (X86GenerationError ("General register number too high: " ^ (string_of_int n)))
    | BasePointerX86        -> "%rbp"
    | StackPointerX86       -> "%rsp"
    | StackAddressX86(n)    -> (string_of_int n) ^ "(" ^ (addressX86_toString BasePointerX86) ^ ")"

let addressX86_toInt x = match x with
    | StackAddressX86(n) -> n
    | _                  -> raise (X86GenerationError "Cannot convert this addressX86 to int")

let instructionX86_toString x = match x with
    | X86_Add(lhs, rhs)             -> "addq " ^ (addressX86_toString lhs) ^ ", " ^ (addressX86_toString rhs)
    | X86_Sub(lhs, rhs)             -> "subq " ^ (addressX86_toString lhs) ^ ", " ^ (addressX86_toString rhs)
    | X86_Mul(addr)                 -> "imulq " ^ (addressX86_toString addr)
    | X86_Div(addr)                 -> "divq " ^ (addressX86_toString addr)

    | X86_MoveAddress(lhs, rhs)     -> "leaq " ^ (addressX86_toString lhs) ^ ", " ^ (addressX86_toString rhs)
    | X86_MoveConstant(n, addr)     -> "movq $" ^ (string_of_int n) ^ ", " ^ (addressX86_toString addr)
    | X86_MoveValueOf(lhs, rhs)     -> "movq " ^ (addressX86_toString lhs) ^ ", " ^ (addressX86_toString rhs)
    | X86_MoveValueOf_In(lhs, rhs)  -> "movq " ^ (addressX86_toString lhs) ^ ", (" ^ (addressX86_toString rhs) ^ ")"
    
    | X86_Push(addr)                -> "pushq " ^ (addressX86_toString addr)
    | X86_Pop(addr)                 -> "popq " ^ (addressX86_toString addr)
    
    | X86_AddAddress(addr, n)       -> if isRegisterAddress addr
                                      then let addrStr = addressX86_toString addr in
                                           "lea " ^ (string_of_int n) ^ "(" ^ addrStr ^ "), " ^ addrStr
                                      else raise (X86GenerationError "Cannot use AddAddress on stack address")
                                      
    | X86_Jump(lbl)                 -> "jmp MC_" ^ lbl
    | X86_JumpIfEqual(lbl)          -> "je MC_" ^ lbl 
    | X86_JumpIfNotEqual(lbl)       -> "jne MC_" ^ lbl
    | X86_JumpIfGreaterThan(lbl)    -> "jnle MC_" ^ lbl
    | X86_JumpIfGreaterOrEqual(lbl) -> "jnl MC_" ^ lbl
    | X86_Call(lbl)                 -> "callq MC_" ^ lbl
    | X86_Return                    -> "ret"
    
    | X86_Label(lbl)                -> "MC_" ^ lbl ^ ":"

let addressToX86 x = match x with
    | RegisterNum(n)    -> if 0 < n && n <= 13 then GeneralRegisterX86(n) else raise (X86GenerationError ("Cannot convert register num: " ^ (string_of_int n)))
    | StackAddress(n)   -> StackAddressX86(stackStart + stackDirection * variableSize * (n - 1))
    | RegisterAcc       -> GeneralRegisterX86(0)
    | RegisterStackPtr  -> StackPointerX86
    | RegisterBasePtr   -> BasePointerX86
    | NoAddress         -> raise (X86GenerationError "Cannot convert NoAddress to X86")
    
let operationX86 lhs rhs op =
    let lhsX86 = addressToX86 lhs in
    let rhsX86 = addressToX86 rhs in
    op lhsX86 rhsX86

let instructionToX86List x = match x with
    | Add(lhs, rhs)      -> operationX86 lhs rhs (fun x y -> [X86_MoveValueOf(x, GeneralRegisterX86(0)); X86_Add(y, GeneralRegisterX86(0))])
    | Subtract(lhs, rhs) -> operationX86 lhs rhs (fun x y -> [X86_MoveValueOf(x, GeneralRegisterX86(0)); X86_Sub(y, GeneralRegisterX86(0))])
    | Multiply(lhs, rhs) -> operationX86 lhs rhs (fun x y -> [X86_MoveValueOf(x, GeneralRegisterX86(0)); X86_Mul(y); X86_MoveValueOf(y, GeneralRegisterX86(0))])
    | Divide(lhs, rhs)   -> operationX86 lhs rhs (fun x y -> [X86_MoveValueOf(x, GeneralRegisterX86(0)); X86_Div(y); X86_MoveValueOf(y, GeneralRegisterX86(0))])
    
    | StoreValue(addr)   -> [X86_MoveValueOf(GeneralRegisterX86(0), (addressToX86 addr))]
    | StoreValueIn(addr) -> [X86_MoveValueOf_In(GeneralRegisterX86(0), (addressToX86 addr))]
    | LoadConstant(n)    -> [X86_MoveConstant(n, GeneralRegisterX86(0))]
    | LoadAddress(addr)  -> [X86_MoveAddress((addressToX86 addr), (GeneralRegisterX86(0)))]
    
    | PushStack(n)       -> [X86_MoveConstant(n * stackDirection * variableSize, GeneralRegisterX86(0)); X86_Add(GeneralRegisterX86(0), StackPointerX86)]
    | PushOnStack(addr)  -> [X86_Push(addressToX86 addr)]
    | PushRegisters      -> []
    | PopStack(n)        -> [X86_MoveConstant(-1 * n * stackDirection * variableSize, GeneralRegisterX86(0)); X86_Add(GeneralRegisterX86(0), StackPointerX86)]
    | PopFromStack(addr) -> [X86_Pop(addressToX86 addr)]
    | PopRegisters       -> []
    
    | MoveData(lhs, rhs) -> [X86_MoveValueOf((addressToX86 lhs), (addressToX86 rhs))]
    
    | Label(lbl)                      -> [X86_Label(lbl)]
    | Jump(lbl)                       -> [X86_Jump(lbl)]
    | JumpIfZero(lbl)                 -> [X86_JumpIfEqual(lbl)]
    | JumpIfNotZero(lbl)              -> [X86_JumpIfNotEqual(lbl)]
    | JumpIfGreaterThanZero(lbl)      -> [X86_JumpIfGreaterThan(lbl)]
    | JumpIfGreaterOrEqualToZero(lbl) -> [X86_JumpIfGreaterOrEqual(lbl)]
    | Call(lbl)                       -> [X86_Call(lbl)]
    | Return                          -> [X86_Return]
    
    | BlankLine          -> []

let rec instructionListToX86List x = match x with
    | hd::tl -> (instructionToX86List hd)@(instructionListToX86List tl)
    | []     -> []