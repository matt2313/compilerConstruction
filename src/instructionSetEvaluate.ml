open InstructionSetType

exception InstructionSimulationError of string

(* Code for handling memory. Data is considered corrupt until a value is put into it (this makes errors more obvious) *)
type memoryCel =
    | Memory of int
    | CorruptMemory

let getMemory x = match x with
    | Memory(iVal)  -> iVal
    | CorruptMemory -> raise (InstructionSimulationError "Attempted corrupt memory access")
    
let ram = ref (Array.make 100 CorruptMemory)
let numRegisters = 10
let registers = ref (Array.make (numRegisters + 3) CorruptMemory)
let ramOffset = 0

let getRam n = getMemory (!ram.(n - ramOffset))
let setRam n x = !ram.(n - ramOffset) <- Memory(x)

let getRegAcc () = getMemory (!registers.(0))
let setRegAcc x = !registers.(0) <- Memory(x)
let getRegBase () = getMemory (!registers.(1))
let setRegBase x = !registers.(1) <- Memory(x)
let getRegStack () = getMemory (!registers.(2))
let setRegStack x = !registers.(2) <- Memory(x)
let getRegGeneric n = getMemory (!registers.(2 + n))
let setRegGeneric n x = !registers.(2 + n) <- Memory(x)

let clearMemory () = ram := (Array.make 100 CorruptMemory);
                     registers := (Array.make 13 CorruptMemory);
                     setRegBase ramOffset;
                     setRegStack (getRegBase ())



(* Code for processing addresses *)
let getValueFrom x = match x with
    | RegisterNum(iVal)  -> getRegGeneric iVal
    | StackAddress(iVal) -> getRam iVal
    | RegisterAcc        -> getRegAcc ()
    | RegisterStackPtr   -> getRegStack ()
    | RegisterBasePtr    -> getRegBase ()
    | NoAddress          -> raise (InstructionSimulationError "Null address failure in getValueFrom")
    
let setValueOf x iVal = match x with
    | RegisterNum(n)   -> setRegGeneric n iVal
    | StackAddress(n)  -> setRam n iVal
    | RegisterAcc      -> setRegAcc iVal
    | RegisterStackPtr -> setRegStack iVal
    | RegisterBasePtr  -> setRegBase iVal
    | NoAddress        -> raise (InstructionSimulationError "Null address failure in setValueOf")

let int_of_address x = match x with
    | StackAddress(n)  -> n
    | _                -> raise (InstructionSimulationError "Attempt to convert non-stack address to int")



(* Code for running the simulation *)
let processInstruction x = match x with
    | Add(lhs, rhs)              -> setRegAcc(getValueFrom(lhs) + getValueFrom(rhs))
    | Subtract(lhs, rhs)         -> setRegAcc(getValueFrom(lhs) - getValueFrom(rhs))
    | Multiply(lhs, rhs)         -> setRegAcc(getValueFrom(lhs) * getValueFrom(rhs))
    | Divide(lhs, rhs)           -> setRegAcc(getValueFrom(lhs) / getValueFrom(rhs))
    
    | StoreValue(addr)           -> setValueOf addr (getValueFrom RegisterAcc)
    | StoreValueIn(addr)         -> setRam (getValueFrom addr) (getValueFrom RegisterAcc)
    | LoadConstant(iVal)         -> setRegAcc iVal
    | LoadAddress(addr)          -> (match addr with
                                           | StackAddress(iVal) -> setRegAcc iVal
                                           | _                  -> raise (InstructionSimulationError "Attempt to load non-stack memory address")
                                    )
    | PushStack(iVal)            -> setRegStack ((getRegStack ()) + iVal)
    | PopStack(iVal)             -> setRegStack ((getRegStack ()) - iVal)
    | MoveData(addrFrom, addrTo) -> setValueOf addrTo (getValueFrom addrFrom)
    
    | Label(name)                -> ()
    | BlankLine                  -> ()
    
let rec evaluateInstructionSet' x = match x with
    | hd::tl -> processInstruction hd;evaluateInstructionSet' tl
    | []     -> getRegAcc ()
    
(* 3 specific registers + 10 generic registers *)
let evaluateInstructionSet x = clearMemory (); evaluateInstructionSet' x