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

let instructionPointer = ref []

let getRam n = try getMemory (!ram.(n - ramOffset)) with
                   | InstructionSimulationError(str) -> raise (InstructionSimulationError (str ^ " in RAM location " ^ (string_of_int n)))
let setRam n x = !ram.(n - ramOffset) <- Memory(x)

let getRegAcc () = try getMemory (!registers.(0)) with
                   | InstructionSimulationError(str) -> raise (InstructionSimulationError (str ^ " in acc register"))
let setRegAcc x = !registers.(0) <- Memory(x)
let getRegBase () = try getMemory (!registers.(1)) with
                   | InstructionSimulationError(str) -> raise (InstructionSimulationError (str ^ " in base register"))
let setRegBase x = !registers.(1) <- Memory(x)
let getRegStack () = try getMemory (!registers.(2)) with
                   | InstructionSimulationError(str) -> raise (InstructionSimulationError (str ^ " in stack register"))
let setRegStack x = !registers.(2) <- Memory(x)
let getRegGeneric n = try getMemory (!registers.(2 + n)) with
                   | InstructionSimulationError(str) -> raise (InstructionSimulationError (str ^ " in general register " ^ (string_of_int n)))
let setRegGeneric n x = !registers.(2 + n) <- Memory(x)

let getInstructionPointer () = match !instructionPointer with
                                     | hd::tl -> instructionPointer := tl; hd
                                     | []     -> raise (InstructionSimulationError "No instruction pointer found")
let setInstructionPointer x = instructionPointer := x::!instructionPointer

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
    
(* Like getValueFrom, but doesn't care or warn you if you try to read invalid memory *)
let getValueFromUnsafe x = try getValueFrom x with
    | InstructionSimulationError(_) -> -1
    
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
    | MoveData(addrFrom, addrTo) -> setValueOf addrTo (getValueFrom addrFrom)
    
    | PushStack(iVal)            -> setRegStack ((getRegStack ()) + iVal)
    | PushOnStack(addr)          -> setRegStack ((getRegStack ()) + 1); setRam (getRegStack ()) (getValueFromUnsafe addr)
    | PushRegisters              -> for n = 1 to numRegisters do
                                        setRegStack ((getRegStack ()) + 1);
                                        setRam (getRegStack ()) (getRegGeneric n)
                                    done
    | PopStack(iVal)             -> setRegStack ((getRegStack ()) - iVal)
    | PopFromStack(addr)         -> setValueOf addr (getRam (getRegStack ())); setRegStack ((getRegStack ()) - 1)
    | PopRegisters               -> for n = 0 to (numRegisters - 1) do
                                        setRegGeneric (numRegisters - n) (getRam (getRegStack ()));
                                        setRegStack ((getRegStack ()) - 1)
                                    done
    
    | Jump(_)                       
    | JumpIfZero(_)                 
    | JumpIfNotZero(_)              
    | JumpIfGreaterThanZero(_)      
    | JumpIfGreaterOrEqualToZero(_) 
    | Label(_)                      
    | Call(_)                    
    | Return                     
    | BlankLine                  -> ()
    
let rec findLabel lbl searchList = match searchList with
    | Label(name)::tl when name = lbl -> tl
    | _::tl                           -> findLabel lbl tl
    | []                              -> raise (InstructionSimulationError ("Label '" ^ lbl ^ "' not found, cannot be jumped to"))

let rec evaluateInstructionSet' currSet completeSet = match currSet with
    | hd::tl -> let newTail = match hd with
                              | Jump(lbl)                       -> findLabel lbl completeSet
                              | JumpIfZero(lbl)                 -> if (getRegAcc ())  = 0 then findLabel lbl completeSet else tl
                              | JumpIfNotZero(lbl)              -> if (getRegAcc ()) != 0 then findLabel lbl completeSet else tl
                              | JumpIfGreaterThanZero(lbl)      -> if (getRegAcc ()) >  0 then findLabel lbl completeSet else tl
                              | JumpIfGreaterOrEqualToZero(lbl) -> if (getRegAcc ()) >= 0 then findLabel lbl completeSet else tl
                              | Call(lbl)                       -> setInstructionPointer tl; findLabel lbl completeSet
                              | Return                          -> getInstructionPointer ()
                              | _                               -> tl
                in
                (*print_endline (instruction_toString hd);*) (* Uncomment for debugging *)
                processInstruction hd;
                evaluateInstructionSet' newTail completeSet
    | []     -> getRegAcc ()
    
let evaluateInstructionSet x = clearMemory (); evaluateInstructionSet' x x