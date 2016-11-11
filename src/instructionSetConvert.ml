open InstructionSetType
open ParseTreeType

exception CodeGenerationError of string

(* Code for managing stack *)
let stackOffset = ref 0

(* Code for managing available memory *)
let maxRegister = ref 0
let currRegister = ref 0
let getNextStackAddress () = stackOffset := !stackOffset + 1; ([PushStack(1);], StackAddress(!stackOffset))
let getNextStore () = currRegister := !currRegister + 1;
                      if !currRegister <= !maxRegister
                      then ([], RegisterNum(!currRegister))
                      else getNextStackAddress ()
let resetRegisters () = currRegister := 0
let resetStack () = stackOffset := 0
let setMaxRegisters n = maxRegister := n

(* Code for managing symbol table *)
type symbol =
    | Symbol of string * address
    | NoSymbol
let symbolTable = ref [[]]
let clearSymbolTable () = symbolTable := [[]]

let rec lookupSymbol'' name lst = match lst with
    | hd::tl -> (match hd with
                       | Symbol(sName, addr) -> if sName = name then addr else lookupSymbol'' name tl
                       | NoSymbol            -> raise (CodeGenerationError ("Found NoSymbol in symbol table"))
                )
    | []     -> NoAddress
let rec lookupSymbol' name lst = match lst with
    | hd::tl -> (let lookupResult = lookupSymbol'' name hd in
                 match lookupResult with
                       | RegisterNum(_)  
                       | StackAddress(_) 
                       | RegisterAcc        
                       | RegisterStackPtr   
                       | RegisterBasePtr    -> lookupResult
                       | NoAddress          -> lookupSymbol' name tl
                )
    | []     -> NoAddress
let lookupSymbol name = let result = lookupSymbol' name !symbolTable in
                        match result with
                              | NoAddress -> raise (CodeGenerationError ("Cannot find symbol '" ^ name ^ "'"))
                              | _         -> result

let addSymbol' s tbl = match s with
    | Symbol(sName, sAddr) -> (match lookupSymbol' sName tbl with
                                     | NoAddress -> (match tbl with
                                                           | hd::tl -> ((Symbol(sName, sAddr))::hd)::tl
                                                           | []     -> raise (CodeGenerationError ("Tried to add symbol with no scope"))
                                                    )
                                     | _         -> raise (CodeGenerationError ("Symbol '" ^ sName^ "' already in table"))
                              )
    | NoSymbol             -> raise (CodeGenerationError ("Cannot add NoSymbol to table"))
let addSymbol toAdd = symbolTable := addSymbol' toAdd !symbolTable

let pushScope' lst = []::lst
let pushScope () = symbolTable := pushScope' !symbolTable

let popScope' lst = match lst with
    | [[]]   -> raise (CodeGenerationError ("Tried to pop global scope"))
    | hd::tl -> tl
    | []     -> raise (CodeGenerationError ("Tried to pop with no scope"))
let popScope () = symbolTable := popScope' !symbolTable



let rec declareIdentifier iden exp = match iden with
    | Identifier_Declaration(_, name) -> let (instructions, addr) = getNextStackAddress () in
                                         addSymbol (Symbol(name, addr));
                                         (MoveData(RegisterAcc, addr))::(expression_toInstructions exp)@instructions
    | Identifier_Reference(_)         -> raise (CodeGenerationError ("Tried to declare from identifier reference"))
and
findItentifier iden = match iden with
    | Identifier_Declaration(_, _) -> raise (CodeGenerationError ("Tried to dereferece from identifier declaration"))
    | Identifier_Reference(name)   -> MoveData(lookupSymbol name, RegisterAcc)
and
updateIdentifier iden exp = operation_toInstructions (expression_identifier_toInstructions iden) (expression_toInstructions exp) (fun (x, y) -> [StoreValueIn(x); MoveData(y, RegisterAcc)])
and
instructionList_of_parseTree x numRegisters = clearSymbolTable (); resetStack (); resetRegisters (); setMaxRegisters numRegisters; match x with
    (* Lists are generated from the end-backwards, so we need to generate the list in reverse for the side-effects to happen in the right order *)
    | ParseTree_Functions(funcList) -> List.rev (function_list_toInstructions funcList)
    | ParseTree_Empty               -> []
and
function_list_toInstructions x  = match x with
    | Function_List_Def(funcDefinition)            -> function_definition_toInstructions funcDefinition
    | Function_List_List(funcDefinition, funcList) -> (function_definition_toInstructions funcDefinition)@(function_list_toInstructions funcList)
    (* Syntax makes this a nightmare to implement, since the label can end up after the let/new statement.
       So just use normal variable declarations for now *)
    | Function_List_Let(_, funcList)               
    | Function_List_New(_, funcList)               -> function_list_toInstructions funcList
and

let_statement_toInstructions x = []
and
new_statement_toInstructions x = []
and
function_definition_toInstructions x = match x with
    | Function_Definition(iden, args, statements) -> (statement_list_toInstructions statements)@[Label(nameOfFunction x)]
and
statement_list_toInstructions x = match x with
    | Statement_List_Empty                -> []
    | Statement_List_Statement(stat)      -> statement_toInstructions stat
    | Statement_List_List(stat, statList) -> (statement_list_toInstructions statList)@(statement_toInstructions stat)
and
statement_toInstructions x =
resetRegisters ();
(BlankLine)::(match x with
    | Statement_Expression(exp)        -> expression_toInstructions exp
    | Statement_Function(func)         -> function_definition_toInstructions func
    | Statement_Return(ret)            -> return_statement_toInstructions ret
    | Statement_Let(_, stat) 
    | Statement_New(_, stat)           -> statement_toInstructions stat
    | Statement_Let_List(_, statList) 
    | Statement_New_List(_, statList)  -> statement_list_toInstructions statList
    )
and
expression_toInstructions x = match x with
    | Expression_Int(exp)        -> expression_int_toInstructions exp
    | Expression_Bool(exp)       -> expression_bool_toInstructions exp
    | Expression_Identifier(exp) -> expression_identifier_toInstructions exp
    | _                          -> []
and
expression_int_toInstructions x = match x with
    | Expression_Int_Literal(iVal)      -> [LoadConstant(iVal)]
    | Expression_Int_Operation(op)      -> expression_int_operation_toInstructions op
    | Expression_Int_Declare(iden, exp) -> declareIdentifier iden (Expression_Int(exp))
    | Expression_Int_Assign(iden, exp)  -> updateIdentifier iden (Expression_Int(exp))
    | _                                 -> []
and
operation_toInstructions lhs_func rhs_func operator_func =
     let (lhsStorageInstructions, lhsStore) = getNextStore () in
     let (rhsStorageInstructions, rhsStore) = getNextStore () in
     (operator_func(lhsStore, rhsStore))
    @[StoreValue(rhsStore)]@(rhs_func)@rhsStorageInstructions
    @[StoreValue(lhsStore)]@(lhs_func)@lhsStorageInstructions
and
expression_int_operation_toInstructions x = match x with
    | Operation_Int_Plus_Int(lhs, rhs)            -> operation_toInstructions (expression_int_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Add(x, y)])
    | Operation_Identifier_Plus_Int(lhs, rhs)     -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Add(x, y)])
    | Operation_Int_Plus_Identifier(lhs, rhs)     -> operation_toInstructions (expression_int_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Add(x, y)])
    
    | Operation_Int_Minus_Int(lhs, rhs)           -> operation_toInstructions (expression_int_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Subtract(x, y)])
    | Operation_Identifier_Minus_Int(lhs, rhs)    -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Subtract(x, y)])
    | Operation_Int_Minus_Identifier(lhs, rhs)    -> operation_toInstructions (expression_int_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Subtract(x, y)])
    
    | Operation_Int_Multiply_Int(lhs, rhs)        -> operation_toInstructions (expression_int_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Multiply(x, y)])
    | Operation_Identifier_Multiply_Int(lhs, rhs) -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Multiply(x, y)])
    | Operation_Int_Multiply_Identifier(lhs, rhs) -> operation_toInstructions (expression_int_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Multiply(x, y)])
    
    | Operation_Int_Divide_Int(lhs, rhs)          -> operation_toInstructions (expression_int_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Divide(x, y)])
    | Operation_Identifier_Divide_Int(lhs, rhs)   -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> [Divide(x, y)])
    | Operation_Int_Divide_Identifier(lhs, rhs)   -> operation_toInstructions (expression_int_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Divide(x, y)])
    
    | Operation_Negate_Int(exp)                   -> expression_int_operation_toInstructions (Operation_Int_Minus_Int(Expression_Int_Literal(0), exp))
    
    | _                                           -> []
and
expression_bool_toInstructions x = match x with
    | _ -> []
and
expression_identifier_toInstructions (x : expression_identifier) = match x with
    | Expression_Identifier_Declare_Int(iden, exp)      -> declareIdentifier iden (Expression_Identifier(exp))
    | Expression_Identifier_Assign(iden, exp)           -> updateIdentifier iden (Expression_Identifier(exp))
    
    | Expression_Identifier_Dereference(iden)           -> [findItentifier iden]
    | Expression_Identifier_Function_Call(iden, params) -> []
    | Expression_Identifier_Variable_Ref(iden)          -> (match iden with
                                                                  | Identifier_Declaration(_, _) -> raise (CodeGenerationError ("Tried to reference identifier declaration"))
                                                                  | Identifier_Reference(name)   -> [LoadAddress(lookupSymbol name)]
                                                           )
    
    | Statement_While(whileStat)                        -> []
    | Statement_If(ifStat)                              -> []
    
    | Expression_Identifier_Operation(op)               -> expression_identifier_operation_toInstructions op
    
    | _                                                 -> []
and
expression_identifier_operation_toInstructions x = match x with
    | Operation_Identifier_Plus_Identifier(lhs, rhs)     -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Add(x, y)])
    | Operation_Identifier_Minus_Identifier(lhs, rhs)    -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Subtract(x, y)])
    | Operation_Identifier_Multiply_Identifier(lhs, rhs) -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Multiply(x, y)])
    | Operation_Identifier_Divide_Identifier(lhs, rhs)   -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> [Divide(x, y)])
    | Operation_Negate_Identifier(exp)                   -> expression_int_operation_toInstructions (Operation_Int_Minus_Identifier(Expression_Int_Literal(0), exp))
    
    | _ -> []
and
return_statement_toInstructions x = match x with
    | Return_Int(exp)        -> expression_int_toInstructions exp
    | Return_Bool(exp)       -> expression_bool_toInstructions exp
    | Return_Identifier(exp) -> expression_identifier_toInstructions exp
    | _                      -> []