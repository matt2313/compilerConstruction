open InstructionSetType
open ParseTreeType

(* Code for managing stack *)
let stackOffset = ref 0

(* Code for managing available registers *)
let maxRegister = ref 9
let currRegister = ref 0
let getNextStore () = currRegister := !currRegister + 1;
                      if !currRegister <= !maxRegister
                      then ([], RegisterNum(!currRegister))
                      else (stackOffset := !stackOffset + 1; ([PushStack(1)], StackAddress(!stackOffset)))
let resetRegisters () = currRegister := 0



let rec instructionList_of_parseTree x = match x with
    | ParseTree_Functions(funcList) -> function_list_toInstructions funcList
    | ParseTree_Empty               -> []
and
function_list_toInstructions x  = match x with
    | Function_List_Def(funcDefinition)            -> function_definition_toInstructions funcDefinition
    | Function_List_Let(_, funcList)    
    | Function_List_New(_, funcList)               -> function_list_toInstructions funcList
    | Function_List_List(funcDefinition, funcList) -> (function_definition_toInstructions funcDefinition)@(function_list_toInstructions funcList)
and
function_definition_toInstructions x = match x with
    | Function_Definition(iden, args, statements) -> Label(nameOfFunction x)::(statement_list_toInstructions statements)
and
statement_list_toInstructions x = match x with
    | Statement_List_Empty                -> []
    | Statement_List_Statement(stat)      -> statement_toInstructions stat
    | Statement_List_List(stat, statList) -> (statement_toInstructions stat)@(statement_list_toInstructions statList)
and
statement_toInstructions x = resetRegisters (); match x with
    | Statement_Expression(exp)        -> expression_toInstructions exp
    | Statement_Function(func)         -> function_definition_toInstructions func
    | Statement_Return(ret)            -> return_statement_toInstructions ret
    | Statement_Let(_, stat) 
    | Statement_New(_, stat)           -> statement_toInstructions stat
    | Statement_Let_List(_, statList) 
    | Statement_New_List(_, statList)  -> statement_list_toInstructions statList
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
    | Expression_Int_Declare(iden, exp) -> expression_int_toInstructions exp
    | Expression_Int_Assign(iden, exp)  -> expression_int_toInstructions exp
    | _                                 -> []
and
operation_toInstructions lhs_func rhs_func operator_func =
     let (lhsStorageInstructions, lhsStore) = getNextStore () in
     let (rhsStorageInstructions, rhsStore) = getNextStore () in
     lhsStorageInstructions@(lhs_func)@[StoreValue(lhsStore)]
    @rhsStorageInstructions@(rhs_func)@[StoreValue(rhsStore)]
    @[operator_func(lhsStore, rhsStore)]
and
expression_int_operation_toInstructions x = match x with
    | Operation_Int_Plus_Int(lhs, rhs)         -> operation_toInstructions (expression_int_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> Add(x, y))
    | Operation_Identifier_Plus_Int(lhs, rhs)  -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> Add(x, y))
    | Operation_Int_Plus_Identifier(lhs, rhs)  -> operation_toInstructions (expression_int_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> Add(x, y))
    | Operation_Int_Minus_Int(lhs, rhs)        -> operation_toInstructions (expression_int_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> Subtract(x, y))
    | Operation_Identifier_Minus_Int(lhs, rhs) -> operation_toInstructions (expression_identifier_toInstructions lhs) (expression_int_toInstructions rhs) (fun (x, y) -> Subtract(x, y))
    | Operation_Int_Minus_Identifier(lhs, rhs) -> operation_toInstructions (expression_int_toInstructions lhs) (expression_identifier_toInstructions rhs) (fun (x, y) -> Subtract(x, y))
    | _                                        -> []
(*
    | Operation_Int_Multiply_Int(lhs, rhs)        -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue * rhsValue))
    | Operation_Int_Divide_Int(lhs, rhs)          -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue / rhsValue))
    | Operation_Negate_Int(exp)                   -> let expEval = (expression_int_eval exp currStore) in
                                                     let expValue = extractIntValue expEval.evaluation in
                                                         evalReturn(expEval.newStore, IntValue(-expValue))
                                                         
    | Operation_Int_Multiply_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue * rhsValue))
    | Operation_Int_Divide_Identifier(lhs, rhs)   -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue / rhsValue))
                                                         
    | Operation_Identifier_Multiply_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue * rhsValue))
    | Operation_Identifier_Divide_Int(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue / rhsValue))
*)
and
expression_bool_toInstructions x = match x with
    | _ -> []
and
expression_identifier_toInstructions x = match x with
    | _ -> []
and
return_statement_toInstructions x = match x with
    | Return_Int(exp)        -> (expression_int_toInstructions exp)
    | Return_Identifier(exp) -> []
    | _                      -> []