open ParseTreeType

exception EvaluationError of string
exception OptimisationError of string



(* Value types are used to store the result of an evaluation, they can store any type that is supported by the language, as well as null values and functions *)
type value =
    | IntValue of int
    | FloatValue of float
    | BoolValue of bool
    | StringValue of string
    | Function of function_definition
    | VariableRef of string
    | Unknown
    | NoValue
let valueToString x = match x with
    | IntValue(x)    -> "Int(" ^ string_of_int x ^ ")"
    | FloatValue(x)  -> "Float(" ^ string_of_float x ^ ")"
    | BoolValue(x)   -> "Bool(" ^ string_of_bool x ^ ")"
    | StringValue(x) -> "String(" ^ x ^ ")"
    | Function(x)    -> function_definition_toString x 0 "  "
    | VariableRef(x) -> "Variable Ref(" ^ x ^ ")"
    | Unknown        -> "Unknown"
    | NoValue        -> "NULL"
    
let extractIntValue x = match x with
                        | IntValue(x)    -> x
                        | FloatValue(_)  -> raise (EvaluationError ("cannot take int value from float"))
                        | BoolValue(_)   -> raise (EvaluationError ("cannot take int value from bool"))
                        | StringValue(_) -> raise (EvaluationError ("cannot take int value from string"))
                        | Function(_)    -> raise (EvaluationError ("cannot take int value from function name"))
                        | VariableRef(_) -> raise (EvaluationError ("cannot take int value from variable reference"))
                        | Unknown        -> raise (OptimisationError ("cannot take int value from unknown value"))
                        | NoValue        -> raise (EvaluationError ("cannot take int value from NULL"))
let extractFloatValue x = match x with
                        | IntValue(x)    -> float_of_int x
                        | FloatValue(x)  -> x
                        | BoolValue(_)   -> raise (EvaluationError ("cannot take float value from bool"))
                        | StringValue(_) -> raise (EvaluationError ("cannot take float value from string"))
                        | Function(_)    -> raise (EvaluationError ("cannot take float value from function name"))
                        | VariableRef(_) -> raise (EvaluationError ("cannot take float value from variable reference"))
                        | Unknown        -> raise (OptimisationError ("cannot take float value from unknown value"))
                        | NoValue        -> raise (EvaluationError ("cannot take float value from NULL"))
let extractBoolValue x = match x with
                        | IntValue(_)    -> raise (EvaluationError ("cannot take bool value from int"))
                        | FloatValue(_)  -> raise (EvaluationError ("cannot take bool value from float"))
                        | BoolValue(x)   -> x
                        | StringValue(_) -> raise (EvaluationError ("cannot take bool value from string"))
                        | Function(_)    -> raise (EvaluationError ("cannot take bool value from function name"))
                        | VariableRef(_) -> raise (EvaluationError ("cannot take bool value from variable reference"))
                        | Unknown        -> raise (OptimisationError ("cannot take bool value from unknown value"))
                        | NoValue        -> raise (EvaluationError ("cannot take bool value from NULL"))
let extractStringValue x = match x with
                        | IntValue(_)    -> raise (EvaluationError ("cannot take string value from int"))
                        | FloatValue(_)  -> raise (EvaluationError ("cannot take string value from float"))
                        | BoolValue(_)   -> raise (EvaluationError ("cannot take string value from bool"))
                        | StringValue(x) -> x
                        | Function(_)    -> raise (EvaluationError ("cannot take string value from function name"))
                        | VariableRef(_) -> raise (EvaluationError ("cannot take string value from variable reference"))
                        | Unknown        -> raise (OptimisationError ("cannot take string value from unknown value"))
                        | NoValue        -> raise (EvaluationError ("cannot take string value from NULL"))
let extractVariableRef x = match x with
                        | IntValue(_)    -> raise (EvaluationError ("cannot take variable reference value from int"))
                        | FloatValue(_)  -> raise (EvaluationError ("cannot take variable reference value from float"))
                        | BoolValue(_)   -> raise (EvaluationError ("cannot take variable reference value from bool"))
                        | StringValue(_) -> raise (EvaluationError ("cannot take variable reference value from string"))
                        | Function(_)    -> raise (EvaluationError ("cannot take variable reference value from function name"))
                        | VariableRef(x) -> x
                        | Unknown        -> raise (OptimisationError ("cannot take variable value from unknown value"))
                        | NoValue        -> raise (EvaluationError ("cannot take variable reference value from NULL"))
let isKnownValue x = match x with
                        | Unknown -> false
                        | _       -> true



(* Store is the type that holds the variables and functions in scope, and their value *)
type storeItem = {name:string; storedValue:value}
(* Each element of this list represents a new scope. We can pop one element off the list when we leave scope *)
type store = storeItem list list

let rec storeLookup' from searchFor = match from with
    | hd::tl -> if hd.name = searchFor then hd.storedValue else storeLookup' tl searchFor
    | []     -> NoValue
let rec storeLookup from searchFor = match from with
    | hd::tl -> let foundValue = storeLookup' hd searchFor in
                    (match foundValue with
                           | NoValue -> storeLookup tl searchFor
                           | _       -> foundValue
                    )
    | []     -> raise (EvaluationError ("Variable '" ^ searchFor ^ "' not assigned."))
   
let rec storeUpdate from searchFor newValue = match from with
    | hd1::tl1 -> (match hd1 with
                  | hd2::tl2 -> if hd2.name = searchFor then (match (hd2.storedValue, newValue) with
                                                             | (IntValue(_), IntValue(_))
                                                             | (FloatValue(_), FloatValue(_))
                                                             | (BoolValue(_), BoolValue(_))
                                                             | (StringValue(_), StringValue(_)) 
                                                             | (_, Unknown)                     -> ({name = searchFor; storedValue = newValue}::tl2)::tl1
                                                             | (x, y)                           -> raise (EvaluationError ("Tried to assign " ^ (valueToString y) ^ " to " ^ (valueToString x)))
                                                             )
                                                        else (match storeUpdate (tl2::tl1) searchFor newValue with
                                                             | hd3::tl3 -> (hd2::hd3)::tl3
                                                             | []       -> []
                                                             )
                  | []       -> hd1::(storeUpdate tl1 searchFor newValue)
                  )
    | []       -> raise (EvaluationError ("Variable '" ^ searchFor ^ "' not assigned."))
    
let storeAdd addTo newName newValue = match addTo with
    | hd::tl -> (match storeLookup' hd newName with
                | NoValue -> ({name = newName; storedValue = newValue}::hd)::tl
                | _       -> raise (EvaluationError("A variable or function with the name '" ^ newName ^ "' already exists in the current scope"))
                )
    | []     -> raise (EvaluationError ("No scope exists"))

let rec store_toString' x = match x with
    | [hd]   -> "(" ^ hd.name ^ ", " ^ (valueToString hd.storedValue) ^ ")"
    | hd::tl -> "(" ^ hd.name ^ ", " ^ (valueToString hd.storedValue) ^ "), " ^ (store_toString' tl)
    | []     -> "emptyStore"
let store_toString x = store_toString' (List.flatten x)

let pushScope addTo = []::addTo
let popScope popFrom = match popFrom with
    | hd::tl -> tl
    | []     -> raise (EvaluationError("Tried to pop global scope"))

let emptyStore = [[]]



(* EvalReturn is the record type that is passed between elements in the parse tree *)
type evalReturn = {newStore:store; evaluation:value}
let evalReturn (x, y) = {newStore = x; evaluation = y}



(* Helper functions for evaluating within new scope *)
(* eval_func is a function that, when given a store, will evaluate to an evalReturn *)
let evalInNewScope eval_func currStore = let eval = eval_func (pushScope currStore) in evalReturn (popScope eval.newStore, eval.evaluation)
(* This function creates new scope with evalInNewScope, then updates the store from that with eval_func_first, then uses that store to evaluate eval_func_second *)
let eval2InNewScope eval_func_first eval_func_second currStore = evalInNewScope (fun(currStore':store) -> let currStore'' = (eval_func_first currStore').newStore in eval_func_second currStore'') currStore



(* Helper functions for converting a generic expression into one of a certain type, with appropriate error handling *)
let getIntExp x = match x with
                  | Expression_Int(x)        -> x
                  | Expression_Identifier(x) -> Expression_Identifier_To_Int(x)
                  | Expression_Float(_)      -> raise (EvaluationError ("implicit conversion from float expression to int expression not valid"))
                  | Expression_Bool(_)       -> raise (EvaluationError ("implicit conversion from string expression to int expression not valid"))
                  | Expression_String(_)     -> raise (EvaluationError ("implicit conversion from bool expression to int expression not valid"))
                  | Expression_IO(_)         -> raise (EvaluationError ("cannot convert output operation to int"))
let getFloatExp x = match x with
                  | Expression_Float(x)      -> x
                  | Expression_Int(x)        -> Expression_Int_To_Float(x)
                  | Expression_Identifier(x) -> Expression_Identifier_To_Float(x)
                  | Expression_Bool(_)       -> raise (EvaluationError ("implicit conversion from string expression to float expression not valid"))
                  | Expression_String(_)     -> raise (EvaluationError ("implicit conversion from bool expression to float expression not valid"))
                  | Expression_IO(_)         -> raise (EvaluationError ("cannot convert output operation to float"))
let getBoolExp x = match x with
                  | Expression_Bool(x)       -> x
                  | Expression_Identifier(x) -> Expression_Identifier_To_Bool(x)
                  | Expression_Int(_)        -> raise (EvaluationError ("implicit conversion from int expression to bool expression not valid"))
                  | Expression_Float(_)      -> raise (EvaluationError ("implicit conversion from float expression to bool expression not valid"))
                  | Expression_String(_)     -> raise (EvaluationError ("implicit conversion from string expression to bool expression not valid"))
                  | Expression_IO(_)         -> raise (EvaluationError ("cannot convert output operation to bool"))
let getStringExp x = match x with
                  | Expression_String(x)     -> x
                  | Expression_Identifier(x) -> Expression_Identifier_To_String(x)
                  | Expression_Int(_)        -> raise (EvaluationError ("implicit conversion from int expression to string expression not valid"))
                  | Expression_Float(_)      -> raise (EvaluationError ("implicit conversion from float expression to string expression not valid"))
                  | Expression_Bool(_)       -> raise (EvaluationError ("implicit conversion from bool expression to string expression not valid"))
                  | Expression_IO(_)         -> raise (EvaluationError ("cannot convert output operation to string"))



(* OCaml doesn't have built in print and read functions for bools, so we have to define them ourself *)
let print_bool x =
    if x then
        print_string "true"
    else
        print_string "false"
let read_bool x =
    let input = read_line x in
    if input = "true"
        then true
    else if input = "false"
        then false
    else
        raise (EvaluationError ("Cannot convert string '" ^ input ^ "' to bool"))



let readInt_function = ref (read_int)
let readFloat_function = ref (read_float)
let readBool_function = ref (read_bool)
let readString_function = ref (read_line)



(* Code for the evaluation of a parse tree *)
let mainFunctionName = "main"

let rec evaluateParseTree x read_int_func read_float_func read_bool_func read_string_func =
    readInt_function := read_int_func;
    readFloat_function := read_float_func;
    readBool_function := read_bool_func;
    readString_function := read_string_func;
    parseTree_eval x emptyStore
and
parseTree_eval x currStore = match x with
    | ParseTree_Functions(funcList) -> function_list_eval funcList currStore
    | ParseTree_Empty -> evalReturn(currStore, NoValue)
and
function_list_eval x currStore = match x with
    | Function_List_Def(funcDefinition)            -> if nameOfFunction funcDefinition = mainFunctionName then main_function_definition_eval funcDefinition currStore
                                                                                                          else evalReturn(currStore, NoValue)
    | Function_List_Let(letStatement, funcList)    -> eval2InNewScope (let_statement_eval letStatement) (function_list_eval funcList) currStore
    | Function_List_New(newStatement, funcList)    -> eval2InNewScope (new_statement_eval newStatement) (function_list_eval funcList) currStore
    | Function_List_List(funcDefinition, funcList) -> if (nameOfFunction funcDefinition) = mainFunctionName then (main_function_definition_eval funcDefinition currStore)
                                                                                                            else let currStore' = (function_definition_eval funcDefinition currStore).newStore in
                                                                                                                     function_list_eval funcList currStore'
and
main_function_definition_eval x currStore = match x with
    | Function_Definition(iden, args, statements) -> evalInNewScope (statement_list_eval statements) currStore
and
function_definition_eval x currStore = match x with
    | Function_Definition(iden, args, statements) -> identifier_declare iden currStore (Function(x))
and
nameOfFunction x = match x with
                   | Function_Definition(iden, _, _) ->(match iden with
                                                        | Identifier_Declaration(_, name) -> name
                                                        | _                               -> raise (EvaluationError ("Cannot take name from identifier assignment"))
                                                       )
and
let_statement_eval x currStore = match x with
    | Let_Statement_Int(iden, exp)        -> let eval = (expression_int_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_Float(iden, exp)      -> let eval = (expression_float_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_Bool(iden, exp)       -> let eval = (expression_bool_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_String(iden, exp)     -> let eval = (expression_string_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_Identifier(iden, exp) -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
and
new_statement_eval x currStore = match x with
    | New_Statement_Int(iden, exp)        -> let eval = (expression_int_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_Float(iden, exp)      -> let eval = (expression_float_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_Bool(iden, exp)       -> let eval = (expression_bool_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_String(iden, exp)     -> let eval = (expression_string_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_Identifier(iden, exp) -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
and
identifier_declare iden currStore newValue = match iden with
    | Identifier_Declaration(_, name) -> evalReturn(storeAdd currStore name newValue, newValue)
    | _                               -> raise (EvaluationError ("Cannot declare identifier here"))
and
identifier_update name currStore newValue = evalReturn(storeUpdate currStore name newValue, newValue)
and
(* A return statement produces a value for the statement list, anything else updates the store only *)
statement_list_eval x currStore = match x with
    | Statement_List_Empty                -> evalReturn(currStore, NoValue)
    | Statement_List_Statement(stat)      -> (match stat with
                                                   | Statement_Return(_) -> (statement_eval stat currStore)
                                                   | _                   -> (let currStore' = (statement_eval stat currStore).newStore in evalReturn(currStore', NoValue))
                                             )
    | Statement_List_List(stat, statList) -> match stat with
                                                   | Statement_Return(_) -> statement_eval stat currStore
                                                   | _                   -> let currStore' = (statement_eval stat currStore).newStore in statement_list_eval statList currStore'
and
statement_eval x currStore = match x with
    | Statement_Expression(exp) -> expression_eval exp currStore
    | Statement_Function(func) -> function_definition_eval func currStore
    | Statement_Return(ret) -> return_statement_eval ret currStore
    | Statement_Let(letStat, stat) -> eval2InNewScope (let_statement_eval letStat) (statement_eval stat) currStore
    | Statement_New(newStat, stat) -> eval2InNewScope (new_statement_eval newStat) (statement_eval stat) currStore
    | Statement_Let_List(letStat, lst) -> eval2InNewScope (let_statement_eval letStat) (statement_list_eval lst) currStore
    | Statement_New_List(newStat, lst) -> eval2InNewScope (new_statement_eval newStat) (statement_list_eval lst) currStore
and
expression_eval x currStore = match x with
    | Expression_Int(exp) -> expression_int_eval exp currStore
    | Expression_Float(exp) -> expression_float_eval exp currStore
    | Expression_Bool(exp) -> expression_bool_eval exp currStore
    | Expression_String(exp) -> expression_string_eval exp currStore
    | Expression_Identifier(exp) -> expression_identifier_eval exp currStore
    | Expression_IO(ioOp) -> io_operation_eval ioOp currStore
and
while_statement_eval x currStore = match x with
    | While_Loop_While(exp, statList) -> let expEvaluation = expression_bool_eval exp (pushScope currStore) in
                                         (match expEvaluation.evaluation with
                                         | BoolValue(true)  -> let eval = statement_list_eval statList expEvaluation.newStore in
                                                               while_statement_eval (While_Loop_While(exp, statList)) (popScope eval.newStore)
                                         | BoolValue(false) -> evalReturn(expEvaluation.newStore, NoValue)
                                         | _                -> raise (EvaluationError ("Loop condition must be bool."))
                                         )
    | While_Loop_Do(statList, exp)    -> let eval = statement_list_eval statList (pushScope currStore) in
                                         let expEvaluation = expression_bool_eval exp eval.newStore in
                                         (match expEvaluation.evaluation with
                                         | BoolValue(true)  -> while_statement_eval (While_Loop_Do(statList, exp)) (popScope expEvaluation.newStore)
                                         | BoolValue(false) -> evalReturn(popScope expEvaluation.newStore, NoValue)
                                         | _                -> raise (EvaluationError ("Loop condition must be bool."))
                                         )
and
if_statement_eval x currStore = match x with
    | If_Statement_If_Bool(exp, statList)                       -> let expEvaluation = expression_bool_eval exp (pushScope currStore) in
                                                                   if extractBoolValue expEvaluation.evaluation = true then
                                                                    let eval = statement_list_eval statList expEvaluation.newStore in
                                                                        evalReturn(popScope eval.newStore, eval.evaluation)
                                                                   else
                                                                    evalReturn(expEvaluation.newStore, NoValue)
    | If_Statement_Else_Bool(exp, statList, statListElse)       -> let expEvaluation = expression_bool_eval exp (pushScope currStore) in
                                                                   if extractBoolValue expEvaluation.evaluation = true then
                                                                    let eval = statement_list_eval statList expEvaluation.newStore in
                                                                        evalReturn(popScope eval.newStore, eval.evaluation)
                                                                   else
                                                                    let eval = statement_list_eval statListElse expEvaluation.newStore in
                                                                        evalReturn(popScope eval.newStore, eval.evaluation)
    | If_Statement_Else_List_Bool(exp, statList, ifStat)        -> let expEvaluation = expression_bool_eval exp (pushScope currStore) in
                                                                   if extractBoolValue expEvaluation.evaluation = true then
                                                                      let eval = statement_list_eval statList expEvaluation.newStore in
                                                                          evalReturn(popScope eval.newStore, eval.evaluation)
                                                                   else
                                                                      let eval = if_statement_eval ifStat expEvaluation.newStore in
                                                                          evalReturn(popScope eval.newStore, eval.evaluation)
    | If_Statement_If_Identifier(exp, statList)                 -> let expEvaluation = expression_identifier_eval exp (pushScope currStore) in
                                                                   if extractBoolValue expEvaluation.evaluation = true then
                                                                    let eval = statement_list_eval statList expEvaluation.newStore in
                                                                        evalReturn(popScope eval.newStore, eval.evaluation)
                                                                   else
                                                                    evalReturn(popScope expEvaluation.newStore, NoValue)
    | If_Statement_Else_Identifier(exp, statList, statListElse) -> let expEvaluation = expression_identifier_eval exp (pushScope currStore) in
                                                                   if extractBoolValue expEvaluation.evaluation = true then
                                                                      let eval = statement_list_eval statList expEvaluation.newStore in
                                                                          evalReturn(popScope eval.newStore, eval.evaluation)
                                                                   else
                                                                      let eval = statement_list_eval statListElse expEvaluation.newStore in
                                                                          evalReturn(popScope eval.newStore, eval.evaluation)
    | If_Statement_Else_List_Identifier(exp, statList, ifStat)  -> let expEvaluation = expression_identifier_eval exp (pushScope currStore) in
                                                                   if extractBoolValue expEvaluation.evaluation = true then
                                                                      let eval = statement_list_eval statList expEvaluation.newStore in
                                                                          evalReturn(popScope eval.newStore, eval.evaluation)
                                                                   else
                                                                      let eval = if_statement_eval ifStat expEvaluation.newStore in
                                                                          evalReturn(popScope eval.newStore, eval.evaluation)
and             
return_statement_eval x currStore = match x with
    | Return_Int(exp)           -> expression_int_eval exp currStore
    | Return_Float(exp)         -> expression_float_eval exp currStore
    | Return_Bool(exp)          -> expression_bool_eval exp currStore
    | Return_String(exp)        -> expression_string_eval exp currStore
    | Return_Identifier(exp)    -> expression_identifier_eval exp currStore
and
io_operation_eval x currStore = match x with
    | IO_Operation_Print_Int(exp)               -> let eval = expression_int_eval exp currStore in print_int (extractIntValue eval.evaluation); eval
    | IO_Operation_Print_Float(exp)             -> let eval = expression_float_eval exp currStore in print_float (extractFloatValue eval.evaluation); eval
    | IO_Operation_Print_Bool(exp)              -> let eval = expression_bool_eval exp currStore in print_bool (extractBoolValue eval.evaluation); eval
    | IO_Operation_Print_String(exp)            -> let eval = expression_string_eval exp currStore in print_string (extractStringValue eval.evaluation); eval
    | IO_Operation_Print_Int_Identifier(exp)    -> let eval = expression_identifier_eval exp currStore in print_int (extractIntValue eval.evaluation); eval
    | IO_Operation_Print_Float_Identifier(exp)  -> let eval = expression_identifier_eval exp currStore in print_float (extractFloatValue eval.evaluation); eval
    | IO_Operation_Print_Bool_Identifier(exp)   -> let eval = expression_identifier_eval exp currStore in print_bool (extractBoolValue eval.evaluation); eval
    | IO_Operation_Print_String_Identifier(exp) -> let eval = expression_identifier_eval exp currStore in print_string (extractStringValue eval.evaluation); eval
and
expression_int_eval x currStore = match x with
    | Expression_Int_Literal(iVal)      -> evalReturn(currStore, IntValue(iVal))
    | Expression_Int_Operation(op)      -> int_operation_eval op currStore
    | Expression_Int_Read               -> evalReturn(currStore, IntValue(!readInt_function ()))
    | Expression_Int_Declare(iden, exp) -> let eval = (expression_int_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Int_Assign(iden, exp)  -> let eval  = expression_identifier_eval iden currStore in
                                           let eval' = (expression_int_eval exp currStore) in
                                           identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation
    | Expression_Float_To_Int(exp)      -> let eval = (expression_float_eval exp currStore) in
                                           let fVal = extractFloatValue eval.evaluation in
                                               evalReturn(eval.newStore, IntValue(int_of_float fVal))
    | Expression_Bool_To_Int(exp)       -> let eval = (expression_bool_eval exp currStore) in
                                           let bVal = extractBoolValue eval.evaluation in
                                               evalReturn(eval.newStore, IntValue(if bVal then 1 else 0))
    | Expression_String_To_Int(exp)     -> let eval = (expression_string_eval exp currStore) in
                                           let sVal = extractStringValue eval.evaluation in
                                               evalReturn(eval.newStore, IntValue(int_of_string sVal))
    | Expression_Identifier_To_Int(exp) -> let eval = expression_identifier_eval exp currStore in
                                           match eval.evaluation with
                                           | IntValue(iVal)    -> evalReturn(eval.newStore, IntValue(iVal))
                                           | FloatValue(fVal)  -> expression_int_eval (Expression_Float_To_Int(Expression_Float_Literal(fVal))) eval.newStore
                                           | BoolValue(bVal)   -> expression_int_eval (Expression_Bool_To_Int(Expression_Bool_Literal(bVal))) eval.newStore
                                           | StringValue(sVal) -> expression_int_eval (Expression_String_To_Int(Expression_String_Literal(sVal))) eval.newStore
                                           | Function(_)       -> raise (EvaluationError ("Cannot convert function name to int"))
                                           | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to int"))
                                           | Unknown           -> raise (OptimisationError ("cannot convert Unknown data to int"))
                                           | NoValue           -> raise (EvaluationError ("Cannot convert NULL to int"))
and
expression_float_eval x currStore = match x with
    | Expression_Float_Literal(fVal)      -> evalReturn(currStore, FloatValue(fVal))
    | Expression_Float_Operation(op)      -> float_operation_eval op currStore
    | Expression_Float_Read               -> evalReturn(currStore, FloatValue(!readFloat_function ()))
    | Expression_Float_Declare(iden, exp) -> let eval = (expression_float_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Float_Assign(iden, exp)  -> let eval  = expression_identifier_eval iden currStore in
                                             let eval' = (expression_float_eval exp currStore) in
                                             identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation
    | Expression_Int_To_Float(exp)        -> let eval = (expression_int_eval exp currStore) in
                                             let iVal = extractIntValue eval.evaluation in
                                                 evalReturn(currStore, FloatValue(float_of_int iVal))
    | Expression_Bool_To_Float(exp)       -> let eval = (expression_bool_eval exp currStore) in
                                             let bVal = extractBoolValue eval.evaluation in
                                                 evalReturn(currStore, FloatValue(if bVal then 1.0 else 0.0))
    | Expression_String_To_Float(exp)     -> let eval = (expression_string_eval exp currStore) in
                                             let sVal = extractStringValue eval.evaluation in
                                                 evalReturn(currStore, FloatValue(float_of_string sVal))
    | Expression_Identifier_To_Float(exp) -> let eval = (expression_identifier_eval exp currStore) in
                                                 match eval.evaluation with
                                                 | IntValue(iVal)    -> expression_float_eval (Expression_Int_To_Float(Expression_Int_Literal(iVal))) eval.newStore
                                                 | FloatValue(fVal)  -> evalReturn(eval.newStore, FloatValue(fVal))
                                                 | BoolValue(bVal)   -> evalReturn(eval.newStore, FloatValue(if bVal then 1.0 else 0.0))
                                                 | StringValue(sVal) -> evalReturn(eval.newStore, FloatValue(float_of_string sVal))
                                                 | Function(_)       -> raise (EvaluationError("Cannot convert function name to float"))
                                                 | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to float"))
                                           | Unknown           -> raise (OptimisationError ("cannot convert Unknown data to float"))
                                                 | NoValue           -> raise (EvaluationError("Cannot convert NULL to float"))
and
expression_bool_eval x currStore = match x with
    | Expression_Bool_Literal(bVal)      -> evalReturn(currStore, BoolValue(bVal))
    | Expression_Bool_Operation(op)      -> bool_operation_eval op currStore
    | Expression_Bool_Read               -> evalReturn(currStore, BoolValue(!readBool_function ()))
    | Expression_Bool_Declare(iden, exp) -> let eval = (expression_bool_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Bool_Assign(iden, exp)  -> let eval  = expression_identifier_eval iden currStore in
                                            let eval' = (expression_bool_eval exp currStore) in
                                            identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation
    | Expression_Int_To_Bool(exp)        -> let eval = (expression_int_eval exp currStore) in
                                            let iVal = extractIntValue eval.evaluation in
                                                evalReturn(currStore, BoolValue(iVal = 1))
    | Expression_Float_To_Bool(exp)      -> let eval = (expression_float_eval exp currStore) in
                                            let fVal = extractFloatValue eval.evaluation in
                                                evalReturn(currStore, BoolValue(fVal = 1.0))
    | Expression_String_To_Bool(exp)     -> let eval = (expression_string_eval exp currStore) in
                                            let sVal = extractStringValue eval.evaluation in
                                                evalReturn(currStore, BoolValue(bool_of_string sVal))
    | Expression_Identifier_To_Bool(exp) -> let eval = (expression_identifier_eval exp currStore) in
                                                match eval.evaluation with
                                                | IntValue(iVal)    -> evalReturn(eval.newStore, BoolValue(iVal = 1))
                                                | FloatValue(fVal)  -> evalReturn(eval.newStore, BoolValue(fVal = 1.0))
                                                | BoolValue(bVal)   -> evalReturn(eval.newStore, BoolValue(bVal))
                                                | StringValue(sVal) -> evalReturn(eval.newStore, BoolValue(bool_of_string sVal))
                                                | Function(_)       -> raise (EvaluationError("Cannot convert function name to bool"))
                                                | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to bool"))
                                           | Unknown           -> raise (OptimisationError ("cannot convert Unknown data to bool"))
                                                | NoValue           -> raise (EvaluationError("Cannot convert NULL to bool"))
and
expression_string_eval x currStore = match x with
    | Expression_String_Literal(sVal)      -> evalReturn(currStore, StringValue(sVal))
    | Expression_String_Operation(op)      -> string_operation_eval op currStore
    | Expression_String_Read               -> evalReturn(currStore, StringValue(!readString_function ()))
    | Expression_String_Declare(iden, exp) -> let eval = (expression_string_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_String_Assign(iden, exp)  -> let eval  = expression_identifier_eval iden currStore in
                                              let eval' = (expression_string_eval exp currStore) in
                                              identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation
    | Expression_Int_To_String(exp)        -> let eval = (expression_int_eval exp currStore) in
                                              let iVal = extractIntValue eval.evaluation in
                                                  evalReturn(currStore, StringValue(string_of_int iVal))
    | Expression_Float_To_String(exp)      -> let eval = (expression_float_eval exp currStore) in
                                              let fVal = extractFloatValue eval.evaluation in
                                                  evalReturn(currStore, StringValue(string_of_float fVal))
    | Expression_Bool_To_String(exp)       -> let eval = (expression_bool_eval exp currStore) in
                                              let bVal = extractBoolValue eval.evaluation in
                                                  evalReturn(currStore, StringValue(string_of_bool bVal))
    | Expression_Identifier_To_String(exp) -> let eval = (expression_identifier_eval exp currStore) in
                                                  match eval.evaluation with
                                                  | IntValue(iVal)    -> evalReturn(eval.newStore, StringValue(string_of_int iVal))
                                                  | FloatValue(fVal)  -> evalReturn(eval.newStore, StringValue(string_of_float fVal))
                                                  | BoolValue(bVal)   -> evalReturn(eval.newStore, StringValue(string_of_bool bVal))
                                                  | StringValue(sVal) -> evalReturn(eval.newStore, StringValue(sVal))
                                                  | Function(_)       -> raise (EvaluationError("Cannot convert function name to string"))
                                                  | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to string"))
                                           | Unknown           -> raise (OptimisationError ("cannot convert Unknown data to string"))
                                                  | NoValue           -> raise (EvaluationError("Cannot convert NULL to string"))
and
expression_identifier_eval x currStore = match x with
    | Expression_Identifier_Dereference(iden)           ->(match iden with
                                                                 | Identifier_Reference(idenName) -> evalReturn(currStore, storeLookup currStore idenName)
                                                                 | _                              -> raise (EvaluationError ("Cannot dereference variable declaration"))
                                                           )
    | Expression_Identifier_Operation(op)               -> identifier_operation_eval op currStore
    | Expression_Identifier_Declare_Int(iden, exp)      -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Identifier_Declare_Float(iden, exp)    -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Identifier_Declare_Bool(iden, exp)     -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Identifier_Declare_String(iden, exp)   -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    | Expression_Identifier_Assign(iden, exp)           -> let eval = expression_identifier_eval iden currStore in
                                                           let idenName = extractVariableRef eval.evaluation in
                                                            (match (storeLookup eval.newStore idenName) with
                                                                   | IntValue(_)    
                                                                   | FloatValue(_)  
                                                                   | BoolValue(_)   
                                                                   | StringValue(_) 
                                                                   | Unknown        -> let eval' = (expression_identifier_eval exp eval.newStore) in evalReturn(storeUpdate eval'.newStore idenName eval'.evaluation, eval'.evaluation)
                                                                   | Function(_)    -> raise (EvaluationError ("Cannot assign variable to function name"))
                                                                   | VariableRef(_) -> raise (EvaluationError ("cannot assign variable to variable ref"))
                                                                   | NoValue        -> raise (EvaluationError ("Cannot assign variable to NULL"))
                                                            )
    | Expression_Identifier_Function_Call(iden, params) -> (match iden with
                                                                 | Identifier_Reference(idenName) -> (match storeLookup currStore idenName with
                                                                                                           | Function(fun_def) -> function_call_eval fun_def params currStore
                                                                                                           | _                 -> raise (EvaluationError ("Expected function identifier but got variable"))
                                                                                                     )
                                                                 | _                              -> raise (EvaluationError ("Cannot call function declaration"))
                                                           )
    | Expression_Identifier_Variable_Ref(iden)          -> (match iden with
                                                                 | Identifier_Reference(idenName) -> evalReturn(currStore, VariableRef(idenName))
                                                                 | _                              -> raise (EvaluationError ("Cannot reference variable from declaration"))
                                                           )
    | Statement_While(whileStat) -> while_statement_eval whileStat currStore
    | Statement_If(ifStat) -> if_statement_eval ifStat currStore
and
function_call_eval fun_def params currStore = match fun_def with
    | Function_Definition(Identifier_Declaration(functionType, functionName), args, statements) -> let currStore' = matchArgsToParams args params (pushScope currStore) in
                                                                                                   let returnValue = evalInNewScope (statement_list_eval statements) currStore' in
                                                                                                       (match (functionType, returnValue.evaluation) with
                                                                                                       | (Int, IntValue(_))       
                                                                                                       | (Float, FloatValue(_))   
                                                                                                       | (Bool, BoolValue(_))     
                                                                                                       | (String, StringValue(_)) -> evalReturn((popScope returnValue.newStore), returnValue.evaluation)
                                                                                                       | (_, NoValue)                     -> raise (EvaluationError ("No return value found for function"))
                                                                                                       | (_, _)                           -> raise (EvaluationError ("Return value does not match the function's type"))
                                                                                                       )
    | _                                                                                         -> raise (EvaluationError ("Function definition does not specify type"))
and
matchArgsToParams args params currStore = match (args, params) with
    | (Argument_List_Empty, Parameter_List_Empty)                        -> currStore
    | (Argument_List_Element(iden), Parameter_List_Element(exp))         -> let eval = expression_eval exp currStore in (identifier_declare iden eval.newStore eval.evaluation).newStore
    | (Argument_List_List(iden, args), Parameter_List_List(exp, params)) -> let eval = expression_eval exp currStore in let currStore' = (identifier_declare iden eval.newStore eval.evaluation).newStore in matchArgsToParams args params currStore'
    | _                                                                  -> raise (EvaluationError ("Number of arguments and parameters don't match"))
and
int_operation_eval x currStore = match x with
    | Operation_Int_Plus_Int(lhs, rhs)            -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue + rhsValue))
    | Operation_Int_Minus_Int(lhs, rhs)           -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue - rhsValue))
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
                                                         
    | Operation_Int_Plus_Identifier(lhs, rhs)     -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue + rhsValue))
    | Operation_Int_Minus_Identifier(lhs, rhs)    -> let lhsEval = (expression_int_eval lhs currStore) in
                                                     let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue - rhsValue))
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
                                                         
    | Operation_Identifier_Plus_Int(lhs, rhs)     -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue + rhsValue))
    | Operation_Identifier_Minus_Int(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                     let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                     let lhsValue = extractIntValue lhsEval.evaluation in
                                                     let rhsValue = extractIntValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, IntValue(lhsValue - rhsValue))
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
    
    | Operation_String_Length(exp)                -> let expEval = (expression_string_eval exp currStore) in
                                                     let expValue = extractStringValue expEval.evaluation in
                                                         evalReturn(expEval.newStore, IntValue(String.length expValue))
and
float_operation_eval x currStore = match x with
    | Operation_Float_Plus_Float(lhs, rhs)          -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue +. rhsValue))
    | Operation_Float_Minus_Float(lhs, rhs)         -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue -. rhsValue))
    | Operation_Float_Multiply_Float(lhs, rhs)      -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue *. rhsValue))
    | Operation_Float_Divide_Float(lhs, rhs)        -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue /. rhsValue))
    | Operation_Negate_Float(exp)                   -> let expEval = (expression_float_eval exp currStore) in
                                                       let expValue = extractFloatValue expEval.evaluation in
                                                           evalReturn(expEval.newStore, FloatValue(-.expValue))
                                                         
    | Operation_Float_Plus_Identifier(lhs, rhs)     -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue +. rhsValue))
    | Operation_Float_Minus_Identifier(lhs, rhs)    -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue -. rhsValue))
    | Operation_Float_Multiply_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue *. rhsValue))
    | Operation_Float_Divide_Identifier(lhs, rhs)   -> let lhsEval = (expression_float_eval lhs currStore) in
                                                       let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue /. rhsValue))
                                                         
    | Operation_Identifier_Plus_Float(lhs, rhs)     -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue +. rhsValue))
    | Operation_Identifier_Minus_Float(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue -. rhsValue))
    | Operation_Identifier_Multiply_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue *. rhsValue))
    | Operation_Identifier_Divide_Float(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                       let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                       let lhsValue = extractFloatValue lhsEval.evaluation in
                                                       let rhsValue = extractFloatValue rhsEval.evaluation in
                                                           evalReturn(rhsEval.newStore, FloatValue(lhsValue /. rhsValue))
and
bool_operation_eval x currStore = match x with
    | Operation_Bool_And_Bool(lhs, rhs)    -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(lhsValue && rhsValue))
    | Operation_Bool_Nand_Bool(lhs, rhs)   -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(not (lhsValue && rhsValue)))
    | Operation_Bool_Or_Bool(lhs, rhs)     -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(lhsValue || rhsValue))
    | Operation_Bool_Xor_Bool(lhs, rhs)    -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
    | Operation_Bool_Nor_Bool(lhs, rhs)    -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(not (lhsValue || rhsValue)))
    | Operation_Bool_Nxor_Bool(lhs, rhs)   -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Bool_Eq_Bool(lhs, rhs)     -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Bool_Not_Eq_Bool(lhs, rhs) -> let lhsEval = (expression_bool_eval lhs currStore) in
                                              let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                              let lhsValue = extractBoolValue lhsEval.evaluation in
                                              let rhsValue = extractBoolValue rhsEval.evaluation in
                                                  evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
    | Operation_Not_Bool(exp)              -> let expEval = (expression_bool_eval exp currStore) in
                                              let expValue = extractBoolValue expEval.evaluation in
                                                  evalReturn(expEval.newStore, BoolValue(not expValue))

    | Operation_Bool_And_Identifier(lhs, rhs)    -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue && rhsValue))
    | Operation_Bool_Nand_Identifier(lhs, rhs)   -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(not (lhsValue && rhsValue)))
    | Operation_Bool_Or_Identifier(lhs, rhs)     -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, BoolValue(lhsValue || rhsValue))
    | Operation_Bool_Xor_Identifier(lhs, rhs)    -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
    | Operation_Bool_Nor_Identifier(lhs, rhs)    -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(not (lhsValue || rhsValue)))
    | Operation_Bool_Nxor_Identifier(lhs, rhs)   -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Bool_Eq_Identifier(lhs, rhs)     -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Bool_Not_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_bool_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
                                                        
    | Operation_Identifier_And_Bool(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue && rhsValue))
    | Operation_Identifier_Nand_Bool(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(not (lhsValue && rhsValue)))
    | Operation_Identifier_Or_Bool(lhs, rhs)     -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                         evalReturn(rhsEval.newStore, BoolValue(lhsValue || rhsValue))
    | Operation_Identifier_Xor_Bool(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
    | Operation_Identifier_Nor_Bool(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(not (lhsValue || rhsValue)))
    | Operation_Identifier_Nxor_Bool(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Identifier_Eq_Bool(lhs, rhs)     -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Identifier_Not_Eq_Bool(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_bool_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractBoolValue lhsEval.evaluation in
                                                    let rhsValue = extractBoolValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))

    | Operation_Int_Less_Than_Int(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue < rhsValue))
    | Operation_Int_Greater_Than_Int(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue > rhsValue))
    | Operation_Int_Less_Than_Or_Eq_Int(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <= rhsValue))
    | Operation_Int_Greater_Than_Or_Eq_Int(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue >= rhsValue))
    | Operation_Int_Eq_Int(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Int_Not_Eq_Int(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
                                                        
    | Operation_Int_Less_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue < rhsValue))
    | Operation_Int_Greater_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue > rhsValue))
    | Operation_Int_Less_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <= rhsValue))
    | Operation_Int_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue >= rhsValue))
    | Operation_Int_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Int_Not_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_int_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
                                                        
    | Operation_Identifier_Less_Than_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue < rhsValue))
    | Operation_Identifier_Greater_Than_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue > rhsValue))
    | Operation_Identifier_Less_Than_Or_Eq_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <= rhsValue))
    | Operation_Identifier_Greater_Than_Or_Eq_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue >= rhsValue))
    | Operation_Identifier_Eq_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Identifier_Not_Eq_Int(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_int_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractIntValue lhsEval.evaluation in
                                                    let rhsValue = extractIntValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))


    | Operation_Float_Less_Than_Float(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue < rhsValue))
    | Operation_Float_Greater_Than_Float(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue > rhsValue))
    | Operation_Float_Less_Than_Or_Eq_Float(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <= rhsValue))
    | Operation_Float_Greater_Than_Or_Eq_Float(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue >= rhsValue))
    | Operation_Float_Eq_Float(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Float_Not_Eq_Float(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
                                                        
    | Operation_Float_Less_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue < rhsValue))
    | Operation_Float_Greater_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue > rhsValue))
    | Operation_Float_Less_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <= rhsValue))
    | Operation_Float_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue >= rhsValue))
    | Operation_Float_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Float_Not_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_float_eval lhs currStore) in
                                                    let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
                                                        
    | Operation_Identifier_Less_Than_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue < rhsValue))
    | Operation_Identifier_Greater_Than_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue > rhsValue))
    | Operation_Identifier_Less_Than_Or_Eq_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <= rhsValue))
    | Operation_Identifier_Greater_Than_Or_Eq_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue >= rhsValue))
    | Operation_Identifier_Eq_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue = rhsValue))
    | Operation_Identifier_Not_Eq_Float(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                    let rhsEval = (expression_float_eval rhs lhsEval.newStore) in
                                                    let lhsValue = extractFloatValue lhsEval.evaluation in
                                                    let rhsValue = extractFloatValue rhsEval.evaluation in
                                                        evalReturn(rhsEval.newStore, BoolValue(lhsValue <> rhsValue))
and
string_operation_eval x currStore = match x with
    | Operation_String_Concat_String(lhs, rhs)  -> let lhsEval = (expression_string_eval lhs currStore) in
                                                   let rhsEval = (expression_string_eval rhs lhsEval.newStore) in
                                                   let lhsValue = extractStringValue lhsEval.evaluation in
                                                   let rhsValue = extractStringValue rhsEval.evaluation in
                                                       evalReturn(rhsEval.newStore, StringValue(lhsValue ^ rhsValue))
    | Operation_Identifier_Concat_String(lhs, rhs)  -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                   let rhsEval = (expression_string_eval rhs lhsEval.newStore) in
                                                   let lhsValue = extractStringValue lhsEval.evaluation in
                                                   let rhsValue = extractStringValue rhsEval.evaluation in
                                                       evalReturn(rhsEval.newStore, StringValue(lhsValue ^ rhsValue))
    | Operation_String_Concat_Identifier(lhs, rhs)  -> let lhsEval = (expression_string_eval lhs currStore) in
                                                   let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                   let lhsValue = extractStringValue lhsEval.evaluation in
                                                   let rhsValue = extractStringValue rhsEval.evaluation in
                                                       evalReturn(rhsEval.newStore, StringValue(lhsValue ^ rhsValue))

    | Operation_Substring_String_Int_Int(strExp, startExp, lenExp) -> let strExpEval = (expression_string_eval strExp currStore) in
                                                                      let startExpEval = (expression_int_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_int_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
    | Operation_Substring_String_Int_Identifier(strExp, startExp, lenExp) -> let strExpEval = (expression_string_eval strExp currStore) in
                                                                      let startExpEval = (expression_int_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_identifier_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
    | Operation_Substring_String_Identifier_Int(strExp, startExp, lenExp) -> let strExpEval = (expression_string_eval strExp currStore) in
                                                                      let startExpEval = (expression_identifier_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_int_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
    | Operation_Substring_String_Identifier_Identifier(strExp, startExp, lenExp) -> let strExpEval = (expression_string_eval strExp currStore) in
                                                                      let startExpEval = (expression_identifier_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_identifier_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
    | Operation_Substring_Identifier_Int_Int(strExp, startExp, lenExp) -> let strExpEval = (expression_identifier_eval strExp currStore) in
                                                                      let startExpEval = (expression_int_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_int_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
    | Operation_Substring_Identifier_Int_Identifier(strExp, startExp, lenExp) -> let strExpEval = (expression_identifier_eval strExp currStore) in
                                                                      let startExpEval = (expression_int_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_identifier_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
    | Operation_Substring_Identifier_Identifier_Int(strExp, startExp, lenExp) -> let strExpEval = (expression_identifier_eval strExp currStore) in
                                                                      let startExpEval = (expression_identifier_eval startExp strExpEval.newStore) in
                                                                      let lenExpEval = (expression_int_eval lenExp startExpEval.newStore) in
                                                                      let strExpValue = extractStringValue strExpEval.evaluation in
                                                                      let startExpValue = extractIntValue startExpEval.evaluation in
                                                                      let lenExpValue = extractIntValue lenExpEval.evaluation in
                                                                      evalReturn(lenExpEval.newStore, StringValue(String.sub strExpValue startExpValue lenExpValue))
and
identifier_operation_eval x currStore = match x with
    | Operation_Identifier_Plus_Identifier(lhs, rhs)     -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use + operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use + operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_eval (Operation_Float_Plus_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_eval (Operation_Float_Plus_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_eval (Operation_Float_Plus_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_eval (Operation_Int_Plus_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use + operator here"))
                                                            )
    | Operation_Identifier_Minus_Identifier(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use - operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use - operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_eval (Operation_Float_Minus_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_eval (Operation_Float_Minus_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_eval (Operation_Float_Minus_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_eval (Operation_Int_Minus_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use - operator here"))
                                                            )
    | Operation_Identifier_Multiply_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use * operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use * operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_eval (Operation_Float_Multiply_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_eval (Operation_Float_Multiply_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_eval (Operation_Float_Multiply_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_eval (Operation_Int_Multiply_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use * operator here"))
                                                            )
    | Operation_Identifier_Divide_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use / operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use / operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_eval (Operation_Float_Divide_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_eval (Operation_Float_Divide_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_eval (Operation_Float_Divide_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_eval (Operation_Int_Divide_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use / operator here"))
                                                            )
    | Operation_Negate_Identifier(exp)                 -> let expEval = (expression_identifier_eval exp currStore) in
                                                              (
                                                              match expEval.evaluation with
                                                              | FloatValue(fValue)                       -> float_operation_eval (Operation_Negate_Float(Expression_Float_Literal(fValue))) expEval.newStore
                                                              | IntValue(iValue)                         -> int_operation_eval (Operation_Negate_Int(Expression_Int_Literal(iValue))) expEval.newStore
                                                              | BoolValue(_)                             -> raise (EvaluationError ("Cannot use - operator on bools."))
                                                              | StringValue(_)                           -> raise (EvaluationError ("Cannot use - operator on strings."))
                                                              | Function(_)                              -> raise (EvaluationError ("Cannot use - operator on function name"))
                                                              | VariableRef(_)                           -> raise (EvaluationError ("Cannot use - operator on variable reference"))
                                                              | Unknown                                  -> raise (OptimisationError ("cannot use - operator on Unknown"))
                                                              | NoValue                                  -> raise (EvaluationError ("Cannot use - operator here"))
                                                              )

    | Operation_Identifier_And_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_eval (Operation_Bool_And_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError("Must use AND operator on bools."))
                                                            )
    | Operation_Identifier_Nand_Identifier(lhs, rhs)  -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_eval (Operation_Bool_Nand_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError("Must use NAND operator on bools."))
                                                            )
    | Operation_Identifier_Or_Identifier(lhs, rhs)    -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_eval (Operation_Bool_Or_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError("Must use OR operator on bools."))
                                                            )
    | Operation_Identifier_Xor_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_eval (Operation_Bool_Xor_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError("Must use XOR operator on bools."))
                                                            )
    | Operation_Identifier_Nor_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_eval (Operation_Bool_Nor_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError("Must use NOR operator on bools."))
                                                            )
    | Operation_Identifier_Nxor_Identifier(lhs, rhs)  -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_eval (Operation_Bool_Nxor_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError("Must use NXOR operator on bools."))
                                                            )
    | Operation_Not_Identifier(exp)                   -> let expEval = (expression_identifier_eval exp currStore) in
                                                            (
                                                            match expEval.evaluation with
                                                            | BoolValue(expVal)                          -> bool_operation_eval (Operation_Not_Bool(Expression_Bool_Literal(expVal))) expEval.newStore
                                                            | _                                          -> raise (EvaluationError("Must use NOT operator on bools."))
                                                            )

    | Operation_Identifier_Less_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use < operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use < operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_eval (Operation_Float_Less_Than_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_eval (Operation_Float_Less_Than_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_eval (Operation_Float_Less_Than_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_eval (Operation_Int_Less_Than_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use < operator here"))
                                                            )
    | Operation_Identifier_Greater_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use > operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use > operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_eval (Operation_Float_Greater_Than_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_eval (Operation_Float_Greater_Than_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_eval (Operation_Float_Greater_Than_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_eval (Operation_Int_Greater_Than_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use > operator here"))
                                                            )
    | Operation_Identifier_Less_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use <= operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use <= operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_eval (Operation_Float_Less_Than_Or_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_eval (Operation_Float_Less_Than_Or_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_eval (Operation_Float_Less_Than_Or_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_eval (Operation_Int_Less_Than_Or_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use <= operator here"))
                                                            )
    | Operation_Identifier_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use >= operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use >= operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_eval (Operation_Float_Greater_Than_Or_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_eval (Operation_Float_Greater_Than_Or_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_eval (Operation_Float_Greater_Than_Or_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_eval (Operation_Int_Greater_Than_Or_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use >= operator here"))
                                                            )
    | Operation_Identifier_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use = operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use = operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_eval (Operation_Float_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_eval (Operation_Float_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_eval (Operation_Float_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_eval (Operation_Int_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use = operator here"))
                                                            )
    | Operation_Identifier_Not_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use != / <> operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use != / <> operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_eval (Operation_Float_Not_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_eval (Operation_Float_Not_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_eval (Operation_Float_Not_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_eval (Operation_Int_Not_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use !=/<> operator here"))
                                                            )

    | Operation_Identifier_Concat_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_eval lhs currStore) in
                                                          let rhsEval = (expression_identifier_eval rhs lhsEval.newStore) in
                                                          (
                                                          match (lhsEval.evaluation, rhsEval.evaluation) with
                                                          | (StringValue(lhsValue), StringValue(rhsValue)) -> string_operation_eval (Operation_String_Concat_String(Expression_String_Literal(lhsValue), Expression_String_Literal(rhsValue))) rhsEval.newStore
                                                          | _                                              -> raise (EvaluationError("Can only use ^ on strings."))
                                                          )
    | Operation_Substring_Identifier_Identifier_Identifier(strExp, startExp, lenExp) -> let strExpEval = expression_identifier_eval strExp currStore in
                                                                                        let startExpEval = expression_identifier_eval startExp strExpEval.newStore in
                                                                                        let lenExpEval = expression_identifier_eval lenExp startExpEval.newStore in
                                                                                        (
                                                                                        match (strExpEval.evaluation, startExpEval.evaluation, lenExpEval.evaluation) with
                                                                                        | (StringValue(strExpValue), IntValue(startExpValue), IntValue(lenExpValue)) -> string_operation_eval (Operation_Substring_String_Int_Int(Expression_String_Literal(strExpValue), Expression_Int_Literal(startExpValue), Expression_Int_Literal(lenExpValue))) lenExpEval.newStore
                                                                                        | _                                                                          -> raise (EvaluationError("Substring must take a string and 2 ints."))
                                                                                        )