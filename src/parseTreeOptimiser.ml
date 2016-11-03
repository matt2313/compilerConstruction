open ParseTreeType
open ParseTreeEvaluator



(* OptimiseReturn is the record type that is passed between elements in the parse tree *)
type 'a optimiseReturn = {optStore:store; optBranch:'a}
let optimiseReturn x y = {optStore = x; optBranch = y}



(* Helper functions for optimising within new scope *)
let optimiseInNewScope opt_func currStore = let opt = opt_func (pushScope currStore) in optimiseReturn (popScope opt.optStore), opt.optBranch
let optimise2InNewScope opt_func_first opt_func_second currStore = let opt = opt_func_first (pushScope currStore) in
                                                                   let opt' = opt_func_second opt.optStore in
                                                                   optimiseReturn (popScope opt'.optStore) (opt.optBranch, opt'.optBranch)



let rec optimiseParseTree x = parseTree_optimise x emptyStore
and
parseTree_optimise x currStore = match x with
    | ParseTree_Functions(funcList) -> ParseTree_Functions((function_list_optimise funcList currStore).optBranch)
    | ParseTree_Empty -> x
and
function_list_optimise x currStore = match x with
    | Function_List_Def(funcDefinition)            -> let opt = function_definition_optimise funcDefinition currStore in optimiseReturn opt.optStore (Function_List_Def(opt.optBranch))
    | Function_List_Let(letStatement, funcList)    -> let opt = optimise2InNewScope (let_statement_optimise letStatement) (function_list_optimise funcList) currStore in
                                                      let (letStatementOpt, funcListOpt) = opt.optBranch in
                                                      optimiseReturn opt.optStore (Function_List_Let(letStatementOpt, funcListOpt))
    | Function_List_New(newStatement, funcList)    -> let opt = optimise2InNewScope (new_statement_optimise newStatement) (function_list_optimise funcList) currStore in
                                                      let (newStatementOpt, funcListOpt) = opt.optBranch in
                                                      optimiseReturn opt.optStore (Function_List_New(newStatementOpt, funcListOpt))
    | Function_List_List(funcDefinition, funcList) -> let opt = function_definition_optimise funcDefinition currStore in
                                                      let opt' = function_list_optimise funcList opt.optStore in
                                                      optimiseReturn opt'.optStore (Function_List_List(opt.optBranch, opt'.optBranch))
and
function_definition_optimise x currStore = match x with
    | Function_Definition(iden, args, statements) -> let opt = statement_list_optimise statements currStore in
                                                     let opt' = identifier_declare iden currStore (Function(x)) in
                                                     optimiseReturn opt'.newStore (Function_Definition(iden, args, opt.optBranch))
and
let_statement_optimise x currStore = match x with
    | Let_Statement_Int(iden, exp)        -> let opt = expression_int_optimise exp currStore in
                                             let eval = expression_int_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (Let_Statement_Int(iden, opt.optBranch))
    | Let_Statement_Float(iden, exp)      -> let opt = expression_float_optimise exp currStore in
                                             let eval = expression_float_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (Let_Statement_Float(iden, opt.optBranch))
    | Let_Statement_Bool(iden, exp)       -> let opt = expression_bool_optimise exp currStore in
                                             let eval = expression_bool_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (Let_Statement_Bool(iden, opt.optBranch))
    | Let_Statement_String(iden, exp)     -> let opt = expression_string_optimise exp currStore in
                                             let eval = expression_string_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (Let_Statement_String(iden, opt.optBranch))
    | Let_Statement_Identifier(iden, exp) -> let opt = expression_identifier_optimise exp currStore in
                                             let eval = expression_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             (match opt.optBranch with
                                                    | Expression_Int(newBranch)        -> optimiseReturn eval'.newStore (Let_Statement_Int(iden, newBranch))
                                                    | Expression_Float(newBranch)      -> optimiseReturn eval'.newStore (Let_Statement_Float(iden, newBranch))
                                                    | Expression_Bool(newBranch)       -> optimiseReturn eval'.newStore (Let_Statement_Bool(iden, newBranch))
                                                    | Expression_String(newBranch)     -> optimiseReturn eval'.newStore (Let_Statement_String(iden, newBranch))
                                                    | Expression_Identifier(newBranch) -> optimiseReturn eval'.newStore (Let_Statement_Identifier(iden, newBranch))
                                                    | Expression_IO(_)                 -> raise (OptimisationError ("Cannot assign output in let statement"))
                                                    | Expression_Empty                 -> raise (EvaluationError ("Cannot assign empty expression in let statement"))
                                             )
and
new_statement_optimise x currStore = match x with
    | New_Statement_Int(iden, exp)        -> let opt = expression_int_optimise exp currStore in
                                             let eval = expression_int_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (New_Statement_Int(iden, opt.optBranch))
    | New_Statement_Float(iden, exp)      -> let opt = expression_float_optimise exp currStore in
                                             let eval = expression_float_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (New_Statement_Float(iden, opt.optBranch))
    | New_Statement_Bool(iden, exp)       -> let opt = expression_bool_optimise exp currStore in
                                             let eval = expression_bool_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (New_Statement_Bool(iden, opt.optBranch))
    | New_Statement_String(iden, exp)     -> let opt = expression_string_optimise exp currStore in
                                             let eval = expression_string_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             optimiseReturn eval'.newStore (New_Statement_String(iden, opt.optBranch))
    | New_Statement_Identifier(iden, exp) -> let opt = expression_identifier_optimise exp currStore in
                                             let eval = expression_partial_eval opt.optBranch opt.optStore in
                                             let eval' = identifier_declare iden eval.newStore eval.evaluation in
                                             (match opt.optBranch with
                                                    | Expression_Int(newBranch)        -> optimiseReturn eval'.newStore (New_Statement_Int(iden, newBranch))
                                                    | Expression_Float(newBranch)      -> optimiseReturn eval'.newStore (New_Statement_Float(iden, newBranch))
                                                    | Expression_Bool(newBranch)       -> optimiseReturn eval'.newStore (New_Statement_Bool(iden, newBranch))
                                                    | Expression_String(newBranch)     -> optimiseReturn eval'.newStore (New_Statement_String(iden, newBranch))
                                                    | Expression_Identifier(newBranch) -> optimiseReturn eval'.newStore (New_Statement_Identifier(iden, newBranch))
                                                    | Expression_IO(_)                 -> raise (OptimisationError ("Cannot assign output in new statement"))
                                                    | Expression_Empty                 -> raise (EvaluationError ("Cannot assign empty expression in new statement"))
                                             )
and
statement_list_optimise x currStore = match x with
    | Statement_List_Empty                -> optimiseReturn currStore x
    | Statement_List_Statement(stat)      -> let opt = statement_optimise stat currStore in
                                             optimiseReturn opt.optStore (Statement_List_Statement(opt.optBranch))
    | Statement_List_List(stat, statList) -> let opt = statement_optimise stat currStore in
                                             let opt' = statement_list_optimise statList opt.optStore in
                                             optimiseReturn opt'.optStore (Statement_List_List(opt.optBranch, opt'.optBranch))
and
statement_optimise x currStore = match x with
    | Statement_Expression(exp)        -> let opt = expression_optimise exp currStore in
                                          optimiseReturn opt.optStore (Statement_Expression(opt.optBranch))
    | Statement_Function(func)         -> let opt = function_definition_optimise func currStore in
                                          optimiseReturn opt.optStore (Statement_Function(opt.optBranch))
    | Statement_Return(ret)            -> let opt = return_statement_optimise ret currStore in
                                          optimiseReturn opt.optStore (Statement_Return(opt.optBranch))
    | Statement_Let(letStat, stat)     -> let opt = optimise2InNewScope (let_statement_optimise letStat) (statement_optimise stat) currStore in
                                          let (letStatementOpt, statOpt) = opt.optBranch in
                                          optimiseReturn opt.optStore (Statement_Let(letStatementOpt, statOpt))
    | Statement_New(newStat, stat)     -> let opt = optimise2InNewScope (new_statement_optimise newStat) (statement_optimise stat) currStore in
                                          let (newStatementOpt, statOpt) = opt.optBranch in
                                          optimiseReturn opt.optStore (Statement_New(newStatementOpt, statOpt))
    | Statement_Let_List(letStat, lst) -> let opt = optimise2InNewScope (let_statement_optimise letStat) (statement_list_optimise lst) currStore in
                                          let (letStatementOpt, funcListOpt) = opt.optBranch in
                                          optimiseReturn opt.optStore (Statement_Let_List(letStatementOpt, funcListOpt))
    | Statement_New_List(newStat, lst) -> let opt = optimise2InNewScope (new_statement_optimise newStat) (statement_list_optimise lst) currStore in
                                          let (newStatementOpt, funcListOpt) = opt.optBranch in
                                          optimiseReturn opt.optStore (Statement_New_List(newStatementOpt, funcListOpt))
and
expression_optimise x currStore = match x with
    | Expression_Int(exp)        -> let opt = expression_int_optimise exp currStore in optimiseReturn opt.optStore (Expression_Int(opt.optBranch))
    | Expression_Float(exp)      -> let opt = expression_float_optimise exp currStore in optimiseReturn opt.optStore (Expression_Float(opt.optBranch))
    | Expression_Bool(exp)       -> let opt = expression_bool_optimise exp currStore in optimiseReturn opt.optStore (Expression_Bool(opt.optBranch))
    | Expression_String(exp)     -> let opt = expression_string_optimise exp currStore in optimiseReturn opt.optStore (Expression_String(opt.optBranch))
    | Expression_Identifier(exp) -> let opt = expression_identifier_optimise exp currStore in optimiseReturn opt.optStore (opt.optBranch)
    | Expression_IO(_)        
    | Expression_Empty           -> optimiseReturn currStore x
and
expression_int_optimise x currStore = match x with
    | Expression_Int_Operation(op)      -> let opt = int_operation_optimise op currStore in
                                           optimiseReturn opt.optStore opt.optBranch
    | Expression_Int_Declare(iden, exp) -> let opt = expression_int_optimise exp currStore in
                                           optimiseReturn opt.optStore (Expression_Int_Declare(iden, opt.optBranch))
    | Expression_Int_Assign(iden, exp)  -> let opt = expression_int_optimise exp currStore in
                                           optimiseReturn opt.optStore (Expression_Int_Assign(iden, opt.optBranch))
(*
    | Expression_Int_Declare(iden, exp) -> let opt = expression_int_optimise exp currStore in
                                           let eval = expression_int_partial_eval opt.optBranch opt.optStore in
                                           let dec = identifier_declare iden eval.newStore eval.evaluation in
                                           optimiseReturn dec.newStore (Expression_Int_Declare(iden, opt.optBranch))
    | Expression_Int_Assign(iden, exp)  -> let opt = expression_int_optimise exp currStore in
                                           let eval = expression_int_partial_eval opt.optBranch opt.optStore in
                                           let iden = expression
                                           let dec = identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation in
                                           optimiseReturn dec.newStore (Expression_Int_Assign(iden, opt.optBranch))
*)
    | Expression_Float_To_Int(exp)      -> let eval = (expression_float_partial_eval exp currStore) in
                                           if isKnownValue eval.evaluation
                                           then ( let fVal = extractFloatValue eval.evaluation in
                                                  optimiseReturn eval.newStore (Expression_Int_Literal(int_of_float fVal)) )
                                           else optimiseReturn currStore x
    | Expression_Bool_To_Int(exp)       -> let eval = (expression_bool_partial_eval exp currStore) in
                                           if isKnownValue eval.evaluation
                                           then ( let bVal = extractBoolValue eval.evaluation in
                                                  optimiseReturn eval.newStore (Expression_Int_Literal(if bVal then 1 else 0)) )
                                           else optimiseReturn currStore x
    | Expression_String_To_Int(exp)     -> let eval = (expression_string_partial_eval exp currStore) in
                                           if isKnownValue eval.evaluation
                                           then ( let sVal = extractStringValue eval.evaluation in
                                                  optimiseReturn eval.newStore (Expression_Int_Literal(int_of_string sVal)) )
                                           else optimiseReturn currStore x
    | Expression_Identifier_To_Int(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                           (match eval.evaluation with
                                           | IntValue(iVal)    -> optimiseReturn eval.newStore (Expression_Int_Literal(iVal))
                                           | FloatValue(fVal)  -> expression_int_optimise (Expression_Float_To_Int(Expression_Float_Literal(fVal))) eval.newStore
                                           | BoolValue(bVal)   -> expression_int_optimise (Expression_Bool_To_Int(Expression_Bool_Literal(bVal))) eval.newStore
                                           | StringValue(sVal) -> expression_int_optimise (Expression_String_To_Int(Expression_String_Literal(sVal))) eval.newStore
                                           | Unknown           -> optimiseReturn currStore x
                                           | Function(_)       -> raise (OptimisationError ("Cannot convert function name to int"))
                                           | VariableRef(_)    -> raise (OptimisationError ("cannot convert variable ref to int"))
                                           | NoValue           -> raise (OptimisationError ("Cannot convert NULL to int"))
                                           )
    | Expression_Int_Read
    | Expression_Int_Literal(_)         -> optimiseReturn currStore x
and
expression_float_optimise x currStore = match x with
    | Expression_Float_Operation(op)      -> let opt = float_operation_optimise op currStore in
                                                 optimiseReturn opt.optStore opt.optBranch
    | Expression_Float_Declare(iden, exp) -> let opt = expression_float_optimise exp currStore in
                                                 optimiseReturn opt.optStore (Expression_Float_Declare(iden, opt.optBranch))
    | Expression_Float_Assign(iden, exp)  -> let opt = expression_float_optimise exp currStore in
                                                 optimiseReturn opt.optStore (Expression_Float_Assign(iden, opt.optBranch))
    | Expression_Int_To_Float(exp)        -> let eval = (expression_int_partial_eval exp currStore) in
                                             if isKnownValue eval.evaluation
                                             then ( let iVal = extractIntValue eval.evaluation in
                                                    optimiseReturn eval.newStore (Expression_Float_Literal(float_of_int iVal)) )
                                             else optimiseReturn currStore x
    | Expression_Bool_To_Float(exp)       -> let eval = (expression_bool_partial_eval exp currStore) in
                                             if isKnownValue eval.evaluation
                                             then ( let bVal = extractBoolValue eval.evaluation in
                                                    optimiseReturn eval.newStore (Expression_Float_Literal(if bVal then 1.0 else 0.0)) )
                                             else optimiseReturn currStore x
    | Expression_String_To_Float(exp)     -> let eval = (expression_string_partial_eval exp currStore) in
                                             if isKnownValue eval.evaluation
                                             then ( let sVal = extractStringValue eval.evaluation in
                                                    optimiseReturn eval.newStore (Expression_Float_Literal(float_of_string sVal)) )
                                             else optimiseReturn currStore x
    | Expression_Identifier_To_Float(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                                 (match eval.evaluation with
                                                 | FloatValue(fVal)  -> optimiseReturn eval.newStore (Expression_Float_Literal(fVal))
                                                 | IntValue(iVal)    -> expression_float_optimise (Expression_Int_To_Float(Expression_Int_Literal(iVal))) eval.newStore
                                                 | BoolValue(bVal)   -> expression_float_optimise (Expression_Bool_To_Float(Expression_Bool_Literal(bVal))) eval.newStore
                                                 | StringValue(sVal) -> expression_float_optimise (Expression_String_To_Float(Expression_String_Literal(sVal))) eval.newStore
                                                 | Unknown           -> optimiseReturn currStore x
                                                 | Function(_)       -> raise (OptimisationError ("Cannot convert function name to int"))
                                                 | VariableRef(_)    -> raise (OptimisationError ("cannot convert variable ref to int"))
                                                 | NoValue           -> raise (OptimisationError ("Cannot convert NULL to int"))
                                                 )
    | Expression_Float_Literal(_)         
    | Expression_Float_Read               -> optimiseReturn currStore x
and
expression_bool_optimise x currStore = match x with
    | Expression_Bool_Operation(op)      -> let opt = bool_operation_optimise op currStore in
                                                 optimiseReturn opt.optStore opt.optBranch
    | Expression_Bool_Declare(iden, exp) -> let opt = expression_bool_optimise exp currStore in
                                                 optimiseReturn opt.optStore (Expression_Bool_Declare(iden, opt.optBranch))
    | Expression_Bool_Assign(iden, exp)  -> let opt = expression_bool_optimise exp currStore in
                                                 optimiseReturn opt.optStore (Expression_Bool_Assign(iden, opt.optBranch))
    | Expression_Int_To_Bool(exp)        -> let eval = (expression_int_partial_eval exp currStore) in
                                            if isKnownValue eval.evaluation
                                            then ( let iVal = extractIntValue eval.evaluation in
                                                   optimiseReturn eval.newStore (Expression_Bool_Literal(iVal == 1)) )
                                            else optimiseReturn currStore x
    | Expression_Float_To_Bool(exp)      -> let eval = (expression_float_partial_eval exp currStore) in
                                            if isKnownValue eval.evaluation
                                            then ( let fVal = extractFloatValue eval.evaluation in
                                                   optimiseReturn eval.newStore (Expression_Bool_Literal(fVal == 1.)) )
                                            else optimiseReturn currStore x
    | Expression_String_To_Bool(exp)     -> let eval = (expression_string_partial_eval exp currStore) in
                                            if isKnownValue eval.evaluation
                                            then ( let sVal = extractStringValue eval.evaluation in
                                                    optimiseReturn eval.newStore (Expression_Bool_Literal(bool_of_string sVal)) )
                                            else optimiseReturn currStore x
    | Expression_Identifier_To_Bool(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                                 (match eval.evaluation with
                                                 | IntValue(iVal)    -> expression_bool_optimise (Expression_Int_To_Bool(Expression_Int_Literal(iVal))) eval.newStore
                                                 | FloatValue(fVal)  -> expression_bool_optimise (Expression_Float_To_Bool(Expression_Float_Literal(fVal))) eval.newStore
                                                 | BoolValue(bVal)   -> optimiseReturn eval.newStore (Expression_Bool_Literal(bVal))
                                                 | StringValue(sVal) -> expression_bool_optimise (Expression_String_To_Bool(Expression_String_Literal(sVal))) eval.newStore
                                                 | Unknown           -> optimiseReturn currStore x
                                                 | Function(_)       -> raise (OptimisationError ("Cannot convert function name to int"))
                                                 | VariableRef(_)    -> raise (OptimisationError ("cannot convert variable ref to int"))
                                                 | NoValue           -> raise (OptimisationError ("Cannot convert NULL to int"))
                                                 )
    | Expression_Bool_Literal(_)         
    | Expression_Bool_Read               -> optimiseReturn currStore x
and
expression_string_optimise x currStore = match x with
    | Expression_String_Operation(op)      -> let opt = string_operation_optimise op currStore in
                                                  optimiseReturn opt.optStore opt.optBranch
    | Expression_String_Declare(iden, exp) -> let opt = expression_string_optimise exp currStore in
                                                  optimiseReturn opt.optStore (Expression_String_Declare(iden, opt.optBranch))
    | Expression_String_Assign(iden, exp)  -> let opt = expression_string_optimise exp currStore in
                                                  optimiseReturn opt.optStore (Expression_String_Assign(iden, opt.optBranch))
    | Expression_Int_To_String(exp)        -> let eval = (expression_int_partial_eval exp currStore) in
                                              if isKnownValue eval.evaluation
                                              then ( let iVal = extractIntValue eval.evaluation in
                                                     optimiseReturn eval.newStore (Expression_String_Literal(string_of_int iVal)) )
                                              else optimiseReturn currStore x
    | Expression_Float_To_String(exp)      -> let eval = (expression_float_partial_eval exp currStore) in
                                              if isKnownValue eval.evaluation
                                              then ( let fVal = extractFloatValue eval.evaluation in
                                                     optimiseReturn eval.newStore (Expression_String_Literal(string_of_float fVal)) )
                                              else optimiseReturn currStore x
    | Expression_Bool_To_String(exp)       -> let eval = (expression_bool_partial_eval exp currStore) in
                                              if isKnownValue eval.evaluation
                                              then ( let bVal = extractBoolValue eval.evaluation in
                                                     optimiseReturn eval.newStore (Expression_String_Literal(string_of_bool bVal)) )
                                              else optimiseReturn currStore x
    | Expression_Identifier_To_String(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                                  (match eval.evaluation with
                                                  | IntValue(iVal)    -> expression_string_optimise (Expression_Int_To_String(Expression_Int_Literal(iVal))) eval.newStore
                                                  | FloatValue(fVal)  -> expression_string_optimise (Expression_Float_To_String(Expression_Float_Literal(fVal))) eval.newStore
                                                  | BoolValue(bVal)   -> expression_string_optimise (Expression_Bool_To_String(Expression_Bool_Literal(bVal))) eval.newStore
                                                  | StringValue(sVal) -> optimiseReturn eval.newStore (Expression_String_Literal(sVal))
                                                  | Unknown           -> optimiseReturn currStore x
                                                  | Function(_)       -> raise (OptimisationError ("Cannot convert function name to int"))
                                                  | VariableRef(_)    -> raise (OptimisationError ("cannot convert variable ref to int"))
                                                  | NoValue           -> raise (OptimisationError ("Cannot convert NULL to int"))
                                                  )
    | Expression_String_Literal(_)         
    | Expression_String_Read               -> optimiseReturn currStore x
and
expression_identifier_optimise x currStore : expression optimiseReturn = match x with
    | Expression_Identifier_Function_Call(iden, params) -> (match iden with
                                                                  | Identifier_Reference(idenName) -> (match storeLookup currStore idenName with
                                                                                                            | Function(Function_Definition(_, _, statList)) -> let opt = statement_list_optimise statList currStore in
                                                                                                                                                               let eval = statement_list_partial_eval opt.optBranch opt.optStore in
                                                                                                                                                               (match eval.evaluation with
                                                                                                                                                                      | IntValue(iVal)    -> optimiseReturn eval.newStore (Expression_Int(Expression_Int_Literal(iVal)))
                                                                                                                                                                      | FloatValue(fVal)  -> optimiseReturn eval.newStore (Expression_Float(Expression_Float_Literal(fVal)))
                                                                                                                                                                      | BoolValue(bVal)   -> optimiseReturn eval.newStore (Expression_Bool(Expression_Bool_Literal(bVal)))
                                                                                                                                                                      | StringValue(sVal) -> optimiseReturn eval.newStore (Expression_String(Expression_String_Literal(sVal)))
                                                                                                                                                                      | _                 -> optimiseReturn currStore (Expression_Identifier(x))
                                                                                                                                                               )
                                                                                                            | _                                             -> raise (OptimisationError ("Expected function identifier but got variable"))
                                                                                                      )
                                                                  | _                              -> raise (OptimisationError ("Cannot call function declaration"))
                                                           )
    | Statement_While(whileStat)                        -> while_statement_optimise whileStat currStore
    | Statement_If(ifStat)                              -> if_statement_optimise ifStat currStore
    | Expression_Identifier_Operation(op)               -> let eval = identifier_operation_partial_eval op currStore in
                                                           (match eval.evaluation with
                                                                  | IntValue(iVal)    -> optimiseReturn eval.newStore (Expression_Int(Expression_Int_Literal(iVal)))
                                                                  | FloatValue(fVal)  -> optimiseReturn eval.newStore (Expression_Float(Expression_Float_Literal(fVal)))
                                                                  | BoolValue(bVal)   -> optimiseReturn eval.newStore (Expression_Bool(Expression_Bool_Literal(bVal)))
                                                                  | StringValue(sVal) -> optimiseReturn eval.newStore (Expression_String(Expression_String_Literal(sVal)))
                                                                  | _                 -> optimiseReturn currStore (Expression_Identifier(x))
                                                           )
    | _                                                 -> let eval = expression_identifier_partial_eval x currStore in optimiseReturn eval.newStore (Expression_Identifier(x))
and
while_statement_optimise x currStore : expression optimiseReturn = match x with
    | While_Loop_While(exp, statList) -> let expOpt = expression_bool_optimise exp currStore in
                                         let statListOpt = statement_list_optimise statList expOpt.optStore in
                                         optimiseReturn statListOpt.optStore (Expression_Identifier(Statement_While(While_Loop_While(expOpt.optBranch, statListOpt.optBranch))))
    | While_Loop_Do(statList, exp)    -> let statListOpt = statement_list_optimise statList currStore in
                                         let expOpt = expression_bool_optimise exp statListOpt.optStore in
                                         optimiseReturn expOpt.optStore (Expression_Identifier(Statement_While(While_Loop_Do(statListOpt.optBranch, expOpt.optBranch))))
and
if_statement_optimise x currStore : expression optimiseReturn = match x with
    | _ -> optimiseReturn currStore (Expression_Identifier(Statement_If(x)))
and
statement_list_partial_eval x currStore = match x with
    | Statement_List_Empty                -> evalReturn(currStore, NoValue)
    | Statement_List_Statement(stat)      -> (match stat with
                                                   | Statement_Return(_) -> (statement_partial_eval stat currStore)
                                                   | _                   -> (let currStore' = (statement_partial_eval stat currStore).newStore in evalReturn(currStore', NoValue))
                                             )
    | Statement_List_List(stat, statList) -> (match stat with
                                                    | Statement_Return(_) -> statement_partial_eval stat currStore
                                                    | _                   -> let currStore' = (statement_partial_eval stat currStore).newStore in statement_list_partial_eval statList currStore'
                                             )
and
statement_partial_eval x currStore = match x with
    | Statement_Expression(exp)        -> expression_partial_eval exp currStore
    | Statement_Return(ret)            -> return_statement_partial_eval ret currStore
    | Statement_Function(_)  
    | Statement_Let(_, _) 
    | Statement_New(_, _) 
    | Statement_Let_List(_, _) 
    | Statement_New_List(_, _)         -> evalReturn(currStore, NoValue)
and
return_statement_partial_eval x currStore = match x with
    | Return_Int(exp)        -> expression_int_partial_eval exp currStore
    | Return_Float(exp)      -> expression_float_partial_eval exp currStore
    | Return_Bool(exp)       -> expression_bool_partial_eval exp currStore
    | Return_String(exp)     -> expression_string_partial_eval exp currStore
    | Return_Identifier(exp) -> expression_identifier_partial_eval exp currStore
and
expression_partial_eval x currStore = match x with
    | Expression_Int(exp)        -> expression_int_partial_eval exp currStore
    | Expression_Float(exp)      -> expression_float_partial_eval exp currStore
    | Expression_Bool(exp)       -> expression_bool_partial_eval exp currStore
    | Expression_String(exp)     -> expression_string_partial_eval exp currStore
    | Expression_Identifier(exp) -> expression_identifier_partial_eval exp currStore
    | Expression_IO(ioOp)        -> io_operation_partial_eval ioOp currStore
    | Expression_Empty           -> evalReturn(currStore, NoValue)
and
expression_int_partial_eval x currStore = match x with
    | Expression_Int_Literal(iVal)      -> evalReturn(currStore, IntValue(iVal))
    | Expression_Int_Operation(op)      -> int_operation_partial_eval op currStore
    | Expression_Int_Declare(_, _) 
    | Expression_Int_Assign(_, _)  
    | Expression_Int_Read               -> evalReturn(currStore, Unknown)
    | Expression_Float_To_Int(exp)      -> let eval = (expression_float_partial_eval exp currStore) in
                                           if isKnownValue eval.evaluation
                                           then let fVal = extractFloatValue eval.evaluation in
                                                evalReturn(eval.newStore, IntValue(int_of_float fVal))
                                           else evalReturn(currStore, Unknown)
    | Expression_Bool_To_Int(exp)       -> let eval = (expression_bool_partial_eval exp currStore) in
                                           if isKnownValue eval.evaluation
                                           then let bVal = extractBoolValue eval.evaluation in
                                                evalReturn(eval.newStore, IntValue(if bVal then 1 else 0))
                                           else evalReturn(currStore, Unknown)
    | Expression_String_To_Int(exp)     -> let eval = (expression_string_partial_eval exp currStore) in
                                           if isKnownValue eval.evaluation
                                           then let sVal = extractStringValue eval.evaluation in
                                                evalReturn(eval.newStore, IntValue(int_of_string sVal))
                                           else evalReturn(currStore, Unknown)
    | Expression_Identifier_To_Int(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                           (match eval.evaluation with
                                           | IntValue(iVal)    -> evalReturn(eval.newStore, IntValue(iVal))
                                           | FloatValue(fVal)  -> expression_int_partial_eval (Expression_Float_To_Int(Expression_Float_Literal(fVal))) eval.newStore
                                           | BoolValue(bVal)   -> expression_int_partial_eval (Expression_Bool_To_Int(Expression_Bool_Literal(bVal))) eval.newStore
                                           | StringValue(sVal) -> expression_int_partial_eval (Expression_String_To_Int(Expression_String_Literal(sVal))) eval.newStore
                                           | Unknown           -> evalReturn(currStore, Unknown)
                                           | Function(_)       -> raise (EvaluationError ("Cannot convert function name to int"))
                                           | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to int"))
                                           | NoValue           -> raise (EvaluationError ("Cannot convert NULL to int"))
                                           )
    (*
    | Expression_Int_Declare(iden, exp) -> let eval = (expression_int_partial_eval exp currStore) in
                                           identifier_declare iden eval.newStore eval.evaluation
    | Expression_Int_Assign(iden, exp)  -> let eval  = expression_identifier_eval iden currStore in
                                           let eval' = (expression_int_partial_eval exp currStore) in
                                           identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation
    *)
and
expression_float_partial_eval x currStore = match x with
    | Expression_Float_Literal(fVal)      -> evalReturn(currStore, FloatValue(fVal))
    | Expression_Float_Operation(op)      -> float_operation_partial_eval op currStore
    | Expression_Float_Declare(_, _) 
    | Expression_Float_Assign(_, _)  
    | Expression_Float_Read               -> evalReturn(currStore, Unknown)
    | Expression_Int_To_Float(exp)        -> let eval = (expression_int_partial_eval exp currStore) in
                                             if isKnownValue eval.evaluation
                                             then let iVal = extractIntValue eval.evaluation in
                                                  evalReturn(eval.newStore, FloatValue(float_of_int iVal))
                                             else evalReturn(currStore, Unknown)
    | Expression_Bool_To_Float(exp)       -> let eval = (expression_bool_partial_eval exp currStore) in
                                             if isKnownValue eval.evaluation
                                             then let bVal = extractBoolValue eval.evaluation in
                                                  evalReturn(eval.newStore, FloatValue(if bVal then 1. else 0.))
                                             else evalReturn(currStore, Unknown)
    | Expression_String_To_Float(exp)     -> let eval = (expression_string_partial_eval exp currStore) in
                                             if isKnownValue eval.evaluation
                                             then let sVal = extractStringValue eval.evaluation in
                                                      evalReturn(eval.newStore, FloatValue(float_of_string sVal))
                                             else evalReturn(currStore, Unknown)
    | Expression_Identifier_To_Float(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                                   (match eval.evaluation with
                                                   | IntValue(iVal)    -> expression_float_partial_eval (Expression_Int_To_Float(Expression_Int_Literal(iVal))) eval.newStore
                                                   | FloatValue(fVal)  -> evalReturn(eval.newStore, FloatValue(fVal))
                                                   | BoolValue(bVal)   -> expression_float_partial_eval (Expression_Bool_To_Float(Expression_Bool_Literal(bVal))) eval.newStore
                                                   | StringValue(sVal) -> expression_float_partial_eval (Expression_String_To_Float(Expression_String_Literal(sVal))) eval.newStore
                                                   | Unknown           -> evalReturn(currStore, Unknown)
                                                   | Function(_)       -> raise (EvaluationError ("Cannot convert function name to int"))
                                                   | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to int"))
                                                   | NoValue           -> raise (EvaluationError ("Cannot convert NULL to int"))
                                                   )
and
expression_bool_partial_eval x currStore = match x with
    | Expression_Bool_Literal(bVal)      -> evalReturn(currStore, BoolValue(bVal))
    | Expression_Bool_Operation(op)      -> bool_operation_partial_eval op currStore
    | Expression_Bool_Declare(_, _) 
    | Expression_Bool_Assign(_, _)  
    | Expression_Bool_Read               -> evalReturn(currStore, Unknown)
    | Expression_Int_To_Bool(exp)        -> let eval = (expression_int_partial_eval exp currStore) in
                                            if isKnownValue eval.evaluation
                                            then let iVal = extractIntValue eval.evaluation in
                                                     evalReturn(eval.newStore, BoolValue(iVal = 1))
                                            else evalReturn(currStore, Unknown)
    | Expression_Float_To_Bool(exp)      -> let eval = (expression_float_partial_eval exp currStore) in
                                            if isKnownValue eval.evaluation
                                            then let fVal = extractFloatValue eval.evaluation in
                                                     evalReturn(eval.newStore, BoolValue(fVal = 1.))
                                            else evalReturn(currStore, Unknown)
    | Expression_String_To_Bool(exp)     -> let eval = (expression_string_partial_eval exp currStore) in
                                            if isKnownValue eval.evaluation
                                            then let sVal = extractStringValue eval.evaluation in
                                                     evalReturn(eval.newStore, BoolValue(bool_of_string sVal))
                                            else evalReturn(currStore, Unknown)
    | Expression_Identifier_To_Bool(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                           (match eval.evaluation with
                                           | IntValue(iVal)    -> expression_bool_partial_eval (Expression_Int_To_Bool(Expression_Int_Literal(iVal))) eval.newStore
                                           | FloatValue(fVal)  -> expression_bool_partial_eval (Expression_Float_To_Bool(Expression_Float_Literal(fVal))) eval.newStore
                                           | BoolValue(bVal)   -> evalReturn(eval.newStore, BoolValue(bVal))
                                           | StringValue(sVal) -> expression_bool_partial_eval (Expression_String_To_Bool(Expression_String_Literal(sVal))) eval.newStore
                                           | Unknown           -> evalReturn(currStore, Unknown)
                                           | Function(_)       -> raise (EvaluationError ("Cannot convert function name to bool"))
                                           | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to bool"))
                                           | NoValue           -> raise (EvaluationError ("Cannot convert NULL to bool"))
                                           )
and
expression_string_partial_eval x currStore = match x with
    | Expression_String_Literal(sVal)      -> evalReturn(currStore, StringValue(sVal))
    | Expression_String_Operation(op)      -> string_operation_partial_eval op currStore
    | Expression_String_Declare(_, _) 
    | Expression_String_Assign(_, _)  
    | Expression_String_Read               -> evalReturn(currStore, Unknown)
    | Expression_Int_To_String(exp)        -> let eval = (expression_int_partial_eval exp currStore) in
                                              if isKnownValue eval.evaluation
                                              then let iVal = extractIntValue eval.evaluation in
                                                       evalReturn(eval.newStore, StringValue(string_of_int iVal))
                                              else evalReturn(currStore, Unknown)
    | Expression_Float_To_String(exp)      -> let eval = (expression_float_partial_eval exp currStore) in
                                              if isKnownValue eval.evaluation
                                              then let fVal = extractFloatValue eval.evaluation in
                                                       evalReturn(eval.newStore, StringValue(string_of_float fVal))
                                              else evalReturn(currStore, Unknown)
    | Expression_Bool_To_String(exp)       -> let eval = (expression_bool_partial_eval exp currStore) in
                                              if isKnownValue eval.evaluation
                                              then let bVal = extractBoolValue eval.evaluation in
                                                       evalReturn(eval.newStore, StringValue(string_of_bool bVal))
                                              else evalReturn(currStore, Unknown)
    | Expression_Identifier_To_String(exp) -> let eval = expression_identifier_partial_eval exp currStore in
                                              (match eval.evaluation with
                                              | IntValue(iVal)    -> expression_string_partial_eval (Expression_Int_To_String(Expression_Int_Literal(iVal))) eval.newStore
                                              | FloatValue(fVal)  -> expression_string_partial_eval (Expression_Float_To_String(Expression_Float_Literal(fVal))) eval.newStore
                                              | BoolValue(bVal)   -> expression_string_partial_eval (Expression_Bool_To_String(Expression_Bool_Literal(bVal))) eval.newStore
                                              | StringValue(sVal) -> evalReturn(eval.newStore, StringValue(sVal))
                                              | Unknown           -> evalReturn(currStore, Unknown)
                                              | Function(_)       -> raise (EvaluationError ("Cannot convert function name to string"))
                                              | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to string"))
                                              | NoValue           -> raise (EvaluationError ("Cannot convert NULL to string"))
                                              )
and
expression_identifier_partial_eval x currStore = match x with
    | Expression_Identifier_Operation(op)               -> identifier_operation_partial_eval op currStore
    | Expression_Identifier_Function_Call(iden, params) -> (match iden with
                                                                  | Identifier_Reference(idenName) -> (match storeLookup currStore idenName with
                                                                                                             | Function(fun_def) -> function_call_partial_eval fun_def params currStore
                                                                                                             | _                 -> raise (EvaluationError ("Expected function identifier but got variable"))
                                                                                                      )
                                                                  | _                              -> raise (EvaluationError ("Cannot call function declaration"))
                                                           )
    | _                                                 -> evalReturn(currStore, Unknown)
and
function_call_partial_eval fun_def params currStore = match fun_def with
    | Function_Definition(Identifier_Declaration(functionType, functionName), args, statements) -> let currStore' = matchArgsToParams args params (pushScope currStore) in
                                                                                                   let returnValue = evalInNewScope (statement_list_partial_eval statements) currStore' in
                                                                                                       (match (functionType, returnValue.evaluation) with
                                                                                                       | (Int, IntValue(_))       
                                                                                                       | (Float, FloatValue(_))   
                                                                                                       | (Bool, BoolValue(_))     
                                                                                                       | (String, StringValue(_)) -> evalReturn((popScope returnValue.newStore), returnValue.evaluation)
                                                                                                       | (_, Unknown)             -> evalReturn((popScope returnValue.newStore), Unknown)
                                                                                                       | (_, NoValue)             -> raise (EvaluationError ("No return value found for function"))
                                                                                                       | (_, _)                   -> raise (EvaluationError ("Return value does not match the function's type"))
                                                                                                       )
    | _                                                                                         -> raise (EvaluationError ("Function definition does not specify type"))
and
int_operation_partial_eval x currStore = let opt = int_operation_optimise x currStore in
                                         match opt.optBranch with
                                               | Expression_Int_Literal(iVal) -> evalReturn(opt.optStore, IntValue(iVal))
                                               | _                            -> evalReturn(opt.optStore, Unknown)
and
float_operation_partial_eval x currStore = let opt = float_operation_optimise x currStore in
                                           match opt.optBranch with
                                                 | Expression_Float_Literal(fVal) -> evalReturn(opt.optStore, FloatValue(fVal))
                                                 | _                              -> evalReturn(opt.optStore, Unknown)
and
bool_operation_partial_eval x currStore = let opt = bool_operation_optimise x currStore in
                                          match opt.optBranch with
                                                | Expression_Bool_Literal(bVal) -> evalReturn(opt.optStore, BoolValue(bVal))
                                                | _                             -> evalReturn(opt.optStore, Unknown)
and
string_operation_partial_eval x currStore = let opt = string_operation_optimise x currStore in
                                            match opt.optBranch with
                                                  | Expression_String_Literal(sVal) -> evalReturn(opt.optStore, StringValue(sVal))
                                                  | _                               -> evalReturn(opt.optStore, Unknown)
and
io_operation_partial_eval x currStore = evalReturn(currStore, NoValue)
and
operation_int_int_int_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_int_optimise lhs currStore in
    let rhsOpt = expression_int_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_int_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_int_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Int_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else optimiseReturn rhsEval.newStore (Expression_Int_Operation(constructor lhsOpt.optBranch rhsOpt.optBranch))
and
operation_int_identifier_int_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_int_optimise lhs currStore in
    let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_int_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Int_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else (match rhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Int_Operation(constructor lhsOpt.optBranch newBranch))
               | _                                -> raise (OptimisationError ("Right side of Int_Identifier statement is not an identifier expression"))
         )
and
operation_identifier_int_int_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_identifier_optimise lhs currStore in
    let rhsOpt = expression_int_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_int_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Int_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else (match lhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Int_Operation(constructor newBranch rhsOpt.optBranch))
               | _                                -> raise (OptimisationError ("Left side of Int_Identifier statement is not an identifier expression"))
         )
and
int_operation_optimise x currStore : expression_int optimiseReturn = match x with
    | Operation_Int_Plus_Int(lhs, rhs)            -> operation_int_int_int_optimise currStore lhs rhs (fun x y -> x + y) (fun x y -> Operation_Int_Plus_Int(x, y))
    | Operation_Int_Minus_Int(lhs, rhs)           -> operation_int_int_int_optimise currStore lhs rhs (fun x y -> x - y) (fun x y -> Operation_Int_Minus_Int(x, y))
    | Operation_Int_Multiply_Int(lhs, rhs)        -> operation_int_int_int_optimise currStore lhs rhs (fun x y -> x * y) (fun x y -> Operation_Int_Multiply_Int(x, y))
    | Operation_Int_Divide_Int(lhs, rhs)          -> operation_int_int_int_optimise currStore lhs rhs (fun x y -> x / y) (fun x y -> Operation_Int_Divide_Int(x, y))
    | Operation_Int_Plus_Identifier(lhs, rhs)     -> operation_int_identifier_int_optimise currStore lhs rhs (fun x y -> x + y) (fun x y -> Operation_Int_Plus_Identifier(x, y))
    | Operation_Int_Minus_Identifier(lhs, rhs)    -> operation_int_identifier_int_optimise currStore lhs rhs (fun x y -> x - y) (fun x y -> Operation_Int_Minus_Identifier(x, y))
    | Operation_Int_Multiply_Identifier(lhs, rhs) -> operation_int_identifier_int_optimise currStore lhs rhs (fun x y -> x * y) (fun x y -> Operation_Int_Multiply_Identifier(x, y))
    | Operation_Int_Divide_Identifier(lhs, rhs)   -> operation_int_identifier_int_optimise currStore lhs rhs (fun x y -> x / y) (fun x y -> Operation_Int_Divide_Identifier(x, y))
    | Operation_Identifier_Plus_Int(lhs, rhs)     -> operation_identifier_int_int_optimise currStore lhs rhs (fun x y -> x + y) (fun x y -> Operation_Identifier_Plus_Int(x, y))
    | Operation_Identifier_Minus_Int(lhs, rhs)    -> operation_identifier_int_int_optimise currStore lhs rhs (fun x y -> x - y) (fun x y -> Operation_Identifier_Minus_Int(x, y))
    | Operation_Identifier_Multiply_Int(lhs, rhs) -> operation_identifier_int_int_optimise currStore lhs rhs (fun x y -> x * y) (fun x y -> Operation_Identifier_Multiply_Int(x, y))
    | Operation_Identifier_Divide_Int(lhs, rhs)   -> operation_identifier_int_int_optimise currStore lhs rhs (fun x y -> x / y) (fun x y -> Operation_Identifier_Divide_Int(x, y))
    | Operation_Negate_Int(exp)                   -> let eval = expression_int_partial_eval exp currStore in
                                                     if isKnownValue eval.evaluation
                                                     then optimiseReturn eval.newStore (Expression_Int_Literal(-(extractIntValue eval.evaluation)))
                                                     else optimiseReturn eval.newStore (Expression_Int_Operation(Operation_Negate_Int(exp)))
    | Operation_String_Length(exp)                -> let eval = expression_string_partial_eval exp currStore in
                                                     if isKnownValue eval.evaluation
                                                     then optimiseReturn eval.newStore (Expression_Int_Literal(String.length (extractStringValue eval.evaluation)))
                                                     else optimiseReturn eval.newStore (Expression_Int_Operation(Operation_String_Length(exp)))
and
operation_float_float_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_float_optimise lhs currStore in
    let rhsOpt = expression_float_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_float_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_float_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Float_Literal(operator (extractFloatValue lhsEval.evaluation) (extractFloatValue rhsEval.evaluation)))
    else optimiseReturn rhsEval.newStore (Expression_Float_Operation(constructor lhsOpt.optBranch rhsOpt.optBranch))
and
operation_float_identifier_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_float_optimise lhs currStore in
    let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_float_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Float_Literal(operator (extractFloatValue lhsEval.evaluation) (extractFloatValue rhsEval.evaluation)))
    else (match rhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Float_Operation(constructor lhsOpt.optBranch newBranch))
               | _                                -> raise (OptimisationError ("Right side of Float_Identifier statement is not an identifier expression"))
         )
and
operation_identifier_float_optimise currStore lhs rhs operator constructor =
    let lhsEval = expression_identifier_partial_eval lhs currStore in
    let rhsEval = expression_float_partial_eval rhs lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Float_Literal(operator (extractFloatValue lhsEval.evaluation) (extractFloatValue rhsEval.evaluation)))
    else let lhsOpt = expression_identifier_optimise lhs currStore in
         let rhsOpt = expression_float_optimise rhs lhsOpt.optStore in
         (match lhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsOpt.optStore (Expression_Float_Operation(constructor newBranch rhsOpt.optBranch))
               | _                                -> raise (OptimisationError ("Left side of Float_Identifier statement is not an identifier expression"))
         )
and
float_operation_optimise x currStore : expression_float optimiseReturn  = match x with
    | Operation_Float_Plus_Float(lhs, rhs)          -> operation_float_float_optimise currStore lhs rhs (fun x y -> x +. y) (fun x y -> Operation_Float_Plus_Float(x, y))
    | Operation_Float_Minus_Float(lhs, rhs)         -> operation_float_float_optimise currStore lhs rhs (fun x y -> x -. y) (fun x y -> Operation_Float_Minus_Float(x, y))
    | Operation_Float_Multiply_Float(lhs, rhs)      -> operation_float_float_optimise currStore lhs rhs (fun x y -> x *. y) (fun x y -> Operation_Float_Multiply_Float(x, y))
    | Operation_Float_Divide_Float(lhs, rhs)        -> operation_float_float_optimise currStore lhs rhs (fun x y -> x /. y) (fun x y -> Operation_Float_Divide_Float(x, y))
                                                       
    | Operation_Float_Plus_Identifier(lhs, rhs)     -> operation_float_identifier_optimise currStore lhs rhs (fun x y -> x +. y) (fun x y -> Operation_Float_Plus_Identifier(x, y))
    | Operation_Float_Minus_Identifier(lhs, rhs)    -> operation_float_identifier_optimise currStore lhs rhs (fun x y -> x -. y) (fun x y -> Operation_Float_Minus_Identifier(x, y))
    | Operation_Float_Multiply_Identifier(lhs, rhs) -> operation_float_identifier_optimise currStore lhs rhs (fun x y -> x *. y) (fun x y -> Operation_Float_Multiply_Identifier(x, y))
    | Operation_Float_Divide_Identifier(lhs, rhs)   -> operation_float_identifier_optimise currStore lhs rhs (fun x y -> x /. y) (fun x y -> Operation_Float_Divide_Identifier(x, y))
                                                       
    | Operation_Identifier_Plus_Float(lhs, rhs)     -> operation_identifier_float_optimise currStore lhs rhs (fun x y -> x +. y) (fun x y -> Operation_Identifier_Plus_Float(x, y))
    | Operation_Identifier_Minus_Float(lhs, rhs)    -> operation_identifier_float_optimise currStore lhs rhs (fun x y -> x -. y) (fun x y -> Operation_Identifier_Minus_Float(x, y))
    | Operation_Identifier_Multiply_Float(lhs, rhs) -> operation_identifier_float_optimise currStore lhs rhs (fun x y -> x *. y) (fun x y -> Operation_Identifier_Multiply_Float(x, y))
    | Operation_Identifier_Divide_Float(lhs, rhs)   -> operation_identifier_float_optimise currStore lhs rhs (fun x y -> x /. y) (fun x y -> Operation_Identifier_Divide_Float(x, y))
    
    | Operation_Negate_Float(exp)                   -> let eval = expression_float_partial_eval exp currStore in
                                                       if isKnownValue eval.evaluation
                                                       then optimiseReturn eval.newStore (Expression_Float_Literal(-.(extractFloatValue eval.evaluation)))
                                                       else optimiseReturn eval.newStore (Expression_Float_Operation(Operation_Negate_Float(exp)))
and
operation_bool_bool_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_bool_optimise lhs currStore in
    let rhsOpt = expression_bool_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_bool_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_bool_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractBoolValue lhsEval.evaluation) (extractBoolValue rhsEval.evaluation)))
    else optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor lhsOpt.optBranch rhsOpt.optBranch))
and
operation_bool_identifier_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_bool_optimise lhs currStore in
    let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_bool_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractBoolValue lhsEval.evaluation) (extractBoolValue rhsEval.evaluation)))
    else (match rhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor lhsOpt.optBranch newBranch))
               | _                                -> raise (OptimisationError ("Right side of Bool_Identifier statement is not an identifier expression"))
         )
and
operation_identifier_bool_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_identifier_optimise lhs currStore in
    let rhsOpt = expression_bool_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_bool_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractBoolValue lhsEval.evaluation) (extractBoolValue rhsEval.evaluation)))
    else (match lhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor newBranch rhsOpt.optBranch))
               | _                                -> raise (OptimisationError ("Left side of Bool_Identifier statement is not an identifier expression"))
         )
and
operation_int_int_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_int_optimise lhs currStore in
    let rhsOpt = expression_int_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_int_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_int_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor lhsOpt.optBranch rhsOpt.optBranch))
and
operation_int_identifier_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_int_optimise lhs currStore in
    let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_int_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else (match rhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor lhsOpt.optBranch newBranch))
               | _                                -> raise (OptimisationError ("Right side of Int_Identifier statement is not an identifier expression"))
         )
and
operation_identifier_int_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_identifier_optimise lhs currStore in
    let rhsOpt = expression_int_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_int_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else (match lhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor newBranch rhsOpt.optBranch))
               | _                                -> raise (OptimisationError ("Left side of Int_Identifier statement is not an identifier expression"))
         )
and
operation_float_float_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_float_optimise lhs currStore in
    let rhsOpt = expression_float_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_float_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_float_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractFloatValue lhsEval.evaluation) (extractFloatValue rhsEval.evaluation)))
    else optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor lhsOpt.optBranch rhsOpt.optBranch))
and
operation_float_identifier_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_float_optimise lhs currStore in
    let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_float_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractFloatValue lhsEval.evaluation) (extractFloatValue rhsEval.evaluation)))
    else (match rhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor lhsOpt.optBranch newBranch))
               | _                                -> raise (OptimisationError ("Right side of Float_Identifier statement is not an identifier expression"))
         )
and
operation_identifier_float_bool_optimise currStore lhs rhs operator constructor =
    let lhsOpt = expression_identifier_optimise lhs currStore in
    let rhsOpt = expression_float_optimise rhs lhsOpt.optStore in
    let lhsEval = expression_partial_eval lhsOpt.optBranch currStore in
    let rhsEval = expression_float_partial_eval rhsOpt.optBranch lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Bool_Literal(operator (extractFloatValue lhsEval.evaluation) (extractFloatValue rhsEval.evaluation)))
    else (match lhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_Bool_Operation(constructor newBranch rhsOpt.optBranch))
               | _                                -> raise (OptimisationError ("Left side of Float_Identifier statement is not an identifier expression"))
         )
and
bool_operation_optimise x currStore : expression_bool optimiseReturn = match x with
    | Operation_Bool_And_Bool(lhs, rhs)          -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> x && y) (fun x y -> Operation_Bool_And_Bool(x, y))
    | Operation_Bool_Nand_Bool(lhs, rhs)         -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> not (x && y)) (fun x y -> Operation_Bool_Nand_Bool(x, y))
    | Operation_Bool_Or_Bool(lhs, rhs)           -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> x || y) (fun x y -> Operation_Bool_Or_Bool(x, y))
    | Operation_Bool_Xor_Bool(lhs, rhs)          -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Bool_Xor_Bool(x, y))
    | Operation_Bool_Nor_Bool(lhs, rhs)          -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> not(x || y)) (fun x y -> Operation_Bool_Nor_Bool(x, y))
    | Operation_Bool_Nxor_Bool(lhs, rhs)         -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Bool_Nxor_Bool(x, y))
    | Operation_Bool_Eq_Bool(lhs, rhs)           -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Bool_Eq_Bool(x, y))
    | Operation_Bool_Not_Eq_Bool(lhs, rhs)       -> operation_bool_bool_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Bool_Not_Eq_Bool(x, y))
    
    | Operation_Bool_And_Identifier(lhs, rhs)    -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> x && y) (fun x y -> Operation_Bool_And_Identifier(x, y))
    | Operation_Bool_Nand_Identifier(lhs, rhs)   -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> not (x && y)) (fun x y -> Operation_Bool_Nand_Identifier(x, y))
    | Operation_Bool_Or_Identifier(lhs, rhs)     -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> x || y) (fun x y -> Operation_Bool_Or_Identifier(x, y))
    | Operation_Bool_Xor_Identifier(lhs, rhs)    -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Bool_Xor_Identifier(x, y))
    | Operation_Bool_Nor_Identifier(lhs, rhs)    -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> not(x || y)) (fun x y -> Operation_Bool_Nor_Identifier(x, y))
    | Operation_Bool_Nxor_Identifier(lhs, rhs)   -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Bool_Nxor_Identifier(x, y))
    | Operation_Bool_Eq_Identifier(lhs, rhs)     -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Bool_Eq_Identifier(x, y))
    | Operation_Bool_Not_Eq_Identifier(lhs, rhs) -> operation_bool_identifier_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Bool_Not_Eq_Identifier(x, y))
    
    | Operation_Identifier_And_Bool(lhs, rhs)    -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> x && y) (fun x y -> Operation_Identifier_And_Bool(x, y))
    | Operation_Identifier_Nand_Bool(lhs, rhs)   -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> not (x && y)) (fun x y -> Operation_Identifier_Nand_Bool(x, y))
    | Operation_Identifier_Or_Bool(lhs, rhs)     -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> x || y) (fun x y -> Operation_Identifier_Or_Bool(x, y))
    | Operation_Identifier_Xor_Bool(lhs, rhs)    -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Identifier_Xor_Bool(x, y))
    | Operation_Identifier_Nor_Bool(lhs, rhs)    -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> not(x || y)) (fun x y -> Operation_Identifier_Nor_Bool(x, y))
    | Operation_Identifier_Nxor_Bool(lhs, rhs)   -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Identifier_Nxor_Bool(x, y))
    | Operation_Identifier_Eq_Bool(lhs, rhs)     -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Identifier_Eq_Bool(x, y))
    | Operation_Identifier_Not_Eq_Bool(lhs, rhs) -> operation_identifier_bool_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Identifier_Not_Eq_Bool(x, y))
    
    
    | Operation_Int_Less_Than_Int(lhs, rhs)                   -> operation_int_int_bool_optimise currStore lhs rhs (fun x y -> x < y) (fun x y -> Operation_Int_Less_Than_Int(x, y))
    | Operation_Int_Greater_Than_Int(lhs, rhs)                -> operation_int_int_bool_optimise currStore lhs rhs (fun x y -> x > y) (fun x y -> Operation_Int_Greater_Than_Int(x, y))
    | Operation_Int_Less_Than_Or_Eq_Int(lhs, rhs)             -> operation_int_int_bool_optimise currStore lhs rhs (fun x y -> x <= y) (fun x y -> Operation_Int_Less_Than_Or_Eq_Int(x, y))
    | Operation_Int_Greater_Than_Or_Eq_Int(lhs, rhs)          -> operation_int_int_bool_optimise currStore lhs rhs (fun x y -> x >= y) (fun x y -> Operation_Int_Greater_Than_Or_Eq_Int(x, y))
    | Operation_Int_Eq_Int(lhs, rhs)                          -> operation_int_int_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Int_Eq_Int(x, y))
    | Operation_Int_Not_Eq_Int(lhs, rhs)                      -> operation_int_int_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Int_Not_Eq_Int(x, y))
    
    | Operation_Int_Less_Than_Identifier(lhs, rhs)            -> operation_int_identifier_bool_optimise currStore lhs rhs (fun x y -> x < y) (fun x y -> Operation_Int_Less_Than_Identifier(x, y))
    | Operation_Int_Greater_Than_Identifier(lhs, rhs)         -> operation_int_identifier_bool_optimise currStore lhs rhs (fun x y -> x > y) (fun x y -> Operation_Int_Greater_Than_Identifier(x, y))
    | Operation_Int_Less_Than_Or_Eq_Identifier(lhs, rhs)      -> operation_int_identifier_bool_optimise currStore lhs rhs (fun x y -> x <= y) (fun x y -> Operation_Int_Less_Than_Or_Eq_Identifier(x, y))
    | Operation_Int_Greater_Than_Or_Eq_Identifier(lhs, rhs)   -> operation_int_identifier_bool_optimise currStore lhs rhs (fun x y -> x >= y) (fun x y -> Operation_Int_Greater_Than_Or_Eq_Identifier(x, y))
    | Operation_Int_Eq_Identifier(lhs, rhs)                   -> operation_int_identifier_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Int_Eq_Identifier((x, y)))
    | Operation_Int_Not_Eq_Identifier(lhs, rhs)               -> operation_int_identifier_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Int_Not_Eq_Identifier(x, y))
    
    | Operation_Identifier_Less_Than_Int(lhs, rhs)            -> operation_identifier_int_bool_optimise currStore lhs rhs (fun x y -> x < y) (fun x y -> Operation_Identifier_Less_Than_Int(x, y))
    | Operation_Identifier_Greater_Than_Int(lhs, rhs)         -> operation_identifier_int_bool_optimise currStore lhs rhs (fun x y -> x > y) (fun x y -> Operation_Identifier_Greater_Than_Int(x, y))
    | Operation_Identifier_Less_Than_Or_Eq_Int(lhs, rhs)      -> operation_identifier_int_bool_optimise currStore lhs rhs (fun x y -> x <= y) (fun x y -> Operation_Identifier_Less_Than_Or_Eq_Int(x, y))
    | Operation_Identifier_Greater_Than_Or_Eq_Int(lhs, rhs)   -> operation_identifier_int_bool_optimise currStore lhs rhs (fun x y -> x >= y) (fun x y -> Operation_Identifier_Greater_Than_Or_Eq_Int(x, y))
    | Operation_Identifier_Eq_Int(lhs, rhs)                   -> operation_identifier_int_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Identifier_Eq_Int(x, y))
    | Operation_Identifier_Not_Eq_Int(lhs, rhs)               -> operation_identifier_int_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Identifier_Not_Eq_Int(x, y))
    
    | Operation_Float_Less_Than_Float(lhs, rhs)               -> operation_float_float_bool_optimise currStore lhs rhs (fun x y -> x < y) (fun x y -> Operation_Float_Less_Than_Float(x, y))
    | Operation_Float_Greater_Than_Float(lhs, rhs)            -> operation_float_float_bool_optimise currStore lhs rhs (fun x y -> x > y) (fun x y -> Operation_Float_Greater_Than_Float(x, y))
    | Operation_Float_Less_Than_Or_Eq_Float(lhs, rhs)         -> operation_float_float_bool_optimise currStore lhs rhs (fun x y -> x <= y) (fun x y -> Operation_Float_Less_Than_Or_Eq_Float(x, y))
    | Operation_Float_Greater_Than_Or_Eq_Float(lhs, rhs)      -> operation_float_float_bool_optimise currStore lhs rhs (fun x y -> x >= y) (fun x y -> Operation_Float_Greater_Than_Or_Eq_Float(x, y))
    | Operation_Float_Eq_Float(lhs, rhs)                      -> operation_float_float_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Float_Eq_Float(x, y))
    | Operation_Float_Not_Eq_Float(lhs, rhs)                  -> operation_float_float_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Float_Not_Eq_Float(x, y))
    
    | Operation_Float_Less_Than_Identifier(lhs, rhs)          -> operation_float_identifier_bool_optimise currStore lhs rhs (fun x y -> x < y) (fun x y -> Operation_Float_Less_Than_Identifier(x, y))
    | Operation_Float_Greater_Than_Identifier(lhs, rhs)       -> operation_float_identifier_bool_optimise currStore lhs rhs (fun x y -> x > y) (fun x y -> Operation_Float_Greater_Than_Identifier(x, y))
    | Operation_Float_Less_Than_Or_Eq_Identifier(lhs, rhs)    -> operation_float_identifier_bool_optimise currStore lhs rhs (fun x y -> x <= y) (fun x y -> Operation_Float_Less_Than_Or_Eq_Identifier(x, y))
    | Operation_Float_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> operation_float_identifier_bool_optimise currStore lhs rhs (fun x y -> x >= y) (fun x y -> Operation_Float_Greater_Than_Or_Eq_Identifier(x, y))
    | Operation_Float_Eq_Identifier(lhs, rhs)                 -> operation_float_identifier_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Float_Eq_Identifier(x, y))
    | Operation_Float_Not_Eq_Identifier(lhs, rhs)             -> operation_float_identifier_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Float_Not_Eq_Identifier(x, y))
    
    | Operation_Identifier_Less_Than_Float(lhs, rhs)          -> operation_identifier_float_bool_optimise currStore lhs rhs (fun x y -> x < y) (fun x y -> Operation_Identifier_Less_Than_Float(x, y))
    | Operation_Identifier_Greater_Than_Float(lhs, rhs)       -> operation_identifier_float_bool_optimise currStore lhs rhs (fun x y -> x > y) (fun x y -> Operation_Identifier_Greater_Than_Float(x, y))
    | Operation_Identifier_Less_Than_Or_Eq_Float(lhs, rhs)    -> operation_identifier_float_bool_optimise currStore lhs rhs (fun x y -> x <= y) (fun x y -> Operation_Identifier_Less_Than_Or_Eq_Float(x, y))
    | Operation_Identifier_Greater_Than_Or_Eq_Float(lhs, rhs) -> operation_identifier_float_bool_optimise currStore lhs rhs (fun x y -> x >= y) (fun x y -> Operation_Identifier_Greater_Than_Or_Eq_Float(x, y))
    | Operation_Identifier_Eq_Float(lhs, rhs)                 -> operation_identifier_float_bool_optimise currStore lhs rhs (fun x y -> x = y) (fun x y -> Operation_Identifier_Eq_Float(x, y))
    | Operation_Identifier_Not_Eq_Float(lhs, rhs)             -> operation_identifier_float_bool_optimise currStore lhs rhs (fun x y -> x <> y) (fun x y -> Operation_Identifier_Not_Eq_Float(x, y))
    
    
    | Operation_Not_Bool(exp) -> let eval = expression_bool_partial_eval exp currStore in
                                 if isKnownValue eval.evaluation
                                 then optimiseReturn eval.newStore (Expression_Bool_Literal(not(extractBoolValue eval.evaluation)))
                                 else optimiseReturn eval.newStore (Expression_Bool_Operation(Operation_Not_Bool(exp)))
and
string_operation_optimise x currStore = match x with
    | Operation_String_Concat_String(lhs, rhs)      -> let lhsOpt = expression_string_optimise lhs currStore in
                                                       let rhsOpt = expression_string_optimise rhs lhsOpt.optStore in
                                                       let lhsEval = expression_string_partial_eval lhsOpt.optBranch currStore in
                                                       let rhsEval = expression_string_partial_eval rhsOpt.optBranch lhsEval.newStore in
                                                       if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
                                                       then optimiseReturn rhsEval.newStore (Expression_String_Literal((extractStringValue lhsEval.evaluation) ^ (extractStringValue rhsEval.evaluation)))
                                                       else optimiseReturn rhsEval.newStore (Expression_String_Operation(Operation_String_Concat_String(lhsOpt.optBranch, rhsOpt.optBranch)))
    | Operation_Identifier_Concat_String(lhs, rhs)  -> let lhsOpt = expression_identifier_optimise lhs currStore in
                                                       let rhsOpt = expression_string_optimise rhs lhsOpt.optStore in
                                                       let lhsEval = expression_partial_eval lhsOpt.optBranch currStore in
                                                       let rhsEval = expression_string_partial_eval rhsOpt.optBranch lhsEval.newStore in
                                                       if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
                                                       then optimiseReturn rhsEval.newStore (Expression_String_Literal((extractStringValue lhsEval.evaluation) ^ (extractStringValue rhsEval.evaluation)))
                                                       else (match lhsOpt.optBranch with
                                                                  | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_String_Operation(Operation_Identifier_Concat_String(newBranch, rhsOpt.optBranch)))
                                                                  | _                                -> raise (OptimisationError ("Left side of Float_Identifier statement is not an identifier expression"))
                                                            )
    | Operation_String_Concat_Identifier(lhs, rhs)  -> let lhsOpt = expression_string_optimise lhs currStore in
                                                       let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
                                                       let lhsEval = expression_string_partial_eval lhsOpt.optBranch currStore in
                                                       let rhsEval = expression_partial_eval rhsOpt.optBranch lhsEval.newStore in
                                                       if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
                                                       then optimiseReturn rhsEval.newStore (Expression_String_Literal((extractStringValue lhsEval.evaluation) ^ (extractStringValue rhsEval.evaluation)))
                                                       else (match rhsOpt.optBranch with
                                                                  | Expression_Identifier(newBranch) -> optimiseReturn rhsEval.newStore (Expression_String_Operation(Operation_String_Concat_Identifier(lhsOpt.optBranch, newBranch)))
                                                                  | _                                -> raise (OptimisationError ("Right side of Float_Identifier statement is not an identifier expression"))
                                                            )

    | Operation_Substring_String_Int_Int(strExp, startExp, lenExp)               -> let strOpt = expression_string_optimise strExp currStore in
                                                                                    let startOpt = expression_int_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_int_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_string_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_int_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_int_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_String_Int_Int(strOpt.optBranch, startOpt.optBranch, lenOpt.optBranch)))
    | Operation_Substring_String_Int_Identifier(strExp, startExp, lenExp)        -> let strOpt = expression_string_optimise strExp currStore in
                                                                                    let startOpt = expression_int_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_identifier_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_string_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_int_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else (match lenOpt.optBranch with
                                                                                               | Expression_Identifier(newBranch) -> optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_String_Int_Identifier(strOpt.optBranch, startOpt.optBranch, newBranch)))
                                                                                               | _                                -> raise (OptimisationError ("Error optimising substring function"))
                                                                                         )
    | Operation_Substring_String_Identifier_Int(strExp, startExp, lenExp)        -> let strOpt = expression_string_optimise strExp currStore in
                                                                                    let startOpt = expression_identifier_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_int_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_string_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_int_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else (match startOpt.optBranch with
                                                                                               | Expression_Identifier(newBranch) -> optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_String_Identifier_Int(strOpt.optBranch, newBranch, lenOpt.optBranch)))
                                                                                               | _                                -> raise (OptimisationError ("Error optimising substring function"))
                                                                                         )
    | Operation_Substring_String_Identifier_Identifier(strExp, startExp, lenExp) -> let strOpt = expression_string_optimise strExp currStore in
                                                                                    let startOpt = expression_identifier_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_identifier_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_string_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else (match (startOpt.optBranch, lenOpt.optBranch) with
                                                                                               | (Expression_Identifier(newBranchL), Expression_Identifier(newBranchR)) -> optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_String_Identifier_Identifier(strOpt.optBranch, newBranchL, newBranchR)))
                                                                                               | _                                                                      -> raise (OptimisationError ("Error optimising substring function"))
                                                                                         )
    | Operation_Substring_Identifier_Int_Int(strExp, startExp, lenExp)           -> let strOpt = expression_identifier_optimise strExp currStore in
                                                                                    let startOpt = expression_int_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_int_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_int_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_int_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else (match strOpt.optBranch with
                                                                                               | Expression_Identifier(newBranch) -> optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_Identifier_Int_Int(newBranch, startOpt.optBranch, lenOpt.optBranch)))
                                                                                               | _                                -> raise (OptimisationError ("Error optimising substring function"))
                                                                                         )
    | Operation_Substring_Identifier_Int_Identifier(strExp, startExp, lenExp)    -> let strOpt = expression_identifier_optimise strExp currStore in
                                                                                    let startOpt = expression_int_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_identifier_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_int_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else (match (strOpt.optBranch, lenOpt.optBranch) with
                                                                                               | (Expression_Identifier(newBranchL), Expression_Identifier(newBranchR)) -> optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_Identifier_Int_Identifier(newBranchL, startOpt.optBranch, newBranchR)))
                                                                                               | _                                                                      -> raise (OptimisationError ("Error optimising substring function"))
                                                                                         )
    | Operation_Substring_Identifier_Identifier_Int(strExp, startExp, lenExp)    -> let strOpt = expression_identifier_optimise strExp currStore in
                                                                                    let startOpt = expression_identifier_optimise startExp strOpt.optStore in
                                                                                    let lenOpt = expression_int_optimise lenExp startOpt.optStore in
                                                                                    let strEval = expression_partial_eval strOpt.optBranch currStore in
                                                                                    let startEval = expression_partial_eval startOpt.optBranch strEval.newStore in
                                                                                    let lenEval = expression_int_partial_eval lenOpt.optBranch startEval.newStore in
                                                                                    if (isKnownValue strEval.evaluation) && (isKnownValue startEval.evaluation) && (isKnownValue lenEval.evaluation)
                                                                                    then optimiseReturn lenEval.newStore (Expression_String_Literal(String.sub (extractStringValue strEval.evaluation) (extractIntValue startEval.evaluation) (extractIntValue lenEval.evaluation)))
                                                                                    else (match (strOpt.optBranch, startOpt.optBranch) with
                                                                                                | (Expression_Identifier(newBranchL), Expression_Identifier(newBranchR)) -> optimiseReturn lenEval.newStore (Expression_String_Operation(Operation_Substring_Identifier_Identifier_Int(newBranchL, newBranchR, lenOpt.optBranch)))
                                                                                                | _                                                                      -> raise (OptimisationError ("Error optimising substring function"))
                                                                                         )
and
identifier_operation_partial_eval x currStore : evalReturn = match x with
    | Operation_Identifier_Plus_Identifier(lhs, rhs)     -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (match (lhsEval.evaluation, rhsEval.evaluation) with
                                                                   | (StringValue(_), _)
                                                                   | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use + operator on strings."))
                                                                   | (BoolValue(_), _)
                                                                   | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use + operator on bools."))
                                                                   | (FloatValue(_), FloatValue(_))             -> float_operation_partial_eval (Operation_Float_Plus_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                                   | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Plus_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                                   | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Plus_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                                   | (IntValue(_), IntValue(_))                 -> int_operation_partial_eval (Operation_Int_Plus_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                                   | (_, _)                                     -> raise (EvaluationError ("Cannot use + operator here"))
                                                            )
                                                            
                                                            
    | Operation_Identifier_Minus_Identifier(lhs, rhs)    -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use - operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use - operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_partial_eval (Operation_Float_Minus_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Minus_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Minus_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_partial_eval (Operation_Int_Minus_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use - operator here"))
                                                            )
    | Operation_Identifier_Multiply_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use * operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use * operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_partial_eval (Operation_Float_Multiply_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Multiply_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Multiply_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_partial_eval (Operation_Int_Multiply_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use * operator here"))
                                                            )
    | Operation_Identifier_Divide_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use / operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use / operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> float_operation_partial_eval (Operation_Float_Divide_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Divide_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> float_operation_partial_eval (Operation_Float_Divide_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> int_operation_partial_eval (Operation_Int_Divide_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use / operator here"))
                                                            )
    | Operation_Negate_Identifier(exp)                 -> let expEval = (expression_identifier_partial_eval exp currStore) in
                                                              (
                                                              match expEval.evaluation with
                                                              | FloatValue(fValue)                       -> float_operation_partial_eval (Operation_Negate_Float(Expression_Float_Literal(fValue))) expEval.newStore
                                                              | IntValue(iValue)                         -> int_operation_partial_eval (Operation_Negate_Int(Expression_Int_Literal(iValue))) expEval.newStore
                                                              | BoolValue(_)                             -> raise (EvaluationError ("Cannot use - operator on bools."))
                                                              | StringValue(_)                           -> raise (EvaluationError ("Cannot use - operator on strings."))
                                                              | Function(_)                              -> raise (EvaluationError ("Cannot use - operator on function name"))
                                                              | VariableRef(_)                           -> raise (EvaluationError ("Cannot use - operator on variable reference"))
                                                              | Unknown                                  -> raise (OptimisationError ("cannot use - operator on Unknown"))
                                                              | NoValue                                  -> raise (EvaluationError ("Cannot use - operator here"))
                                                              )

    | Operation_Identifier_And_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_partial_eval (Operation_Bool_And_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError("Must use AND operator on bools."))
                                                            )
    | Operation_Identifier_Nand_Identifier(lhs, rhs)  -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_partial_eval (Operation_Bool_Nand_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError("Must use NAND operator on bools."))
                                                            )
    | Operation_Identifier_Or_Identifier(lhs, rhs)    -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_partial_eval (Operation_Bool_Or_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError("Must use OR operator on bools."))
                                                            )
    | Operation_Identifier_Xor_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_partial_eval (Operation_Bool_Xor_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError("Must use XOR operator on bools."))
                                                            )
    | Operation_Identifier_Nor_Identifier(lhs, rhs)   -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_partial_eval (Operation_Bool_Nor_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError("Must use NOR operator on bools."))
                                                            )
    | Operation_Identifier_Nxor_Identifier(lhs, rhs)  -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (BoolValue(lhsVal), BoolValue(rhsVal))     -> bool_operation_partial_eval (Operation_Bool_Nxor_Bool(Expression_Bool_Literal(lhsVal), Expression_Bool_Literal(rhsVal))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError("Must use NXOR operator on bools."))
                                                            )
    | Operation_Not_Identifier(exp)                   -> let expEval = (expression_identifier_partial_eval exp currStore) in
                                                            (
                                                            match expEval.evaluation with
                                                            | BoolValue(expVal)                          -> bool_operation_partial_eval (Operation_Not_Bool(Expression_Bool_Literal(expVal))) expEval.newStore
                                                                   | Unknown                               -> evalReturn(expEval.newStore, Unknown)
                                                            | _                                          -> raise (EvaluationError("Must use NOT operator on bools."))
                                                            )

    | Operation_Identifier_Less_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use < operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use < operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_partial_eval (Operation_Float_Less_Than_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Less_Than_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Less_Than_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_partial_eval (Operation_Int_Less_Than_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use < operator here"))
                                                            )
    | Operation_Identifier_Greater_Than_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use > operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use > operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_partial_eval (Operation_Float_Greater_Than_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Greater_Than_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Greater_Than_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_partial_eval (Operation_Int_Greater_Than_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use > operator here"))
                                                            )
    | Operation_Identifier_Less_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use <= operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use <= operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_partial_eval (Operation_Float_Less_Than_Or_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Less_Than_Or_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Less_Than_Or_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_partial_eval (Operation_Int_Less_Than_Or_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use <= operator here"))
                                                            )
    | Operation_Identifier_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use >= operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use >= operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_partial_eval (Operation_Float_Greater_Than_Or_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Greater_Than_Or_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Greater_Than_Or_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_partial_eval (Operation_Int_Greater_Than_Or_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use >= operator here"))
                                                            )
    | Operation_Identifier_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use = operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use = operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_partial_eval (Operation_Float_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_partial_eval (Operation_Int_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use = operator here"))
                                                            )
    | Operation_Identifier_Not_Eq_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                            let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                            (
                                                            match (lhsEval.evaluation, rhsEval.evaluation) with
                                                            | (StringValue(_), _)
                                                            | (_, StringValue(_))                        -> raise (EvaluationError("Cannot use != / <> operator on strings."))
                                                            | (BoolValue(_), _)
                                                            | (_, BoolValue(_))                          -> raise (EvaluationError("Cannot use != / <> operator on bools."))
                                                            | (FloatValue(_), FloatValue(_))             -> bool_operation_partial_eval (Operation_Float_Not_Eq_Float(Expression_Identifier_To_Float(lhs), Expression_Identifier_To_Float(rhs))) rhsEval.newStore
                                                            | (IntValue(lhsValue), FloatValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Not_Eq_Float(Expression_Int_To_Float(Expression_Int_Literal(lhsValue)), Expression_Float_Literal(rhsValue))) rhsEval.newStore
                                                            | (FloatValue(lhsValue), IntValue(rhsValue)) -> bool_operation_partial_eval (Operation_Float_Not_Eq_Float(Expression_Float_Literal(lhsValue), Expression_Int_To_Float(Expression_Int_Literal(rhsValue)))) rhsEval.newStore
                                                            | (IntValue(_), IntValue(_))                 -> bool_operation_partial_eval (Operation_Int_Not_Eq_Int(Expression_Identifier_To_Int(lhs), Expression_Identifier_To_Int(rhs))) rhsEval.newStore
                                                                   | (Unknown, _)
                                                                   | (_, Unknown)                               -> evalReturn(rhsEval.newStore, Unknown)
                                                            | (_, _)                                     -> raise (EvaluationError ("Cannot use !=/<> operator here"))
                                                            )

    | Operation_Identifier_Concat_Identifier(lhs, rhs) -> let lhsEval = (expression_identifier_partial_eval lhs currStore) in
                                                          let rhsEval = (expression_identifier_partial_eval rhs lhsEval.newStore) in
                                                          (
                                                          match (lhsEval.evaluation, rhsEval.evaluation) with
                                                          | (StringValue(lhsValue), StringValue(rhsValue)) -> string_operation_partial_eval (Operation_String_Concat_String(Expression_String_Literal(lhsValue), Expression_String_Literal(rhsValue))) rhsEval.newStore
                                                          | _                                              -> raise (EvaluationError("Can only use ^ on strings."))
                                                          )
    | Operation_Substring_Identifier_Identifier_Identifier(strExp, startExp, lenExp) -> let strExpEval = expression_identifier_partial_eval strExp currStore in
                                                                                        let startExpEval = expression_identifier_partial_eval startExp strExpEval.newStore in
                                                                                        let lenExpEval = expression_identifier_partial_eval lenExp startExpEval.newStore in
                                                                                        (
                                                                                        match (strExpEval.evaluation, startExpEval.evaluation, lenExpEval.evaluation) with
                                                                                        | (StringValue(strExpValue), IntValue(startExpValue), IntValue(lenExpValue)) -> string_operation_partial_eval (Operation_Substring_String_Int_Int(Expression_String_Literal(strExpValue), Expression_Int_Literal(startExpValue), Expression_Int_Literal(lenExpValue))) lenExpEval.newStore
                                                                                        | _                                                                          -> raise (EvaluationError("Substring must take a string and 2 ints."))
                                                                                        )
and
return_statement_optimise x currStore = match x with
    | Return_Int(exp)           -> let opt = expression_int_optimise exp currStore in optimiseReturn opt.optStore (Return_Int(opt.optBranch))
    | Return_Float(exp)         -> let opt = expression_float_optimise exp currStore in optimiseReturn opt.optStore (Return_Float(opt.optBranch))
    | Return_Bool(exp)          -> let opt = expression_bool_optimise exp currStore in optimiseReturn opt.optStore (Return_Bool(opt.optBranch))
    | Return_String(exp)        -> let opt = expression_string_optimise exp currStore in optimiseReturn opt.optStore (Return_String(opt.optBranch))
    | Return_Identifier(exp)    -> let opt = expression_identifier_optimise exp currStore in
                                   (match opt.optBranch with
                                          | Expression_Int(newBranch)        -> optimiseReturn opt.optStore (Return_Int(newBranch))
                                          | Expression_Float(newBranch)      -> optimiseReturn opt.optStore (Return_Float(newBranch))
                                          | Expression_Bool(newBranch)       -> optimiseReturn opt.optStore (Return_Bool(newBranch))
                                          | Expression_String(newBranch)     -> optimiseReturn opt.optStore (Return_String(newBranch))
                                          | Expression_Identifier(newBranch) -> optimiseReturn opt.optStore (Return_Identifier(newBranch))
                                          | Expression_IO(_)                 -> raise (OptimisationError ("Cannot return an io statement"))
                                          | Expression_Empty                 -> raise (OptimisationError ("Cannot return empty statement"))
                                   )