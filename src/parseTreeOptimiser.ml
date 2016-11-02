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
                                                     optimiseReturn opt.optStore (Function_Definition(iden, args, opt.optBranch))
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
    | Expression_IO(ioOp)        -> optimiseReturn currStore x
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
(*
    | Expression_Float_Operation(op)      -> float_operation_eval op currStore
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
                                                 (match eval.evaluation with
                                                 | IntValue(iVal)    -> expression_float_eval (Expression_Int_To_Float(Expression_Int_Literal(iVal))) eval.newStore
                                                 | FloatValue(fVal)  -> evalReturn(eval.newStore, FloatValue(fVal))
                                                 | BoolValue(bVal)   -> evalReturn(eval.newStore, FloatValue(if bVal then 1.0 else 0.0))
                                                 | StringValue(sVal) -> evalReturn(eval.newStore, FloatValue(float_of_string sVal))
                                                 | Function(_)       -> raise (EvaluationError("Cannot convert function name to float"))
                                                 | VariableRef(_)    -> raise (EvaluationError ("cannot convert variable ref to float"))
                                                 | Unknown           -> raise (OptimisationError ("cannot convert Unknown data to float"))
                                                 | NoValue           -> raise (EvaluationError("Cannot convert NULL to float"))
                                                 )
    | Expression_Float_Literal(_)      
    | Expression_Float_Read               -> optimiseReturn currStore x
    *)
    | _                                   -> optimiseReturn currStore x
(*
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
*)
and
expression_bool_optimise x currStore = optimiseReturn currStore x
and
expression_string_optimise x currStore = optimiseReturn currStore x
and
expression_identifier_optimise x currStore : expression optimiseReturn = match x with
    (*
    | Expression_Identifier_Dereference(iden) -> (match iden with
                                                        | Identifier_Reference(idenName) -> (match storeLookup currStore idenName with
                                                                                                   | IntValue(iVal)    -> optimiseReturn currStore (Expression_Int(Expression_Int_Literal(iVal)))
                                                                                                   | FloatValue(fVal)  -> optimiseReturn currStore (Expression_Float(Expression_Float_Literal(fVal)))
                                                                                                   | BoolValue(bVal)   -> optimiseReturn currStore (Expression_Bool(Expression_Bool_Literal(bVal)))
                                                                                                   | StringValue(sVal) -> optimiseReturn currStore (Expression_String(Expression_String_Literal(sVal)))
                                                                                                   | Unknown           -> optimiseReturn currStore (Expression_Identifier(x))
                                                                                                   | Function(_)       -> raise (OptimisationError ("function call missing parameters"))
                                                                                                   | VariableRef(_)
                                                                                                   | NoValue           -> raise (OptimisationError ("Error dereferencing variable"))
                                                                                            )
                                                        | _                              -> raise (EvaluationError ("Cannot dereference variable declaration"))
                                                 )
    *)
    | _                                       -> let eval = expression_identifier_partial_eval x currStore in optimiseReturn eval.newStore (Expression_Identifier(x))
and
expression_partial_eval x currStore = match x with
    | Expression_Int(exp)        -> expression_int_partial_eval exp currStore
    | Expression_Float(exp)      -> expression_float_partial_eval exp currStore
    | Expression_Bool(exp)       -> expression_bool_partial_eval exp currStore
    | Expression_String(exp)     -> expression_string_partial_eval exp currStore
    | Expression_Identifier(exp) -> expression_identifier_partial_eval exp currStore
    | Expression_IO(ioOp)        -> io_operation_partial_eval ioOp currStore
and
expression_int_partial_eval x currStore = match x with
    | Expression_Int_Read               -> evalReturn(currStore, Unknown)
    (*
    | Expression_Int_Declare(iden, exp) -> let eval = (expression_int_partial_eval exp currStore) in
                                           identifier_declare iden eval.newStore eval.evaluation
    | Expression_Int_Assign(iden, exp)  -> let eval  = expression_identifier_eval iden currStore in
                                           let eval' = (expression_int_partial_eval exp currStore) in
                                           identifier_update (extractVariableRef eval.evaluation) eval'.newStore eval'.evaluation
    *)
    | Expression_Identifier_To_Int(exp) -> let eval = expression_identifier_eval exp currStore in
                                           (match eval.evaluation with
                                           | IntValue(iVal)    -> evalReturn(eval.newStore, IntValue(iVal))
                                           | FloatValue(fVal)  -> expression_int_eval (Expression_Float_To_Int(Expression_Float_Literal(fVal))) eval.newStore
                                           | BoolValue(bVal)   -> expression_int_eval (Expression_Bool_To_Int(Expression_Bool_Literal(bVal))) eval.newStore
                                           | StringValue(sVal) -> expression_int_eval (Expression_String_To_Int(Expression_String_Literal(sVal))) eval.newStore
                                           | Unknown           -> evalReturn(eval.newStore, Unknown)
                                           | Function(_)       -> raise (OptimisationError ("Cannot convert function name to int"))
                                           | VariableRef(_)    -> raise (OptimisationError ("cannot convert variable ref to int"))
                                           | NoValue           -> raise (OptimisationError ("Cannot convert NULL to int"))
                                           )
    | Expression_Int_Declare(_, _)      
    | Expression_Int_Assign(_, _)       
    | Expression_Int_Literal(_)         
    | Expression_Int_Operation(_)       
    | Expression_Float_To_Int(_)        
    | Expression_Bool_To_Int(_)         
    | Expression_String_To_Int(_)       -> expression_int_eval x currStore
and
expression_float_partial_eval x currStore = evalReturn(currStore, Unknown)
and
expression_bool_partial_eval x currStore = evalReturn(currStore, Unknown)
and
expression_string_partial_eval x currStore = evalReturn(currStore, Unknown)
and
expression_identifier_partial_eval (x : expression_identifier) currStore = match x with
    (*
    | Expression_Identifier_Dereference(iden)      -> (match iden with
                                                        | Identifier_Reference(idenName) -> evalReturn(currStore, storeLookup currStore idenName)
                                                        | _                              -> raise (EvaluationError ("Cannot dereference variable declaration"))
                                                      ) 
    | Expression_Identifier_Declare_Int(iden, exp) -> let eval = (expression_identifier_partial_eval exp currStore) in let currStore' = eval.newStore in identifier_declare iden currStore' eval.evaluation
    *)
    | _                                            -> evalReturn(currStore, Unknown)
and
io_operation_partial_eval x currStore = evalReturn(currStore, NoValue)
and
operation_int_int_optimise currStore lhs rhs operator constructor =
    let lhsEval = expression_int_partial_eval lhs currStore in
    let rhsEval = expression_int_partial_eval rhs lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Int_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else let lhsOpt = expression_int_optimise lhs currStore in
         let rhsOpt = expression_int_optimise rhs lhsOpt.optStore in
         optimiseReturn rhsOpt.optStore (Expression_Int_Operation(constructor lhsOpt.optBranch rhsOpt.optBranch))
and
operation_int_identifier_optimise currStore lhs rhs operator constructor =
    let lhsEval = expression_int_partial_eval lhs currStore in
    let rhsEval = expression_identifier_partial_eval rhs lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Int_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else let lhsOpt = expression_int_optimise lhs currStore in
         let rhsOpt = expression_identifier_optimise rhs lhsOpt.optStore in
         (match rhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsOpt.optStore (Expression_Int_Operation(constructor lhsOpt.optBranch newBranch))
               | _                                -> raise (OptimisationError ("Right side of Int_Identifier statement is not an identifier expression"))
         )
and
operation_identifier_int_optimise currStore lhs rhs operator constructor =
    let lhsEval = expression_identifier_partial_eval lhs currStore in
    let rhsEval = expression_int_partial_eval rhs lhsEval.newStore in
    if (isKnownValue lhsEval.evaluation) && (isKnownValue rhsEval.evaluation)
    then optimiseReturn rhsEval.newStore (Expression_Int_Literal(operator (extractIntValue lhsEval.evaluation) (extractIntValue rhsEval.evaluation)))
    else let lhsOpt = expression_identifier_optimise lhs currStore in
         let rhsOpt = expression_int_optimise rhs lhsOpt.optStore in
         (match lhsOpt.optBranch with
               | Expression_Identifier(newBranch) -> optimiseReturn rhsOpt.optStore (Expression_Int_Operation(constructor newBranch rhsOpt.optBranch))
               | _                                -> raise (OptimisationError ("Left side of Int_Identifier statement is not an identifier expression"))
         )
and
int_operation_optimise x currStore : expression_int optimiseReturn = match x with
    | Operation_Int_Plus_Int(lhs, rhs)            -> operation_int_int_optimise currStore lhs rhs (fun x y -> x + y) (fun x y -> Operation_Int_Plus_Int(x, y))
    | Operation_Int_Minus_Int(lhs, rhs)           -> operation_int_int_optimise currStore lhs rhs (fun x y -> x - y) (fun x y -> Operation_Int_Minus_Int(x, y))
    | Operation_Int_Multiply_Int(lhs, rhs)        -> operation_int_int_optimise currStore lhs rhs (fun x y -> x * y) (fun x y -> Operation_Int_Multiply_Int(x, y))
    | Operation_Int_Divide_Int(lhs, rhs)          -> operation_int_int_optimise currStore lhs rhs (fun x y -> x / y) (fun x y -> Operation_Int_Divide_Int(x, y))
    | Operation_Int_Plus_Identifier(lhs, rhs)     -> operation_int_identifier_optimise currStore lhs rhs (fun x y -> x + y) (fun x y -> Operation_Int_Plus_Identifier(x, y))
    | Operation_Int_Minus_Identifier(lhs, rhs)    -> operation_int_identifier_optimise currStore lhs rhs (fun x y -> x - y) (fun x y -> Operation_Int_Minus_Identifier(x, y))
    | Operation_Int_Multiply_Identifier(lhs, rhs) -> operation_int_identifier_optimise currStore lhs rhs (fun x y -> x * y) (fun x y -> Operation_Int_Multiply_Identifier(x, y))
    | Operation_Int_Divide_Identifier(lhs, rhs)   -> operation_int_identifier_optimise currStore lhs rhs (fun x y -> x / y) (fun x y -> Operation_Int_Divide_Identifier(x, y))
    | Operation_Identifier_Plus_Int(lhs, rhs)     -> operation_identifier_int_optimise currStore lhs rhs (fun x y -> x + y) (fun x y -> Operation_Identifier_Plus_Int(x, y))
    | Operation_Identifier_Minus_Int(lhs, rhs)    -> operation_identifier_int_optimise currStore lhs rhs (fun x y -> x - y) (fun x y -> Operation_Identifier_Minus_Int(x, y))
    | Operation_Identifier_Multiply_Int(lhs, rhs) -> operation_identifier_int_optimise currStore lhs rhs (fun x y -> x * y) (fun x y -> Operation_Identifier_Multiply_Int(x, y))
    | Operation_Identifier_Divide_Int(lhs, rhs)   -> operation_identifier_int_optimise currStore lhs rhs (fun x y -> x / y) (fun x y -> Operation_Identifier_Divide_Int(x, y))
    | Operation_Negate_Int(exp)                   -> let eval = expression_int_partial_eval exp currStore in
                                                     if isKnownValue eval.evaluation
                                                     then optimiseReturn eval.newStore (Expression_Int_Literal(extractIntValue eval.evaluation))
                                                     else optimiseReturn eval.newStore (Expression_Int_Operation(Operation_Negate_Int(exp)))
    | Operation_String_Length(exp)                -> let eval = expression_string_partial_eval exp currStore in
                                                     if isKnownValue eval.evaluation
                                                     then optimiseReturn eval.newStore (Expression_Int_Literal(String.length (extractStringValue eval.evaluation)))
                                                     else optimiseReturn eval.newStore (Expression_Int_Operation(Operation_String_Length(exp)))
and
float_operation_optimise x currStore = optimiseReturn currStore x
and
bool_operation_optimise x currStore = optimiseReturn currStore x
and
string_operation_optimise x currStore = optimiseReturn currStore x
and
identifier_operation_optimise x currStore = optimiseReturn currStore x
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
                                   )