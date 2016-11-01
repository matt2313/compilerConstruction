open ParseTreeType
open ParseTreeEvaluator



(* OptimiseReturn is the record type that is passed between elements in the parse tree *)
type 'a optimiseReturn = {newStore:store; newBranch:'a}
let optimiseReturn x y = {newStore = x; newBranch = y}



(* Helper functions for optimising within new scope *)
let optimiseInNewScope opt_func currStore = let opt = opt_func (pushScope currStore) in optimiseReturn (popScope opt.newStore), opt.newBranch
let optimise2InNewScope opt_func_first opt_func_second currStore = let opt = opt_func_first (pushScope currStore) in
                                                                   let opt' = opt_func_second opt.newStore in
                                                                   optimiseReturn (popScope opt'.newStore) (opt.newBranch, opt'.newBranch)



let rec optimiseParseTree x = parseTree_optimise x emptyStore
and
parseTree_optimise x currStore = match x with
    | ParseTree_Functions(funcList) -> ParseTree_Functions((function_list_optimise funcList currStore).newBranch)
    | ParseTree_Empty -> x
and
function_list_optimise x currStore = match x with
    | Function_List_Def(funcDefinition)            -> let opt = function_definition_optimise funcDefinition currStore in optimiseReturn opt.newStore (Function_List_Def(opt.newBranch))
    | Function_List_Let(letStatement, funcList)    -> let opt = optimise2InNewScope (let_statement_optimise letStatement) (function_list_optimise funcList) currStore in
                                                      let (letStatementOpt, funcListOpt) = opt.newBranch in
                                                      optimiseReturn opt.newStore (Function_List_Let(letStatementOpt, funcListOpt))
    | Function_List_New(newStatement, funcList)    -> let opt = optimise2InNewScope (new_statement_optimise newStatement) (function_list_optimise funcList) currStore in
                                                      let (newStatementOpt, funcListOpt) = opt.newBranch in
                                                      optimiseReturn opt.newStore (Function_List_New(newStatementOpt, funcListOpt))
    | Function_List_List(funcDefinition, funcList) -> let opt = function_definition_optimise funcDefinition currStore in
                                                      let opt' = function_list_optimise funcList opt.newStore in
                                                      optimiseReturn opt'.newStore (Function_List_List(opt.newBranch, opt'.newBranch))
and
function_definition_optimise x currStore = match x with
    | Function_Definition(iden, args, statements) -> optimiseReturn currStore x
and
let_statement_optimise x currStore = optimiseReturn currStore x
and
new_statement_optimise x currStore = optimiseReturn currStore x
(*
and
let_statement_optimise x currStore = match x with
    | Let_Statement_Int(iden, exp)        -> let eval = (expression_int_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_Float(iden, exp)      -> let eval = (expression_float_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_Bool(iden, exp)       -> let eval = (expression_bool_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_String(iden, exp)     -> let eval = (expression_string_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | Let_Statement_Identifier(iden, exp) -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
and
new_statement_optimise x currStore = match x with
    | New_Statement_Int(iden, exp)        -> let eval = (expression_int_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_Float(iden, exp)      -> let eval = (expression_float_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_Bool(iden, exp)       -> let eval = (expression_bool_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_String(iden, exp)     -> let eval = (expression_string_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
    | New_Statement_Identifier(iden, exp) -> let eval = (expression_identifier_eval exp currStore) in let currStore' = eval.newStore in (identifier_declare iden currStore' eval.evaluation)
*)