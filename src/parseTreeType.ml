(* This module contains the datatype that is used to store the parse tree *)

type
parseTree =
      ParseTree_Functions of function_list
    | ParseTree_Empty
and  
function_list = 
      Function_List_Def of function_definition
    | Function_List_Let of let_statement * function_list
    | Function_List_New of new_statement * function_list
    | Function_List_List of function_definition * function_list
and   
function_definition =
      Function_Definition of identifier * argument_list * statement_list
and
let_statement =
      Let_Statement_Int of identifier * expression_int
    | Let_Statement_Float of identifier * expression_float
    | Let_Statement_Bool of identifier * expression_bool
    | Let_Statement_String of identifier * expression_string
    | Let_Statement_Identifier of identifier * expression_identifier
and
new_statement =
      New_Statement_Int of identifier * expression_int
    | New_Statement_Float of identifier * expression_float
    | New_Statement_Bool of identifier * expression_bool
    | New_Statement_String of identifier * expression_string
    | New_Statement_Identifier of identifier * expression_identifier
and
identifier =
      Identifier_Declaration of typename * string
    | Identifier_Reference of string
and
argument_list =
      Argument_List_Element of identifier
    | Argument_List_List of identifier * argument_list
    | Argument_List_Empty
and
statement_list = 
      Statement_List_Statement of statement
    | Statement_List_List of statement * statement_list
    | Statement_List_Empty
and
typename =
      Int
    | Float
    | Bool
    | String
and
statement =
      Statement_Expression of expression
    | Statement_Function of function_definition
    | Statement_Return of return_statement
    | Statement_Let of let_statement * statement
    | Statement_New of new_statement * statement
    | Statement_Let_List of let_statement * statement_list
    | Statement_New_List of new_statement * statement_list
and
expression =
      Expression_Int of expression_int
    | Expression_Float of expression_float
    | Expression_Bool of expression_bool
    | Expression_String of expression_string
    | Expression_IO of io_operation
    | Expression_Identifier of expression_identifier
    | Expression_Empty
and
while_statement =
      While_Loop_While of expression_bool * statement_list
    | While_Loop_Do of statement_list * expression_bool
and
if_statement =
      If_Statement_If_Bool of expression_bool * statement_list
    | If_Statement_Else_Bool of expression_bool * statement_list * statement_list
    | If_Statement_Else_List_Bool of expression_bool * statement_list * if_statement
    | If_Statement_If_Identifier of expression_identifier * statement_list
    | If_Statement_Else_Identifier of expression_identifier * statement_list * statement_list
    | If_Statement_Else_List_Identifier of expression_identifier * statement_list * if_statement
and
return_statement =
      Return_Int of expression_int
    | Return_Float of expression_float
    | Return_Bool of expression_bool
    | Return_String of expression_string
    | Return_Identifier of expression_identifier
and
io_operation =
      IO_Operation_Print_Int of expression_int
    | IO_Operation_Print_Float of expression_float
    | IO_Operation_Print_Bool of expression_bool
    | IO_Operation_Print_String of expression_string
    
    | IO_Operation_Print_Int_Identifier of expression_identifier
    | IO_Operation_Print_Float_Identifier of expression_identifier
    | IO_Operation_Print_Bool_Identifier of expression_identifier
    | IO_Operation_Print_String_Identifier of expression_identifier
and
expression_int =
      Expression_Int_Literal of int
    | Expression_Int_Operation of int_operation
    | Expression_Int_Read
    | Expression_Int_Declare of identifier * expression_int
    | Expression_Int_Assign of expression_identifier * expression_int
    
    | Expression_Float_To_Int of expression_float
    | Expression_Bool_To_Int of expression_bool
    | Expression_String_To_Int of expression_string
    | Expression_Identifier_To_Int of expression_identifier
and
expression_float =
      Expression_Float_Literal of float
    | Expression_Float_Operation of float_operation
    | Expression_Float_Read
    | Expression_Float_Declare of identifier * expression_float
    | Expression_Float_Assign of expression_identifier * expression_float
    
    | Expression_Int_To_Float of expression_int
    | Expression_Bool_To_Float of expression_bool
    | Expression_String_To_Float of expression_string
    | Expression_Identifier_To_Float of expression_identifier
and
expression_bool =
      Expression_Bool_Literal of bool
    | Expression_Bool_Operation of bool_operation
    | Expression_Bool_Read
    | Expression_Bool_Declare of identifier * expression_bool
    | Expression_Bool_Assign of expression_identifier * expression_bool
    
    | Expression_Int_To_Bool of expression_int
    | Expression_Float_To_Bool of expression_float
    | Expression_String_To_Bool of expression_string
    | Expression_Identifier_To_Bool of expression_identifier
and
expression_string =
      Expression_String_Literal of string
    | Expression_String_Operation of string_operation
    | Expression_String_Read
    | Expression_String_Declare of identifier * expression_string
    | Expression_String_Assign of expression_identifier * expression_string
    
    | Expression_Int_To_String of expression_int
    | Expression_Float_To_String of expression_float
    | Expression_Bool_To_String of expression_bool
    | Expression_Identifier_To_String of expression_identifier
and
expression_identifier =
      Expression_Identifier_Dereference of identifier
    | Expression_Identifier_Function_Call of identifier * parameter_list
    | Expression_Identifier_Operation of identifier_operation
    
    | Expression_Identifier_Declare_Int of identifier * expression_identifier
    | Expression_Identifier_Declare_Float of identifier * expression_identifier
    | Expression_Identifier_Declare_Bool of identifier * expression_identifier
    | Expression_Identifier_Declare_String of identifier * expression_identifier
    | Expression_Identifier_Assign of expression_identifier * expression_identifier
    
    | Expression_Identifier_Variable_Ref of identifier
    | Statement_While of while_statement
    | Statement_If of if_statement
and
int_operation =
      Operation_Int_Plus_Int of expression_int * expression_int
    | Operation_Int_Minus_Int of expression_int * expression_int
    | Operation_Int_Multiply_Int of expression_int * expression_int
    | Operation_Int_Divide_Int of expression_int * expression_int
    | Operation_Negate_Int of expression_int
    
    | Operation_Int_Plus_Identifier of expression_int * expression_identifier
    | Operation_Int_Minus_Identifier of expression_int * expression_identifier
    | Operation_Int_Multiply_Identifier of expression_int * expression_identifier
    | Operation_Int_Divide_Identifier of expression_int * expression_identifier
    
    | Operation_Identifier_Plus_Int of expression_identifier * expression_int
    | Operation_Identifier_Minus_Int of expression_identifier * expression_int
    | Operation_Identifier_Multiply_Int of expression_identifier * expression_int
    | Operation_Identifier_Divide_Int of expression_identifier * expression_int
    | Operation_String_Length of expression_string
and
float_operation =
      Operation_Float_Plus_Float of expression_float * expression_float
    | Operation_Float_Minus_Float of expression_float * expression_float
    | Operation_Float_Multiply_Float of expression_float * expression_float
    | Operation_Float_Divide_Float of expression_float * expression_float
    | Operation_Negate_Float of expression_float
      
    | Operation_Float_Plus_Identifier of expression_float * expression_identifier
    | Operation_Float_Minus_Identifier of expression_float * expression_identifier
    | Operation_Float_Multiply_Identifier of expression_float * expression_identifier
    | Operation_Float_Divide_Identifier of expression_float * expression_identifier
      
    | Operation_Identifier_Plus_Float of expression_identifier * expression_float
    | Operation_Identifier_Minus_Float of expression_identifier * expression_float
    | Operation_Identifier_Multiply_Float of expression_identifier * expression_float
    | Operation_Identifier_Divide_Float of expression_identifier * expression_float
and
bool_operation =
      Operation_Bool_And_Bool of expression_bool * expression_bool
    | Operation_Bool_Nand_Bool of expression_bool * expression_bool
    | Operation_Bool_Or_Bool of expression_bool * expression_bool
    | Operation_Bool_Xor_Bool of expression_bool * expression_bool
    | Operation_Bool_Nor_Bool of expression_bool * expression_bool
    | Operation_Bool_Nxor_Bool of expression_bool * expression_bool
    | Operation_Bool_Eq_Bool of expression_bool * expression_bool
    | Operation_Bool_Not_Eq_Bool of expression_bool * expression_bool
    | Operation_Not_Bool of expression_bool
    
    | Operation_Bool_And_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Nand_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Or_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Xor_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Nor_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Nxor_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Eq_Identifier of expression_bool * expression_identifier
    | Operation_Bool_Not_Eq_Identifier of expression_bool * expression_identifier
    
    | Operation_Identifier_And_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Nand_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Or_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Xor_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Nor_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Nxor_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Eq_Bool of expression_identifier * expression_bool
    | Operation_Identifier_Not_Eq_Bool of expression_identifier * expression_bool
    
    
    | Operation_Int_Less_Than_Int of expression_int * expression_int
    | Operation_Int_Greater_Than_Int of expression_int * expression_int
    | Operation_Int_Less_Than_Or_Eq_Int of expression_int * expression_int
    | Operation_Int_Greater_Than_Or_Eq_Int of expression_int * expression_int
    | Operation_Int_Eq_Int of expression_int * expression_int
    | Operation_Int_Not_Eq_Int of expression_int * expression_int
    
    | Operation_Int_Less_Than_Identifier of expression_int * expression_identifier
    | Operation_Int_Greater_Than_Identifier of expression_int * expression_identifier
    | Operation_Int_Less_Than_Or_Eq_Identifier of expression_int * expression_identifier
    | Operation_Int_Greater_Than_Or_Eq_Identifier of expression_int * expression_identifier
    | Operation_Int_Eq_Identifier of expression_int * expression_identifier
    | Operation_Int_Not_Eq_Identifier of expression_int * expression_identifier
    
    | Operation_Identifier_Less_Than_Int of expression_identifier * expression_int
    | Operation_Identifier_Greater_Than_Int of expression_identifier * expression_int
    | Operation_Identifier_Less_Than_Or_Eq_Int of expression_identifier * expression_int
    | Operation_Identifier_Greater_Than_Or_Eq_Int of expression_identifier * expression_int
    | Operation_Identifier_Eq_Int of expression_identifier * expression_int
    | Operation_Identifier_Not_Eq_Int of expression_identifier * expression_int
    
    
    | Operation_Float_Less_Than_Float of expression_float * expression_float
    | Operation_Float_Greater_Than_Float of expression_float * expression_float
    | Operation_Float_Less_Than_Or_Eq_Float of expression_float * expression_float
    | Operation_Float_Greater_Than_Or_Eq_Float of expression_float * expression_float
    | Operation_Float_Eq_Float of expression_float * expression_float
    | Operation_Float_Not_Eq_Float of expression_float * expression_float
    
    | Operation_Float_Less_Than_Identifier of expression_float * expression_identifier
    | Operation_Float_Greater_Than_Identifier of expression_float * expression_identifier
    | Operation_Float_Less_Than_Or_Eq_Identifier of expression_float * expression_identifier
    | Operation_Float_Greater_Than_Or_Eq_Identifier of expression_float * expression_identifier
    | Operation_Float_Eq_Identifier of expression_float * expression_identifier
    | Operation_Float_Not_Eq_Identifier of expression_float * expression_identifier
    
    | Operation_Identifier_Less_Than_Float of expression_identifier * expression_float
    | Operation_Identifier_Greater_Than_Float of expression_identifier * expression_float
    | Operation_Identifier_Less_Than_Or_Eq_Float of expression_identifier * expression_float
    | Operation_Identifier_Greater_Than_Or_Eq_Float of expression_identifier * expression_float
    | Operation_Identifier_Eq_Float of expression_identifier * expression_float
    | Operation_Identifier_Not_Eq_Float of expression_identifier * expression_float
and
string_operation =
      Operation_String_Concat_String of expression_string * expression_string
    | Operation_String_Concat_Identifier of expression_string * expression_identifier
    | Operation_Identifier_Concat_String of expression_identifier * expression_string
    
    | Operation_Substring_String_Int_Int of expression_string * expression_int * expression_int
    | Operation_Substring_String_Int_Identifier of expression_string * expression_int * expression_identifier
    | Operation_Substring_String_Identifier_Int of expression_string * expression_identifier * expression_int
    | Operation_Substring_String_Identifier_Identifier of expression_string * expression_identifier * expression_identifier
    | Operation_Substring_Identifier_Int_Int of expression_identifier * expression_int * expression_int
    | Operation_Substring_Identifier_Int_Identifier of expression_identifier * expression_int * expression_identifier
    | Operation_Substring_Identifier_Identifier_Int of expression_identifier * expression_identifier * expression_int
and
identifier_operation =
      Operation_Identifier_Plus_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Minus_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Multiply_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Divide_Identifier of expression_identifier * expression_identifier
    | Operation_Negate_Identifier of expression_identifier
    
    | Operation_Identifier_And_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Nand_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Or_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Xor_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Nor_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Nxor_Identifier of expression_identifier * expression_identifier
    | Operation_Not_Identifier of expression_identifier
    
    | Operation_Identifier_Less_Than_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Greater_Than_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Less_Than_Or_Eq_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Greater_Than_Or_Eq_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Eq_Identifier of expression_identifier * expression_identifier
    | Operation_Identifier_Not_Eq_Identifier of expression_identifier * expression_identifier
    
    | Operation_Identifier_Concat_Identifier of expression_identifier * expression_identifier
    | Operation_Substring_Identifier_Identifier_Identifier of expression_identifier * expression_identifier * expression_identifier
and
parameter_list =
      Parameter_List_Element of expression
    | Parameter_List_List of expression * parameter_list
    | Parameter_List_Empty

    
(* ToString functions *)

let rec repeatChars numTimes toRepeat = match numTimes with
    | 0 -> ""
    | n -> toRepeat ^ (repeatChars (n - 1) toRepeat)

let rec parseTree_toString x numTabs tabChars = match x with
    | ParseTree_Functions(funcList) -> (repeatChars numTabs tabChars) ^ (function_list_toString funcList numTabs tabChars)
    | ParseTree_Empty -> (repeatChars numTabs tabChars) ^ "Empty File"
and
function_list_toString x numTabs tabChars = match x with
    | Function_List_Def(funcDefinition) -> function_definition_toString funcDefinition numTabs tabChars
    | Function_List_Let(letStatement, funcList) -> (let_statement_toString letStatement) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (function_list_toString funcList numTabs tabChars)
    | Function_List_New(newStatement, funcList) -> (new_statement_toString newStatement) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (function_list_toString funcList numTabs tabChars)
    | Function_List_List(funcDefinition, funcList) -> (function_definition_toString funcDefinition numTabs tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (function_list_toString funcList numTabs tabChars)
and
function_definition_toString x numTabs tabChars = match x with
    | Function_Definition(iden, args, statements) -> "function: " ^ (identifier_toString iden) ^ "\n" ^ (repeatChars numTabs tabChars) ^ "args: " ^ (argument_list_toString args) ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statements (numTabs + 1) tabChars) ^ "\n"
and
let_statement_toString x = match x with
    | Let_Statement_Int(iden, exp) -> "Let Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_int_toString exp) ^ ")"
    | Let_Statement_Float(iden, exp) -> "Let Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_float_toString exp) ^ ")"
    | Let_Statement_Bool(iden, exp) -> "Let Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_bool_toString exp) ^ ")"
    | Let_Statement_String(iden, exp) -> "Let Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_string_toString exp) ^ ")"
    | Let_Statement_Identifier(iden, exp) -> "Let Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_identifier_toString exp) ^ ")"
and
new_statement_toString x = match x with
    | New_Statement_Int(iden, exp) -> "New Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_int_toString exp) ^ ")"
    | New_Statement_Float(iden, exp) -> "New Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_float_toString exp) ^ ")"
    | New_Statement_Bool(iden, exp) -> "New Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_bool_toString exp) ^ ")"
    | New_Statement_String(iden, exp) -> "New Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_string_toString exp) ^ ")"
    | New_Statement_Identifier(iden, exp) -> "New Statement: " ^ (identifier_toString iden) ^ " of value (" ^ (expression_identifier_toString exp) ^ ")"
and
identifier_toString x = match x with
    | Identifier_Declaration(t, name) -> "Identifier Declaration(" ^ (typename_toString t) ^ ": '" ^ name ^ "')"
    | Identifier_Reference(name) -> "Identifier Reference('" ^ name ^ "')"
and
argument_list_toString x = match x with
    | Argument_List_Empty -> "no args"
    | Argument_List_Element(iden) -> identifier_toString iden
    | Argument_List_List(iden, argList) -> (identifier_toString iden) ^ ", " ^ (argument_list_toString argList)
and
statement_list_toString x numTabs tabChars = match x with
    | Statement_List_Empty -> "no statements"
    | Statement_List_Statement(stat) -> (statement_toString stat numTabs tabChars)
    | Statement_List_List(stat, statList) -> (statement_toString stat numTabs tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (statement_list_toString statList numTabs tabChars)
and
typename_toString x = match x with
    | Int -> "Int"
    | Float -> "Float"
    | Bool -> "Bool"
    | String -> "String"
and
statement_toString x numTabs tabChars = match x with
    | Statement_Expression(exp) -> expression_toString exp numTabs tabChars
    | Statement_Function(func) -> function_definition_toString func numTabs tabChars
    | Statement_Return(ret) -> return_statement_toString ret
    | Statement_Let(letStat, stat) -> (let_statement_toString letStat) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (statement_toString stat numTabs tabChars)
    | Statement_New(newStat, stat) -> (new_statement_toString newStat) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (statement_toString stat numTabs tabChars)
    | Statement_Let_List(newStat, lst) -> (let_statement_toString newStat) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (statement_list_toString lst (numTabs + 1) tabChars)
    | Statement_New_List(newStat, lst) -> (new_statement_toString newStat) ^ "\n" ^ (repeatChars numTabs tabChars) ^ (statement_list_toString lst (numTabs + 1) tabChars)
and
expression_toString x numTabs tabChars = match x with
    | Expression_Int(exp) -> expression_int_toString exp
    | Expression_Float(exp) -> expression_float_toString exp
    | Expression_Bool(exp) -> expression_bool_toString exp
    | Expression_String(exp) -> expression_string_toString exp
    | Expression_Identifier(exp) -> expression_identifier_toString exp
    | Expression_IO(ioOp) -> io_operation_toString ioOp
    | Expression_Empty -> "empty expression"
and
while_statement_toString x numTabs tabChars = match x with
    | While_Loop_While(exp, statList) -> "while: (" ^ (expression_bool_toString exp) ^ ")" ^ "\n" ^ (repeatChars numTabs tabChars) ^ "do" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars)
    | While_Loop_Do(statList, exp) -> "do:" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ "while: (" ^ (expression_bool_toString exp) ^ ")"
and
if_statement_toString x numTabs tabChars = match x with
    | If_Statement_If_Bool(exp, statList) -> "if (" ^ (expression_bool_toString exp) ^ ")" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars)
    | If_Statement_Else_Bool(exp, statList, statListElse) -> "if (" ^ (expression_bool_toString exp) ^ ")" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ "else" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statListElse (numTabs + 1) tabChars)
    | If_Statement_Else_List_Bool(exp, statList, ifStat) -> "if (" ^ (expression_bool_toString exp) ^ ")" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ "else " ^ (if_statement_toString ifStat numTabs tabChars)
    | If_Statement_If_Identifier(exp, statList) ->  "if (" ^ (expression_identifier_toString exp) ^ ")" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars)
    | If_Statement_Else_Identifier(exp, statList, statListElse) -> "if (" ^ (expression_identifier_toString exp) ^ ")" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ "else" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statListElse (numTabs + 1) tabChars)
    | If_Statement_Else_List_Identifier(exp, statList, ifStat) -> "if (" ^ (expression_identifier_toString exp) ^ ")" ^ "\n" ^ (repeatChars (numTabs + 1) tabChars) ^ (statement_list_toString statList (numTabs + 1) tabChars) ^ "\n" ^ (repeatChars numTabs tabChars) ^ "else " ^ (if_statement_toString ifStat numTabs tabChars)
and
return_statement_toString x = match x with
    | Return_Int(exp) -> "return(" ^ expression_int_toString(exp) ^ ")"
    | Return_Float(exp) -> "return(" ^ expression_float_toString(exp) ^ ")"
    | Return_Bool(exp) -> "return(" ^ expression_bool_toString(exp) ^ ")"
    | Return_String(exp) -> "return(" ^ expression_string_toString(exp) ^ ")"
    | Return_Identifier(exp) -> "return(" ^ expression_identifier_toString(exp) ^ ")"
and
io_operation_toString x = match x with
    | IO_Operation_Print_Int(exp) -> "print(" ^ expression_int_toString(exp) ^ ")"
    | IO_Operation_Print_Float(exp) -> "print(" ^ expression_float_toString(exp) ^ ")"
    | IO_Operation_Print_Bool(exp) -> "print(" ^ expression_bool_toString(exp) ^ ")"
    | IO_Operation_Print_String(exp) -> "print(" ^ expression_string_toString(exp) ^ ")"
    
    | IO_Operation_Print_Int_Identifier(exp) -> "print(" ^ expression_identifier_toString(exp) ^ ")"
    | IO_Operation_Print_Float_Identifier(exp) -> "print(" ^ expression_identifier_toString(exp) ^ ")"
    | IO_Operation_Print_Bool_Identifier(exp) -> "print(" ^ expression_identifier_toString(exp) ^ ")"
    | IO_Operation_Print_String_Identifier(exp) -> "print(" ^ expression_identifier_toString(exp) ^ ")"
and
expression_int_toString x = match x with
    | Expression_Int_Literal(value) -> "int literal(" ^ (string_of_int value) ^ ")"
    | Expression_Int_Operation(op) -> int_operation_toString op
    | Expression_Int_Read -> "int input"
    | Expression_Int_Declare(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_int_toString exp) ^ ")"
    | Expression_Int_Assign(iden, exp) -> "assign (" ^ (expression_identifier_toString iden) ^ ") with value (" ^ (expression_int_toString exp) ^ ")"
    | Expression_Float_To_Int(exp) -> "cast float to int(" ^ (expression_float_toString exp) ^ ")"
    | Expression_Bool_To_Int(exp) -> "cast bool to int(" ^ (expression_bool_toString exp) ^ ")"
    | Expression_String_To_Int(exp) -> "cast string to int(" ^ (expression_string_toString exp) ^ ")"
    | Expression_Identifier_To_Int(exp) -> "cast identifier to int(" ^ (expression_identifier_toString exp) ^ ")"
and
expression_float_toString x = match x with
    | Expression_Float_Literal(value) -> "float literal(" ^ (string_of_float value) ^ ")"
    | Expression_Float_Operation(op) -> float_operation_toString op
    | Expression_Float_Read -> "float input"
    | Expression_Float_Declare(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_float_toString exp) ^ ")"
    | Expression_Float_Assign(iden, exp) -> "assign (" ^ (expression_identifier_toString iden) ^ ") with value (" ^ (expression_float_toString exp) ^ ")"
    | Expression_Int_To_Float(exp) -> "cast int to float(" ^ (expression_int_toString exp)^ ")"
    | Expression_Bool_To_Float(exp) -> "cast bool to float(" ^ (expression_bool_toString exp) ^ ")"
    | Expression_String_To_Float(exp) -> "cast string to float(" ^ (expression_string_toString exp) ^ ")"
    | Expression_Identifier_To_Float(exp) -> "cast identifier to float(" ^ (expression_identifier_toString exp) ^ ")"
and
expression_bool_toString x = match x with
    | Expression_Bool_Literal(value) -> "bool literal(" ^ (string_of_bool value) ^ ")"
    | Expression_Bool_Operation(op) -> bool_operation_toString op
    | Expression_Bool_Read -> "bool input"
    | Expression_Bool_Declare(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_bool_toString exp) ^ ")"
    | Expression_Bool_Assign(iden, exp) -> "assign (" ^ (expression_identifier_toString iden) ^ ") with value (" ^ (expression_bool_toString exp) ^ ")"
    | Expression_Int_To_Bool(exp) -> "cast int to bool(" ^ (expression_int_toString exp) ^ ")"
    | Expression_Float_To_Bool(exp) -> "cast float to bool(" ^ (expression_float_toString exp) ^ ")"
    | Expression_String_To_Bool(exp) -> "cast string to bool(" ^ (expression_string_toString exp) ^ ")"
    | Expression_Identifier_To_Bool(exp) -> "cast identifier to bool(" ^ (expression_identifier_toString exp) ^ ")"
and
expression_string_toString x = match x with
    | Expression_String_Literal(value) -> "string literal('" ^ value ^ "')"
    | Expression_String_Operation(op) -> string_operation_toString op
    | Expression_String_Read -> "string input"
    | Expression_String_Declare(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_string_toString exp) ^ ")"
    | Expression_String_Assign(iden, exp) -> "assign (" ^ (expression_identifier_toString iden) ^ ") with value (" ^ (expression_string_toString exp) ^ ")"
    | Expression_Int_To_String(exp) -> "cast int to string(" ^ (expression_int_toString exp) ^ ")"
    | Expression_Float_To_String(exp) -> "cast float to string(" ^ (expression_float_toString exp) ^ ")"
    | Expression_Bool_To_String(exp) -> "cast bool to string(" ^ (expression_bool_toString exp) ^ ")"
    | Expression_Identifier_To_String(exp) -> "cast identifier to string(" ^ (expression_identifier_toString exp) ^ ")"
and
expression_identifier_toString x = match x with
    | Expression_Identifier_Dereference(iden) -> "dereference(" ^ (identifier_toString iden) ^ ")"
    | Expression_Identifier_Function_Call(iden, params) -> "function call (" ^ (identifier_toString iden) ^ ") with params (" ^ (parameter_list_toString params) ^ ")"
    | Expression_Identifier_Operation(op) -> identifier_operation_toString op
    | Expression_Identifier_Declare_Int(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_identifier_toString exp) ^ ")"
    | Expression_Identifier_Declare_Float(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_identifier_toString exp) ^ ")"
    | Expression_Identifier_Declare_Bool(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_identifier_toString exp) ^ ")"
    | Expression_Identifier_Declare_String(iden, exp) -> "declare (" ^ (identifier_toString iden) ^ ") with value (" ^ (expression_identifier_toString exp) ^ ")"
    | Expression_Identifier_Assign(iden, exp) -> "assign (" ^ (expression_identifier_toString iden) ^ ") with value (" ^ (expression_identifier_toString exp) ^ ")"
    | Expression_Identifier_Variable_Ref(iden) -> "variable reference(" ^ (identifier_toString iden) ^ ")"
    (*
    | Statement_While(whileStat) -> while_statement_toString whileStat numTabs tabChars
    | Statement_If(ifStat) -> if_statement_toString ifStat numTabs tabChars
    *)
    | Statement_While(whileStat) -> while_statement_toString whileStat 0 ""
    | Statement_If(ifStat) -> if_statement_toString ifStat 0 ""
and
int_operation_toString x = match x with
    | Operation_Int_Plus_Int(lhs, rhs) -> "add(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Minus_Int(lhs, rhs) -> "minus(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Multiply_Int(lhs, rhs) -> "multiply(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Divide_Int(lhs, rhs) -> "divide(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Negate_Int(exp) -> "negate(" ^ (expression_int_toString exp) ^ ")"
    
    | Operation_Int_Plus_Identifier(lhs, rhs) -> "add(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Minus_Identifier(lhs, rhs) -> "minus(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Multiply_Identifier(lhs, rhs) -> "multiply(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Divide_Identifier(lhs, rhs) -> "divide(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Identifier_Plus_Int(lhs, rhs) -> "add(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Minus_Int(lhs, rhs) -> "minus(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Multiply_Int(lhs, rhs) -> "multiply(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Divide_Int(lhs, rhs) -> "divide(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_String_Length(exp) -> "length(" ^ (expression_string_toString exp) ^ ")"
and
float_operation_toString x = match x with
    | Operation_Float_Plus_Float(lhs, rhs) -> "add(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Minus_Float(lhs, rhs) -> "minus(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Multiply_Float(lhs, rhs) -> "multiply(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Divide_Float(lhs, rhs) -> "divide(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Negate_Float(exp) -> "negate(" ^ (expression_float_toString exp) ^ ")"
    
    | Operation_Float_Plus_Identifier(lhs, rhs) -> "add(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Minus_Identifier(lhs, rhs) -> "minus(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Multiply_Identifier(lhs, rhs) -> "multiply(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Divide_Identifier(lhs, rhs) -> "divide(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Identifier_Plus_Float(lhs, rhs) -> "add(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Minus_Float(lhs, rhs) -> "minus(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Multiply_Float(lhs, rhs) -> "multiply(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Divide_Float(lhs, rhs) -> "divide(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
and
bool_operation_toString x = match x with
    | Operation_Bool_And_Bool(lhs, rhs) -> "and(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Nand_Bool(lhs, rhs) -> "nand(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Or_Bool(lhs, rhs) -> "or(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Xor_Bool(lhs, rhs) -> "xor(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Nor_Bool(lhs, rhs) -> "nor(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Nxor_Bool(lhs, rhs) -> "nxor(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Eq_Bool(lhs, rhs) -> "equal(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Bool_Not_Eq_Bool(lhs, rhs) -> "not equal(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Not_Bool(exp) -> "not(" ^ (expression_bool_toString exp) ^ ")"
    
    | Operation_Bool_And_Identifier(lhs, rhs) -> "and(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Nand_Identifier(lhs, rhs) -> "nand(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Or_Identifier(lhs, rhs) -> "or(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Xor_Identifier(lhs, rhs) -> "xor(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Nor_Identifier(lhs, rhs) -> "nor(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Nxor_Identifier(lhs, rhs) -> "nxor(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Eq_Identifier(lhs, rhs) -> "equal(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Bool_Not_Eq_Identifier(lhs, rhs) -> "not equal(" ^ (expression_bool_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Identifier_And_Bool(lhs, rhs) -> "and(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Nand_Bool(lhs, rhs) -> "nand(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Or_Bool(lhs, rhs) -> "or(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Xor_Bool(lhs, rhs) -> "xor(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Nor_Bool(lhs, rhs) -> "nor(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Nxor_Bool(lhs, rhs) -> "nxor(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Eq_Bool(lhs, rhs) -> "equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    | Operation_Identifier_Not_Eq_Bool(lhs, rhs) -> "not equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_bool_toString rhs) ^ ")"
    
    
    | Operation_Int_Less_Than_Int(lhs, rhs) -> "less than(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Greater_Than_Int(lhs, rhs) -> "greater than(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Less_Than_Or_Eq_Int(lhs, rhs) -> "less than or equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Greater_Than_Or_Eq_Int(lhs, rhs) -> "greater than or equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Eq_Int(lhs, rhs) -> "equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Int_Not_Eq_Int(lhs, rhs) -> "not equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    
    | Operation_Int_Less_Than_Identifier(lhs, rhs) -> "less than(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Greater_Than_Identifier(lhs, rhs) -> "greater than(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Less_Than_Or_Eq_Identifier(lhs, rhs) -> "less than or equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> "greater than or equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Eq_Identifier(lhs, rhs) -> "equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Int_Not_Eq_Identifier(lhs, rhs) -> "not equal(" ^ (expression_int_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Identifier_Less_Than_Int(lhs, rhs) -> "less than(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Greater_Than_Int(lhs, rhs) -> "greater than(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Less_Than_Or_Eq_Int(lhs, rhs) -> "less than or equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Greater_Than_Or_Eq_Int(lhs, rhs) -> "greater than or equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Eq_Int(lhs, rhs) -> "equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    | Operation_Identifier_Not_Eq_Int(lhs, rhs) -> "not equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_int_toString rhs) ^ ")"
    
    
    | Operation_Float_Less_Than_Float(lhs, rhs) -> "less than(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Greater_Than_Float(lhs, rhs) -> "greater than(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Less_Than_Or_Eq_Float(lhs, rhs) -> "less than or equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Greater_Than_Or_Eq_Float(lhs, rhs) -> "greater than or equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Eq_Float(lhs, rhs) -> "equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Float_Not_Eq_Float(lhs, rhs) -> "not equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    
    | Operation_Float_Less_Than_Identifier(lhs, rhs) -> "less than(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Greater_Than_Identifier(lhs, rhs) -> "greater than(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Less_Than_Or_Eq_Identifier(lhs, rhs) -> "less than or equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> "greater than or equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Eq_Identifier(lhs, rhs) -> "equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Float_Not_Eq_Identifier(lhs, rhs) -> "not equal(" ^ (expression_float_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Identifier_Less_Than_Float(lhs, rhs) -> "less than(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Greater_Than_Float(lhs, rhs) -> "greater than(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Less_Than_Or_Eq_Float(lhs, rhs) -> "less than or equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Greater_Than_Or_Eq_Float(lhs, rhs) -> "greater than or equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Eq_Float(lhs, rhs) -> "equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
    | Operation_Identifier_Not_Eq_Float(lhs, rhs) -> "not equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_float_toString rhs) ^ ")"
and
string_operation_toString x = match x with
    | Operation_String_Concat_String(lhs, rhs) -> "concat(" ^ (expression_string_toString lhs) ^ ", " ^ (expression_string_toString rhs) ^ ")"
    | Operation_Identifier_Concat_String(lhs, rhs) -> "concat(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_string_toString rhs) ^ ")"
    | Operation_String_Concat_Identifier(lhs, rhs) -> "concat(" ^ (expression_string_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Substring_String_Int_Int(strExp, startExp, lenExp) -> "substring(" ^ (expression_string_toString strExp) ^ ", " ^ (expression_int_toString startExp) ^ ", " ^ (expression_int_toString lenExp) ^ ")"
    | Operation_Substring_String_Int_Identifier(strExp, startExp, lenExp) -> "substring(" ^ (expression_string_toString strExp) ^ ", " ^ (expression_int_toString startExp) ^ ", " ^ (expression_identifier_toString lenExp) ^ ")"
    | Operation_Substring_String_Identifier_Int(strExp, startExp, lenExp) -> "substring(" ^ (expression_string_toString strExp) ^ ", " ^ (expression_identifier_toString startExp) ^ ", " ^ (expression_int_toString lenExp) ^ ")"
    | Operation_Substring_String_Identifier_Identifier(strExp, startExp, lenExp) -> "substring(" ^ (expression_string_toString strExp) ^ ", " ^ (expression_identifier_toString startExp) ^ ", " ^ (expression_identifier_toString lenExp) ^ ")"
    | Operation_Substring_Identifier_Int_Int(strExp, startExp, lenExp) -> "substring(" ^ (expression_identifier_toString strExp) ^ ", " ^ (expression_int_toString startExp) ^ ", " ^ (expression_int_toString lenExp) ^ ")"
    | Operation_Substring_Identifier_Int_Identifier(strExp, startExp, lenExp) -> "substring(" ^ (expression_identifier_toString strExp) ^ ", " ^ (expression_int_toString startExp) ^ ", " ^ (expression_identifier_toString lenExp) ^ ")"
    | Operation_Substring_Identifier_Identifier_Int(strExp, startExp, lenExp) -> "substring(" ^ (expression_identifier_toString strExp) ^ ", " ^ (expression_identifier_toString startExp) ^ ", " ^ (expression_int_toString lenExp) ^ ")"
and
identifier_operation_toString x = match x with
    | Operation_Identifier_Plus_Identifier(lhs, rhs) -> "add(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Minus_Identifier(lhs, rhs) -> "minus(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Multiply_Identifier(lhs, rhs) -> "multiply(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Divide_Identifier(lhs, rhs) -> "divide(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Negate_Identifier(exp) -> "negate(" ^ (expression_identifier_toString exp) ^ ")"
    
    | Operation_Identifier_And_Identifier(lhs, rhs) -> "and(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Nand_Identifier(lhs, rhs) -> "nand(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Or_Identifier(lhs, rhs) -> "or(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Xor_Identifier(lhs, rhs) -> "xor(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Nor_Identifier(lhs, rhs) -> "nor(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Nxor_Identifier(lhs, rhs) -> "nxor(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Not_Identifier(exp) -> "not(" ^ (expression_identifier_toString exp) ^ ")"
    
    | Operation_Identifier_Less_Than_Identifier(lhs, rhs) -> "less than(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Greater_Than_Identifier(lhs, rhs) -> "greater than(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Less_Than_Or_Eq_Identifier(lhs, rhs) -> "less than or equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Greater_Than_Or_Eq_Identifier(lhs, rhs) -> "greater than or equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Eq_Identifier(lhs, rhs) -> "equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Identifier_Not_Eq_Identifier(lhs, rhs) -> "not equal(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    
    | Operation_Identifier_Concat_Identifier(lhs, rhs) -> "concat(" ^ (expression_identifier_toString lhs) ^ ", " ^ (expression_identifier_toString rhs) ^ ")"
    | Operation_Substring_Identifier_Identifier_Identifier(strExp, startExp, lenExp) -> "substring(" ^ (expression_identifier_toString strExp) ^ ", " ^ (expression_identifier_toString startExp) ^ ", " ^ (expression_identifier_toString lenExp) ^ ")"
and
parameter_list_toString x = match x with
    | Parameter_List_Element(exp) -> expression_toString exp 0 ""
    | Parameter_List_List(exp, paramList) -> (expression_toString exp 0 "") ^ ", " ^ (parameter_list_toString paramList)
    | Parameter_List_Empty -> "no params"
    
let string_of_parseTree tree = parseTree_toString tree 0 "  "
;;