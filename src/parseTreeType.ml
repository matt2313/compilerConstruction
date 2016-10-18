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
      Let_Statement
and
new_statement =
      New_Statement
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
    | Statement_While of while_statement
    | Statement_If of if_statement
    | Statement_Function of function_definition
    | Statement_Return of return_statement
    | Statement_Let of let_statement * statement
    | Statement_New of new_statement * statement
and
expression =
      Expression_Int of expression_int
    | Expression_Float of expression_float
    | Expression_Bool of expression_bool
    | Expression_String of expression_string
    | Expression_IO of io_operation
    | Expression_Identifier of expression_identifier
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
    | Expression_Int_Assign of identifier * expression_int
    
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
    | Expression_Float_Assign of identifier * expression_float
    
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
    | Expression_Bool_Assign of identifier * expression_bool
    
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
    | Expression_String_Assign of identifier * expression_string
    
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
    | Expression_Identifier_Assign of identifier * expression_identifier
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
;;