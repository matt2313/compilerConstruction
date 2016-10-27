%{
    open ParseTreeType
%}

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL

%token INT_TYPENAME
%token FLOAT_TYPENAME
%token BOOL_TYPENAME
%token STRING_TYPENAME
%token <string> IDENTIFIER  /* Name of a variable or function */
%token ASSIGN               /* Assignment operator */
%token RETURN               /* Return statement to be used in function definitions */
%token CAST                 /* Explicitly cast expression to another type */
%token LET
%token NEW
%token IN

%token OPBRACKET    /* Open bracket */
%token CLBRACKET    /* Close bracket */
%token OPBRACE      /* Open brace */
%token CLBRACE      /* Close brace */

%token READ_INT     /* Get int from terminal */
%token READ_FLOAT   /* Get float form terminal */
%token READ_BOOL    /* Get bool from terminal */
%token READ_STRING  /* Get string from terminal */
%token PRINT_INT    /* Print int to terminal */
%token PRINT_FLOAT  /* Print float to terminal */
%token PRINT_BOOL   /* Print bool to terminal */
%token PRINT_STRING /* Print string to terminal */

%token NEGATE       /* Make a number negative */
%token PLUS         /* Addition of 2 expressions */
%token MINUS        /* Substraction of 2 expressions */
%token MULTIPLY     /* Multiplication of 2 expressions */
%token DIVIDE       /* Division of left expression over right expression */

%token L_THAN       /* Less than comparison */
%token G_THAN       /* Greater than comparison */
%token L_THAN_EQ    /* Less than or equal to comparison */
%token G_THAN_EQ    /* Greater than or equal to comparison */
%token EQ           /* Equal comparison */
%token NOT_EQ       /* Not equal comparison */

%token CONCAT       /* String concatonation */
%token SUBSTRING    /* Substring operator */
%token LENGTH       /* Returns the length of a string */

%token AND          /* 'and' boolean operator */
%token OR           /* Inclusive 'or' boolean operator */
%token XOR          /* Exclusive 'or' boolean operator */
%token NOT          /* Boolean negation */
%token NAND         /* Inverse 'and' boolean operatorr */
%token NOR          /* Inverse inclusive 'or' boolean operator */
%token NXOR         /* Inverse exclusive 'or' boolean operator */

%token IF           /* Start of an if statement */
%token ELSE         /* Start of an else statement */

%token WHILE        /* Condition for while and do while loops */
%token DO           /* Used to specify start of a do while loop */

%token SEPERATOR    /* Seperator for lists of expressions */
%token EOE          /* End of expression */
%token EOF          /* End of file*/

%right RETURN       /* Lowest precedence */
%right ASSIGN

%left CAST

%left AND
%left NAND
%left OR
%left XOR
%left NOR
%left NXOR
%nonassoc NOT

%left L_THAN
%left G_THAN
%left L_THAN_EQ
%left G_THAN_EQ
%left NOT_EQ
%left EQ

%left PLUS
%left MINUS
%left MULTIPLY 
%left DIVIDE
%nonassoc NEGATE

%left CONCAT       /* Highest precedence */

%start start
%type <ParseTreeType.parseTree> start

%%

start:
    | function_list EOF                { ParseTree_Functions($1) }
    | EOF                              { ParseTree_Empty }
;

function_list:
    | function_definition               { Function_List_Def($1) }
    | let_statement function_list       { Function_List_Let($1, $2) }
    | new_statement function_list       { Function_List_New($1, $2) }
    | function_definition function_list { Function_List_List($1, $2) }
;

statement_list:
    | statement                         { Statement_List_Statement($1) }
    | statement statement_list          { Statement_List_List($1, $2) }
;

scoped_statement_list:
    | OPBRACE statement_list CLBRACE    { $2 }
    | OPBRACE CLBRACE                   { Statement_List_Empty }

statement:
    | exp EOE                           { Statement_Expression($1) }
    | while_loop                        { Statement_While($1) }
    | if_statement                      { Statement_If($1) }
    | function_definition               { Statement_Function($1) }
    | return_statement EOE              { Statement_Return($1) }
    | let_statement statement           { Statement_Let($1, $2) }
    | new_statement statement           { Statement_New($1, $2) }
;

exp:
    | exp_int                           { Expression_Int($1) }
    | exp_float                         { Expression_Float($1) }
    | exp_bool                          { Expression_Bool($1) }
    | exp_string                        { Expression_String($1) }
    | io_operation                      { Expression_IO($1) }
    | exp_identifier                    { Expression_Identifier($1) }
;

while_loop:
    | WHILE exp_identifier scoped_statement_list        { While_Loop_While(Expression_Identifier_To_Bool($2), $3) }
    | WHILE exp_bool scoped_statement_list              { While_Loop_While($2, $3) }
    | DO scoped_statement_list WHILE exp_bool EOE       { While_Loop_Do($2, $4) }
    | DO scoped_statement_list WHILE exp_identifier EOE { While_Loop_Do($2, Expression_Identifier_To_Bool($4)) }
;

if_statement:
    | IF exp_bool scoped_statement_list                             { If_Statement_If_Bool($2, $3) }
    | IF exp_bool scoped_statement_list ELSE scoped_statement_list  { If_Statement_Else_Bool($2, $3, $5) }
    | IF exp_bool scoped_statement_list ELSE if_statement           { If_Statement_Else_List_Bool($2, $3, $5) }
    
    | IF exp_identifier scoped_statement_list                             { If_Statement_If_Identifier($2, $3) }
    | IF exp_identifier scoped_statement_list ELSE scoped_statement_list  { If_Statement_Else_Identifier($2, $3, $5) }
    | IF exp_identifier scoped_statement_list ELSE if_statement           { If_Statement_Else_List_Identifier($2, $3, $5) }
;

let_statement:
    | LET INT_TYPENAME IDENTIFIER ASSIGN exp_int IN         { Let_Statement_Int(Identifier_Declaration(Int, $3), $5) }
    | LET FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float IN     { Let_Statement_Float(Identifier_Declaration(Float, $3), $5) }
    | LET FLOAT_TYPENAME IDENTIFIER ASSIGN exp_int IN       { Let_Statement_Float(Identifier_Declaration(Float, $3), Expression_Int_To_Float($5)) }
    | LET BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool IN       { Let_Statement_Bool(Identifier_Declaration(Bool, $3), $5) }
    | LET STRING_TYPENAME IDENTIFIER ASSIGN exp_string IN   { Let_Statement_String(Identifier_Declaration(String, $3), $5) }
    
    | LET INT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN      { Let_Statement_Identifier(Identifier_Declaration(Int, $3), $5) }
    | LET FLOAT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN    { Let_Statement_Identifier(Identifier_Declaration(Float, $3), $5) }
    | LET BOOL_TYPENAME IDENTIFIER ASSIGN exp_identifier IN     { Let_Statement_Identifier(Identifier_Declaration(Bool, $3), $5) }
    | LET STRING_TYPENAME IDENTIFIER ASSIGN exp_identifier IN   { Let_Statement_Identifier(Identifier_Declaration(String, $3), $5) }
;

new_statement:
    | NEW INT_TYPENAME IDENTIFIER ASSIGN exp_int IN         { New_Statement_Int(Identifier_Declaration(Int, $3), $5) }
    | NEW FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float IN     { New_Statement_Float(Identifier_Declaration(Float, $3), $5) }
    | NEW FLOAT_TYPENAME IDENTIFIER ASSIGN exp_int IN       { New_Statement_Float(Identifier_Declaration(Float, $3), Expression_Int_To_Float($5)) }
    | NEW BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool IN       { New_Statement_Bool(Identifier_Declaration(Bool, $3), $5) }
    | NEW STRING_TYPENAME IDENTIFIER ASSIGN exp_string IN   { New_Statement_String(Identifier_Declaration(String, $3), $5) }
    
    | NEW INT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN      { New_Statement_Identifier(Identifier_Declaration(Int, $3), $5) }
    | NEW FLOAT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN    { New_Statement_Identifier(Identifier_Declaration(Float, $3), $5) }
    | NEW BOOL_TYPENAME IDENTIFIER ASSIGN exp_identifier IN     { New_Statement_Identifier(Identifier_Declaration(Bool, $3), $5) }
    | NEW STRING_TYPENAME IDENTIFIER ASSIGN exp_identifier IN   { New_Statement_Identifier(Identifier_Declaration(String, $3), $5) }
;

function_definition:
    | INT_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list      { Function_Definition(Identifier_Declaration(Int, $2), $3, $4) }
    | FLOAT_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list    { Function_Definition(Identifier_Declaration(Float, $2), $3, $4) }
    | BOOL_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list     { Function_Definition(Identifier_Declaration(Bool, $2), $3, $4) }
    | STRING_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list   { Function_Definition(Identifier_Declaration(String, $2), $3, $4) }
;

bracketed_arg_list:
    | OPBRACKET CLBRACKET               { Argument_List_Empty }
    | OPBRACKET arg_list CLBRACKET      { $2 }
;

arg_list:
    | INT_TYPENAME IDENTIFIER               { Argument_List_Element(Identifier_Declaration(Int, $2)) }
    | FLOAT_TYPENAME IDENTIFIER             { Argument_List_Element(Identifier_Declaration(Float, $2)) }
    | BOOL_TYPENAME IDENTIFIER              { Argument_List_Element(Identifier_Declaration(Bool, $2)) }
    | STRING_TYPENAME IDENTIFIER            { Argument_List_Element(Identifier_Declaration(String, $2)) }
    | INT_TYPENAME IDENTIFIER SEPERATOR arg_list    { Argument_List_List(Identifier_Declaration(Int, $2), $4) }
    | FLOAT_TYPENAME IDENTIFIER SEPERATOR arg_list  { Argument_List_List(Identifier_Declaration(Float, $2), $4) }
    | BOOL_TYPENAME IDENTIFIER SEPERATOR arg_list   { Argument_List_List(Identifier_Declaration(Bool, $2), $4) }
    | STRING_TYPENAME IDENTIFIER SEPERATOR arg_list { Argument_List_List(Identifier_Declaration(String, $2), $4) }
;

bracketed_param_list:
    | OPBRACKET CLBRACKET               { Parameter_List_Empty }
    | OPBRACKET param_list CLBRACKET    { $2 }
;

param_list:
    | exp SEPERATOR param_list    { Parameter_List_List($1, $3) }
    | exp                       { Parameter_List_Element($1) }
;

return_statement:
    | RETURN exp_int                    { Return_Int($2) }
    | RETURN exp_float                  { Return_Float($2) }
    | RETURN exp_bool                   { Return_Bool($2) }
    | RETURN exp_string                 { Return_String($2) }
    | RETURN exp_identifier             { Return_Identifier($2) }
;

io_operation:
    | PRINT_INT OPBRACKET exp_int CLBRACKET         { IO_Operation_Print_Int($3) }
    | PRINT_FLOAT OPBRACKET exp_float CLBRACKET     { IO_Operation_Print_Float($3) }
    | PRINT_BOOL OPBRACKET exp_bool CLBRACKET       { IO_Operation_Print_Bool($3) }
    | PRINT_STRING OPBRACKET exp_string CLBRACKET   { IO_Operation_Print_String($3) }
    
    | PRINT_INT OPBRACKET exp_identifier CLBRACKET      { IO_Operation_Print_Int_Identifier($3) }
    | PRINT_FLOAT OPBRACKET exp_identifier CLBRACKET    { IO_Operation_Print_Float_Identifier($3) }
    | PRINT_BOOL OPBRACKET exp_identifier CLBRACKET     { IO_Operation_Print_Bool_Identifier($3) }
    | PRINT_STRING OPBRACKET exp_identifier CLBRACKET   { IO_Operation_Print_String_Identifier($3) }
;

exp_int:
    | OPBRACKET exp_int CLBRACKET               { $2 }
    | INT_LITERAL                               { Expression_Int_Literal($1) }
    | operation_int                             { Expression_Int_Operation($1) }
    | READ_INT OPBRACKET CLBRACKET              { Expression_Int_Read }
    | INT_TYPENAME IDENTIFIER ASSIGN exp_int    { Expression_Int_Declare(Identifier_Declaration(Int, $2), $4) }
    | IDENTIFIER ASSIGN exp_int                 { Expression_Int_Assign(Identifier_Reference($1), $3) }
    
    | exp_int CAST INT_TYPENAME                 { $1 }
    | exp_float CAST INT_TYPENAME               { Expression_Float_To_Int($1) }
    | exp_bool CAST INT_TYPENAME                { Expression_Bool_To_Int($1) }
    | exp_string CAST INT_TYPENAME              { Expression_String_To_Int($1) }
    | exp_identifier CAST INT_TYPENAME          { Expression_Identifier_To_Int($1) }
;

exp_float:
    | OPBRACKET exp_float CLBRACKET     { $2 }
    | FLOAT_LITERAL                     { Expression_Float_Literal($1) }
    | operation_float                   { Expression_Float_Operation($1) }
    | READ_FLOAT OPBRACKET CLBRACKET    { Expression_Float_Read }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float    { Expression_Float_Declare(Identifier_Declaration(Float, $2), $4) }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_int      { Expression_Float_Declare(Identifier_Declaration(Float, $2), Expression_Int_To_Float($4)) }
    | IDENTIFIER ASSIGN exp_float       { Expression_Float_Assign(Identifier_Reference($1), $3) }
    
    | exp_float CAST FLOAT_TYPENAME     { $1 }
    | exp_int CAST FLOAT_TYPENAME       { Expression_Int_To_Float($1) }
    | exp_bool CAST FLOAT_TYPENAME      { Expression_Bool_To_Float($1) }
    | exp_string CAST FLOAT_TYPENAME    { Expression_String_To_Float($1) }
    | exp_identifier CAST FLOAT_TYPENAME { Expression_Identifier_To_Float($1) }
;

exp_bool:
    | OPBRACKET exp_bool CLBRACKET      { $2 }
    | BOOL_LITERAL                      { Expression_Bool_Literal($1) }
    | operation_bool                    { Expression_Bool_Operation($1) }
    | READ_BOOL OPBRACKET CLBRACKET     { Expression_Bool_Read }
    | BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool      { Expression_Bool_Declare(Identifier_Declaration(Bool, $2), $4) }
    | IDENTIFIER ASSIGN exp_bool        { Expression_Bool_Assign(Identifier_Reference($1), $3) }
    
    | exp_bool CAST BOOL_TYPENAME       { $1 }
    | exp_int CAST BOOL_TYPENAME        { Expression_Int_To_Bool($1) }
    | exp_float CAST BOOL_TYPENAME      { Expression_Float_To_Bool($1) }
    | exp_string CAST BOOL_TYPENAME     { Expression_String_To_Bool($1) }
    | exp_identifier CAST BOOL_TYPENAME { Expression_Identifier_To_Bool($1) }
;

exp_string:
    | OPBRACKET exp_string CLBRACKET    { $2 }
    | STRING_LITERAL                    { Expression_String_Literal($1) }
    | operation_string                  { Expression_String_Operation($1) }
    | READ_STRING OPBRACKET CLBRACKET   { Expression_String_Read }
    | STRING_TYPENAME IDENTIFIER ASSIGN exp_string  { Expression_String_Declare(Identifier_Declaration(String, $2), $4) }
    | IDENTIFIER ASSIGN exp_string      { Expression_String_Assign(Identifier_Reference($1), $3) }
    
    | exp_string CAST STRING_TYPENAME   { $1 }
    | exp_int CAST STRING_TYPENAME      { Expression_Int_To_String($1) }
    | exp_float CAST STRING_TYPENAME    { Expression_Float_To_String($1) }
    | exp_bool CAST STRING_TYPENAME     { Expression_Bool_To_String($1) }
    | exp_identifier CAST STRING_TYPENAME { Expression_Identifier_To_String($1) }
;

exp_identifier:
    | OPBRACKET exp_identifier CLBRACKET    { $2 }
    | IDENTIFIER                            { Expression_Identifier_Dereference(Identifier_Reference($1)) }
    | operation_identifier                  { Expression_Identifier_Operation($1) }
    | IDENTIFIER bracketed_param_list       { Expression_Identifier_Function_Call(Identifier_Reference($1), $2) }
    
    | INT_TYPENAME IDENTIFIER ASSIGN exp_identifier     { Expression_Identifier_Declare_Int(Identifier_Declaration(Int, $2), $4) }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_identifier   { Expression_Identifier_Declare_Float(Identifier_Declaration(Float, $2), $4) }
    | BOOL_TYPENAME IDENTIFIER ASSIGN exp_identifier    { Expression_Identifier_Declare_Bool(Identifier_Declaration(Bool, $2), $4) }
    | STRING_TYPENAME IDENTIFIER ASSIGN exp_identifier  { Expression_Identifier_Declare_String(Identifier_Declaration(String, $2), $4) }
    | IDENTIFIER ASSIGN exp_identifier                  { Expression_Identifier_Assign(Identifier_Reference($1), $3) }
;

operation_int:
    | exp_int PLUS exp_int              { Operation_Int_Plus_Int($1, $3) }
    | exp_int MINUS exp_int             { Operation_Int_Minus_Int($1, $3) }
    | exp_int MULTIPLY exp_int          { Operation_Int_Multiply_Int($1, $3) }
    | exp_int DIVIDE exp_int            { Operation_Int_Divide_Int($1, $3) }
    | MINUS exp_int %prec NEGATE        { Operation_Negate_Int($2) }
    
    | exp_int PLUS exp_identifier           { Operation_Int_Plus_Identifier($1, $3) }
    | exp_int MINUS exp_identifier          { Operation_Int_Minus_Identifier($1, $3) }
    | exp_int MULTIPLY exp_identifier       { Operation_Int_Multiply_Identifier($1, $3) }
    | exp_int DIVIDE exp_identifier         { Operation_Int_Divide_Identifier($1, $3) }
    
    | exp_identifier PLUS exp_int           { Operation_Identifier_Plus_Int($1, $3) }
    | exp_identifier MINUS exp_int          { Operation_Identifier_Minus_Int($1, $3) }
    | exp_identifier MULTIPLY exp_int       { Operation_Identifier_Multiply_Int($1, $3) }
    | exp_identifier DIVIDE exp_int         { Operation_Identifier_Divide_Int($1, $3) }
    
    | LENGTH OPBRACKET exp_string CLBRACKET  { Operation_String_Length($3) }
;

operation_float:
    | exp_float PLUS exp_float          { Operation_Float_Plus_Float($1, $3) }
    | exp_float MINUS exp_float         { Operation_Float_Minus_Float($1, $3) }
    | exp_float MULTIPLY exp_float      { Operation_Float_Multiply_Float($1, $3) }
    | exp_float DIVIDE exp_float        { Operation_Float_Divide_Float($1, $3) }
    | MINUS exp_float %prec NEGATE      { Operation_Negate_Float($2) }
    
    | exp_float PLUS exp_int            { Operation_Float_Plus_Float($1, Expression_Int_To_Float($3)) }
    | exp_float MINUS exp_int           { Operation_Float_Minus_Float($1, Expression_Int_To_Float($3)) }
    | exp_float MULTIPLY exp_int        { Operation_Float_Multiply_Float($1, Expression_Int_To_Float($3)) }
    | exp_float DIVIDE exp_int          { Operation_Float_Divide_Float($1, Expression_Int_To_Float($3)) }
    
    | exp_int PLUS exp_float            { Operation_Float_Plus_Float(Expression_Int_To_Float($1), $3) }
    | exp_int MINUS exp_float           { Operation_Float_Minus_Float(Expression_Int_To_Float($1), $3) }
    | exp_int MULTIPLY exp_float        { Operation_Float_Multiply_Float(Expression_Int_To_Float($1), $3) }
    | exp_int DIVIDE exp_float          { Operation_Float_Divide_Float(Expression_Int_To_Float($1), $3) }
    
    | exp_float PLUS exp_identifier     { Operation_Float_Plus_Identifier($1, $3) }
    | exp_float MINUS exp_identifier    { Operation_Float_Minus_Identifier($1, $3) }
    | exp_float MULTIPLY exp_identifier { Operation_Float_Multiply_Identifier($1, $3) }
    | exp_float DIVIDE exp_identifier   { Operation_Float_Divide_Identifier($1, $3) }
    
    | exp_identifier PLUS exp_float     { Operation_Identifier_Plus_Float($1, $3) }
    | exp_identifier MINUS exp_float    { Operation_Identifier_Minus_Float($1, $3) }
    | exp_identifier MULTIPLY exp_float { Operation_Identifier_Multiply_Float($1, $3) }
    | exp_identifier DIVIDE exp_float   { Operation_Identifier_Divide_Float($1, $3) }
;

operation_bool:
    | exp_bool AND exp_bool             { Operation_Bool_And_Bool($1, $3) }
    | exp_bool NAND exp_bool            { Operation_Bool_Nand_Bool($1, $3) }
    | exp_bool OR exp_bool              { Operation_Bool_Or_Bool($1, $3) }
    | exp_bool XOR exp_bool             { Operation_Bool_Xor_Bool($1, $3) }
    | exp_bool NOR exp_bool             { Operation_Bool_Nor_Bool($1, $3) }
    | exp_bool NXOR exp_bool            { Operation_Bool_Nxor_Bool($1, $3) }
    | exp_bool NOT_EQ exp_bool          { Operation_Bool_Not_Eq_Bool($1, $3) }
    | exp_bool EQ exp_bool              { Operation_Bool_Eq_Bool($1, $3) }
    | NOT exp_bool                      { Operation_Not_Bool($2) }
    
    | exp_bool AND exp_identifier       { Operation_Bool_And_Identifier($1, $3) }
    | exp_bool NAND exp_identifier      { Operation_Bool_Nand_Identifier($1, $3) }
    | exp_bool OR exp_identifier        { Operation_Bool_Or_Identifier($1, $3) }
    | exp_bool XOR exp_identifier       { Operation_Bool_Xor_Identifier($1, $3) }
    | exp_bool NOR exp_identifier       { Operation_Bool_Nor_Identifier($1, $3) }
    | exp_bool NXOR exp_identifier      { Operation_Bool_Nxor_Identifier($1, $3) }
    | exp_bool NOT_EQ exp_identifier    { Operation_Bool_Not_Eq_Identifier($1, $3) }
    | exp_bool EQ exp_identifier        { Operation_Bool_Eq_Identifier($1, $3) }
    
    | exp_identifier AND exp_bool       { Operation_Identifier_And_Bool($1, $3) }
    | exp_identifier NAND exp_bool      { Operation_Identifier_Nand_Bool($1, $3) }
    | exp_identifier OR exp_bool        { Operation_Identifier_Or_Bool($1, $3) }
    | exp_identifier XOR exp_bool       { Operation_Identifier_Xor_Bool($1, $3) }
    | exp_identifier NOR exp_bool       { Operation_Identifier_Nor_Bool($1, $3) }
    | exp_identifier NXOR exp_bool      { Operation_Identifier_Nxor_Bool($1, $3) }
    | exp_identifier NOT_EQ exp_bool    { Operation_Identifier_Not_Eq_Bool($1, $3) }
    | exp_identifier EQ exp_bool        { Operation_Identifier_Eq_Bool($1, $3) }
    

    | exp_int L_THAN exp_int            { Operation_Int_Less_Than_Int($1, $3) }
    | exp_int G_THAN exp_int            { Operation_Int_Greater_Than_Int($1, $3) }
    | exp_int L_THAN_EQ exp_int         { Operation_Int_Less_Than_Or_Eq_Int($1, $3) }
    | exp_int G_THAN_EQ exp_int         { Operation_Int_Greater_Than_Or_Eq_Int($1, $3) }
    | exp_int NOT_EQ exp_int            { Operation_Int_Not_Eq_Int($1, $3) }
    | exp_int EQ exp_int                { Operation_Int_Eq_Int($1, $3) }
    
    | exp_float L_THAN exp_float        { Operation_Float_Less_Than_Float($1, $3) }
    | exp_float G_THAN exp_float        { Operation_Float_Greater_Than_Float($1, $3) }
    | exp_float L_THAN_EQ exp_float     { Operation_Float_Less_Than_Or_Eq_Float($1, $3) }
    | exp_float G_THAN_EQ exp_float     { Operation_Float_Greater_Than_Or_Eq_Float($1, $3) }
    | exp_float NOT_EQ exp_float        { Operation_Float_Not_Eq_Float($1, $3) }
    | exp_float EQ exp_float            { Operation_Float_Eq_Float($1, $3) }
    
    | exp_float G_THAN exp_int          { Operation_Float_Greater_Than_Float($1, Expression_Int_To_Float($3)) }
    | exp_float L_THAN exp_int          { Operation_Float_Less_Than_Float($1, Expression_Int_To_Float($3)) }
    | exp_float G_THAN_EQ exp_int       { Operation_Float_Greater_Than_Or_Eq_Float($1, Expression_Int_To_Float($3)) }
    | exp_float L_THAN_EQ exp_int       { Operation_Float_Less_Than_Or_Eq_Float($1, Expression_Int_To_Float($3)) }
    | exp_float NOT_EQ exp_int          { Operation_Float_Not_Eq_Float($1, Expression_Int_To_Float($3)) }
    | exp_float EQ exp_int              { Operation_Float_Eq_Float($1, Expression_Int_To_Float($3)) }
    
    | exp_int G_THAN exp_float          { Operation_Float_Greater_Than_Float(Expression_Int_To_Float($1), $3) }
    | exp_int L_THAN exp_float          { Operation_Float_Less_Than_Float(Expression_Int_To_Float($1), $3) }
    | exp_int G_THAN_EQ exp_float       { Operation_Float_Greater_Than_Or_Eq_Float(Expression_Int_To_Float($1), $3) }
    | exp_int L_THAN_EQ exp_float       { Operation_Float_Less_Than_Or_Eq_Float(Expression_Int_To_Float($1), $3)}
    | exp_int NOT_EQ exp_float          { Operation_Float_Not_Eq_Float(Expression_Int_To_Float($1), $3) }
    | exp_int EQ exp_float              { Operation_Float_Eq_Float(Expression_Int_To_Float($1), $3) }
    
    
    | exp_int G_THAN exp_identifier     { Operation_Int_Greater_Than_Identifier($1, $3) }
    | exp_int L_THAN exp_identifier     { Operation_Int_Less_Than_Identifier($1, $3) }
    | exp_int G_THAN_EQ exp_identifier  { Operation_Int_Greater_Than_Or_Eq_Identifier($1, $3) }
    | exp_int L_THAN_EQ exp_identifier  { Operation_Int_Less_Than_Or_Eq_Identifier($1, $3) }
    | exp_int NOT_EQ exp_identifier     { Operation_Int_Not_Eq_Identifier($1, $3) }
    | exp_int EQ exp_identifier         { Operation_Int_Eq_Identifier($1, $3) }
    
    | exp_identifier G_THAN exp_int     { Operation_Identifier_Greater_Than_Int($1, $3) }
    | exp_identifier L_THAN exp_int     { Operation_Identifier_Less_Than_Int($1, $3) }
    | exp_identifier G_THAN_EQ exp_int  { Operation_Identifier_Greater_Than_Or_Eq_Int($1, $3) }
    | exp_identifier L_THAN_EQ exp_int  { Operation_Identifier_Less_Than_Or_Eq_Int($1, $3) }
    | exp_identifier NOT_EQ exp_int     { Operation_Identifier_Not_Eq_Int($1, $3) }
    | exp_identifier EQ exp_int         { Operation_Identifier_Eq_Int($1, $3) }
    
    | exp_float G_THAN exp_identifier      { Operation_Float_Greater_Than_Identifier($1, $3) }
    | exp_float L_THAN exp_identifier      { Operation_Float_Less_Than_Identifier($1, $3) }
    | exp_float G_THAN_EQ exp_identifier   { Operation_Float_Greater_Than_Or_Eq_Identifier($1, $3) }
    | exp_float L_THAN_EQ exp_identifier   { Operation_Float_Less_Than_Or_Eq_Identifier($1, $3) }
    | exp_float NOT_EQ exp_identifier      { Operation_Float_Not_Eq_Identifier($1, $3) }
    | exp_float EQ exp_identifier          { Operation_Float_Eq_Identifier($1, $3) }
    
    | exp_identifier G_THAN exp_float      { Operation_Identifier_Greater_Than_Float($1, $3) }
    | exp_identifier L_THAN exp_float      { Operation_Identifier_Less_Than_Float($1, $3) }
    | exp_identifier G_THAN_EQ exp_float   { Operation_Identifier_Greater_Than_Or_Eq_Float($1, $3) }
    | exp_identifier L_THAN_EQ exp_float   { Operation_Identifier_Less_Than_Or_Eq_Float($1, $3) }
    | exp_identifier NOT_EQ exp_float      { Operation_Identifier_Not_Eq_Float($1, $3) }
    | exp_identifier EQ exp_float          { Operation_Identifier_Eq_Float($1, $3) }
;

operation_string:
    | exp_string CONCAT exp_string      { Operation_String_Concat_String($1, $3) }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_int SEPERATOR exp_int CLBRACKET  { Operation_Substring_String_Int_Int($3, $5, $7) }
    
    | exp_identifier CONCAT exp_string  { Operation_Identifier_Concat_String($1, $3) }
    | exp_string CONCAT exp_identifier  { Operation_String_Concat_Identifier($1, $3) }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_identifier SEPERATOR exp_identifier CLBRACKET    { Operation_Substring_String_Identifier_Identifier($3, $5, $7) }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_identifier SEPERATOR exp_int CLBRACKET           { Operation_Substring_String_Identifier_Int($3, $5, $7) }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_int SEPERATOR exp_identifier CLBRACKET           { Operation_Substring_String_Int_Identifier($3, $5, $7) }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_identifier SEPERATOR exp_int CLBRACKET       { Operation_Substring_Identifier_Identifier_Int($3, $5, $7) }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_int SEPERATOR exp_identifier CLBRACKET       { Operation_Substring_Identifier_Int_Identifier($3, $5, $7) }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_int SEPERATOR exp_int CLBRACKET              { Operation_Substring_Identifier_Int_Int($3, $5, $7) }
;

operation_identifier:
    | exp_identifier PLUS exp_identifier        { Operation_Identifier_Plus_Identifier($1, $3) }
    | exp_identifier MINUS exp_identifier       { Operation_Identifier_Minus_Identifier($1, $3) }
    | exp_identifier MULTIPLY exp_identifier    { Operation_Identifier_Multiply_Identifier($1, $3) }
    | exp_identifier DIVIDE exp_identifier      { Operation_Identifier_Divide_Identifier($1, $3) }
    | MINUS exp_identifier %prec NEGATE         { Operation_Negate_Identifier($2) }
    
    | exp_identifier AND exp_identifier         { Operation_Identifier_And_Identifier($1, $3) }
    | exp_identifier NAND exp_identifier        { Operation_Identifier_Nand_Identifier($1, $3) }
    | exp_identifier OR exp_identifier          { Operation_Identifier_Or_Identifier($1, $3) }
    | exp_identifier XOR exp_identifier         { Operation_Identifier_Xor_Identifier($1, $3) }
    | exp_identifier NOR exp_identifier         { Operation_Identifier_Nor_Identifier($1, $3) }
    | exp_identifier NXOR exp_identifier        { Operation_Identifier_Nxor_Identifier($1, $3) }
    | NOT exp_identifier                        { Operation_Not_Identifier($2) }
    
    | exp_identifier L_THAN exp_identifier      { Operation_Identifier_Less_Than_Identifier($1, $3) }
    | exp_identifier G_THAN exp_identifier      { Operation_Identifier_Greater_Than_Identifier($1, $3) }
    | exp_identifier L_THAN_EQ exp_identifier   { Operation_Identifier_Less_Than_Or_Eq_Identifier($1, $3) }
    | exp_identifier G_THAN_EQ exp_identifier   { Operation_Identifier_Greater_Than_Or_Eq_Identifier($1, $3) }
    | exp_identifier NOT_EQ exp_identifier      { Operation_Identifier_Not_Eq_Identifier($1, $3) }
    | exp_identifier EQ exp_identifier          { Operation_Identifier_Eq_Identifier($1, $3) }
    
    | exp_identifier CONCAT exp_identifier      { Operation_Identifier_Concat_Identifier($1, $3) }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_identifier SEPERATOR exp_identifier CLBRACKET    { Operation_Substring_Identifier_Identifier_Identifier($3, $5, $7) }
;