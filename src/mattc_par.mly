/*
TODO:

Change program to list of functions instead of list of commands

Substring

Type conversions

Char datatype
Read Char
Print Char

For Loop
IF ELSE-IF ... ELSE

Let ... = ... in ...
New ... = ... in ...
*/

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

%token OPBRACKET    /* Open bracket */
%token CLBRACKET    /* Close bracket */
%token OPBRACE      /* Open brace */
%token CLBRACE      /* Close brace */

%token READ_INT     /* Get int from terminal */
%token READ_STRING  /* Get string from terminal */
%token READ_FLOAT   /* Get float form terminal */
%token PRINT_INT    /* Print int to terminal */
%token PRINT_STRING /* Print string to terminal */
%token PRINT_FLOAT  /* Print float to terminal */

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
%type <string list> start

%%

start:
    | statement_list EOF                      { $1 }
    | EOF                               { [""] }
;

statement_list:
    | statement                         { $1 }
    | statement statement_list          { $1@$2 }
;

scoped_statement_list:
    | OPBRACE statement_list CLBRACE    { $2 }
    | OPBRACE CLBRACE                   { [] }

statement:
    | exp EOE                           { $1 }
    | while_loop                        { $1 }
    | if_statement                      { $1 }
    | function_definition               { $1 }
    | return_statement EOE              { $1 }
;

exp:
    | exp_int                           { [string_of_int $1] }
    | exp_float                         { [string_of_float $1] }
    | exp_bool                          { [string_of_bool $1] }
    | exp_string                        { [$1] }
    | io_operation                      { $1 }
;

while_loop:
    | WHILE exp_bool scoped_statement_list          { (string_of_bool $2)::$3 }
    | DO scoped_statement_list WHILE exp_bool EOE   { $2@[string_of_bool $4] }
;

if_statement:
    | IF exp_bool scoped_statement_list                             { (string_of_bool $2)::$3 }
    | IF exp_bool scoped_statement_list ELSE scoped_statement_list  { (string_of_bool $2)::$3@$5 }
;

function_definition:
    | INT_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list      { $2::$3@$4 }
    | FLOAT_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list    { $2::$3@$4 }
    | BOOL_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list     { $2::$3@$4 }
    | STRING_TYPENAME IDENTIFIER bracketed_arg_list scoped_statement_list   { $2::$3@$4 }
;

bracketed_arg_list:
    | OPBRACKET CLBRACKET               { ["noargs"] }
    | OPBRACKET arg_list CLBRACKET      { $2 }
;

arg_list:
    | INT_TYPENAME IDENTIFIER               { [$2] }
    | FLOAT_TYPENAME IDENTIFIER             { [$2] }
    | BOOL_TYPENAME IDENTIFIER              { [$2] }
    | STRING_TYPENAME IDENTIFIER            { [$2] }
    | INT_TYPENAME IDENTIFIER SEPERATOR arg_list    { $2::$4 }
    | FLOAT_TYPENAME IDENTIFIER SEPERATOR arg_list  { $2::$4 }
    | BOOL_TYPENAME IDENTIFIER SEPERATOR arg_list   { $2::$4 }
    | STRING_TYPENAME IDENTIFIER SEPERATOR arg_list { $2::$4 }
;

return_statement:
    | RETURN exp_int                    { [string_of_int $2] }
    | RETURN exp_float                  { [string_of_float $2] }
    | RETURN exp_bool                   { [string_of_bool $2] }
    | RETURN exp_string                 { [$2] }
;

io_operation:
    | PRINT_INT OPBRACKET exp_int CLBRACKET         { [string_of_int $3] }
    | PRINT_STRING OPBRACKET exp_string CLBRACKET   { [$3] }
;

exp_int:
    | OPBRACKET exp_int CLBRACKET            { $2 }
    | INT_LITERAL                            { $1 }
    | operation_int                          { $1 }
    | READ_INT OPBRACKET CLBRACKET           { 0 }
    | INT_TYPENAME IDENTIFIER ASSIGN exp_int { $4 }
    | IDENTIFIER ASSIGN exp_int              { $3 }
;

exp_float:
    | OPBRACKET exp_float CLBRACKET     { $2 }
    | FLOAT_LITERAL                     { $1 }
    | operation_float                   { $1 }
    | READ_FLOAT OPBRACKET CLBRACKET    { 0.0 }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float    { $4 }
    | IDENTIFIER ASSIGN exp_float       { $3 }
;

exp_bool:
    | OPBRACKET exp_bool CLBRACKET      { $2 }
    | BOOL_LITERAL                      { $1 }
    | operation_bool                    { $1 }
    | BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool      { $4 }
    | IDENTIFIER ASSIGN exp_bool        { $3 }
;

exp_string:
    | OPBRACKET exp_string CLBRACKET    { $2 }
    | STRING_LITERAL                    { $1 }
    | operation_string                  { $1 }
    | STRING_TYPENAME IDENTIFIER ASSIGN exp_string  { $4 }
    | IDENTIFIER ASSIGN exp_string      { $3 }
;

operation_int:
    | exp_int PLUS exp_int              { $1 + $3 }
    | exp_int MINUS exp_int             { $1 - $3 }
    | MINUS exp_int %prec NEGATE        { -$2 }
    | exp_int MULTIPLY exp_int          { $1 * $3 }
    | exp_int DIVIDE exp_int            { $1 / $3 }
;

operation_float:
    | exp_float PLUS exp_float          { $1 +. $3 }
    | exp_float MINUS exp_float         { $1 -. $3 }
    | MINUS exp_float %prec NEGATE      { -.$2 }
    | exp_float MULTIPLY exp_float      { $1 *. $3 }
    | exp_float DIVIDE exp_float        { $1 /. $3 }
    /* For now, assume all variables and functions are floats (since we can't actually check) */
    | IDENTIFIER                        { 0.0 }
    | IDENTIFIER bracketed_arg_list     { 0.0 }
;

operation_bool:
    | exp_bool AND exp_bool             { $1 && $3 }
    | exp_bool NAND exp_bool            { not ($1 && $3) }
    | exp_bool OR exp_bool              { $1 || $3 }
    | exp_bool XOR exp_bool             { ($1 && not $3) || ($3 && not $1) }
    | exp_bool NOR exp_bool             { not ($1 || $3) }
    | exp_bool NXOR exp_bool            { ($1 && $3) || (not $1 && not $3) }
    | NOT exp_bool                      { not $2 }
    | exp_bool NOT_EQ exp_bool          { not ($1 = $3) }
    | exp_bool EQ exp_bool              { $1 = $3 }

    | exp_int L_THAN exp_int            { $1 < $3 }
    | exp_int G_THAN exp_int            { $1 > $3 }
    | exp_int L_THAN_EQ exp_int         { $1 <= $3 }
    | exp_int G_THAN_EQ exp_int         { $1 >= $3 }
    | exp_int NOT_EQ exp_int            { not ($1 = $3) }
    | exp_int EQ exp_int                { $1 = $3 }
    
    | exp_float L_THAN exp_float        { $1 < $3 }
    | exp_float G_THAN exp_float        { $1 > $3 }
    | exp_float L_THAN_EQ exp_float     { $1 <= $3 }
    | exp_float G_THAN_EQ exp_float     { $1 >= $3 }
    | exp_float NOT_EQ exp_float        { not ($1 = $3) }
    | exp_float EQ exp_float            { $1 = $3 }
;

operation_string:
    | exp_string CONCAT exp_string      { $1 ^ $3 }
    | READ_STRING OPBRACKET CLBRACKET   { "" }
;