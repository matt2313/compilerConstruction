/*
TODO:

Read Int
Read String
Print String
Print Int

Assign Variable
    - This will mean making sure keywords have whitespace at the end
Evaluate Variable

Function Definition
Function Application

Change program to list of functions instead of list of commands

Type conversions

Float datatype
Read Float
Print Float
For Loop
IF-ELSE IF-ELSE... statements

Let ... = ... in ...
New ... = ... in ...
*/

%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL

%token OPBRACKET    /* Open bracket */
%token CLBRACKET    /* Close bracket */
%token OPBRACE      /* Open brace */
%token CLBRACE      /* Close brace */

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

%token EOE          /* End of expression */
%token EOF          /* End of file*/

%left AND           /* Lowest precedence */
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
%nonassoc NEGATE    /* Highest precedence */

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
;

exp:
    | exp_int                           { [string_of_int $1] }
    | exp_bool                          { [string_of_bool $1] }
;

while_loop:
    | WHILE exp_bool scoped_statement_list          { (string_of_bool $2)::$3 }
    | DO scoped_statement_list WHILE exp_bool EOE   { $2@[string_of_bool $4] }
;

if_statement:
    | IF exp_bool scoped_statement_list                             { (string_of_bool $2)::$3 }
    | IF exp_bool scoped_statement_list ELSE scoped_statement_list  { (string_of_bool $2)::$3@$5 }
;

exp_int:
    | OPBRACKET exp_int CLBRACKET       { $2 }
    | INT_LITERAL                       { $1 }
    | operation_int                     { $1 }
;

exp_bool:
    | OPBRACKET exp_bool CLBRACKET      { $2 }
    | BOOL_LITERAL                      { $1 }
    | operation_bool                    { $1 }
;

operation_int:
    | exp_int PLUS exp_int              { $1 + $3 }
    | exp_int MINUS exp_int             { $1 - $3 }
    | MINUS exp_int %prec NEGATE        { -$2 }
    | exp_int MULTIPLY exp_int          { $1 * $3 }
    | exp_int DIVIDE exp_int            { $1 / $3 }
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
;