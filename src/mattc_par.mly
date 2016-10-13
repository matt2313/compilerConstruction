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
%type <string list> start

%%

start:
    | function_list EOF                { $1 }
    | EOF                              { [""] }
;

function_list:
    | function_definition               { $1 }
    | let_statement function_list       { $1@$2 }
    | new_statement function_list       { $1@$2 }
    | function_definition function_list { $1@$2 }
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
    | let_statement statement           { $1@$2 }
    | new_statement statement           { $1@$2 }
;

exp:
    | exp_int                           { [string_of_int $1] }
    | exp_float                         { [string_of_float $1] }
    | exp_bool                          { [string_of_bool $1] }
    | exp_string                        { [$1] }
    | io_operation                      { $1 }
    | exp_identifier                    { [] }
;

while_loop:
    | WHILE exp_bool scoped_statement_list          { (string_of_bool $2)::$3 }
    | DO scoped_statement_list WHILE exp_bool EOE   { $2@[string_of_bool $4] }
;

if_statement:
    | IF exp_bool scoped_statement_list                             { (string_of_bool $2)::$3 }
    | IF exp_bool scoped_statement_list ELSE scoped_statement_list  { (string_of_bool $2)::$3@$5 }
    | IF exp_bool scoped_statement_list ELSE if_statement           { (string_of_bool $2)::$3@$5 }
;

let_statement:
    | LET INT_TYPENAME IDENTIFIER ASSIGN exp_int IN         { $3::[string_of_int $5] }
    | LET FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float IN     { $3::[string_of_float $5]}
    | LET FLOAT_TYPENAME IDENTIFIER ASSIGN exp_int IN       { $3::[string_of_int $5]}
    | LET BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool IN       { $3::[string_of_bool $5] }
    | LET STRING_TYPENAME IDENTIFIER ASSIGN exp_string IN   { $3::[$5] }
    
    | LET INT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN      { [$3] }
    | LET FLOAT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN    { [$3] }
    | LET BOOL_TYPENAME IDENTIFIER ASSIGN exp_identifier IN     { [$3] }
    | LET STRING_TYPENAME IDENTIFIER ASSIGN exp_identifier IN   { [$3] }
;

new_statement:
    | NEW INT_TYPENAME IDENTIFIER ASSIGN exp_int IN         { $3::[string_of_int $5] }
    | NEW FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float IN     { $3::[string_of_float $5] }
    | NEW FLOAT_TYPENAME IDENTIFIER ASSIGN exp_int IN       { $3::[string_of_int $5] }
    | NEW BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool IN       { $3::[string_of_bool $5] }
    | NEW STRING_TYPENAME IDENTIFIER ASSIGN exp_string IN   { $3::[$5] }
    
    | NEW INT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN      { [$3] }
    | NEW FLOAT_TYPENAME IDENTIFIER ASSIGN exp_identifier IN    { [$3] }
    | NEW BOOL_TYPENAME IDENTIFIER ASSIGN exp_identifier IN     { [$3] }
    | NEW STRING_TYPENAME IDENTIFIER ASSIGN exp_identifier IN   { [$3] }
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
    | RETURN exp_identifier             { [] }
;

io_operation:
    | PRINT_INT OPBRACKET exp_int CLBRACKET         { [string_of_int $3] }
    | PRINT_FLOAT OPBRACKET exp_float CLBRACKET     { [string_of_float $3] }
    | PRINT_BOOL OPBRACKET exp_bool CLBRACKET       { [if $3 then "true" else "false"] }
    | PRINT_STRING OPBRACKET exp_string CLBRACKET   { [$3] }
    
    | PRINT_INT OPBRACKET exp_identifier CLBRACKET      { [] }
    | PRINT_FLOAT OPBRACKET exp_identifier CLBRACKET    { [] }
    | PRINT_BOOL OPBRACKET exp_identifier CLBRACKET     { [] }
    | PRINT_STRING OPBRACKET exp_identifier CLBRACKET   { [] }
;

exp_int:
    | OPBRACKET exp_int CLBRACKET               { $2 }
    | INT_LITERAL                               { $1 }
    | operation_int                             { $1 }
    | READ_INT OPBRACKET CLBRACKET              { 0 }
    | INT_TYPENAME IDENTIFIER ASSIGN exp_int    { $4 }
    | IDENTIFIER ASSIGN exp_int                 { $3 }
    
    | exp_int CAST INT_TYPENAME                 { $1 }
    | exp_float CAST INT_TYPENAME               { int_of_float $1 }
    | exp_bool CAST INT_TYPENAME                { if $1 then 1 else 0 }
    | exp_string CAST INT_TYPENAME              { int_of_string $1 }
;

exp_float:
    | OPBRACKET exp_float CLBRACKET     { $2 }
    | FLOAT_LITERAL                     { $1 }
    | operation_float                   { $1 }
    | READ_FLOAT OPBRACKET CLBRACKET    { 0.0 }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_float    { $4 }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_int      { float_of_int $4 }
    | IDENTIFIER ASSIGN exp_float       { $3 }
    
    | exp_float CAST FLOAT_TYPENAME     { $1 }
    | exp_int CAST FLOAT_TYPENAME       { float_of_int $1 }
    | exp_bool CAST FLOAT_TYPENAME      { if $1 then 1.0 else 0.0 }
    | exp_string CAST FLOAT_TYPENAME    { float_of_string $1 }
;

exp_bool:
    | OPBRACKET exp_bool CLBRACKET      { $2 }
    | BOOL_LITERAL                      { $1 }
    | operation_bool                    { $1 }
    | READ_BOOL OPBRACKET CLBRACKET     { false }
    | BOOL_TYPENAME IDENTIFIER ASSIGN exp_bool      { $4 }
    | IDENTIFIER ASSIGN exp_bool        { $3 }
    
    | exp_bool CAST BOOL_TYPENAME       { $1 }
    | exp_int CAST BOOL_TYPENAME        { if $1 = 1 then true else false }
    | exp_float CAST BOOL_TYPENAME      { if $1 = 1.0 then true else false }
    | exp_string CAST BOOL_TYPENAME     { if $1 = "true" then true else (if $1 = "false" then false else false) }
;

exp_string:
    | OPBRACKET exp_string CLBRACKET    { $2 }
    | STRING_LITERAL                    { $1 }
    | operation_string                  { $1 }
    | READ_STRING OPBRACKET CLBRACKET   { "input" }
    | STRING_TYPENAME IDENTIFIER ASSIGN exp_string  { $4 }
    | IDENTIFIER ASSIGN exp_string      { $3 }
    
    | exp_string CAST STRING_TYPENAME   { $1 }
    | exp_int CAST STRING_TYPENAME      { string_of_int $1 }
    | exp_float CAST STRING_TYPENAME    { string_of_float $1 }
    | exp_bool CAST STRING_TYPENAME     { string_of_bool $1 }
;

exp_identifier:
    | OPBRACKET exp_identifier CLBRACKET    { () }
    | IDENTIFIER                            { () }
    | IDENTIFIER bracketed_param_list       { () }
    | operation_identifier                  { () }
    
    | INT_TYPENAME IDENTIFIER ASSIGN exp_identifier     { () }
    | FLOAT_TYPENAME IDENTIFIER ASSIGN exp_identifier   { () }
    | BOOL_TYPENAME IDENTIFIER ASSIGN exp_identifier    { () }
    | STRING_TYPENAME IDENTIFIER ASSIGN exp_identifier  { () }
    | IDENTIFIER ASSIGN exp_identifier                  { () }
    
    | exp_identifier CAST BOOL_TYPENAME     { () }
    | exp_identifier CAST FLOAT_TYPENAME    { () }
    | exp_identifier CAST INT_TYPENAME      { () }
    | exp_identifier CAST STRING_TYPENAME   { () }
;

operation_int:
    | exp_int PLUS exp_int              { $1 + $3 }
    | exp_int MINUS exp_int             { $1 - $3 }
    | MINUS exp_int %prec NEGATE        { -$2 }
    | exp_int MULTIPLY exp_int          { $1 * $3 }
    | exp_int DIVIDE exp_int            { $1 / $3 }
    
    | exp_int PLUS exp_identifier           { $1 }
    | exp_int MINUS exp_identifier          { $1 }
    | exp_int MULTIPLY exp_identifier       { $1 }
    | exp_int DIVIDE exp_identifier         { $1 }
    
    | exp_identifier PLUS exp_int           { $3 }
    | exp_identifier MINUS exp_int          { $3 }
    | exp_identifier MULTIPLY exp_int       { $3 }
    | exp_identifier DIVIDE exp_int         { $3 }
    
    | LENGTH OPBRACKET exp_string CLBRACKET  { String.length $3 }
;

operation_float:
    | exp_float PLUS exp_float          { $1 +. $3 }
    | exp_float MINUS exp_float         { $1 -. $3 }
    | MINUS exp_float %prec NEGATE      { -.$2 }
    | exp_float MULTIPLY exp_float      { $1 *. $3 }
    | exp_float DIVIDE exp_float        { $1 /. $3 }
    
    | exp_float PLUS exp_int            { $1 +. float_of_int $3 }
    | exp_float MINUS exp_int           { $1 -. float_of_int $3 }
    | exp_float MULTIPLY exp_int        { $1 *. float_of_int $3 }
    | exp_float DIVIDE exp_int          { $1 /. float_of_int $3 }
    
    | exp_int PLUS exp_float            { float_of_int $1 +. $3 }
    | exp_int MINUS exp_float           { float_of_int $1 -. $3 }
    | exp_int MULTIPLY exp_float        { float_of_int $1 *. $3 }
    | exp_int DIVIDE exp_float          { float_of_int $1 /. $3 }
    
    | exp_float PLUS exp_identifier         { $1 }
    | exp_float MINUS exp_identifier        { $1 }
    | exp_float MULTIPLY exp_identifier     { $1 }
    | exp_float DIVIDE exp_identifier       { $1 }
    
    | exp_identifier PLUS exp_float         { $3 }
    | exp_identifier MINUS exp_float        { $3 }
    | exp_identifier MULTIPLY exp_float     { $3 }
    | exp_identifier DIVIDE exp_float       { $3 }
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
    
    | exp_bool AND exp_identifier      { $1 }
    | exp_bool NAND exp_identifier     { $1 }
    | exp_bool OR exp_identifier       { $1 }
    | exp_bool XOR exp_identifier      { $1 }
    | exp_bool NOR exp_identifier      { $1 }
    | exp_bool NXOR exp_identifier     { $1 }
    | exp_bool NOT_EQ exp_identifier   { $1 }
    | exp_bool EQ exp_identifier       { $1 }
    
    | exp_identifier AND exp_bool      { $3 }
    | exp_identifier NAND exp_bool     { $3 }
    | exp_identifier OR exp_bool       { $3 }
    | exp_identifier XOR exp_bool      { $3 }
    | exp_identifier NOR exp_bool      { $3 }
    | exp_identifier NXOR exp_bool     { $3 }
    | exp_identifier NOT_EQ exp_bool   { $3 }
    | exp_identifier EQ exp_bool       { $3 }
    

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
    
    | exp_float L_THAN exp_int          { $1 < float_of_int $3 }
    | exp_float G_THAN exp_int          { $1 > float_of_int $3 }
    | exp_float L_THAN_EQ exp_int       { $1 <= float_of_int $3 }
    | exp_float G_THAN_EQ exp_int       { $1 >= float_of_int $3 }
    | exp_float NOT_EQ exp_int          { not ($1 = float_of_int $3) }
    | exp_float EQ exp_int              { $1 = float_of_int $3 }
    
    | exp_int L_THAN exp_float          { float_of_int $1 < $3 }
    | exp_int G_THAN exp_float          { float_of_int $1 > $3 }
    | exp_int L_THAN_EQ exp_float       { float_of_int $1 <= $3 }
    | exp_int G_THAN_EQ exp_float       { float_of_int $1 >= $3 }
    | exp_int NOT_EQ exp_float          { not (float_of_int $1 = $3) }
    | exp_int EQ exp_float              { float_of_int $1 = $3 }
    
    | exp_int L_THAN exp_identifier    { false }
    | exp_int G_THAN exp_identifier    { false }
    | exp_int L_THAN_EQ exp_identifier { false }
    | exp_int G_THAN_EQ exp_identifier { false }
    | exp_int NOT_EQ exp_identifier    { false }
    | exp_int EQ exp_identifier        { false }
    
    | exp_identifier L_THAN exp_int    { false }
    | exp_identifier G_THAN exp_int    { false }
    | exp_identifier L_THAN_EQ exp_int { false }
    | exp_identifier G_THAN_EQ exp_int { false }
    | exp_identifier NOT_EQ exp_int    { false }
    | exp_identifier EQ exp_int        { false }
    
    | exp_float L_THAN exp_identifier      { false }
    | exp_float G_THAN exp_identifier      { false }
    | exp_float L_THAN_EQ exp_identifier   { false }
    | exp_float G_THAN_EQ exp_identifier   { false }
    | exp_float NOT_EQ exp_identifier      { false }
    | exp_float EQ exp_identifier          { false }
    
    | exp_identifier L_THAN exp_float      { false }
    | exp_identifier G_THAN exp_float      { false }
    | exp_identifier L_THAN_EQ exp_float   { false }
    | exp_identifier G_THAN_EQ exp_float   { false }
    | exp_identifier NOT_EQ exp_float      { false }
    | exp_identifier EQ exp_float          { false }
;

operation_string:
    | exp_string CONCAT exp_string      { $1 ^ $3 }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_int SEPERATOR exp_int CLBRACKET  { String.sub $3 $5 $7 }
    
    | exp_identifier CONCAT exp_string { $3 }
    | exp_string CONCAT exp_identifier { $1 }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_identifier SEPERATOR exp_identifier CLBRACKET      { $3 }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_identifier SEPERATOR exp_int CLBRACKET              { $3 }
    | SUBSTRING OPBRACKET exp_string SEPERATOR exp_int SEPERATOR exp_identifier CLBRACKET              { $3 }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_identifier SEPERATOR exp_int CLBRACKET         { "" }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_int SEPERATOR exp_identifier CLBRACKET         { "" }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_int SEPERATOR exp_int CLBRACKET                 { "" }
;

operation_identifier:
    | exp_identifier PLUS exp_identifier        { () }
    | exp_identifier MINUS exp_identifier       { () }
    | MINUS exp_identifier %prec NEGATE         { () }
    | exp_identifier MULTIPLY exp_identifier    { () }
    | exp_identifier DIVIDE exp_identifier      { () }
    
    | exp_identifier AND exp_identifier         { () }
    | exp_identifier NAND exp_identifier        { () }
    | exp_identifier OR exp_identifier          { () }
    | exp_identifier XOR exp_identifier         { () }
    | exp_identifier NOR exp_identifier         { () }
    | exp_identifier NXOR exp_identifier        { () }
    | NOT exp_identifier                        { () }
    
    | exp_identifier L_THAN exp_identifier      { () }
    | exp_identifier G_THAN exp_identifier      { () }
    | exp_identifier L_THAN_EQ exp_identifier   { () }
    | exp_identifier G_THAN_EQ exp_identifier   { () }
    | exp_identifier NOT_EQ exp_identifier      { () }
    | exp_identifier EQ exp_identifier          { () }
    
    | exp_identifier CONCAT exp_identifier      { () }
    | SUBSTRING OPBRACKET exp_identifier SEPERATOR exp_identifier SEPERATOR exp_identifier CLBRACKET    { () }
;

bracketed_param_list:
    | OPBRACKET CLBRACKET               { 0.0 }
    | OPBRACKET param_list CLBRACKET    { 0.0 }
;

param_list:
    | exp                               { 0.0 }
    | exp SEPERATOR param_list          { 0.0 }
;