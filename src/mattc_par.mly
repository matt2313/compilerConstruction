/*
type expression =
  | While of expression * expression (* while e do e *)
  | If of expression * expression * expression (* if e do e else e *)
  | Asg of expression * expression (* e := e *)
  | Deref of expression (* !e *)
  | Application of expression * expression (* e(e) *)
  | Readint (* read_int () *)
  | Printint of expression (* print_int (e) *)
  | Identifier of string (* x *)
  | Let of string * expression * expression (* let x = e in e *)
  | New of string * expression * expression (* new x = e in e *)
type fundef = string * string list * expression
type program = fundef list
*/

%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL

%token OPBRACKET    /* Open bracket */
%token CLBRACKET    /* Close bracket */

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
    | exp_list EOF                      { $1 }
;

exp_list:
    | exp EOE                           { [$1] }
    | exp EOE exp_list                  { $1::$3 }
;

exp:
    | exp_int                           { string_of_int $1 }
    | exp_bool                          { string_of_bool $1 }
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