%token <int> INT

%token OPBRACKET    /* Open bracket */
%token CLBRACKET    /* Close bracket */

%token PLUS         /* Addition of 2 expressions */
%token MINUS        /* Substraction of 2 expressions */
%token MULTIPLY     /* Multiplication of 2 expressions */
%token DIVIDE       /* Division of left expression over right expression */

%token EOE          /* End of expression */
%token EOF          /* End of file*/

%left PLUS          /* Lowest precedence */
%left MINUS
%left MULTIPLY 
%left DIVIDE        /* Highest precedence */

%start start
%type <int list> start

%%

start:
    | exp_list EOF                      { $1 }
;

exp_list:
    | exp                               { [$1] }
    | exp EOE exp_list                  { $1::$3 }
;

exp:
    | OPBRACKET exp CLBRACKET           { $2 }
    | INT                               { $1 }
    | operation                         { $1 }
;

operation:
    | exp PLUS exp                      { $1 + $3 }
    | exp MINUS exp                     { $1 - $3 }
    | exp MULTIPLY exp                  { $1 * $3 }
    | exp DIVIDE exp                    { $1 / $3 }
;