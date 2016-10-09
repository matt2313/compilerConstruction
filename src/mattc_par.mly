%token <int> INT

%token PLUS     /* Addition of 2 expressions */

%token EOE      /* End of expression */
%token EOF      /* End of file*/

%left PLUS      /* Lowest precedence */

%start start
%type <int list> start

%%

start:
      exp_list EOF                      { $1 }
;

exp_list:
      exp                               { [$1] }
    | exp EOE exp_list                  { $1@$3 }
;

exp:
      INT                               { $1 }
    | operation                         { $1 }
;

operation:
    | exp PLUS exp                       { $1 + $3 }
;