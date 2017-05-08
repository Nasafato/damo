/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI COMMA
%token LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE EXP LOG MOD 
%token NOT AND OR
%token ASSIGN
%token EQ NEQ LT LEQ GT GEQ 
%token IF ELSEIF ELSE FOR WHILE 
%token INT BOOL NUM STRING SYMBOL VOID
%token DEF RETURN COLON
%token TRUE FALSE
%token <string> STRING_LITERAL
%token <float> NUM_LITERAL
%token <int> INT_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELSEIF
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right EXP 
%left LOG
%right MOD
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  program_sequence EOF { List.rev $1 }

program_sequence:
  /* nothing */ { [] }
  | program_sequence vdecl { $2 @ $1 }
  | program_sequence fdecl { FuncUnit($2) :: $1 }
  | program_sequence stmt { StmtUnit($2) :: $1 }

fdecl:
   DEF ID LPAREN formals_opt RPAREN COLON typ LBRACE program_sequence RBRACE
    { 
      { 
        typ = $7;
        fname = $2;
        formals = $4;
        body = List.rev $9
      } 
    }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [Decl($1,$2)] }
  | formal_list COMMA typ ID { Decl($3,$4) :: $1 }

/*function_sequence:
   nothing  { [] }
  | function_sequence vdecl { $2 @ $1 }
  | function_sequence stmt { StmtFunit($2) :: $1 }
*/

typ:
    INT { Int }
  | NUM { Num }
  | BOOL { Bool }
  | STRING { String }
  | SYMBOL { Symbol }
  | VOID { Void }

vdecl:
    typ ID SEMI { [VarUnit(Decl($1, $2))] }
  | typ ID ASSIGN expr SEMI { [StmtUnit(Expr(Assign(Idl($2), $4))) ; VarUnit(Decl($1, $2))] }
  | typ ID LBRACKET brackets RBRACKET SEMI { [VarUnit(ArrDecl($1, $2, List.rev $4))] }

brackets:
    expr { [$1] }
  | brackets RBRACKET LBRACKET expr { $4 :: $1 } 

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt else_stmt    { If($3, $5, $6) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

else_stmt:
  ELSEIF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | ELSEIF LPAREN expr RPAREN stmt else_stmt { If($3, $5, $6) }
  | ELSEIF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) } 
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL      { IntLit($1) }
  | NUM_LITERAL      { NumLit($1) }
  | STRING_LITERAL   { StringLit($1) }   
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | arrid            { $1 }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EXP    expr { Binop($1, Exp, $3) }
  | expr LOG    expr { Binop($1, Log, $3) }
  | expr MOD    expr { Binop($1, Mod, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign(Idl($1), $3) }
  | larrid ASSIGN expr { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

larrid:
  ID LBRACKET brackets RBRACKET { ArrIdl($1, List.rev $3) }

arrid:
  ID LBRACKET brackets RBRACKET { ArrId($1, List.rev $3) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
