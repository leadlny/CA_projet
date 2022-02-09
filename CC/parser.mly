%{
(* ========================================================================== *)
(* == PROJET                                                               == *)
(* ========================================================================== *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> INT
%token <bool> BOOL
%token <string> IDENT BINOP UNOP BIUNOP STRING
%token  SEMICOL COMMA DOT COLON EQ LCURLY RCURLY LBRACK RBRACK LPAR RPAR LET FUNCTION FOR OF IF ELSE FUNCION LAMBDA RETURN WHILE NEWLINE 

%type <Ast.expr> expr
%type <Ast.stmt> stmt
%type <Ast.expr list> args
%type <Ast.stmt list> stmts
%type <Ast.func> func
%type <Ast.func list> funcs
%type <string list> params
%type <Ast.prog> prog

%start stmt             /* the entry point */

%%

  expr :
    INT                                 { ASTNum($1) }
  | IDENT                               { ASTId($1) }
  | BOOL                                { ASTBool($1)}
  | LPAR expr RPAR                      { ASTExprPar($2)}
  | expr BINOP expr                     {ASTBinop($1,$2,$3)}
  | expr BIUNOP expr                    {ASTBinop($1,$2,$3)}
  | UNOP expr                           {ASTUnop($1,$2)}
  | BIUNOP expr                         {ASTUnop($1,$2)}
  | expr LPAR args RPAR                 {ASTFunCall($1,$3)}
  ;

  args :
    expr                { [$1] }
  | expr COMMA args     { $1::$3 }
  ;

  params :
    IDENT               { [$1] }
  | IDENT COMMA params  { $1::$3}
  ;

  stmt :
    LET IDENT EQ expr                                                 {ASTLet($2,$4)}
  | IDENT EQ expr                                                     {ASTAssign($1,$3)}
  | IF LPAR expr RPAR LBRACK stmts RBRACK                             {ASTIF($3,$6,[])}
  | IF LPAR expr RPAR LBRACK stmts RBRACK ELSE LBRACK stmts RBRACK    {ASTIF($3,$6,$10)}
  | WHILE LPAR expr RPAR LBRACK stmts RBRACK                          {ASTWhile($3,$6)}
  | FOR LPAR stmt SEMICOL expr SEMICOL stmt RPAR LBRACK stmts RBRACK  {ASTFor($3,$5,$7,$10)}
  | RETURN expr                                                       {ASTReturn($2)}
  ;

  stmts :
    stmt                                        { [$1] }
  | stmt NEWLINE stmts                          {$1::$3} 
  | stmt SEMICOL stmts                          {$1::$3}


  func :
    FUNCTION IDENT LPAR params RPAR LBRACK stmts RBRACK               {ASTFun($2,$4,$7)}

  funcs :
    func             { [$1] }
  | func funcs        { $1::$2 }  

  prog :
    funcs stmts        {ASTProg($1,$2)}

