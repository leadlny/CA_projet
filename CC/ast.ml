


type expr =
    ASTNum of int
  | ASTId of string
  | ASTBool of bool
  | ASTBinop of expr * string * expr
  | ASTUnop of string * expr
  | ASTFunCall of expr * expr list 
  | ASTExprPar of expr
  
and stmt =
    ASTLet of string * expr
  | ASTAssign of string * expr
  | ASTIF of expr * stmt list * stmt list
  | ASTWhile of expr * stmt list
  | ASTFor of stmt * expr * stmt * stmt list
  | ASTReturn of expr

and func =
    ASTFun of string * string list * stmt list

and prog =
    ASTProg of func list * stmt list