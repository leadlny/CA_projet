(* ========================================================================== *)
(* == PROJET                                                               == *)
(* ========================================================================== *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
(* pas sure de ça je l'ai vu sur internet mais vu que les *)
(* chaine de caractère sont optionnelles on verra *)

rule token = parse
    [' ' '\t' ]       { token lexbuf }     (* skip blanks *)
    | '\n'                                  { NEWLINE }
    | ';'                                   { SEMICOL }
    | ','                                   { COMMA }
    | '.'                                   { DOT }
    | ':'                                   { COLON }
    | '='                                   { EQ }
    | '{'                                   { LCURLY }
    | '}'                                   { RCURLY }
    | '['                                   { LBRACK }
    | ']'                                   { RBRACK }
    | '('                                   { LPAR }
    | ')'                                   { RPAR }
    | "true"                                { BOOL(true)}
    | "false"                               { BOOL(false)}
    | ['0'-'9']+             as lxm         { INT(int_of_string lxm)}
    | "let"                                 { LET }
    | "if"                                  { IF }  
    | "else"                                { ELSE }  
    | "while"                               { WHILE }
    | "for"                                 { FOR }
    | "return"                              { RETURN }
    | "lambda"                              { LAMBDA }
    | "function"                            { FUNCTION }
    | ['+'  '*' '/' '%']      as op         { BINOP(String.make 1 op) }
    | "&&" | "||" | "==" | (['<' '>']'='?)     as op      { BINOP(op) }
    | '!'                        as op      { UNOP(String. make 1 op) }
    | '-'                        as op      { BIUNOP(String. make 1 op) }
    | '\"'['!'-'~']*'\"'             as lxm         { STRING(String.sub lxm 1 ((String.length lxm) - 2))}
    | '\n'          {NEWLINE}
    | ['a'-'z''A'-'Z''_']['0'-'9''a'-'z''A'-'Z''_']* as lxm         { IDENT(lxm)}
    | eof         { raise Eof }





