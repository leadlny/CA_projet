
(* ========================================================================== *)
(* == PROJET                                                               == *)
(* ========================================================================== *)
(* ==  Ecriture de bytecode dans un fichier                                == *)
(* ========================================================================== *)
open Ast
open Int32

(* Nous aurait permis d'écrire dans le fichier que nous aurons passé à la vm
let filewrite = Sys.argv.(2) ;;
let out_file = open_out filewrite ;;
*)

(* CF. Partie 4 TD3 pour ne pas utiliser r1,r2...*)
let acc = 0;;
let sa = 1;;
let sp = 2;;
let tmp1 = 3;; (* 8 registres de la vm *)
let tmp2 = 4;;
let tmp3 = 5;;
let one = 6;;
let minus_one = 7;;

(* Pour nous simplifier la tache on veut avoir 1 et -1 dans nos registres : 
ortho(one, 1)
nand(minus_one, one, one) 
add(minus_one, minus_one, one) 
on peut donc érire : *)
one = 1;;
minus_one = -1;;


(* Faire une fonction pour chaque opérateurs de la vm qui ecrivent le bytecode 
 Visite de l'AST avec fonctions comme APS mais au lieu de print on fait ce qu'il y a le TD3 



let write_op(op: int32) (a :int32 ) (b: int32) (c: int32) =           
  let d = Int32.logor (Int32.shift_left a 6) (Int32.logor (Int32.shift_left b 3) c) in
  let res = Int32.logor (Int32.shift_left op 28) d in
  (* faut le reverse*)
  wrint_int res;

(* Ecrit les 4 bytes dans le fichier de sortie*)
let write_int( op : int32)= 
  output_binary_int out_file op;


(* Push et pop permettent de factoriser plusieurs opérations redondantes *)
let push(r : int) =
  modif_tableau sa sp r;
  ortho tmp1 1;
  add sp sp tmp1;

let pop(dst : int)=
  ortho tmp1 1;
  nand tmp2 tmp1 tmp1;
  add tmp1 tmp2 tmp1;
  add sp sp tmp1;
  index dst sa sp;


(* Code pour les 13 opérations *)

let ortho(a :int32) (value : int32) =
  let result = Int32.logor (Int32.shift_left 13 28) (Int32.logor (Int32.shift_left a 25) value) in
  wrint_int result;


let mouv(a :int32) (b: int32) (c: int32)=
  write_op 0 a b c;

let index(a :int32) (b: int32) (c: int32) =
   write_op 1 a b c;

let modif_tableau(a :int32) (b: int32) (c: int32)=
  write_op 2 a b c;

let add(a :int32) (b: int32) (c: int32)=
  write_op 3 a b c;

let mul(a :int32) (b: int32) (c: int32)=
  write_op 4 a b c;

let div(a :int32) (b: int32) (c: int32)=
  write_op 5 a b c;

let nand(a :int32) (b: int32) (c: int32) =
  write_op 6 a b c;

let alloc(b: int32) (c :int32) =
  write_op 8 ? b c; (* On ne sait pas quel entier passer à la fonction quand on a pas a b et c tous les 3*)

let aban(c :int)=
  write_op 9 ? ? c;

let exit(c :int)=
  write_op 10 ? ? c;

let entry(c :int)=
  write_op 11 ? ? c;

let load(b:int) (c:int)=
  write_op 12 ? b c;
*)




let rec print_prog e =
  match e with
    ASTProg (funcs,stmts) ->(
      print_funcs(funcs);
      print_stmts(stmts);
    ) 


and print_funcs f =
  match f with
    []->()
    |[f] ->print_func f
    |f::fs -> (
      print_func f;
      print_funcs fs;
    ) 

and print_func f =
  match f with
    ASTFun(ident, params, stmts) -> Printf.printf"ASTFun"

and print_stmts s =
  match s with
    []->()
    |[s] ->print_stmt s
    |s::ss -> (
      print_stmt s;
      print_stmts ss;
    ) 

and print_stmt s =
  match s with
    ASTLet(ident, expr) ->(
      Printf.printf"let %s = " ident;
      print_expr expr;
    ) 
    |ASTAssign(ident, expr) -> (
      Printf.printf"%s = " ident;
      print_expr expr;
    ) 
    |ASTIF(expr, stmts, stmts_else) -> (
      Printf.printf"ortho(acc, ";
      print_expr expr;
      Printf.printf")\n";
      Printf.printf"ortho(tmp2, lbl_if) \n";
      (*if stmts_else != [] then *)Printf.printf"ortho(tmp3, lbl_else)\n";
      Printf.printf"cmove(tmp3, tmp2, acc)\n";
      Printf.printf"ortho(tmp1, 0)\n";
      Printf.printf"load(tmp1, tmp3)\n";

      Printf.printf"lbl_if:\n";
      print_stmts stmts;
      Printf.printf"ortho(tmp1, 0)\n";
      Printf.printf"ortho(tmp2, lbl_end\n";
      Printf.printf"load(tmp1, tmp2)\n";

      Printf.printf"lbl_else:\n";
      print_stmts stmts_else;

      Printf.printf"lbl_end:\n";
    ) 
    |ASTWhile(expr, stmts) -> Printf.printf"ASTWhile"
    |ASTFor(stmt, expr, stmt2, stmts) -> Printf.printf"ASTFor"
    |ASTReturn(expr) -> Printf.printf"ASTReturn"

and print_expr e =
  match e with
    ASTNum n -> Printf.printf"ortho(acc, %d)" n
    |ASTId x -> Printf.printf"%s" x
    |ASTBool b ->(
      if b then Printf.printf"ortho(acc, 1)" else Printf.printf"ortho(acc, 0)"
    ) 
    |ASTExprPar e -> (
      Printf.printf"(";
      print_expr e;
      Printf.printf")";
    ) 
    |ASTBinop(exprG, op, exprD) -> match_binop exprG op exprD (* fais appel a une fonction qui regarde quel opérateur c'est match_op(ExprG,op,exprD)*)
    |ASTUnop(op, expr) -> (
      Printf.printf"not(acc, ";
      print_expr expr;
      Printf.printf")\n";
    )
    |ASTFunCall(expr, args) -> Printf.printf"ASTFunCall"

and match_binop exprG op exprD =
  match op with
    "+" ->(     (* op_plus va faire push(exprG) puis pop(exprD) puis add(acc,tmp1,acc)*) 
      (*op_plus exprG exprD;*)
      Printf.printf"compile(leftExpr)";
      print_expr exprG;
      Printf.printf"push(acc)";
      Printf.printf"compile(rightExpr)";
      print_expr exprD;
      Printf.printf"pop(tmp1)";
      (* ------------- *)
      Printf.printf") \n add(acc, acc, tmp1)";
    )
    |"-" -> (   (*op_minus va push(exprG) puis pop(exprD) puis mult(acc,acc,minus_one) puis add(acc,tmp1,acc)  *)
      (*op_minus exprG exprD;*)
      Printf.printf"compile(leftExpr)";
      print_expr exprG;
      Printf.printf"push(acc)";
      Printf.printf"compile(rightExpr)";
      print_expr exprD;
      Printf.printf"pop(tmp1)";
      (* ------------- *)
      Printf.printf"mult(acc, acc, minus_one";
      Printf.printf"add(acc, tmp1, acc)";
    )
    |"*" ->(    (* op_plus va faire push(exprG) puis pop(exprD) puis mul(acc,tmp1,acc)*)
      (*op_mult exprG exprD;*)
      Printf.printf"compile(leftExpr)";
      print_expr exprG;
      Printf.printf"push(acc)";
      Printf.printf"compile(rightExpr)";
      print_expr exprD;
      Printf.printf"pop(tmp1)";
      (* ------------- *)
      Printf.printf") \n mult(acc, acc, tmp1)";
    ) 
    |"/" ->(   (* op_plus va faire push(exprG) puis pop(exprD) puis div(acc,tmp1,acc)*)
      (*op_div exprG exprD;*)
      Printf.printf"compile(leftExpr)";
      print_expr exprG;
      Printf.printf"push(acc)";
      Printf.printf"compile(rightExpr)";
      print_expr exprD;
      Printf.printf"pop(tmp1)";
      (* ------------- *)
      Printf.printf") \n div(acc, tmp2, tmp1)";
    )  
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parser.stmt Lexer.token lexbuf in
      print_stmt e;
  with Lexer.Eof ->
    exit 0     
