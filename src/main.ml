open Semantics;;
open TypeSystem;;
open Syntax;;


(* Converte tipo para string *)
let rec tipo_to_string (tp:tipo) : string =
  match tp with
  | Tbool -> "bool"
  | Tint  -> "int"
  | Tfun(tp1,tp2) -> "(" ^ (tipo_to_string tp1) ^ " -> " ^ (tipo_to_string tp2) ^ ")"
;;

(* Converte termo para string *)
let rec term_to_string (t:term) : string =
  match t with
  | Num(x) -> string_of_int x 
  | Bool(true) -> "true"
  | Bool(false) -> "false"
  | Binop(Plus,t1,t2) -> "(" ^ (term_to_string t1) ^ " + " ^ (term_to_string t2) ^ ")"
  | Binop(Geq,t1,t2) -> "(" ^ (term_to_string t1) ^ " >= " ^ (term_to_string t2) ^ ")"
  | If(t1,t2,t3) -> "(if " ^ (term_to_string t1) ^ " then " ^ (term_to_string t2) ^ " else " ^ (term_to_string t3) ^ ")"
  | Var(x) -> x
  | App(t1,t2) -> "(" ^ (term_to_string t1) ^ " " ^ (term_to_string t2) ^ ")"
  | Fun(x,tp,t1) -> "(fun " ^ x ^ ":" ^ (tipo_to_string tp) ^ "=>" ^ (term_to_string t1) ^ ")"
  | Let(x,tp,t1,t2) -> "(let " ^ x ^ ":" ^ (tipo_to_string tp) ^ "=" ^ (term_to_string t1) ^ " in " ^ (term_to_string t2) ^ ")"
;;  








(*  TESTES *) 

let test1 =  (Fun ("x",Tint,Binop(Plus,Binop(Plus, Var "x", Num 1),Num 2))) ;;
let test2 =  Num(23) ;;
let test3 =  App(test1,test2) ;;

let one_step   = step test3 ;;
let full_eval  = eval test3 ;;

print_endline (term_to_string test3) ;;
print_endline (term_to_string full_eval) ;;

