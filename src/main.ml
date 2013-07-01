open Semantics;;
open TypeSystem;;
open Syntax;;


(* Converte tipo para string *)
let rec tipo_to_string (tp:tipo) : string =
  match tp with
  | Tbool -> "bool"
  | Tint  -> "int"
  | Tlist a -> (tipo_to_string a) ^ " list"
  | Tempty -> "empty"
  | Tfun(tp1,tp2) -> "(" ^ (tipo_to_string tp1) ^ " -> " ^ (tipo_to_string tp2) ^ ")"
;;

(* Converte termo para string *)
let rec term_to_string (t:term) : string =
  match t with
  | Num(x) -> string_of_int x
  | Bool(true) -> "true"
  | Bool(false) -> "false"
  | Unop (Head,t1) -> "( Head " ^ (term_to_string t1) ^ ")"
  | Unop (Tail,t1) -> "( Tail " ^ (term_to_string t1) ^ ")"
  | Unop (IsEmpty,t1) -> "( IsEmpty " ^ (term_to_string t1) ^ ")"
  | Binop(Plus,t1,t2) -> "(" ^ (term_to_string t1) ^ " + " ^ (term_to_string t2) ^ ")"
  | Binop(Minus,t1,t2) -> "(" ^ (term_to_string t1) ^ " - " ^ (term_to_string t2) ^ ")"
  | Binop(Geq,t1,t2) -> "(" ^ (term_to_string t1) ^ " >= " ^ (term_to_string t2) ^ ")"
  | If(t1,t2,t3) -> "(if " ^ (term_to_string t1) ^ " then " ^ (term_to_string t2) ^ " else " ^ (term_to_string t3) ^ ")"
  | Var(x) -> x
  | App(t1,t2) -> "(" ^ (term_to_string t1) ^ " " ^ (term_to_string t2) ^ ")"
  | Fun(x,tp,t1) -> "(fun " ^ x ^ ":" ^ (tipo_to_string tp) ^ "=>" ^ (term_to_string t1) ^ ")"
  | Let(x,tp,t1,t2) -> "(let " ^ x ^ ":" ^ (tipo_to_string tp) ^ "=" ^ (term_to_string t1) ^ " in " ^ (term_to_string t2) ^ ")"
  | LetRec(x,tp,t1,t2) -> "(let rec " ^ x ^ ":" ^ (tipo_to_string tp) ^ "=" ^ (term_to_string t1) ^ " in " ^ (term_to_string t2) ^ ")"
  | Cons(a,b) -> "(" ^ (term_to_string a) ^ "::" ^ (term_to_string b) ^ ")"
  | Empty -> "[]"
;;

(*  TESTES *)

let test1 = (Fun ("x",Tint,Binop(Plus,Binop(Plus, Var "x", Num 1),Num 2))) ;;
let test2 = Num(23) ;;
let test3 = App(test1,test2) ;;

let test4 = Let("x",Tint,Num(6),Binop(Plus,Var "x",Num 3));;
let test5 = Let("y",Tfun(Tint,Tbool),Fun("y",Tint,Binop(Geq,Var "y",Num 5)),App(Var "y",Num 2));;
let test6 = LetRec("inc",Tfun (Tint,Tint),Fun("y",Tint,If(Binop(Geq, Var "y" , Num 0), Binop(Plus,Var "y", Num 1), Num 0 )),App(Var "inc",Num 5));;
let test7 = LetRec("sum",
                  Tfun (Tint,Tint),
                  Fun("y",
                      Tint,
                      If(Binop(Geq, Var "y" , Num 0),
                         Binop(Plus,(Var "y"),App(Var "sum", Binop(Minus,Var "y", Num 1))),
                         Num 0 )),
                  App(Var "sum",Num 5));;
let test8 = Cons(Let("x",Tint,Num 5,Binop(Minus,Var "x",Num 3)),Empty);;
let test9 = Unop(Tail,Cons(Let("x",Tint,Num 5,Binop(Minus,Var "x",Num 3)),Empty));;
let test10 = Unop(Head,Cons(Let("x",Tint,Num 5,Binop(Minus,Var "x",Num 3)),Empty));;
let test11 = Let("f",Tfun(Tint,Tlist Tint),Fun(
                                               "y",Tint,
                                               Cons(Var "y",Cons(Var "y",Empty))
                                              ),
                App(Var "f",Num 12));;

let test12 = Cons(Let("x",Tint,Num 5, (Binop (Geq,Var "x",Num 4))),Empty);;

let test13 = LetRec    ("map",

            Tfun(Tlist(Tint),Tlist(Tint)),
            Fun    ("list",
                Tlist(Tint),
                If     (Unop(IsEmpty,Var "list"),
                    Empty,
                    Cons(Binop(Plus,Unop(Head,Var "list"),Unop(Head,Var "list")),App(Var "map",Unop(Tail,Var "list")))
                    )
                ),
            App(Var "map",Cons(Num 8, Cons(Num 14, Cons (Num ~-3,Empty))))
            );;

let id =
	Let("id",
				Tfun(Tlist Tint, Tlist Tint),
				Fun("lst",(Tlist Tint),
				Cons(Unop(Head,Var "lst"), (Unop (Tail,Var "lst")))),
				App(Var "id", Cons(Num 4,Cons(Num 3,Empty)))
	)

let rec showTrace lst = match lst with
	| (h::r) ->
		print_endline (term_to_string h);
		showTrace r
	| [] -> ()
;;

let rec testAll lst = match lst with
	| (h::r) ->
		print_endline (term_to_string h);
		(try
			let t = typeCheck h (Hashtbl.create 88) in
			(match t with
				| Some t0 -> print_endline (tipo_to_string t0)
				| None -> print_endline "Ill-typed")
			;
		with
			| Match_failure (a,b,c) -> Printf.printf "Type-system failed at %s (%d, %d)\n" a b c);
		print_endline "## Execution trace: ##";
		showTrace (trace h);
		print_endline "### Trace End ###";
		print_endline (term_to_string (eval h));
		print_endline "###########";
		testAll r
	| [] -> ();;

let tests = [test1;test2;test3;test4;test5;test6;test7;test8;test9;test10;test11;test12;test13;id];;

testAll tests;;

(*let typeTest1 = typeCheck (Num 88) (Hashtbl.create 88);;*)

