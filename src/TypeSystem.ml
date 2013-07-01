open Syntax;;

let rec compareType t1 t2 = match (t1,t2) with
	| Tlist a, Tlist b when a = b -> true
	| Tlist a, Tlist Tempty -> true
	| Tlist Tempty, Tlist a -> true
	| Tlist a, Tlist b when a<>b -> compareType a b
	| (a, b) when a=b -> true
	| _ -> false
;;

(* SISTEMA DE TIPOS: *)

(*Recebe um termo, retorna o tipo dele se ele for bem tipado, senão retorna None*)

let rec typeCheck (t:term) gamma =
	match t with
	| Empty              -> Some (Tlist Tempty)
	| Num  (_)           -> Some Tint
	| Bool (_)           -> Some Tbool
	| Unop (Head,t1) ->
		let (ta) = (typeCheck t1 gamma) in
		(match (ta) with
			| (Some (Tlist tb)) -> Some tb
			| _ -> None)
	(*| Unop (Tail,Cons(t1,t2)) ->
		let (ta) = (typeCheck t2 gamma) in
		(match (ta) with
			| (Some (Tlist tb)) -> ta
			| _ -> None)*)
	| Unop (Tail,t1) ->
		let (ta) = (typeCheck t1 gamma) in
		(match (ta) with
			| (Some (Tlist tb)) -> ta
			| _ -> None)
	| Unop (IsEmpty,t1) ->
		let (ta) = (typeCheck t1 gamma) in
		(match (ta) with
			| (Some (Tlist tb)) -> Some Tbool
			| _ -> None)
	| Binop (Plus,t1,t2) ->
		let (ta,tb) = (typeCheck t1 gamma, typeCheck t2 gamma) in
		(match (ta,tb) with
			| (Some tc, Some td) ->
				if tc = Tint && td = Tint then
					Some Tint
				else
					None
			| (None,_) -> None
			| (_,None) -> None)
	| Binop (Geq,t1,t2)  ->
		let (ta,tb) = (typeCheck t1 gamma, typeCheck t2 gamma) in
		(match (ta,tb) with
			| (Some tc, Some td) ->
				if tc = Tint && td = Tint then
					Some Tbool
				else
					None
			| (None,_) -> None
			| (_,None) -> None)
	| Binop (Minus,t1,t2)  ->
		let (ta,tb) = (typeCheck t1 gamma, typeCheck t2 gamma) in
		(match (ta,tb) with
			| (Some tc, Some td) ->
				if tc = Tint && td = Tint then
					Some Tint
				else
					None
			| (None,_) -> None
			| (_,None) -> None)
	| If (t1,t2,t3)      ->
		let ta = typeCheck t1 gamma in
		let tb = typeCheck t2 gamma in
		let tc = typeCheck t3 gamma in
		(match (ta,tb,tc) with
			| (Some Tbool, Some td, Some tc) ->
				if (compareType td tc) then Some td else None
			| (_,_,_) -> None)
	| Var (x)            ->
		(try
			Some (Hashtbl.find gamma x)
		with Not_found ->
			None)
	| App (t1,t2)        ->
		let ta = typeCheck t1 gamma in
		let tb = typeCheck t2 gamma in
		(match (ta,tb) with
		| (Some (Tfun (tc,td)), Some te) -> if tc = te then Some td else None
		| _ -> None)
	| Fun (x,tp,t1)      ->
		Hashtbl.add gamma x tp;
		let ta = typeCheck t1 gamma in
		let t = (match ta with
			| Some t' -> Some (Tfun (tp,t'))
			| _ -> None
		) in
		Hashtbl.remove gamma x;
		t
	| Let (x,tp,t1,t2)   ->
		let ta = typeCheck t1 gamma in
		Hashtbl.add gamma x tp;
		let tb = typeCheck t2 gamma in
		let t = (match (ta,tb) with
			| (Some tc, Some td) -> if tc = tp then Some td else None
			| _ -> None) in
		Hashtbl.remove gamma x;
		t
	| Cons (t1,t2)       ->
		let ta = typeCheck t1 gamma in
		let tb = typeCheck t2 gamma in
		(match (ta,tb) with
			| (Some tc, Some (Tlist Tempty)) -> Some (Tlist tc)
			| (Some tc, Some (Tlist td)) -> if tc = td then Some (Tlist tc) else None
			| (_,_) -> None)
	| LetRec (x,(Tfun (tx1,tx2)),(Fun (y,ty1,e1)),e2) when compareType tx1 ty1->
		Hashtbl.add gamma x (Tfun (tx1,tx2));
		let tb = typeCheck e2 gamma in
		Hashtbl.add gamma y ty1;
		let ta = typeCheck e1 gamma in
		let t = (match (ta,tb) with
		| (Some tc, Some td) when compareType tc tx2 -> Some td
		| (Some tc, Some td) -> None
		| _ -> None) in
		Hashtbl.remove gamma y;
		Hashtbl.remove gamma x;
		t
	| _ -> None

;;
