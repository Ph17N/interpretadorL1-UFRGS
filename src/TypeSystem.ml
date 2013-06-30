open Syntax;;

(* SISTEMA DE TIPOS: *)

(*Recebe um termo, retorna o tipo dele se ele for bem tipado, senão retorna None*)

let rec typeCheck (t:term) gamma =
	match t with
	| Empty              -> Some Tempty
	| Num  (_)           -> Some Tint
	| Bool (_)           -> Some Tbool
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
				if td = tc then Some td else None
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
			| (Some tc, Some Tempty) -> Some (Tlist tc)
			| (Some tc, Some (Tlist td)) -> if tc = td then Some (Tlist tc) else None
			| (_,_) -> None)
(*	| LetRec (x,tp,t1,t2)   ->
		let ta = typeCheck t1 gamma in
		let tb = typeCheck t2 gamma in
		(match (ta,tb) with
		| (Some tc, Some td) -> let rec tc = tp then Some td else None
		| _ -> None) *)
	| _ -> None (* só pra parar de dar fatal error na execução *)

;;
