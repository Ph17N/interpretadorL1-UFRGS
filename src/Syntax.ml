(*  SINTAXE  *)

type unop = Head | Tail
;;

type binop = Plus | Geq | Minus
;;

type tipo = Tbool | Tint | Tfun of tipo*tipo | Tlist of tipo | Tempty
;;

type term =
|  Num   of int
|  Bool  of bool
|  Binop of binop*term*term
|  Unop  of unop*term
|  If    of term*term*term
|  Var   of string
|  App   of term*term
|  Fun   of string*tipo*term
|  Let   of string*tipo*term*term
|  Cons  of term*term
|  Empty
|  LetRec of string*tipo*term*term
;;
