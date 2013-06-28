
(*  SINTAXE  *)

type binop = Plus | Geq
;;

type tipo = Tbool | Tint | Tfun of tipo*tipo
;;

type term =
|  Num   of int
|  Bool  of bool
|  Binop of binop*term*term
|  If    of term*term*term
|  Var   of string
|  App   of term*term
|  Fun   of string*tipo*term
|  Let   of string*tipo*term*term
|  Cons  of term*term
|  LetRec of string*tipo*term*term
;;
