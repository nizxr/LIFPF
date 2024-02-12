let rec map2 (v : 'a -> 'b -> 'c) (l1 : 'a list) (l2 : 'b list) : 'c list =
  match (l1, l2) with
  | [] , _ -> []
  | _ , [] -> []
  | x1::l1' , x2::l2' -> v x1 x2 :: map2 v l1' l2'
;;

let rec zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match (l1, l2) with
  | [] , _ -> []
  | _ , [] -> []
  | x1::l1' , x2::l2' -> (x1, x2) :: zip l1' l2'
;;

let rec unzip (l : ('a * 'b) list) : 'a list * 'b list =
  match l with
  | ([],[]) -> [] 
  | x1 :: l1' , x2 :: l2' -> (** FAUT UTILISER LIST.FOLDRIGHT*)


type bool = Vrai | Faux;; 

type boolexpr =
| Var of string
| Vrai
| Non of boolxpr
| Et of boolexpr * boolexpr
;;

type environement =
| 