open List;;
open Fun;;
(*1*)
let entiers_pairs: int list -> int list =
  filter(fun x -> x mod 2 = 0)
;;

(*2*)
let entiers_borne: (x : int) (y : int) : int list -> int list =
  filter(fun v -> v mod x = 0)(init (y-1)(fun v -> v+2))
;;

(*3*)
let a_divisible (a:int) : int list -> bool = 
  exists(fun v -> v mod a = 0)
;;

(*4*)
let maxi_list (m : 'a) (l : 'a list) : bool =
  for_all(fun v -> v < m) l &&
  exists(fun v -> v = m) l
;;

(*5*)
let eleve_carre: int list -> int list =
  map(fun x -> x*x)
;;

(*6*)
let ajoute_d (d : int) : int list -> int list = 
  map(fun x -> x + d)
;;

(*7*)
let string_of_list: int list -> string list =
  map(fun x -> string_of_int x)
;;

(*8*)
let rev_coded: 'a list -> 'a list =
  fold_left(fun acc x -> x::acc) []
;;

(*9*)
let map_coded: 'a list -> 'a list =
  fold_right()[]
;;

(*10*)
let filter_coded: 'a list -> 'a list = 

;;

(*11*)
let 