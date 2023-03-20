(**
[concatene l1 l2] concatene deux listes    
@param l1 premiere liste
@param l2 deuxieme liste
@return liste [l1; l2]
*)
let rec concatene (l1: int list) (l2: int list) = 
  match l1 with
  | [] -> l2
  | x :: l -> x :: (concatene l l2)
;;


(**
[applatit]

//peut etre utile? (x::l1) :: l2
*)
let rec applatit (l: (int list) list): int list =
  match l with
  | [] -> []
  | x :: l1 -> concatene x (applatit l1)
;;

(**
[renverse ajoute l1 l2]
@param l1
@param l2

*)
let rec renverse_ajoute (l1: int list) (l2: int list) =
  match l1 with
  | [] -> l2
  | 