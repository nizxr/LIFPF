(**
[discriminant a b c] calcule le discriminant d'un trinôme.
Cette fonction est utile pour trouver les racines d'un trinôme.
@param a le coefficient d'ordre 2
@param b le coefficient d'ordre 1
@param c le coefficient d'ordre 0
@return le discriminant
*)
let discriminant (a:float) (b:float) (c:float) : float = b *. b -. 4.0 *. a *. c;;

type couleur = Rouge | Jaune | Bleu | Violet | Orange | Vert| RJB of int * int * int;;

let nom_couleur c : string =
  match c with
  | Rouge -> "rouge"
  | Bleu -> "bleu"
  | Jaune -> "jaune"
  | Violet -> "violet"
  | Orange -> "orange"
  | Vert -> "vert"
  | RJB (r, j, b) -> "mélange";;
  

let paragraphe_bottles (n:int) : string =
match n with
  | 0 -> "No more bottles of beer on the wall, no more bottles of beer." ^ "\n" ^ "Go to the store and buy some more, 99 bottles of beer on the wall."
  | 1 -> "1 bottle of beer on the wall, 1 bottle of beer." ^ "\n" ^ "Take one down and pass it around, no more bottles of beer on the wall."
  | 2 -> "2 bottles of beer on the wall, 2 bottles of beer." ^ "\n" ^ "Take one down and pass it around, 1 bottle of beer on the wall."
  | n -> string_of_int n ^ " bottles of beer on the wall, " ^ string_of_int n ^ " bottles of beer." ^ "\n" ^ "Take one down and pass it around, " ^ string_of_int (n-1) ^ " bottles of beer on the wall."
;;


let rec factorielle (n:int): int =
  if n = 0
  then 1
  else n * factorielle (n-1);;

let rec sum_f (l: float list) : float = 
  match l with
  | [] -> 0.0
| v :: l2 -> v +. sum_f l2;;

let rec liste_n_0 (n:int): int list =
  match n with
  | 0 -> [0]
  | n -> n :: liste_n_0 (n-1);;

let rec bottles_of_list (l:int list) : string list =
  match l with
  | [] -> []
  | v :: l2 -> paragraphe_bottles v :: bottles_of_list l2
;;

let chanson_99_bottles = 
  let rec print (n: int): string = 
    match n with
    | 0 -> paragraphe_bottles 0
    | n -> paragraphe_bottles n ^ "\n\n" ^ print (n-1)
  in print 99