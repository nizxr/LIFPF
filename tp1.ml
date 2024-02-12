ocaml

3*2+5/2;;

3.14 +. 5.;;

"ceci est " ^ "un test"

3 < 2;;

int_of_float 5.0 ;;

let x1 = 3.5 +. float_of_int 2
in x1 +. 3.0;;

let x1 = 3.5 +. float_of_int 2 in
let x2 = x1 *. x1 in
x2 *. 2.0 ;;

(if true then 3 else 5) > 4 ;; 

(**
[discriminant a b c] calcule le discriminant d'un trinôme.
Cette fonction est utile pour trouver les racines d'un trinôme.
@param a le coefficient d'ordre 2
@param b le coefficient d'ordre 1
@param c le coefficient d'ordre 0
@return le discriminant
*)
let discriminant (a : float) (b : float) (c : float) : float = 
  b *. b -. 4. *. a *. c ;;


discriminant 2.0 8.0 8.0;;


type couleur = Rouge | Jaune | Bleu ;;

Rouge;;
Rouge = Rouge ;; 
Rouge != Bleu ;; 
Bleu > Jaune ;; 

type couleur = Rouge | Jaune | Bleu | Violet | Orange | Vert;;

type couleur = Rouge | Jaune | Bleu | Violet | Orange | Vert | RJB of int * int * int;;


(3, 5, 6);;

match c with
| Rouge -> "rouge"
| Bleu -> "bleu"
| Jaune -> "jaune"
| _ -> "mélange"


let nom_couleur (c : couleur) : string = 
  match c with
  | Rouge -> "rouge"
  | Jaune -> "jaune"
  | Bleu -> "bleu"
  | Violet -> "violet"
  | Orange -> "orange"
  | Vert -> "vert"
  | RJB (_,_,_) -> "mélange"

  nom_couleur (RJB (2,0,6)) ;;

  (**
  Renvoie la somme des n premiers entiers
  @parm n le nombre d'entiers à sommer
  @return la somme    
  *)
  let rec sum_n (n : int) : int = 
    if n <= 0
      then 0
  else n + sum_n (n-1) ;;

    (**
  Renvoie la factorielle de n
  @parm n un nombre 
  @return la factorielle de n    
  *)
  let rec factorielle (n : int) : int = 
  match n with
  | 0 -> 1
  | 1 -> 1
  | v -> v * factorielle(v-1)
;;

factorielle 4;;

[1; 2; 3; 4] ;;
1 :: 2 :: 3 ::[] ;;

(**
Cette fonction calcule la longueur d'une liste de string
@param l la liste dont on veut la longueur
@return la longueur de l    
*)
let rec longueur (l : string list) : int =
  match l with 
  | [] -> 0
  | _ :: l2 -> 1 + longueur l2
;;

(**
Cette fonction fait la somme des éléments d'une list
@param l la liste sur laquelle on fait la somme
@return somme des elements d'une liste
*)
let rec sum_f (l : float list) : float =
  match l with
  | [] -> 0.
  | v :: l2 -> v +. sum_f l2
;;