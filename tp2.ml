(**
Concatene deux liste en mettant la deuxieme a la fin de la premiere
@param l1,l2 listes d'entiers
@return concatenation de l1 & l2
*)
let rec concatene (l1 : int list) (l2 : int list) : int list =
  match l1 with
  | [] -> l2
  | x :: l -> x :: (concatene l l2)
;;


(**
liste constituée de la concaténation des éléments de cette liste de liste
@param l liste de listes d'entiers
@return liste d'entiers
*)
let rec applatit (l : (int list) list) : int list =
  match l with
  | [] -> []
  | x :: l1 -> concatene x (applatit l1)
;;

(**
BUG

deuxieme version de applatit
@param l liste de listes d'entiers
@return liste d'entiers
*)
let rec applatit2 (l : (int list) list) : int list =
  match l with
  | [] -> []
  | [] :: l1 -> applatit2 l1
  | (x :: l1) :: l2 -> x :: applatit2 (l1::l2)
;;


(**
prend deux listes en argument et renvoie la première renversée concaténée à la seconde
@param l1, l2 listes d'entiers
@return une liste d'entiers resultante de la concatenation du renverse de la premiere et ensuite la deuxieme
*)
let rec renverse_ajoute (l1 : int list) (l2 : int list) : int list = 
match l1 with
| [] -> l2
| x :: l -> renverse_ajoute l (x::l2)
;;

(**
Renverse une liste d'entiers
@param l liste d'entiers
@return la liste d'entiers passé en parametres renversée
*)
let renverse (l : int list) : int list =
  renverse_ajoute l []
;;

(**
Insert un entier dans une liste triée
@param v entier à inserer
@param l liste d'entiers triés
@return liste d'entiers triés comprenant l'insertion de l'entier passé en param
*)
let rec insertion (v : int) (l : int list) : int list =
  match l with
  | [] -> [v]
  | l1 :: l2 -> if (l1 < v) then l1 :: (insertion v l2)
                            else v :: l1 :: l2
;;
     
(**
Tri une liste d'entiers en utilisant l'algo d'insertion
@param l liste d'entiers potentiellement non triée
@return la meme liste d'entiers triée
*)
let rec tri_insertion (l : int list) : int list =
  match l with
  | [] -> []
  | v :: lr -> insertion v (tri_insertion lr)
;;

type 'a resultat = 
| Absence
| Valeur of 'a
;;

let rec cherche (cle : int) (l : (int * string) list) : string resultat =
  match l with
  | [] -> Absence
  | (k,v) :: reste ->
    if k = cle then
      Valeur v
    else
      cherche cle reste
;;

type binop = 
| Plus 
| Moins 
| Mult 
| Div
;;

type elt_expr = 
| Op of binop 
| Cst of int
;;

type resultat = 
| Ok of int 
| ErrDivZero 
| ErrExpr
;;

(**
    
*)
let eval_op (op : binop) (v1 : resultat) (v2 : resultat) : resultat =
  