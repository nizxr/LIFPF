type arbre_bin =
| ABVide
| ABNoeud of int * arbre_bin * arbre_bin
;;

(**
[taille_ab a]
@param a arbre binaire d'int
@return taille de l'arbre    
*)
let rec taille_ab (a: arbre_bin) : int =
  match a with
  | ABVide -> 0
  | ABNoeud(_, fg, fd) -> 1 + taille_ab fg + taille_ab fd
;;

(**
[produit_ab a]
@param a arbre binaire
@return produit des elements de l'arbre
*)
let rec produit_ab (a: arbre_bin) : int =
  match a with
  | ABVide -> 1
  | ABNoeud(n, fg, fd) -> n * produit_ab fg * produit_ab fd
;;

(**
[insere_arbre_bin_recherche e a]
@param e element a inserer dans l'abr
@param a arbre binaire de recherche
@return abr
*)
let rec insere_arbre_bin_recherche (e: int) (a: arbre_bin) : arbre_bin =
  match a with
  | ABVide -> ABNoeud(e, ABVide, ABVide)
  | ABNoeud(n, fg, fd) -> if (e < n) 
                          then ABNoeud(n, insere_arbre_bin_recherche e fg, fd)
                          else ABNoeud(n, fg, insere_arbre_bin_recherche e fd)
;;

let rec concatene (l1: int list) (l2: int list) = 
  match l1 with
  | [] -> l2
  | x :: l -> x :: (concatene l l2)
;;

(**
[list_of_arbre_bin a]  
@param a arbre binaire de recherche
@return l liste
*)
let rec list_of_arbre_bin (a: arbre_bin): int list =
  match a with
  | ABVide -> []
  | ABNoeud(e, fg, fd) -> list_of_arbre_bin fg @ (e :: list_of_arbre_bin fd)
;;

(**
[arbre_bin_rech_of_int_list l]
@param l liste
@return abr    
*)
let rec arbre_bin_rech_of_int_list (l: int list) : arbre_bin =
  match l with
  | [] -> ABVide
  | x :: l1 -> insere_arbre_bin_recherche x (arbre_bin_rech_of_int_list l1)
;; 

(**
[tri_abr l]
@param l liste
@return list    
*)
let rec tri_abr (l : int list) : int list =
  match l with 
  | [] -> []
  | l' -> list_of_arbre_bin(arbre_bin_rech_of_int_list(l'))
;;

type binop = Plus | Moins | Mult ;;
type expr = 
| Cst of int
| Binop of binop * expr * expr
;;

let rec string_of_expr (e : expr) : string = 
  match e with
  | cst -> cst
  | Plus * v1 * v2 -> (string_of_expr v1) + (string_of_expr v2)
  | Moins * v1 * v2 -> (string_of_expr v1) - (string_of_expr v2)
  | Mult * v1 * v2 -> (string_of_expr v1 * string_of_expr v2)
;;