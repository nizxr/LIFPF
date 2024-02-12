(**
2.Arbres binaires    
*)
type arbre_bin =
| ABVide
| ABNoeud of arbre_bin * int * arbre_bin
;;

(**
Retourne la taille d'un arbre binaire d'entiers passé en parametres
@param arb arbre binaire d'entiers
@return taille de l'arbre passé en parametre    
*)
let rec taille_ab (arb : arbre_bin) : int =
  match arb with
  | ABVide -> 0
  | ABNoeud(fg, _, fd) -> 1 + taille_ab fg + taille_ab fd
;;

assert (taille_ab ABVide = 0);;
assert (taille_ab ab1 = 1);;
assert (taille_ab ab2 = 2);;


(**
Effectue le produit des elements d'un arbre binaire d'int
@param arb arbre binaire d'int
@return le produit de tout les éléments de l'arbre passé en param    
*)
let rec produit_ab (arb : arbre_bin) : int =
  match arb with
  | ABVide -> 1
  | ABNoeud(fg, v, fd) -> produit_ab fg * v * produit_ab fd
;;

(**
Insere un element dans un arbre binaire de recherche
@param e l'element à inserer
@param arb l'arbre binaire de recherche où sera efféctuée l'insertion
@return un nouvel arbre binaire de rechre contenant l'element inseré au bon endroit    
*)
let rec insere_arbre_bin_recherche (e : int) (arb : arbre_bin) : arbre_bin =
  match arb with
  | ABVide -> ABNoeud(ABVide, e, ABVide)
  | ABNoeud(fg, v, fd) -> if (v > e) then ABNoeud(insere_arbre_bin_recherche e fg, v, fd)
                                      else ABNoeud(fg, v, insere_arbre_bin_recherche e fd)
                                    ;;

(**
Calcule la liste contenat les elements d'un arbre binaire
@param arb arbre binaire
@return liste contenant les éléments de l'arbre    
*)
let rec list_of_arbre_bin (arb : arbre_bin) : int list =
  match arb with
  | ABVide -> []
  | ABNoeud(fg, v, fd) -> [v] @ (list_of_arbre_bin fg) @ (list_of_arbre_bin fd)
;;

(**
Transforme une liste en arbre binaire de recherche
@param li liste d'entiers
@return un arbre binaire de recherche    
*)
let rec arbre_bin_rech_of_int_list (li : int list) : arbre_bin =
  match li with
  | [] -> ABVide
  | [v] -> ABNoeud(ABVide, v, ABVide)
  | v :: l -> insere_arbre_bin_recherche v (arbre_bin_rech_of_int_list li)
;;

(**
Trier une liste d'entiers en utilisant les fonctions codés dans ce tp  
@param l est une liste d'entiers
@return la liste d'entiers passée en param triée en utilisant le fonctions codés pour les arbres binaires
*)
let rec tri_abr (l : int list) : int list =
  list_of_arbre_bin (arbre_bin_rech_of_int_list l)
;;

(**
3.Evaluation d'expressions arithmétiques
*)
type binop =
| Plus
| Moins 
| Mult
;;

type expr = 
| Cst of int
| Binop of binop * expr * expr
;;

(**
Prend en argument une expression et la transforme en une chaîne de caractères
@param exp est une expression
@return une chaine de caracteres
*)
let rec string_of_expr (exp : expr) : string =
  match exp with
  | Cst(v) -> string_of_int v
  | Binop(op, e1, e2) -> "( " ^ (string_of_expr e1) ^ (string_of_binop op) ^ (string_of_expr e2) ^ " )"
  
  and string_of_binop (op : binop) : string =
    match op with
    | Plus -> "+"
    | Moins -> "-"
    | Mult -> "*"
;;

(**
Prend en argument une expression et renvoie le resultat de son evaluation
@param e est une expression
@return l'evaluation de l'expression passée en param
*)
let rec eval_expr (e : expr) : int = 
  match e with 
  | Cst (v) -> v 
  | Binop(op, v1, v2) -> 
    match op with
    | Plus -> eval_expr v1 + eval_expr v2
    | Moins -> eval_expr v1 - eval_expr v2
    | Mult -> eval_expr v1 * eval_expr v2
  ;;


  type binop =
  | Plus
  | Moins 
  | Mult
  | Div
  ;;
  
  let rec eval_expr (e : expr) : resultat = 
    match e with 
    | Cst (v) -> Ok v 
    | Binop(op, v1, v2) -> 
      match op with
      | Plus -> (match (eval_expr v1, eval_expr v2) with 
                  | (Ok x, Ok y) -> Ok (x + y)
                  | _ -> ErrDivZero)
      | Moins -> (match (eval_expr v1, eval_expr v2) with 
                  | (Ok x, Ok y) -> Ok (x - y)
                  | _ -> ErrDivZero)
      | Mult ->  (match (eval_expr v1, eval_expr v2) with 
                  | (Ok x, Ok y) -> Ok (x * y)
                  | _ -> ErrDivZero)
      | Div -> (match (eval_expr v1, eval_expr v2) with
                |(_, Ok 0) -> Err DivZero
                |(Ok x, Ok y) -> Ok (x / y)
                | _ -> ErrDivZero)
    ;;

type eval_err = DivZero;;
type resultat =
| Ok of int
| Err of eval_err
  ;;

  (**
  3.3 Variables    
  *)

  type expr =
  | Cst of int
  | Binop of binop * expr * expr
  | Var of string
;;

type eval_err =
| DivZero
| VarNonDef
;;

let rec eval_expr (e : expr) ( l : (string * int) list) : resultat = 
  match e with 
  | Cst (v) -> Ok v 
  | Var (v) -> 
  | Binop(op, v1, v2) ->
    match op with
    | Plus -> (match (eval_expr v1, eval_expr v2) with 
                | (Ok x, Ok y) -> Ok (x + y)
                | _ -> ErrDivZero)
    | Moins -> (match (eval_expr v1, eval_expr v2) with 
                | (Ok x, Ok y) -> Ok (x - y)
                | _ -> ErrDivZero)
    | Mult ->  (match (eval_expr v1, eval_expr v2) with 
                | (Ok x, Ok y) -> Ok (x * y)
                | _ -> ErrDivZero)
    | Div -> (match (eval_expr v1, eval_expr v2) with
              |(_, Ok 0) -> Err DivZero
              |(Ok x, Ok y) -> Ok (x / y)
              | _ -> ErrDivZero)
  ;;