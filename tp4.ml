(**
Definition du type Arbre n-aires  

Def: Un arbre n-aire est un arbre dans lequel chaque noeud peut avoir un nombre quelconque de fils.
Dans le cadre de cet exercice, on va considérer des arbres dont les données sont stockées uniquement dans les feuilles.
Une forêt est une liste d'arbres.
Un noeud contiendra donc simplement une forêt.

*)
type 'a arbre_n =
| Feuille of 'a
| Noeud of 'a arbre_n list
;; 


(**
1.2. Hauteur d'un arbre  
hauteur_arbre
@param arbre de type 'a arbre_n
@return entier
hauteur foret
@param liste_arbre de type liste 'a arbre_n
@return entier
*)
let rec hauteur_arbre (arbre : 'a arbre_n) : int =
  match arbre with
    | Feuille _ -> 1
    | Noeud l -> 1 + hauteur_foret l
  and
        hauteur_foret (liste_arbre : ('a arbre_n) list) : int =
    match liste_arbre with
    | [] -> 0
    | a::lr -> max ( hauteur_arbre a) (hauteur_foret lr)
  ;;

  (**
  1.3. Elements d'un arbre
  liste_of_arbre_aux
  @param
  @return
  list_of_foret
  @param
  @return    
  *)
  let rec list_of_arbre_aux (arbre : 'a arbre_n) (acc : ('a arbre_n) list) : ('a arbre_n) list =

  and list_of_foret (foret : ('a arbre_n) list) (acc : (('a arbre_n) list) list) : (('a arbre_n) list) list =
