(**********************************************************************)
(* 1 Arbre n-aires: recodage *)
(**********************************************************************)

(**********************************************************************)
(* 1.1. Recodage de quelques fonctions de base avec la bibliothèque
        standard OCaml *)

        type 'a arbre_n = Feuille of 'a | Noeud of 'a arbre_n list

        (**
        Calcule la hauteur d'un arbre.
        
        @param a l'arbre dont on veut calculer la hauteur
        @return la hauteur de l'arbre
        *)
        let rec hauteur_arbre (a : 'a arbre_n) : int =
          match a with
          | Feuille _ -> 1
          | Noeud foret ->
              let hauteurs = List.map hauteur_arbre foret in
              let h_max = List.fold_left max 0 hauteurs in
              h_max + 1
        
        (**
        Extrait les éléments d'un arbre dans une liste 
        
        @param l'arbre dont on veut les éléments
        @return la liste des éléments de l'arbre obtenue par un parcours en profondeur.
        *)
        let list_of_arbre : 'a arbre_n -> 'a list =
          (*
             Ajoute les éléments de l'arbre à la liste.
        
             @param a l'arbre dont on veut extraires les éléments
             @param acc la liste à laquelle on veut ajouter les éléments.
          *)
          let rec list_of_arbre_aux (a : 'a arbre_n) (acc : 'a list) :
              'a list =
            match a with
            | Feuille x -> x :: acc
            | Noeud foret ->
                List.fold_left
                  (fun acc2 fils -> list_of_arbre_aux fils acc2)
                  acc foret
          in
          fun a -> list_of_arbre_aux a []
        
        (**********************************************************************)
        (* 1.2. Gestion d'option, fold et minimum *)
        
        (**
        [lift_option_2 f] choisi son deuxième argument si son premier argument est None.
        Sinon utilise f pour produire une valeur avec ses deux arguments.
        
        @param f la fonction utilisée pour combiner les arguments
        @param x une option 
        @param y la valeur à combiner à la valeur de x ou à prendre si x est None
        @return Some de la combinaison des valeurs de x et y, ou bien y si x est None
        *)
        let lift_option_2 (f : 'a -> 'a -> 'a) :
            'a option -> 'a -> 'a option =
         fun x y ->
          match x with None -> Some y | Some x' -> Some (f x' y)
        
        (**
          aggrège une valeur en utilisant un accumulateur et une fonction appelée pour
          mettre à jour l'accumulateur en fonction de l'élément de l'arbre rencontrée.
          Appelle la fonction en utilisant les un après les autres tous les éléments de
          l'accumulateur.    
        
          @param f la fonction de mise à jour de l'accumulateur
          @param init la valeur de départ de l'accumulateur
          @param a l'arbre à parcourir
          @return la valeur de l'accumulateur résultant des mises à jour
                  successives par les appels à f sur les éléments de a.
          *)
        let rec fold_left_arbre (f : 'b -> 'a -> 'b) (init : 'b)
            (a : 'a arbre_n) : 'b =
          match a with
          | Feuille x -> f init x
          | Noeud foret -> List.fold_left (fold_left_arbre f) init foret
        (* Pour le dernier cas on aurait pu écrire
        
           List.fold_left (fun acc fils -> fold_left_arbre f acc fils) init foret
        
           mais c'est plus long
        *)
        
        (**
        Aggrège une valeur en utilisant une fonction de combinaison de valeurs
        
        @param f la fonction de combinaison de valeurs
        @param a l'arbre dont on veut combiner les valeurs
        @return Some du résultat de la combinaisons des valeurs de a par f, 
                ou None si a n'a pas d'élément
        *)
        let reduce (f : 'a -> 'a -> 'a) (a : 'a arbre_n) : 'a option =
          fold_left_arbre (lift_option_2 f) None a
        
        (**
        Extrait les éléments d'un arbre dans une liste 
        
        @param l'arbre dont on veut les éléments
        @return la liste des éléments de l'arbre obtenue par un parcours en profondeur.
        *)
        let list_of_arbre' : 'a arbre_n -> 'a list =
          fold_left_arbre (fun l e -> e :: l) []
        