(* Ces exercices sont suggérés pour la pratique de la programmation et
   pour penser en termes de programmation fonctionnelle. Vous pouvez
   essayer chacun en utilisant l'interpreteur OCaml, mais une fois que
   vous avez la réponse, stockez-le dans ce fichier. Vous pouvez
   ensuite charger tout le fichier en utilisant "#use".  *)

(* 1a. Donnez à x la valeur 42 en ajoutant 22 à 20 *)
(* 
let x = 
*) 
let x = 22 + 20;;


(* 1b. Donnez à x1 la valeur 42.0 en effectuant un «cast» sur x. *)
(* Astuce: Consultez la documentation d'OCaml dans le chapitre Pervasives:
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
   et recherchez "float". *)
(* 
let x1 = (* Votre code ici. *)  
*)
let x1 = float_of_int x;; (*float x*)



(* 1c. Écrivez une fonction qui prend une chaîne et ajoute
 * " est mon langage de programmation préféré." à la fin. *)

(* 
let favourizer (arg:string)  = (* Votre code ici *) 
*)
let favourizer (arg:string) = 
    arg ^ " est mon langage de programmation préféré.";;

(* appelez la fonction "favourizer" pour créer une chaîne complète *)
(*
let myfavourite:string = ...
*)
let myfavourite:string = favourizer "C++"


(* 1d. Ecrivez une fonction qui prend un nombre et renvoie
 * la différence entre ce nombre et 42.
 * Par exemple, si «num» est 50, le résultat devrait être 8.
 * Si «num» est 30, le résultat devrait être -12 *)
(* 
let diff_42 num = (* Votre code ici *) 
*)
let diff_42 num =
    num - 42 ;;


(* 1e. Un autre exemple arithmétique simple...
 * Ecrivez une fonction qui renvoie le volume d'un cylindre
 * qui a rayon r, hauteur h. (le volume est pi * r^2 * h) *)
(* 
let pi = 4.0 *. atan 1.0
let volume_cylinder (r:float) (h:float) : float = (* Votre code ici *) 
*)
let pi = 4.0 *. atan 1.0;;
let volume_cylinder (r:float) (h:float) : float = 
    pi *. r**2. *. h;;


(* 1f. Déterminez si un entier est pair. Encore une fois, utilisez Pervasives. *)
(* 
let even (x: int) : bool = (* Votre code ici *)   
*)
let even (x:int) : bool =
    (x mod 2) = 0;;


(* 1g. Déterminez si un entier est impair en appelant votre fonction «even» *)
(* 
let odd (x: int) : bool = (* Votre code ici *)  
*)
let odd (x:int) :  bool = 
    not (even x);;


(* 1h. OCaml a une grande bibliothèque standard qui inclut Pervasives
 * et de nombreuses autres fonctions utilitaires que vous n’aurez pas
 * à écrire vous-même.
 * Par exemple, consultez le module String
 * (http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html)
 * Maintenant ... écrivez une fonction qui prend une chaîne et indique
 * si cette chaîne contient plus de 10 caractères. *)
(* 
let gt_10_chars str : bool = (* Votre code ici. *) 
*)
let gt_10_chars str : bool = 
    String.length str > 10;;



(* 2. Comparez les deux valeurs booléennes suivantes. Pourquoi la différence? *)
(* let why1 = (1.0 = 1.0) *)
(* let why2 = (1.0 == 1.0) *)
(* let because = ""  *)

(* Attention: n'utilisez pas == sauf si vous savez vraiment ce que
 * vous faites
 *)
 let why1 = (1.0 = 1.0);;(*true*)
 let why2 = (1.0 == 1.0);;(*false*)
 let because = "= est pour l'égalité de la structure et == pour l'égalité physique des types";;


(* 3. Calculez le GCD de deux entiers en utilisant la récursivité d'Euclid
 * https://en.wikipedia.org/wiki/Euclidean_algorithm *)

(* 
let gcd (x : int) (y : int) : int = 
*)
let rec gcd (x : int) (y : int) : int = 
    if y = 0 
        then x else gcd y (x mod y);;



(* 4. Calculez la fonction McCarthy 91 comme expliqué dans
 * http://en.wikipedia.org/wiki/McCarthy_91_function
 *)

(* 
let mccarthy (x : int) : int = 
*)
let rec mccarthy (x : int) : int = 
    if x > 100 
        then (x - 10) else mccarthy (mccarthy (x + 11))



(* 5. Exercice avancé facultatif: Calculez la racine carrée de x à
 *  l'aide de l'algorithme de Heron d'Alexandrie (environ 100 après
 * JC). x doit être supérieur à 1.0.

 * Nous commençons par une première (mauvaise) conjecture que la racine
 * carrée est 1.0. Appelons notre hypothèse actuelle g. Nous
 * maintiendrons l'invariant que g^2 est inférieur à x et donc que g
 * est inférieur à la racine carrée de x.

 * Notez: si g est inférieur à la racine carrée de x alors x/g est
 * légèrement supérieur à la racine carrée de x. La vraie racine carrée
 * est alors comprise entre g et x/g.

 * Pour calculer une estimation légèrement meilleure que g, nous pouvons
 * prendre la moyenne de g et x/g:

     g + x/g
     -------
        2

 * Nous pouvons continuer à améliorer nos prévisions en calculant la
 * moyenne encore et encore. Arrêtez le processus lorsque vous êtes assez
 * proche. En particulier, arrêtez-vous lorsque la différence entre g et
 * x/g est inférieure à 0.001, puis calculez une moyenne finale et
 * renvoyez-la.

 *  Vous pouvez trouver plus d'informations sur cette méthode ici:

 * http://www.mathpages.com/home/kmath190.htm

 * Vous pouvez trouver plus d'informations sur Heron of Alexandria ici:

 * http://en.wikipedia.org/wiki/Hero_of_Alexandria
 *)

(* 
let squareRoot (x : float) : float =
*)
let squareRoot (x : float) : float =
    let diff = 0.001 in
    let rec estimation g = 
      let g2 = (g +. (x /. g)) /. 2. in
      if abs_float (g2 -. g) < diff 
        then g2 else estimation g2
    in
    estimation 1.0;;
