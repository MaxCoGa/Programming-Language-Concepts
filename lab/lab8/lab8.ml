(*
  Exceptions et Continuations
 *)

(* QUESTION 1. Exceptions et récursion *)
(* Considérez les types d’étapes d’exécution ci-dessous.  Vous les
   utiliserez pour indiquer les étapes d'exécution du code
   ci-dessous.
   - appel de fonction (avec argument)
   - retour de la fonction (avec valeur de retour)
   - lever une exception
   - gérer une exception
   - dépiler un enregistrement d'activation d'un appel à une fonction
     sans retourner contrôle à la fonction

   La dernière étape doit être utilisée quand une fonction f appelle
   une fonction g (qui pourrait être un appel récursif à f), et g lève
   une exception, et f ne gère pas cette exception. Dans ce cas,
   l'enregistrement d'activation de f est dépiler sans rendre le
   contrôle à f.

   Considérez la fonction OCaml f1 suivante qui utilise une exception
   appelée Odd. *)

exception Odd
let rec f1 (x:int) =
  match x with
  | 0 -> 1
  | 1 -> raise Odd
  | 3 -> f1 (3-2)
  | _ -> try f1 (x-2) with | Odd -> (-x)

(* Indiquez la séquence des étapes pour l’appel de fonction (f1 11). Le
   premier est "appel (f1 11)" et le second est "appel (f1 9)".
   Donnez la liste du reste des étapes à partir d'ici. *)


(* QUESTION 2. Exceptions et gestion de la mémoire *)
(* Les deux versions suivantes de la fonction "closest" prennent un
   entier x et un arbre t comme arguments et renvoient la valeur de la
   feuille qui est le plus proche de x en valeur absolue. La première
   fonction est une fonction récursive simple et la seconde utilise
   des exceptions. Les deux sont appliquées au même exemple.  Cet
   exemple est un arbre simple.

   Dessinez les piles d'activation pour l'exécution de chacun de ces
   programmes. Dans vos enregistrements d’activation, incluez les
   liens d’accès, les paramètres, les variables locales (y compris les
   gestionnaires d'exceptions) et les adresses de valeurs de
   retour. Également incluez les résultats intermédiaires pour (abs
   (x-lf)) et (abs (x-rt)) dans les appels à la fonction "closest".

   Si des enregistrements d'activation doivent être dépilés de la
   pile, ne les effacez pas.  Au lieu de les effacer, marquez-les
   comme dépilés et continuez en dessous.

   Expliquez pourquoi la deuxième fonction est parfois plus efficace
   que le première fonction. *)

type 'a tree =
  | Leaf of 'a
  | Nd of 'a tree * 'a tree

let _ =
  let tree1 = Nd (Leaf 1,Leaf 2) in
  let rec closest (x:int) (t:int tree) =
    match t with
    | Leaf y -> y
    | Nd (y,z) ->
       let lf = closest x y in
       let rt = closest x z in
       if abs (x-lf) < abs (x-rt) then lf else rt
  in closest 1 tree1

exception Found of int
let _ =
  let tree1 = Nd (Leaf 1,Leaf 2) in
  let rec closest (x:int) (t:int tree) =
    match t with
    | Leaf y -> if x=y then raise (Found x) else y
    | Nd (y,z) ->
       let lf = closest x y in
       let rt = closest x z in
       if abs (x-lf) < abs (x-rt) then lf else rt
  in try closest 1 tree1 with | Found n -> n


(* QUESTION 3. Continuations et exceptions *)
(* Considérez les fonctions OCaml suivantes, qui utilisent une
   continuation pour le calcul normal et une continuation pour le
   calcul d'erreur. Écrivez une nouvelle version de cette fonction qui
   fait le même calcul qui n'utilise aucune continuation et qui
   utilise une exception au lieu d'une continuation pour la condition
   d'erreur. *)

let f (x:int) (normal_cont:int->int) (exception_cont:unit -> int) =
  if x < 0
  then exception_cont()
  else normal_cont (x/2)
let g (y:int) = f y (fun z -> 1+z) (fun () -> 0)
