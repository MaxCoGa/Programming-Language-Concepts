(* Question 1 *)
(* Rappelez les combinateurs de tube (« pipe ») et les combinateurs de
   paires suivants: *)

(* let (|>) x f = f x *)
let both   f (x,y) = (f x, f y)
let do_fst f (x,y) = (f x,   y)
let do_snd f (x,y) = (  x, f y)

(* 1(a). Définissez une fonction qui utilise les combinateurs
   ci-dessus et fait le opérations suivantes à son argument
   d'entrée. L'argument doit être une paire de paires où chaque
   élément a le type "float", par exemple: ((75.3,45.2), (88.9,40.0))

    1. doublez le deuxième élément de chaque paire
    2. remplacez la première paire par le minimum de ses deux éléments
    3. remplacez la deuxième paire par le maximum de ses deux éléments
    4. renvoyez la moyenne des éléments qui reste

   Vous pouvez utiliser les fonctions "double", "min", "max" et
   "average" cd-dessous.. *)

let double x = 2. *. x
let min (p:float*float) =
  let (x,y) = p in
  if x < y then x else y
let max (p:float*float) =
  let (x,y) = p in
  if x > y then x else y
let average (p:float*float) =
  let (x,y) = p in (x +. y) /. 2.0

(* Solution à 1(a): *)
let f1 x =
  x |> both (do_snd double)
    |> do_fst min
    |> do_snd max
    |> average

let example = ((75.3,45.2),(88.9,40.0))
let _ = f1 example
(* - : float = 77.65 *)

(* 1(b). Quel est le type de votre fonction? *)
let _ = f1
(* Solution:
- : (float * float) * (float * float) -> float = <fun> *)


(* Question 2 *)
(* Considérez la fonction suivante en Concurrent ML:

let f2 (x:bool) (y:bool) (result:bool ref) =
  spawn (fn () => (if x = true then result := true));
  spawn (fn () => (if y = true then result := true));
  if (x = false && y = false) then result := false *)

(* 2(a). Supposons que chaque test et chaque instruction d'affectation
   est exécuté atomiquement. Décrivez tous les ordres d'évaluation
   possibles des instructions du processus parent et des processus des
   deux enfants. Astuce: commencez par dessiner un diagramme comme
   celui de la page 31 des notes de cours pour le chapitre 14 du
   Mitchell.

Solution: Le diagramme suivant (similaire à celui des notes de cours)
   montre les contraintes sur l’ordre d’exécution de l’évaluation.
   Les cases marquées d'un ** sont exécuté seulement si le test
   évalue à "true".

                    ---------     ---------------
                 ->|x = true?|-->|result:=true **|
                /   ---------     ---------------
 ------------  /    ---------     ---------------
|begin parent|---->|y = true?|-->|result:=true **|
 ------------ \     ---------     ---------------
               \    -----------------------     ----------------
                 ->|x = false && y = false?|-->|result:=false **|
                    -----------------------     ---------------- 

   Les ordres possibles dépendent des valeurs de x et y.
   Par exemple, les ordres possibles quand x=true et y=true incluent:

   x = true; result:= true; y = true; result:=true; x = false && y == false;
   x = true; result:= true; y = true; x = false && y == false; result:=true;
   x = true; result:= true; x = false && y == false; y = true; result:=true;
   x = true; x = false && y == false; result:= true; y = true; result:=true;
   x = false && y == false; x = true; result:= true; y = true; result:=true;

   ainsi que tous les autres où l'une des deux instructions "result: = true"
   est exécuté après le test "x = true" et l'autre "result: = true" est
   exécuté après le test "y = true"

   2(b) Quelle opération est calculée par cette fonction?
   Solution: booléen "ou"
 *)


(* Question 3 *)
(* Considérez le code OCaml suivant: *)
      
let point (x:float) (y:float) = object
    val mutable x_coord = x
    val mutable y_coord = y
    method get_x = x
    method get_y = y
    method move x y =
      (x_coord <- x;
       y_coord <- y)
  end

type primary_colour = Red | Yellow | Blue
type colour = Primary of primary_colour | Green | Orange | Purple | Garnet | Other of string

let coloured_point (x:float) (y:float) (c:colour) = object
    val mutable x_coord = x
    val mutable y_coord = y
    val mutable colour = c
    method get_x = x
    method get_y = y
    method move x y =
      (x_coord <- x;
       y_coord <- y)
    method get_colour = c
    method change_colour c =
      colour <- c
  end

let p = point 1.0 2.3
(* val p : < get_x : float; get_y : float; move : float -> float -> unit > =
   <obj> *)
let cp = coloured_point 5.2 3.0 (Primary Red)
(* val cp :
  < change_colour : colour -> unit; get_colour : colour;
    get_x : float; get_y : float; move : float -> float -> unit > =
  <obj> *)

let double_and_move p =
  let new_x = (p#get_x) *. 2.0 in
  let new_y = (p#get_y) *. 2.0 in
  (p#move new_x new_y;
   (new_x,new_y))

let has_primary_colour p =
  let c = (p#get_colour) in
  match c with
  | Primary _ -> true
  | _ -> false

(* 3(a). Notez les types d'objets "p" et "cp". Est-ce que l'un des
   types est un sous-type de l'autre? Sinon, pourquoi pas? Si oui,
   lequel est le sous-type et lequel est le type? *)

(* Solution: Le type de "cp" est un sous-type du type "p" parce qu'il
   inclut toutes les mêmes méthodes, ainsi que des méthodes
   supplémentaires. *)

(*    Quelle sonts les valeurs des expressions suivantes?
   3(b). double_and_move p;;
   Solution:
- : float * float = (2., 4.6)
   3(c). double_and_move cp;;
   Solution:
- : float * float = (10.4, 6.) 
   3(d). has_primary_colour p;;
   Solution:
   Error: This expression has type
         < get_x : float; get_y : float; move : float -> float -> unit >
       but an expression was expected of type < get_colour : colour; .. >
       The first object type has no method get_colour 
   3(e). has_primary_colour cp;;
   Solution:
- : bool = true
 *)
       

(* Question 4. *)
(* Considérez le code OCaml suivant: *)

exception Excpt of int

let e4_a = 
  let twice f x = try f (f x) with | Excpt z -> z in
  let pred x = if x = 0 then raise (Excpt x) else x-1 in
  let dumb x = raise (Excpt x) in
  let smart x = try 1 + pred x with | Excpt z -> 1 in
  twice pred 1

let e4_b = 
  let twice f x = try f (f x) with | Excpt z -> z in
  let pred x = if x = 0 then raise (Excpt x) else x-1 in
  let dumb x = raise (Excpt x) in
  let smart x = try 1 + pred x with | Excpt z -> 1 in
  twice dumb 1

let e4_c = 
  let twice f x = try f (f x) with | Excpt z -> z in
  let pred x = if x = 0 then raise (Excpt x) else x-1 in
  let dumb x = raise (Excpt x) in
  let smart x = try 1 + pred x with | Excpt z -> 1 in
  twice smart 0

(* 4(a). Dessinez la pile d'activation pour l'exécution du code qui
   évalue e4_a. Incluez les liens d’accès, les paramètres, les
   variables locales et les gestionnaires d'exception. Représentez des
   fonctions comme des fermetures. Quel est la valeur de e4_a? Quelle
   exception est levée (le cas échéant) et où se trouve la
   gestionnaire qui traite cette exception?
   Solution: consultez le fichier PDF séparé pour la pile d'activation.
   val e4_a: int = 0
   Les étapes de l’évaluation peuvent être résumées comme suit:
   (twice pred 1) -->
   (try pred (pred 1) with | Excpt x -> x) -->
   (try pred 0 with | Excpt x -> x) -->
   raise (Excpt 0) | Excpt x -> x) --> 0   

   4(b). Quel est la valeur de e4_b? Quelle exception est levée (le
   cas échéant) et où se trouve la gestionnaire qui traite cette
   exception?
   Solution:
   val e4_b : int = 1
   Les étapes de l’évaluation peuvent être résumées comme suit:
   (twice dumb 1) -->
   (try dumb (dumb 1) with | Excpt x -> x) -->
   raise (Excpt 1) | Excpt x -> x) --> 1

   4(c). Quel est la valeur de e4_c? Quelle exception est levée (le
   cas échéant) et où se trouve la gestionnaire qui traite cette
   exception?
   Solution:
   val e4_c : int = 1
   Les étapes de l’évaluation peuvent être résumées comme suit:
   (twice smart 1) -->
   (try smart (smart 1) with | Excpt x -> x) -->
   (try smart 0 with | Excpt x -> x) -->
   (try (try 1 + pred 0 with | Excpt x -> 1) with | Excpt x -> x) -->
   1
   Le gestionnaire utilisé est  Excpt x -> 1
 *)

(* Question 5. *)
(* En utilisant la structure de données ci-dessous, écrivez une
   fonction qui prend une entier x et un "int tree" t commes arguments
   et renvoie la valeur de feuille qui est le plus proche en valeur
   absolue de x. *)

type 'a tree = 
  | Leaf of 'a
  | Node of 'a tree * 'a tree

(* Quelques solutions: *)

let rec closest1 x t =
  match t with
  | Leaf y -> y
  | Node (t1,t2) ->
     let left = closest1 x t1 in
     let right = closest1 x t2 in
     if abs (x - left) < abs (x - right) then left else right

exception Found
let closest2 x t =
  let rec closest_aux t =
    match t with
    | Leaf y -> if x = y then raise Found else y
    | Node (t1,t2) ->
       let left = closest_aux t1 in
       let right = closest_aux t2 in
       if abs (x - left) < abs (x - right) then left else right in
  try closest_aux t with | Found -> x


(* Question 6. *)
(* Solution: consultez le fichier PDF séparé pour la question et la
   solution. *)

(* Question 7. *)
(* Vous trouverez ci-dessous une signature incomplète pour une
   structure de données de dictionnaire.  Cette structure contient des
   paires dans lesquelles le premier composant est un entier et le
   deuxième composant et une valeur associée à cet entier.  La
   signature est suivie d'une implémentation d'un dictionnaire sous
   forme de liste de paires. L'implémentation de cette structure
   contient des informations de type (mais pas toutes) et inclut
   certaines opérations usuelles sur les dictionnaires. En utilisant
   les informations de l'implémentation, complétez la signature DICT
   ci-dessous en complétant les trois types manquants. *)

module type DICT = 
  sig
    type 'a dict

    (* un dictionnaire vide *)
    val empty : unit -> 'a dict

    (* Renvoie sous forme d'option la valeur associée à l'entier
       fourni. Si l'entier n'est pas dans le dictionnaire, renvoyez
       None. *)
    val lookup : 'a dict -> int -> 'a option

    (* Enleve le nombre entier donné (deuxième argument) et sa valeur
       associée du dictionnaire. Si l'entier n'est pas dans le
       dictionnaire, renvoyez le dictionnaire d'origine (premier
       argument). *)
    val remove : 'a dict -> int -> 'a dict
  end

module ListDict : DICT =
  struct
    type 'a dict = (int * 'a) list

    let empty () : 'a dict = []

    let rec lookup (d:'a dict) (k:int) =
      match d with
      | [] -> None
      | (hd_k,hd_v)::tl -> if hd_k = k then Some hd_v
                           else lookup tl k

    let rec remove d k =
      match d with
      | [] -> []
      | (hd_k,hd_v)::tl -> if hd_k = k
                           then tl
                           else (hd_k,hd_v)::remove tl k
  end

(* Question 8. *)
(* Considérez le programme en OCaml suivant: *)

let _ =
  let counter () =
    let c = ref 1 in
    let counter_fun () =
      let v = !c in
      (c := v+1 ; v)
    in counter_fun in
  let countA = counter() in
  let countB = counter() in
  countA()+countB()

(* 8(a). Quelle est la valeur de l'expression ci-dessus?

   Solution:
   - : int = 2

   8(b). Ignorez la première ligne et dessiner la pile d'activation
   pour l'exécution de ce code.  Commencez par la déclaration du
   fonction "counter".
   Solution: consultez le fichier PDF séparé pour la pile d'activation.
 *)
