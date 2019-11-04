(* Objectif: 
   - le polymorphisme
   - plus de pratique sur la récursivité, les fonctions d'ordre supérieur et les options *)


(**************)
(* PROBLÈME 1 *)
(**************)
(* Les fonctions ci-dessous sont des solutions possibles aux
   questions 4 et 6 du laboratoire 2. Quel est le modèle? Comment
   pouvons-nous factoriser un code similaire? Votre tâche consiste à
   factoriser le code commun en implémentant une fonction d'ordre
   supérieur pour les opérations binaires sur les options telle que:
 * Si les deux arguments sont présents, appliquez l'opération.
 * Si les deux arguments sont None, renvoyez None.
 * Si un argument est (Some x) et l'autre argument est None,
   la fonction devrait renvoyer (Some x) *)

let min_option (x: int option) (y: int option) : int option = 
  match x with
  | Some a -> (match y with
               | Some b -> if a < b then x else y
               | None -> x)
  | None -> y

let max_option (x: int option) (y: int option) : int option = 
  match x with
  | Some a -> (match y with
               | Some b -> if a < b then y else x
               | None -> x)
  | None -> y

let and_option (x:bool option) (y: bool option) : bool option = 
  match x with
  | Some a -> (match y with
               | Some b -> if a then y else Some false
               | None -> x)
  | None -> y
           
let or_option (x:bool option) (y: bool option) : bool option = 
  match x with
  | Some a -> (match y with
               | Some b -> if a then Some true else y
               | None -> x)
  | None -> y

(*
let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
*)
(*calc_option max_option (Some(Some 3)) (Some(Some 2));;
calc_option min_option (Some(Some 3)) (Some(Some 2));;
calc_option and_option (Some(Some true)) (Some(Some false));;
calc_option or_option (Some(Some true)) (Some(Some false));;
calc_option or_option (Some(Some true)) None;;*)
let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option = 
  match x, y with
    | None, None -> None
    | None, a | a, None -> a
    | Some a, Some b -> Some (f a b)

(**************)
(* PROBLÈME 2 *)
(**************)
(* Maintenant, réécrivez les fonctions suivantes en utilisant la
   fonction calc_option ci-dessus. *)

(*
let min_option2 (x: int option) (y: int option) : int option = 

let max_option2 (x: int option) (y: int option) : int option = 

let and_option2 (x:bool option) (y: bool option) : bool option = 

let or_option2 (x:bool option) (y: bool option) : bool option = 
*)
(*min_option2 (Some 2) (Some 3);;
max_option2 (Some 2) (Some 3);;
and_option2 (Some true) (Some false);;
or_option2 (Some true) (Some false);;*) (*??use of min/max??*)
let min_option2 (x: int option) (y: int option) : int option = calc_option min x y;; 

let max_option2 (x: int option) (y: int option) : int option = calc_option max x y;; 
  
let and_option2 (x:bool option) (y: bool option) : bool option = calc_option (&&) x y;; 
  
let or_option2 (x:bool option) (y: bool option) : bool option = calc_option (||) x y ;;


(**************)
(* PROBLÈME 3 *)
(**************)
(* Problème 3a. Implementez une fonction qui prend une liste de
   valeurs booléennes [x1; x2; ...; xn] et renvoie
   x1 AND x2 AND ... AND xn.
   Supposez que la valeur de (and_list []) soit TRUE. *)

(*
let rec and_list (lst: bool list) : bool =
 *)
 (*and_list [true; true];; -> true*)
 let rec and_list (lst: bool list) : bool =
  match lst with
    [] -> true
  | hd::tl -> (&&) hd (and_list tl);;

(* Problème 3b. Faites la même chose que ci-dessus pour OR.
   Supposez que la valeur de (or_list []) soit FALSE. *)

(* 
let rec or_list (lst: bool list) : bool = 
 *)
 (*or_list [false; false];; -> false*)
 let rec or_list (lst: bool list) : bool =
  match lst with
    [] -> false
  | hd::tl -> (||) hd (or_list tl);;

(**************)
(* PROBLÈME 4 *)
(**************)
(* Ecrivez une fonction récursive qui renvoie le maximum d'une liste
   d'entiers et qui renvoie None si la liste est vide. Vous pouvez
   utiliser max_option ou max_option2 ci-dessus, mais ce n'est pas
   obligatoire. *)

(*
let rec max_of_list (lst:int list) : int option =
 *)
 (*max_of_list [3;2;6;4;22;10;0];;*)
 let rec max_of_list (lst: int list) : int option =
  match lst with
    [] -> None
  | hd::tl -> max_option2 (Some hd) (max_of_list tl);;


(**************)
(* PROBLÈME 5 *)
(**************)
(* Dans les exercices suivants, nous utiliserons "map" (comme nous
   l'avons vu en classe) pour implémenter certaines fonctions. *)

let rec map (f:'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)

(* Problème 5a. Ecrivez une fonction qui prend comme argument une
   liste d’entiers et qui multiplie chaque entier par 3. Utilisez
   "map". *)
(*
let times_3 (lst: int list): int list = 
 *)
 (*times_3 [3;2;6;4;22;10;0];;*)

 let times_3 (lst: int list): int list = 
  map (fun x -> x * 3) lst

(* Problème 5b. Ecrivez une fonction qui prend comme arguments une
   liste d’entiers et un entier et qui multiplie chaque élément de la
   liste par l’entier. Utilisez "map". *)
(* let times_x (x: int) (lst: int list) : int list =
 *)
 (*times_x 10 [3;2;6;4;22;10;0];;*)

 let times_x (x: int) (lst: int list) : int list =
  map (fun a -> a * x) lst

(* Problème 5c. Réécrivez times_3 en utilisant times_x. Cela devrait
   nécessiter très peu de code. *)
(*            
let times_3_shorter =
 *)
 (*times_3_shorter [3;2;6;4;22;10;0];;*)
 let times_3_shorter =
  times_x 3


(**************)
(* PROBLÈME 6 *)
(**************)
(* Considérez la fonction d'ordre supérieur suivante appelée
   "reduce". *)

let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
  | [] -> u
  | hd::tl -> f hd (reduce f u tl);;

(* Considérez les fonctions suivantes définies en utilisant "reduce" *)
(* sum [3;2;6;];; prod [3;2;6;];;*)
let sum xs = reduce (fun x y -> x+y) 0 xs
let prod xs = reduce (fun x y -> x*y) 1 xs

(* Que font les fonctions "sum" et "prod"? Que fait la fonction
   "reduce"?  Tracez le code suivant pour vous aider à
   comprendre. Montrez votre trace et donnez votre explication ici:

RÉPONSE ICI 
la fonction reduce reduit une liste donnée a partir d'une fonction et d'une premiere valeur(u), la reduction se fait en utilisant la fonction, le head et une recursion sur reduce avec le reste de la liste moins le head;;
sum & prod prend en argument une liste et utilise en ordre supérieure reduce pour effectuer sa somme ou le produit

 *)

let mysum = sum [2;5;6]
let myprod = prod [2;5;6]

(*test*)
let _ = 
  calc_option max_option (Some(Some 3)) (Some(Some 2));;
  calc_option min_option (Some(Some 3)) (Some(Some 2));;
  calc_option and_option (Some(Some true)) (Some(Some false));;
  calc_option or_option (Some(Some true)) (Some(Some false));;
  calc_option or_option (Some(Some true)) None;;

  min_option2 (Some 2) (Some 3);;
  max_option2 (Some 2) (Some 3);;
  and_option2 (Some true) (Some false);;
  or_option2 (Some true) (Some false);;

  and_list [true; true];;
  or_list [false; false];;

  max_of_list [3;2;6;4;22;10;0];;

  times_3 [3;2;6;4;22;10;0];;
  times_x 10 [3;2;6;4;22;10;0];;