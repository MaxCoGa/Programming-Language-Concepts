(* Si vous avez terminé les questions 1 à 6 de lab3.ml, continuez avec
   les questions de ce fichier pour plus de pratique. *)

let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
  | [] -> u
  | hd::tl -> f hd (reduce f u tl);;

(**************)
(* PROBLÈME 7 *)
(**************)
(* Écrivez une nouvelle version de and_list et or_list de la question
   3, mais cette fois-ci, utilisez "reduce" comme défini ci-dessus. *)

(*           
let and_list' (lst: bool list) : bool = 
           
let or_list' (lst: bool list) : bool = 
 *)
 (*and_list' [true;false];;
 or_list' [true;false];;*)
let and_list' (lst: bool list) : bool = 
  reduce (fun a b -> a && b) true lst

let or_list' (lst: bool list) : bool = 
  reduce (fun a b -> a || b) false lst

(**************)
(* PROBLÈME 8 *)
(**************)
(* Implémentez "length" en utilisant "reduce". "length lst" renvoie
   la longueur de "lst". length [] = 0. *)

(*           
let length (lst: int list) : int =
 *)
 (*length [10;22;313;4;3;4;22;25];;*)
 let length (lst: int list) : int =
  reduce (fun _ count -> count + 1) 0 lst

  
(**************)
(* PROBLÈME 9 *)
(**************)
(* Refaites la question 4 en utilisant "reduce". En d'autres termes,
   écrivez une fonction qui renvoie le maximum d'une liste d'entiers
   et qui renvoie None si la liste est vide. *)

(*
let max_of_list' (lst:int list) : int option = 
 *)


(***************)
(* PROBLÈME 10 *)
(***************)
(* 10. Ecrivez une fonction qui renvoie les valeurs minimale et
   maximale d'une liste ou None si la liste est vide. Vous pouvez
   utiliser "reduce", mais ce n’est pas obligatoire. *)

(* let bounds (lst:int list) : (int option * int option) = *)
