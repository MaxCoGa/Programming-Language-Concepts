(* Si vous avez terminé les questions 1 à 6 de lab3.ml, continuez avec
   les questions de ce fichier pour plus de pratique. *)

let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
  | [] -> u
  | hd::tl -> f hd (reduce f u tl)

let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
  match x with
  | Some a -> (match y with
               | Some b -> Some (f a b)
               | None -> x)
  | None -> y

let min (a:int) (b:int) : int = if a < b then a else b
let max (a:int) (b:int) : int = if a < b then b else a

let min_option2 (x: int option) (y: int option) : int option = 
  calc_option min x y
    
let max_option2 (x: int option) (y: int option) : int option = 
  calc_option max x y
                  
(**************)
(* PROBLÈME 7 *)
(**************)
(* Écrivez une nouvelle version de and_list et or_list de la question
   3, mais cette fois-ci, utilisez "reduce" comme défini ci-dessus. *)

let and_list' (lst: bool list) : bool = 
  reduce (fun x y -> x && y) true lst

let or_list' (lst: bool list) : bool = 
  reduce (fun x y -> x || y) false lst


(**************)
(* PROBLÈME 8 *)
(**************)
(* Implémentez "length" en utilisant "reduce". "length lst" renvoie
   la longueur de "lst". length [] = 0. *)

let length (lst: int list) : int =
  reduce (fun x y -> y + 1) 0 lst

  
(**************)
(* PROBLÈME 9 *)
(**************)
(* Refaites la question 4 en utilisant "reduce". En d'autres termes,
   écrivez une fonction qui renvoie le maximum d'une liste d'entiers
   et qui renvoie None si la liste est vide. *)

let max_of_list' (lst:int list) : int option = 
  reduce (fun x y -> max_option2 (Some x) y) None lst
               

(***************)
(* PROBLÈME 10 *)
(***************)
(* 10. Ecrivez une fonction qui renvoie les valeurs minimale et
   maximale d'une liste ou None si la liste est vide. Vous pouvez
   utiliser "reduce", mais ce n’est pas obligatoire. *)

let bounds (lst:int list) : (int option * int option)  =
  let bounds_aux (x:int) (y:int option * int option) : (int option * int option) =
    let (min_opt,max_opt) = y in
    (min_option2 (Some x) min_opt, max_option2 (Some x) max_opt) in
  reduce bounds_aux (None,None) lst
