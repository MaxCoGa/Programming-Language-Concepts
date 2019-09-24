(* 

  Plan:
   * tuples et listes
   * options

*)

(* Un employé est un tuple de nom, d'âge et de booléen indiquant le statut de mariage *)
type employee = string * int * bool
                                 
(* 1. Écrivez une fonction qui prend un employé et imprime les informations sous une forme lisible. *)

(*
let print_employee_info t = 
  let test2 (e: string * int * bool) : employee= match e with (x,y,z) -> "nom " ^ x
*) 
let print_employee_info (e: string * int * bool)  = 
     match e with 
     (x,y,z) -> "nom = " ^ x ^ " || " ^"age = " ^ string_of_int y ^ " || " ^ "mariage = " ^ string_of_bool z

(* 2. Réimplémentez les fonctions standard OCaml List.length et List.rev *)

(*
let length (l:int list) : int = 

let rev (l:int list) : int list = 
*)
let rec length (l:int list) : int =
  match l with
  [] -> 0
  | _::l -> 1 + length(l);;

let rec rev (l:int list) : int list =
  match l with 
  [] -> []
  | x::l -> (rev l) @ [x];;


(* 3. Supprimez le k ième élément d'une liste. Supposons l'indexation à partir de 0 *)
(* exemple : rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] 
 * résultats : [["to" ; "be" ; "not" ; "to" ; "be"] *)
(* let rmk (k:int) (l:string list) : string list =  *)
let rmk (k:int) (l:string list) : string list = 
  let rec aux position list acc =
  match list with
  | [] -> acc
  | h::t -> aux (position+1) t (acc@ if position <> k then [h] else [])
  in aux 0 l [];;


(* 4. Écrivez une fonction qui renvoie la plus petite des deux arguments de type "int option",
 * ou None si les deux sont None. Si exactement un argument est None, renvoyez l'autre. Faire
 * le même pour la plus grande des deux options int.*)

(*
let min_option (x: int option) (y: int option) : int option = 
*)
let min_option (x: int option) (y: int option) : int option =
  if (x = None && y = None) then None
  else if (x = None) then y
  else if (y = None) then x
  else min x y;;

(*
let max_option (x: int option) (y: int option) : int option = 
*)
let max_option (x: int option) (y: int option) : int option =
  if (x = None && y = None) then None
  else if (x = None) then y
  else if (y = None) then x
  else max x y;;  

(* 5. Écrivez une fonction qui renvoie l'entier enterré dans l'argument
 * ou None autrement *)  

(* 
let get_option (x: int option option option option) : int option = 
*)
let get_option (x: int option option option option) : int option =
  match x with
  | Some Some Some Some x -> Some x
  | _ -> None;;

(* 6. Écrivez une fonction qui renvoie le booléen AND / OR de deux options bool,
 * ou None si les deux sont None. Si exactement l'un est None, renvoyez l'autre. *)

(*
let and_option (x:bool option) (y: bool option) : bool option = 
*)
let and_option (x:bool option) (y: bool option) : bool option =
  match x, y with
  | None, None -> None
  | Some x, None -> Some x
  | None, Some y -> Some y
  | Some x, Some y -> Some (x&&y);;

(*
let or_option (x:bool option) (y: bool option) : bool option = 
*)
let or_option (x:bool option) (y: bool option) : bool option =
  match x, y with
  | None, None -> None
  | Some x, None -> Some x
  | None, Some y -> Some y
  | Some x, Some y -> Some (x||y);;

(* 7. Écrivez une fonction qui retourne l'élément final d'une liste,
   s'il existe, et None autrement*)
(*
let final (l: int list) : int option = 
*)
let rec final (l: int list) : int option = 
  match l with
  | [] -> None
  | h :: [] -> Some h
  | h :: t -> final t;;


  let me : employee = "Max",20,false;;
  let ll = [2;4;5;12;55;1;24];;
  let lls = ["to" ; "be" ; "or" ; "not" ; "to" ; "be"];;

  (*
  1.
  print_employee_info me

  2.
  length ll;;
  rev ll;

  3.
  rmk 2 lls

  4.
  min_option (Some 4) (Some 3);;
  max_option (Some 4) (Some 3);;


  5.
  get_option (Some (Some (Some (Some 3))));;
  get_option (Some None);;

  6.
  and_option (Some true) (Some false);;
  or_option (Some true) (Some false);;

  7.
  final ll;;
  *)
