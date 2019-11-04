(* Quelques solutions *)
(* Question de révision 1: les références *)

type point = int ref * int ref

let f (p1:point) (p2:point) : int =
  match (p1,p2) with
  | (x1,y1),(x2,y2) ->
              (x1 := 17;
               let z = !x1 in
               x2 := 42;
               z)

let point1 = (ref 3, ref 4)
let point2 = (ref 8, ref 6)

let a = f point1 point2
let b = match point1 with
    (x,y) -> (!x,!y)
let c = match point2 with
    (x,y) -> (!x,!y)

let a' = f point1 point1
let b' = match point1 with
    (x,y) -> (!x,!y)

(* Quelles sont les valeurs de a, b, c, a', et b'? *)
(* 
val a : int = 17
val b : int * int = (17, 4)
val c : int * int = (42, 6)
val a' : int = 17
val b' : int * int = (42, 4)
 *)

(* Question 2: types de données inductifs *)

type tree = 
  | Empty
  | Node of tree * int * tree

(* fonction auxiliaire pour la construction d'arbres *)
let leaf (i:int) : tree =
  Node(Empty, i, Empty)

let t1 = 
  Node(Node(leaf 2, 1, Empty),
       0,
       Node(leaf 7, 6, leaf 8))

(* Question 2a. Dessinez l'arbre représenté par t1. *)
(* Question 2b. Que fait la fonction suivante? *)

let fun_2b (t:tree) : bool =
  begin match t with
  | Node(Empty,_,Empty) -> true
  | _ -> false
  end

(* Question 2c. Quelle est la valeur de l1 ci-dessous?
   Que fait la fonction fun_2c? *)

let rec fun_2c (t:tree) : int list =
  begin match t with
  | Empty -> []
  | Node(l,n,r) -> n::(fun_2c l)@(fun_2c r)
  end

let l1 = fun_2c t1
(* val l1 : int list = [0; 1; 2; 6; 7; 8] *)

(* Question 3: Programmation d'ordre supérieur et polymorphisme *)

let add (x:int) (y:int) : int = x + y
let mult (x:int) (y:int) : int = x * y
           
let rec fun3 x n =
  match n with
  | 0 -> []
  | n -> if n < 0 then []
         else x::(fun3 x (n-1))

(* Question 3a. Que fait la fonction fun3? *)
(* Question 3b. Quel est le type de la fonction fun3? *)
(* val fun3 : 'a -> int -> 'a list = <fun> *)

let a3 = fun3 true 4
let b3 = fun3 add 2
let c3 = fun3 (add 3) 2
let d3 = fun3 (fun3 false 3) 2

(* Question 3c. Quels sont les types et les valeurs de a3, b3, c3, et d3? *)
(*
val a3 : bool list = [true; true; true; true]
val b3 : (int -> int -> int) list = [<fun>; <fun>]
val c3 : (int -> int) list = [<fun>; <fun>]
val d3 : bool list list = [[false; false; false]; [false; false; false]]
 *)
                   
let e3 = (1,false)
let f3 = [(1,false);(2,true)]
let g3 = ([1;2],[false;true])
let h3 = fun3 4.3
let i3 = fun x -> x < 5
let j3 = [add;mult]

(* Question 3d.  Quels sont les types de e3, f3, g3, h3, i3, j3? *)
(*
val e3 : int * bool = (1, false)
val f3 : (int * bool) list = [(1, false); (2, true)]
val g3 : int list * bool list = ([1; 2], [false; true])
val h3 : int -> float list = <fun>
val i3 : int -> bool = <fun>
val j3 : (int -> int -> int) list = [<fun>; <fun>]
 *)

(* Question 6: Fonctions anonymes
Quelle est la valeur de x6 ? *)
       
let rec guess (f:int -> int -> int) (l:int list) : int option =
  match l with
  | [] -> None
  | [a] -> Some a
  | a::b::l' -> guess f ((f a b)::l')

let x6 = guess (fun x -> fun y -> (x * x) + y) [2;3;5]
(* val x6 : int option = Some 54 *)

(* Question 5: Types de données inductifs (nouvel exemple) *)

type ('a,'b) newtype =
    New_const1
  | New_const2 of (('a,'b) newtype * 'a * 'b)

let rec q5_fun (xs:('a,'b) newtype) =
  match xs with
  | New_const1 -> []
  | New_const2 (xs',xa,xb) -> (xa::q5_fun xs')

let q5a = q5_fun (New_const2 (New_const2 (New_const2 (New_const1,3,true),7,false),5,false))

(* Question 5a. Quelle est la valeur de q5a? *)
(* val q5a : int list = [5; 7; 3] *)

(* Question 5b. Quel est le type de q5_fun? *)
(* val q5_fun : ('a, 'b) newtype -> 'a list = <fun> *)

(* Question 5c. Que fait q5_fun? *)
(* Question 5d. Définissez une fonction qui prend une entrée de type
   "(int, float) newtype" et retourne une paire de type "int * float"
   où le premier élément du résultat est la somme de tous les éléments
   de type "int" qui apparaissent dans l'entrée et le deuxième élément
   est le somme de tous les éléments de type "float" qui apparaissent
   dans l'entrée.  *)

let rec q5d_fun (xs:(int,float) newtype) : int * float =
  match xs with
  | New_const1 -> (0,0.0)
  | New_const2 (xs',xa,xb) ->
     let (result_a,result_b) = q5d_fun xs' in
     (xa + result_a, xb +. result_b)
       
(* Question 5e. Écrire une expression de type
   "(int, float) newtype" qui contient 3 entiers et 3 flottants et
   qui peut être utilisé comme entrée à la fonction de
   Question 5d. *)

let q5e = q5d_fun (New_const2 (New_const2 (New_const2 (New_const1,3,3.1),7,5.2),5,8.5))

(* Question 6 Types abstraits de données, données impératives *)

module type DICTIONARY =
  sig
    (* Un 'a dict définit une correspondance entre des chaînes et des
       éléments de' a. Nous écrivons {k1-> v1, k2-> v2, ...} pour le
       dictionnaire dans lequel k1 est associé à v1, k2 à v2, etc. *)
    type key = string
    type 'a dict

    (* Créer un dictionnaire vide. *)
    val make : unit -> 'a dict

    (* Insérer une clé et une valeur dans le dictionnaire *)
    val insert : 'a dict -> key -> 'a -> unit

    (* Renvoyez la valeur associée à une clé dans le dictionnaire.
       Renvoyez None s'il n'y a pas de valeur associée à la clé. *)
    val lookup : 'a dict -> key -> 'a option
        
  end

(* Remplissez l'implantation du module ci-dessous.  Utilisez une liste
   triée pour l'implantation du dictionnaire. *)

module SortedAssocList : DICTIONARY =
  struct
    type key = string
    type 'a dict = (key * 'a) list ref
        
    let make() : 'a dict = ref []

    let insert (d : 'a dict) (k : key) (x : 'a) : unit =
      let rec aux (l:(key * 'a) list) : (key * 'a) list =
        match l with
        | [] -> [(k, x)]
        | (k', x') :: tl ->
           if k = k' then (k, x)::tl
           else if k < k' then (k, x)::l
           else (k', x')::aux tl
      in d := aux !d

    let lookup (d : 'a dict) (k : key) : 'a option =
      let rec aux (l:(key * 'a) list) : 'a option =
        match l with
        | [] -> None
        | (k', x) :: tl ->
           if k = k' then Some x
           else if k < k' then None
           else aux tl in
      aux !d
    end

let d = SortedAssocList.make()
let _ = SortedAssocList.insert d "Sam" 22
let _ = SortedAssocList.insert d "Ada" 19
let _ = SortedAssocList.insert d "Eric" 24
let _ = SortedAssocList.lookup d "Sam"
let _ = SortedAssocList.lookup d "Eric"
let _ = SortedAssocList.lookup d "Ada"
let _ = SortedAssocList.lookup d "Christine"

(* Question 7. Lambda Calcul
Remarque: "lambda" est noté "|"
Utilisez la beta-réduction et trouvez une expression plus courte pour

(|x.|y.xy)(|x.xy)

Solution:
(|x.|y.xy)(|x.xy)
(|w.|z.wz)(|x.xy) =
[(|x.xy)/w](|z.wz) =
|z.((|x.xy)z) =
|z.([z/x](xy)) =
|z.(zy)
 *)

(* Question 8. Analyse syntaxique et priorité

Dessinez des arbres d'analyse pour les expressions suivantes, 
en utilisant la grammaire et la priorité décrites dans la classe:

Multipication ("*") a une priorité plus élevée que l'addition ("+") ou
soustraction ("-").  Tous les 3 opérateurs ont une associativité gauche.
(On dit aussi qu'ils sont des opérateurs « associants à gauche ».)

Utilisez la grammaire suivante pour les expressions:
e ::= n | e+e | e-e | e*e
n ::= 1

8a. 1 - 1 * 1
8b. 1 - 1 + 1
8c. 1 - 1 + 1 - 1 + 1 (pour cette question, supposez que "+" a une priorité
                       plus élevée que "-")

Solutions:
8a.   e        1 - (1 * 1)
     /|\
    e - e
   |    /|\
   n   e * e
   |   |   |
   1   n   n   
       |   |
       1   1   

8b.   e        (1 - 1) + 1
     /|\
    e + e
  /|\    |
 e - e   n
 |   |   |
 n   n   1
 |   |
 1   1   

8c. 1 - 1 + 1 - 1 + 1 (pour cette question, supposez que "+" a une priorité
                       plus élevée que "-")

                             e
                           / | \
                          /  |  \
                         e   -   e
                       /|\      /|\
                      e - e    e + e
                     |   /|\   |   |
                     n  e + e  n   n
                     |  |   |  |   |
                     1  n   n  1   1
                        |   |
                        1   1
 *)

          
