(* Question 1 *)
type rgb  = Red | Green | Blue

type colour =
    Black
  | White
  | Primary of rgb

let plus (x:int) (y:int) : int = x + y
let mult (x:int) (y:int) : int = x * y

let _ = Blue
let _ = (fun x -> Primary x)
let _ = Some 8
let _ = mult 4
let _ = [(plus,false);(mult,true)]

(*
- : rgb = Blue
- : rgb -> colour = <fun>
- : int option = Some 8
- : int -> int = <fun>
- : ((int -> int -> int) * bool) list = [(<fun>, false); (<fun>, true)]
 *)
      
(* Question 2 *)
let x = ref 1
let y = 2
let aa (w:int) : int * int * int =
  let x = ref 20 in
  let z = (!x + y) in
  (!x,w,z)
let (v1,v2,v3) = aa !x

(*
Notez que la question ne demande que les valeurs. La réponse ci-dessous
montre également les types.

Notez que OCaml renvoie ce qui suit (ce qui est correct):
val x: int ref = {contenus = 1}

La réponse ci-dessous est la réponse appris en classe:
val x: int ref = ref 1

Les deux sont équivalents:

# let _ = ref 1;;
- : int ref = {contents = 1}

val y : int = 2
val aa : int -> int * int * int = <fun>
val v1 : int = 20
val v2 : int = 1
val v3 : int = 22
 *)

(* Question 3 *)
(* 3a Vrai (Nous avons fait un exemple en classes en utilisant presque exactement la même grammaire.) *)
(* 3b Faux *)
(* 3c Vrai *)
(* 3d Vrai *)
let _ =
  let p = (2,3) in
    let (x,y) = p in (y,x)

let _ =
  let p = (2,3) in
    match p with (x,y) -> (y,x)
(* 3e français: Faux (c'est la phase d'analyse lexicale *)
(* 3e English: False (only pure functional langauges pass the test) *)
(* 3f False (seules les variables liées peuvent être renommées) *)

(* Question 4 *)
let test (x:int) (y:int) : bool = x > y
let rec combine (f:int -> int -> bool)
                (xs: int list) (ys: int list) : int list =
  match xs,ys with
  | [],l -> l
  | l,[] -> l
  | (h1::t1),(h2::t2) -> match (f h1 h2) with
                         | true -> h1::(combine f t1 (h2::t2))
                         | false -> h2::(combine f (h1::t1) t2)

let result = combine test [1;4;7] [2;0;5;7]
(* (b)
   val result : int list = [2; 3; 5; 7; 1; 4; 7]
 *)

(* Question 6 *)
type my_intlist = 
    My_nil
  | My_one of int
  | My_app of my_intlist * my_intlist

let ml1 = My_app (My_one 1, My_app (My_one 2, My_nil))
let ml2 = My_app (My_one 1, My_one 2)


let rec my_intlist_to_list l =
  match l with
  | My_nil -> []
  | My_one n -> [n]
  | My_app (l1,l2) -> my_intlist_to_list l1 @ my_intlist_to_list l2

(* val my_intlist_to_list : my_intlist -> int list = <fun> *)

let ml3 = My_app (My_nil, ml2)
let ml4 = My_app (My_one 8, ml3)
let x = my_intlist_to_list ml4

(* val x : int list = [8; 1; 2] *)
