(*ordre superieur*)
let rec map (f:fint->int) (xs:int list) =
  match xs with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)
  ;;

(*polymorphe*)
let rec mapP (f:'a -> 'b) (xs:'a list) : 'b list =
  match xs with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)
  ;;


(*algo inductif pour le calcul de l'aire*)
type point = float * float;;
type radius = float;;
type side = float;;

type shape =
  Square of side
    | Ellipse of radius * radius
    | RtTriangle of side * side
    | Polygon of point list
    ;;

let distance (p1:point) (p2:point) : float =
    let square x = x *. x in
    let (x1,y1) = p1 in
    let (x2,y2) = p2 in
    sqrt (square (x2 -. x1) +. square (y2 -. y1))
    ;;

let tri_area (p1:point) (p2:point) (p3:point) : float =
    let a = distance p1 p2 in
    let b = distance p2 p3 in
    let c = distance p3 p1 in
    let s = 0.5 *. (a +. b +. c) in
    sqrt (s *. (s -. a) *. (s -. b) *. (s -. c))
    ;;

let rec poly_area (ps : point list) : float =
  match ps with
    | p1 :: p2 :: p3 :: tail ->
     tri_area p1 p2 p3 +. poly_area (p1::p3::tail)
    | _ -> 0.
    ;;
let pi = 3.14;;
let area (s : shape) : float =
  match s with
  | Square s -> s *. s
  | Ellipse (r1, r2)-> pi *. r1 *. r2
  | RtTriangle (s1, s2) -> s1 *. s2 /. 2.
  | Polygon ps -> poly_area ps (*cas inductif*) 
  ;;

(*"arbre binaire" type de donnees inductifs*)
type key = int;;
type value = string;;

type tree = 
  Leaf
  | Node of key * value * tree * tree
  ;;
let arbre1 = Node(9,"a",Node (3,"b",Leaf , 
                                    Node (7,"de",Leaf,Leaf)), 
                        Node (12,"c",Leaf,Leaf));;

let rec insert (t:tree) (k:key) (v:value) :tree = 
  match t with 
  | Leaf -> Node (k, v, Leaf, Leaf)
  | Node (k', v', left, right) ->  (*cas inductif*)
      if k < k' then Node (k', v', insert left k v, right)
      else if k > k' then Node (k', v',left , insert right k v)
      else Node (k', v', left, right)          
      ;;            





let double (n : int) : int =
  if n < 0 then raise (Failure "negative input!")
  else double_nat n

type nat = Zero | Succ of nat
let rec nat_to_int (n : nat) : int =
match n with
Zero -> 0
| Succ n -> 1 + nat_to_int n
let rec double_nat (n : nat) : nat =
match n with
| Zero -> Zero
| Succ m -> Succ (Succ (double_nat m))

(*type parametres*)
type tree2 = (int,string) tree


(*Pile fonctionelle*)
module type INT_STACK =
  sig
    type t
    val empty : unit -> t
    val push : int -> t -> t
    val is_empty : t -> bool
    val pop : t -> t
    val top : t -> int option
end

module type STACK =
  sig
    type 'a stack
    val empty : unit -> 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val is_empty : 'a stack -> bool
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
end

module ListStack : STACK =
  struct
    type 'a stack = 'a list
    let empty() : 'a stack = []
    let push (x:'a)(s:'a stack) : 'a stack = x::s
    let is_empty(s:'a stack) =
      match s with
      | [] -> true
      | _::_ -> false
    let pop (s:'a stack) : 'a stack =
      match s with
      | [] -> []
      | _::tl -> tl
    let top (s:'a stack) :'a option =
      match s with
      | [] -> None
      | h::_ -> Some h
end
(*Piles impérative*)
module type IMP_STACK =
  sig
    type ‘a stack
    val empty : unit -> 'a stack
    val push : 'a -> 'a stack -> unit
    val pop : 'a stack -> 'a option
end

module ImpStack:IMP_STACK =
  struct
    type ‘a stack = ('a list) 
    
    let empty() : 'a stack = ref []

    let push (x:'a) (s:'a stack) : unit = s := x::(!s)

    let pop (s:'a stack) : 'a option =
      match !s with
      | [] -> None
      | h::t -> (s := t ; Some h)
end