(*ordre superieur*)
let rec map f xs =
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
  | Polygon ps -> poly_area ps
  ;;

(*"arbre binaire" type de donnees inductifs*)
type key = int;;
type value = string;;

type tree = 
  Leaf
  | Node of key * value * tree * tree
  ;;