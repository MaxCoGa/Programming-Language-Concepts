(* Question 1 *)

let avg3 x y z = (x +. y +. z) /. 3.

let f x = match x with | true -> x
                       | false -> x

let g x y = match y with | true -> x
                         | false -> x

let q1a = "CSI " ^ string_of_int 3520

let q1b = [true;false;(true && false)]

let q1c = ([None;Some false],[3.0;4.0])

(* let q1d = [3;3.0;"3"] *)

let q1e = avg3

let q1f = f

let q1g = g

let q1h = g 3

(* 
val q1a : string = "CSI 3520"
val q1b : bool list = [true; false; false]
val q1c : bool option list * float list = ([None; Some false], [3.; 4.])
let q1d = [3;3.0;"3"]
               ^^^
Error: This expression has type float but an expression was expected of type
         int
val q1e : float -> float -> float -> float = <fun>
val q1f : bool -> bool = <fun>
val q1g : 'a -> bool -> 'a = <fun>
val q1h : bool -> int = <fun> *)


(* Question 2 *)

let u = false
let x = ref (10.5,true)
let g z =
  let (a,b) = !z in
  let c = a +. 3.0 in
  let d = u && b in
  let _ = z := (c,d) in
  (a,c)
let (j,k) = g x
let _ = !x

let q3b =
  let s1 = "bonjour" in
  let f x = x ^ s1 in
  let s1 = "hello" in
  let s2 = f s1 in
  (s1,s2)

(* val j : float = 10.5
   val k : float = 13.5
   - : float * bool = (13.5, false)

   val q3b : string * string = ("hello", "hellobonjour")
 *)


(* Question 3 *)

type int_exp = Var of string | Int of int
type bool_exp = Equal of int_exp * int_exp
type program = Assign of string * int_exp
             | Seq of program * program
             | IfThen of bool_exp * program
             | IfThenElse of bool_exp * program * program

let int_exp2string x = "x"
let bool_exp2string x = "x"

let rec program2string (p:program) : string =
  match p with
  | Assign (x,e) -> x ^ " := " ^ int_exp2string e
  | Seq (p1,p2) -> program2string p1 ^ "; " ^ program2string p2
  | IfThen (b,p) -> "if " ^ bool_exp2string b ^
                    " then " ^ program2string p
  | IfThenElse (b,p1,p2) -> "if " ^ bool_exp2string b ^
                            " then " ^ program2string p1 ^
                              " else " ^ program2string p2


(* Question 4 *)
                          
module type PAIR = 
  sig
    type 'a pair

    (* init_pair x renvoie la paire (x,x) *)
    val init_pair : 'a -> 'a pair

    (* lookup a n renvoie une option ou le resultat est la valeur de
       la paire situee a l'index n. Les indices valides sont 1 et 2.
       Si l'index est un nombre autre que 1 ou 2, renvoyez None. *)
    val lookup : 'a pair -> int -> 'a option

    (* replace p n m renvoie une paire ou la valeur a la position n
       dans p est remplacee par la valeur m. Si l'index n'est pas
       valide, renvoyez la paire d'entree sans la modifier. *)
    val replace : 'a pair -> int -> 'a -> 'a pair
  end

(* trois réponses possibles sont ci-dessous *)
module PairPair : PAIR =
  struct
    type 'a pair = ('a * 'a)

    let init_pair (x:'a) : 'a pair = (x,x)

    let rec lookup (p:'a pair) (i:int) : 'a option =
      match p with
        (x,y) -> match i with
                 | 1 -> Some x
                 | 2 -> Some y
                 | _ -> None

    let rec replace (p:'a pair) (i:int) (z:'a) : 'a pair =
      match p with
        (x,y) -> match i with
                 | 1 -> (z,y)
                 | 2 -> (x,z)
                 | _ -> (x,y)
  end

module ListPair : PAIR =
  struct
    type 'a pair = 'a list

    let init_pair (x:'a) : 'a pair = [x;x]

    let rec lookup (p:'a pair) (i:int) : 'a option =
      match p with
      | [x;y] -> (match i with
                  | 1 -> Some x
                  | 2 -> Some y
                  | _ -> None)
      | _ -> None

    let rec replace (p:'a pair) (i:int) (z:'a) : 'a pair =
      match p with
        [x;y] -> (match i with
                  | 1 -> [z;y]
                  | 2 -> [x;z]
                  | _ -> [x;y])
      | _ -> p
  end

type 'a mypair =
  Pair of ('a * 'a)

module PairPair : PAIR =
  struct
    type 'a pair = 'a mypair

    let init_pair (x:'a) : 'a pair = Pair (x,x)

    let rec lookup (p:'a pair) (i:int) : 'a option =
      match p with
        Pair (x,y) -> match i with
                      | 1 -> Some x
                      | 2 -> Some y
                      | _ -> None

    let rec replace (p:'a pair) (i:int) (z:'a) : 'a pair =
      match p with
        Pair (x,y) -> match i with
                      | 1 -> Pair (z,y)
                      | 2 -> Pair (x,z)
                      | _ -> Pair (x,y)
  end


module type PAIR = 
  sig
    type 'a pair

    val init_pair : 'a -> 'a pair

    val lookup : 'a pair -> int -> 'a option

    val replace : 'a pair -> int -> 'a -> unit
  end


(* Question 5 *)
(* a) Vrai
   b) Faux (la phase d'analyse syntaxique)
   c) Vrai
   d) Faux (pas toujours, seulement si leurs valeurs ne sont pas modifiées)
   e) Vrai *)

(* Question 6 *)

let rec count (x:'a) (lst: 'a list) : int option =
  match lst with
  | [] -> None
  | h::tl -> match (count x tl) with
            | None -> if x = h then Some 1 else None
            | Some n -> if x = h then Some (n+1) else Some n


