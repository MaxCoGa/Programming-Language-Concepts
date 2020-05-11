(* Question 1 *)
(* Rappelez les combinateurs de tube (« pipe ») et les combinateurs de
   paires suivants: *)

let (|>) x f = f x 
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
   (*match t with |(x,y) -> (do_snd double x, do_snd double y);;*)
let double x = 2. *. x
let min (p:float*float) =
  let (x,y) = p in
  if x < y then x else y
let max (p:float*float) =
  let (x,y) = p in
  if x > y then x else y
let average (p:float*float) =
  let (x,y) = p in (x +. y) /. 2.0

(* 1(b). Quel est le type de votre fonction? *)


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

   2(b) Quelle opération est calculée par cette fonction?  *)


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

(* 3(a). Notez les types d'objets "p" et "cp". Est-ce que l'un des types est un sous-type de
    l'autre? Sinon, pourquoi pas? Si oui, lequel est le sous-type et lequel est le type?

   Quelle sonts les valeurs des expressions suivantes?
   3(b). double_and_move p;;
   3(c). double_and_move cp;;
   3(d). has_primary_colour p;;
   3(e). has_primary_colour cp;;
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

   4(b). Quel est la valeur de e4_b? Quelle exception est levée (le
   cas échéant) et où se trouve la gestionnaire qui traite cette
   exception?

   4(c). Quel est la valeur de e4_c? Quelle exception est levée (le
   cas échéant) et où se trouve la gestionnaire qui traite cette
   exception? *)


(* Question 5. *)
(* Rappelez les grammaires qui définissent les prédicats, les
   booléens, les expressions et les instructions de programme.

   P ::= B | P and P | P or P | not P | P => P
   B ::= true | false | E = E | E <> E | E > E | E < E |
         E <= E | E >= E | ...
   E ::= x | n | E + E | E – E | E * E | E / E | E! | ...
   S ::= x := E | S;S | if B then S else S |
         while B do S end

   5(a). Trouvez un programme (en utilisant la grammaire S) tel que le
   triplet de Hoare suivant peut être prouvé en utilisant les règles
   d'inférence de la logique de Hoare:

   { true } if a>0 then b<x+y else b=x-2 { ( a>0 => b<x+y ) and ((not(a>0)) => b=x-2) }

   5(b). Exercice à faire à la maison: démontrez votre triplet de
   Hoare à l’aide de votre invariant et les règles d'inférence de la
   logique de Hoare. *)

  
(* Question 6. *)
(* Considérez le programme et le triplet de Hoare suivants.

   { y > 0 }
   a := x;
   b := 0;
   while b <> y do
        a := a-1;
        b := b+1
   end
   { a = x-y }

   6(a). Trouvez un invariant de la boucle dans le programme ci-dessus
   qui peut être utilisé pour prouver que le triplet de Hoare
   ci-dessus est vrai. (Astuce: utilisez la méthode que nous avons
   utilisée en classe pour trouver un invariant. Un exemple de cette
   méthode est trouvé à la page 10 des solutions du laboratoire 4.)

   6(b). Exercice à faire à la maison: démontrez votre triplet de
   Hoare à l’aide des règles d'inférence de la logique de Hoare et
   votre invariant. *)

  
(* Question 5. *)
(* En utilisant la structure de données ci-dessous, écrivez une
   fonction qui prend une entier x et un "int tree" t commes arguments
   et renvoie la valeur de feuille qui est le plus proche en valeur
   absolue de x. *)

type 'a tree = 
  | Leaf of 'a
  | Node of 'a tree * 'a tree


(* Question 6. *)
(* Solution: consultez le fichier PDF séparé *)

          
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
    val empty : (* quel type ? *)

    (* Renvoie sous forme d'option la valeur associée à l'entier
       fourni. Si l'entier n'est pas dans le dictionnaire, renvoyez
       None. *)
    val lookup : (* quel type ? *)

    (* Enleve le nombre entier donné (deuxième argument) et sa valeur
       associée du dictionnaire. Si l'entier n'est pas dans le
       dictionnaire, renvoyez le dictionnaire d'origine (premier
       argument). *)
    val remove :  (* quel type ? *)
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

   8(b). Ignorez la première ligne et dessiner la pile d'activation
   pour l'exécution de ce code.  Commencez par la déclaration du
   fonction "counter". *)
