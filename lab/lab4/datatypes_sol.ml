(* Exercice 1 *)
(* Considérez la définition de type de données suivante: *)
type colour = Red | Yellow | Blue | Green | Orange | Purple | Garnet | Other of string
type favourite = Colour of colour | Movie of string | Tvshow of string |
                 Number of float | Letter of char

(* Vous trouverez ci-dessous des exemples de listes de films préférés, couleurs préférés, etc.
 * Vous pouvez considérer chacune de ces listes comme une liste de choses préférées d'une personne.
 * Vous pouvez les utiliser pour tester vos fonctions. *)

let a : favourite list = [Movie "Love Story"; Colour Blue;
                          Tvshow "The Simpsons"; Colour Orange]

let b : favourite list = [Number 1.0; Number 2.0; Number 5.0;
                          Number 14.0; Number 42.0]

let c : favourite list = [Movie "A Beautiful Mind"; Tvshow "House";
                          Letter 'P'; Colour Orange]

let d : favourite list = [Tvshow "Lost"; Number 3.14]

let students = [a; b; c; d]


(* 1a. Définissez une valeur de type "favourite list" pour quelqu'un dont
 * sa couleur préférée est Aubergine et son nombre préféré est 5. *)

let prob1a : favourite list = [Colour (Other "Aubergine"); Number 5.]


(* 1b. Écrivez une fonction qui prend comme argument une valeur de
   type "favourite list" (comme l'exemple ci-dessus) et renvoie soit
   le titre du film préféré de cette personne, soit "None" si un film
   favori n'est pas dans la liste. Si plusieurs films sont dans la
   liste, renvoyez le premier. Quelle est le type de cette fonction?
   (Donnez le type de "favmovie" sous forme de chaîne de caractères
   dans "favmovie_type" ci-dessous.) *)

let favmovie_type = "favourite list -> string option"

let rec favmovie (lst: favourite list) : string option  =
  match lst with
  | [] -> None
  | hd::tl ->
    match hd with
    | Movie m -> Some m
    | _ -> favmovie tl


(* 1c. Écrivez une fonction qui prend une valeur de type "favourite
   list" et renvoie "true" si et seulement si cette personne a inclus
   "Garnet" en tant que couleur préférée. Quelle est le type de cette
   fonction? *)

let uottawa_colours_type = "favourite list -> bool"
let rec uottawa_colours (lst: favourite list) : bool =
  match lst with
  | [] -> false
  | (Colour Garnet)::_ -> true
  | _::tl -> uottawa_colours tl


(* Exercice 2 *)
(* 2a. Définissez un type de données qui représente des "int"s ou des
   "float"s *)

type realnum = 
  | Int of int
  | Float of float


(* 2b. Définissez deux fonctions pour créer des nombres réels à partir
   d'un "int" et d'un "float", respectivement *)

let real_of_int (i:int) : realnum = Int i
let real_of_float (f:float) : realnum = Float f


(* 2c. Définissez une fonction qui teste si deux éléments de "realnum"
   sont égaux. La fonction doit fonctionner sur des "int"s et des
   "float"s, par exemple (realequal 4 4.0) => True. *)

(* remarque: il ne faut vraiment pas comparer les flottants pour
   l'égalité, mais nous le faisons ici pour plus de simplicité *)
(* notez aussi: nous pourrions séparer les cas int/int, où la
   comparaison est "pure" *)

let realequal (a: realnum) (b: realnum) : bool = 
  let float_of_real (x: realnum) : float =
    match x with 
    | Int y -> float_of_int y
    | Float y -> y
  in
  float_of_real a = float_of_real b


(* Exercice 3: *)

(* Vous trouverez ci-dessous la définition d’un type de données pour
   la logique propositionnelle avec connecteurs pour la conjonction,
   (l'opérateur AND / \), disjonction (l'opérateur OR \ /) et
   implication logique (=>). Par exemple, ce qui suit est une formule
   de la logique propositionnelle.

   (p /\ q) => (r \/ s)

   Notez que les chaînes sont utilisées pour représenter les variables
   propositionnelles.
*)

type prop = string

type form =
  | True
  | False
  | Prop of prop 
  | And of form * form
  | Or of form * form
  | Imp of form * form

(* 3a. Représentez la formule ci-dessus, c'est-à-dire (p /\ q) => (r \/ s)
   sous la forme d'une expression de type "form" *)

let form3a : form = Imp(And(Prop "p",Prop "q"),Or(Prop "r",Prop "s"))

                  
(* 3b. Ecrivez une fonction qui prend comme entreé une formule
   booléenne, et renvoie la liste de tous les variables
   propositionnelles uniques qui apparaissent dans la formule.
   *)

let fvars (f:form) : prop list  = 
  let rec alreadySeen (v:prop) (vl: prop list) : bool =
    match vl with
    | [] -> false
    | hd::tl -> if hd=v then true else alreadySeen v tl
  in
  let rec collect_props (f:form) : prop list =
    match f with
    | True -> []
    | False -> []
    | Prop v -> [v]
    | And(x,y) -> collect_props x @ collect_props y
    | Or(x,y) -> collect_props x @ collect_props y
    | Imp(x,y) -> collect_props x @ collect_props y
  in
  let rec remove_duplicates (vl: prop list) : prop list =
    match vl with
    | [] -> []
    | hd::tl -> let tl' = remove_duplicates tl
                in if (alreadySeen hd tl') then tl' else hd::tl'
  in
  remove_duplicates (collect_props f)


(* 3c. Écrivez une fonction qui prend une formule booléenne et teste si
   il est sous forme normale conjonctive (CNF). Une expression CNF est
   représentée par une série d'expressions OR réunis par AND.

   Par exemple, la formule suivante est en forme CNF.

   (x1 \/ x2) /\ (x3 \/ x4 \/ x5) /\ x6

   De plus, chacun des variables (x1, ..., x6) est soit True, soit False,
   soit une variable propositionnelle. x6 peut être considéré comme une
   "expression OR" sans occurrence de OR (cas de base). Des autres cas de
   base sont (1) le cas où il n’y a qu’une formule qui contient un seul 
   "expression OR", et (2) le cas où il n’y a aucune occurrence de /\ ni \/.
   Par exemple, les formules suivantes sont en forme CNF.

   (x3 \/ x4 \/ x5)
   x
   True
   False

   Notez que \/ et /\ sont associatifs, donc par exemple
   (x3 \/ x4 \/ x5) représente ((x3 \/ x4) \/ x5) et est cette formule
   est équivalent à (x3 \/ (x4 \/ x5)).  Cette formule peut donc être
   représentée comme
   soit Or (Or (Prop "x3",Prop "x4"),Prop "x5")
   soit Or (Prop "x3",Or (Prop "x4",Prop "x5"))
*)

let is_cnf (f:form) : bool =
  let rec is_or_series (f:form) : bool =
    match f with
    | True -> true
    | False -> true
    | Prop _ -> true
    | Or(x,y) -> is_or_series x && is_or_series y
    | _ -> false
  in
  let rec is_cnf_aux (f:form) : bool =
    match f with
    | And(x,y) -> (is_cnf_aux x || is_or_series x) && (is_cnf_aux y || is_or_series y)
    | x -> is_or_series x
  in is_cnf_aux f


(* Quelques tests pour 3a et 3b. *)

let c1 = Or (Prop "x1",Prop "x2")
let c2 = Or (Or (Prop "x3",Prop "x4"),Prop "x5")
let c3 = Prop "x2"

let test_is_cnf1 = is_cnf (And (c1,And (c2,c3)))
let test_is_cnf2 = is_cnf (And (And (c1,c2),c3))
let test_is_cnf3 = is_cnf (And (Imp (c1,c2),c3))
let test_is_cnf4 = is_cnf c2
let test_is_cnf5 = is_cnf c3

let test_fvars1 = fvars c2
let test_fvars2 = fvars (And (Imp (c1,c2),c3))
