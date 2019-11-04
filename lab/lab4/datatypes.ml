(* Exercice 1 *)
(* Considérez la définition de type de données suivante: *)
type colour = Red | Yellow | Blue | Green | Orange | Purple | Garnet | Other of string
type favourite = Colour of colour | Movie of string | Tvshow of string |
                 Number of float | Letter of char

(* Vous trouverez ci-dessous des exemples de listes de films préférés, couleurs préférés, etc.
 * Vous pouvez considérer chacune de ces listes comme une liste de choses préférées d'une personne.
 * Vous peut les utiliser pour tester vos fonctions. *)

let a : favourite list = [Movie "Love Story"; Colour Blue;
                          Tvshow "The Simpsons"; Colour Orange]

let b : favourite list = [Number 1.0; Number 2.0; Number 5.0;
                          Number 14.0; Number 42.0]

let c : favourite list = [Movie "A Beautiful Mind"; Tvshow "House";
                          Letter 'P'; Colour Orange]

let d : favourite list = [Tvshow "Lost"; Number 3.14]

let students = [a; b; c; d]

(* 1a. Définissez une valeur de type "favourite list" pour quelqu'un dont
 * son couleur préférée est Aubergine et son nombre préféré est 5. *)

(*
let prob1a : favourite list = 
*)

(* 1b. Écrivez une fonction qui prend comme argument une valeur de
   type "favourite list" (comme l'exemple ci-dessus) et renvoie soit
   le titre du film préféré de cette personne, soit "None" si un film
   favori n'est pas dans la liste. Si plusieurs films sont dans la
   liste, renvoyez le premier. Quelle est le type de cette fonction?
   (Donnez le type de "favmovie" sous forme de chaîne de caractères
   dans "favmovie_type" ci-dessous.) *)

(*
let favmovie_type = ""
*)

(*
let rec favmovie lst   =
*)


(* 1c. Écrivez une fonction qui prend une valeur de type "favourite
   list" et renvoie "true" si et seulement si cette personne a inclus
   "Garnet" en tant que couleur préférée. Quelle est le type de cette
   fonction? *)

(*
let uottawa_colours_type = ""
*)
(*
let rec uottawa_colours lst  =
*)


(* Exercice 2 *)
(* 2a. Définissez un type de données qui représente des "int"s ou des
   "float"s *)

(*
type realnum = 
*)

(* 2b. Définissez deux fonctions pour créer des nombres réels à partir
   d'un "int" et d'un "float", respectivement *)

(* 
let real_of_int (i:int) : realnum = 
let real_of_float (f:float) : realnum = 
*)

(* 2c. Définissez une fonction qui teste si deux éléments de "realnum"
   sont égaux. La fonction doit fonctionner sur des "int"s et des
   "float"s, par exemple (realequal 4 4.0) => True. *)

(*
let realequal (a: realnum) (b: realnum) : bool = 
*)


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

(* 
let form3a : form =
 *)

                  
(* 3b. Ecrivez une fonction qui prend comme entreé une formule
   booléenne, et renvoie la liste de tous les variables
   propositionnelles uniques qui apparaissent dans la formule.
   *)

(*
let fvars (f:form) : prop list  = 
*)

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

(*
let is_cnf (f:form) : bool =
 *)

(* Quelques tests pour 3b et 3c. *)

let c1 = Or (Prop "x1",Prop "x2")
let c2 = Or (Or (Prop "x3",Prop "x4"),Prop "x5")
let c3 = Prop "x2"

(*
let test_is_cnf1 = is_cnf (And (c1,And (c2,c3)))
let test_is_cnf2 = is_cnf (And (And (c1,c2),c3))
let test_is_cnf3 = is_cnf (And (Imp (c1,c2),c3))
let test_is_cnf4 = is_cnf c2
let test_is_cnf5 = is_cnf c3

let test_fvars1 = fvars c2
let test_fvars2 = fvars (And (Imp (c1,c2),c3))
 *)
