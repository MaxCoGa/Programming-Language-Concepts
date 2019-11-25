(* Programmation par objet dans OCaml:
   Représenter les grammaires sous forme de classes et les expressions sous forme d'objets *)

(* On peut représenter des expressions générées par la grammaire:

   e ::= num | e + e

   en utilisant des objets d'une classe appelée "expression". On
   commence par définir une "classe abstraite", également appelée
   "interface" (en utilisant le mot clé "virtual" dans OCaml).  Bien
   que cette classe "expression" n'ait pas d'instance, elle liste les
   opérations communes à tous les types d'expressions. Ces opérations
   incluent (1) un prédicat "is_atomic" qui indique si l'argument a
   des sous-expressions ou non, (2) et (3) des opérations pour
   récupérer les sous-expressions de gauche et de droite (si
   l'expression n'est pas atomique), et (4) une méthode qui calcule la
   valeur de l'expression.  *)

class virtual expression = object
  method virtual is_atomic : bool
  method virtual left_sub : expression option
  method virtual right_sub : expression option
  method virtual value : int
end


(* La grammaire ayant deux cas, nous avons deux sous-classes de
   "expression", une pour les nombres et une pour les sommes.
 *)

class number_exp (n:int) = object
  inherit expression as super
  val mutable number_val = n
  method is_atomic = true
  method left_sub = None
  method right_sub = None
  method value = number_val
end               

class sum_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method left_sub = Some left_exp
  method right_sub = Some right_exp
  method value = left_exp#value + right_exp#value
end


(* QUESTION 1. Une classe qui représente les produits et des appels de méthodes *)
(* 1(a). Étendez cette hiérarchie de classe en écrivant une classe
   "prod_exp".  Cette classe devrait représenter des expressions de
   produit de la forme:

   e ::= ... | e * e
 *)

(* 1(b). Créez des objets qui représentent les expressions suivantes:
   - une expression "a" qui représente le nombre 3
   - une expression "b" qui représente le nombre 0
   - une expression "c" qui représente le nombre 5
   - une expression "d" qui représente la sum de "a" et "b"
   - une expression "e" qui représente le produit de "d" et "c"
   Puis envoyez le message "value" à "e".
   Notez que "e" représente l'expression (3+0)*5.

   Pour répondre a 1(b), décommentez ce code et complétez-le:
   let a = ...
   let b = ...
   ...
 *)
                                              
(* QUESTION 2. Expressions unaires *)
(* Étendez la hiérarchie des classes en écrivant un "square_exp".
   L'expression ci-dessous (écrite e^2) signifie "e carré":

   e ::= ... | e^2

   Des modifications seront nécessaires dans l'interface "expression".
   Pour ce faire, vous devrez réimplémenter toutes les classes
   "expression", "number_exp", "sum_ex" et "prod_exp".  Essayez de
   faire le moins de modifications possible. *)

(* QUESTION 3. Expressions ternaires et plus d'appels de méthode *)
(* 3(a). Étendez cette hiérarchie de classe en écrivant une classe
   "cond_exp" qui représente les expressions conditionnelles de la
   forme.

   e ::= ... | e?e:e *)

(* Dans une expression conditionnelle a?b:c, on évalue "a" et si la
   valeur n'est pas 0, alors la valeur de "b" est renvoyée. Si la
   valeur de "a" est 0, puis la valeur de "c" est renvoyée.

   Encore une fois, essayez de faire le moins de modifications
   possible.  Si nécessaire, redéfinissez la hiérarchie de classes
   créée pour la question 2 de sorte qu'elle soit plus générale et
   qu'elle puisse traiter des opérations unaires, binaires et
   ternaires.  *)

(* 3(b). Recréez tous les objets a, b, c, d, e ci-dessus et créez les
   nouveaux objets suivants:
   - une expression "f" qui représente le carré de "c"
   - une expression "g" qui représentant l'expression conditionnelle b?e:f
   Puis envoyez le message "value" à "g".
 *)


(* 3(c) Tapez les expressions ci-dessous (pour a, b, c, d, e, f, g)
   dans l'interpréteur OCaml et regardez ce qui est imprimé. Le type de
   chaque expression sera imprimé. Notez que ces types ne sont pas tous
   pareils.

   Ensuite, entrez l'expression qui définit la valeur de "e_list".
   Remarquez que "e_list" (ci-dessous) est une liste qui contient
   des éléments de type "expression", et que ce type n'est pas le même
   que les types imprimés pour a, b, c, d, e, f, g. Expliquez pourquoi
   il est possible que ces éléments aient plusieurs types dans OCaml.

   Pour répondre à 3 (c), décommentez ce code et exécutez-le.

let _ = a
let _ = b
let _ = c
let _ = d
let _ = e
let _ = f
let _ = g
let e_list : expression list = [a;b;c;d;e;f;g]
 *)

(* QUESTION 4. Redéfinissez à nouveau la hiérarchie complète. Cette
   fois, if faut inclure une nouvelle opération qui prend un argument
   (x: int) et modifie un objet de la classe "expression" de sorte que
   toutes ses feuilles soient incrémentées par la valeur de x.  (Les
   feuilles d'une expression sont toutes les sous-expressions qui
   appartiennent à la classe "number_exp".) Cette opération ne doit
   pas créer une nouvelle instance d'une "expression". Elle devrait
   modifier les instances auxquelles elle est appliqué.

   Recréez à nouveau tous les objets a, b, c, d, e, f, g. Puis envoyez
   le message "value" à "g". Puis appliquez la nouvelle opération avec
   une valeur supérieure à 0 comme argument. Envoyez encore une fois
   le message "value" à "g". La nouvelle valeur doit être différente
   de celle d'origine.  Vérifiez que votre implémentation donne la
   nouvelle valeur attendue. *)
