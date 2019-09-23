(*  Programme FizzBuzz: une version modifiée de

http://fsharpforfunandprofit.com/posts/railway-oriented-programming-carbonated/

Ecrivez un programme qui imprime les nombres de 0 à 100, un par ligne.
* Pour les nombres qui sont multiples de trois, écrivez "Fizz" au lieu du nombre.
* Pour les nombres qui sont multiples de cinq, imprimez "Buzz".
* Pour les nombres qui sont multiples de trois et cinq, indiquez "FizzBuzz".
*)

(* Fonctions d'impression utiles (que font-ils? Quel type ont-ils?):

  print_endline
  print_string
  print_newline
  print_int

  Printf fonctionne (presque) comme en C. printf est un cas particulier
  dans OCaml. Un exemple:

  Printf.printf "%d" 17   

  Manuel avec codes de caractères:

  http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html

  Autres remarques utiles:

  e1 mod e2   -- l'opération mod renvoie un entier

  e1 = e2     -- l'opération d'égalité teste l'égalité de deux valeurs

  let rec f (x:t) :t = ...    -- rappelez-vous d'inclure le mot clé "rec" quand
                                 vous définissez une fonction récursive

*)
let rec fizzbuzz (y : int) (x : int) : string =
  if y <= x then
    match y mod 3, y mod 5 with
    0, 0 -> "FizzBuzz" ^  "\n" ^ fizzbuzz (y+1) x
    | 0, _ ->  "Fizz" ^  "\n" ^ fizzbuzz (y+1) x
    | _, 0 ->  "Buzz" ^  "\n" ^ fizzbuzz (y+1) x
    | _, _ -> fizzbuzz (y+1) x
  else "done"
  
  
;;


let _ = 
  print_string (fizzbuzz 0 100);;
  print_newline();;

