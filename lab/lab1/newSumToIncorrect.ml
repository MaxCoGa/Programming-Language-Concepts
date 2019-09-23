(* additionnez les nombres de 0 à n
   pré condition : n doit être un nombre naturel

   sumTo 0 ==> 0
   sumTo 3 ==> 6
*)
let rec sumTo (n:int) : int =

  match n with
    0 -> 0
  | n -> n + sumTo (n-1)
;;

let _ =
  print_int (sumTo 8);
  print_newline()
