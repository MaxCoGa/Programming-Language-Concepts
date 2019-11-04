(* Une version condensée de la signature dans range.mli. Votre
   première étape consiste à étudier le contenu de range.mli. *)
module type RANGE =
sig
  type t
  type e
  val singleton : e -> t
  val range : e -> e -> t
  val sadd : t -> e -> t
  val smult : t -> e -> t
  val bridge : t -> t -> t
  val size : t -> int
  val contains : t -> e -> bool
  val rless : t -> t -> bool option
end

(* Une implémentation d'un type de données RANGE où les éléments sont
   des entiers et les intervalles sont représentés par des paires *)
module LoHiPairRange : RANGE with type e = int =
struct
  type e = int
  type t = e * e
  let singleton (i:e) : t = (i,i)
  let range (i:e) (j:e) : t = ((min i j), (max i j))
  let sadd (x:t) (i:e) : t = let (lo,hi) = x in (lo+i,hi+i)
  let smult (x:t) (i:e) : t =
    let (lo, hi) = x in
    if i >= 0 then (lo*i,hi*i)
    else (hi*i,lo*i)
  let bridge (x:t) (y:t) : t =
    let (lx, hx) = x in
    let (ly, hy) = y in
    ((min lx ly), (max hx hy))
  let size (x:t) : int =
    let (lo,hi) = x in
    hi - lo - (-1)
  let contains (x:t) (i:e) : bool =
    let (lo,hi) = x in
    (lo <= i) && (i <= hi)
  let rless (x:t) (y:t) : bool option =
    let (lx, hx) = x in
    let (ly, hy) = y in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end

(* Exercice 1: Completez la nouvelle implémentation de RANGE dans le
   module ListRange ci-dessous. La partie qui est déjà implémentée
   devrait vous donner suffisamment d’informations pour implémenter le
   reste. Ajoutez du code pour tester votre implémentation. *)
    
(* Une implémentation d'un type de données RANGE où les éléments sont
   des entiers et les intervalles sont représentés par des listes *)
module ListRange : RANGE with type e = int =
struct
  type e = int
  type t = e list

  (* des fonctions auxilliares *)
  let minmax (l:t) : (e*e) option =
      let rec max (t:t) (e:e) : e =
          match t with
          | [] -> e
          | h::r -> max r h
      in
      match l with
      | [] -> None
      | h::r -> Some (h, (max r h))
  let rec build (i:e) (j:e) : e list =
    if i = j then [j]
    else i :: build (i+1) j
  
  let singleton (i:e) : t = [i]
  let range (i:e) (j:e) : t = build (min i j) (max i j)
  (* TODO Exercise 1: Remplacez le code ci-dessous par des
     implémentations correctes de toutes les opérations. *)
  let sadd (x:t) (i:e) : t = List.map (fun y->y+i) x
  let smult (x:t) (i:e) : t = 
    match (minmax x) with
    | None -> []
    | Some (xl,xh)  ->
      if i < 0 then build (xh*i) (xl*i)
      else  build (xl*i) (xh*i)
  let bridge (x:t) (y:t) : t = 
    match (minmax x), (minmax y) with
    | None, None -> []
    | None, Some _ -> y
    | Some _, None -> x
    | Some (xl,xh), Some (yl,yh) ->
      build (min xl yl) (max xh yh)
  let size (x:t) : int = List.length x
  let contains (x:t) (i:e) : bool = List.mem i x
  let rless (x:t) (y:t) : bool option = 
    match (minmax x), (minmax y) with
    | None, _ -> None
    | _ , None -> None
    | Some (lx,hx), Some(ly,hy) ->
      if hx < ly then Some true
      else if hy < lx then Some false
      else None
end

(* TODO Exercice 1: Ajoutez du code pour tester votre
   nouvelle implantation. *)
let s1 = ListRange.range 2 5;;
let sizes1 =ListRange.size s1;; 
let s2 = ListRange.sadd s1 3;;
let s3 = ListRange.smult s1 2;;
let s4 = ListRange.range 3 9;;
let s5 = ListRange.bridge s1 s4;;
let sizes4 = ListRange.size s4;;
let s6 = ListRange.contains s4 7;;
let s7 = ListRange.rless s1 s4;;
(* Exercice 2: Implémentez une version impérative de RANGE. D'abord,
   vous pouvez copier range.mli, le coller ci-dessous, et changer
   les types si nécessaire. Et puis copiez l'implémentation de
   LoHiPairRange et faites les modifications nécessaires.  If faut
   inclure toutes les opérations qui sont dans la version
   fonctionnelle. Les opérations "singleton" et "range" doivent
   créer une nouvelle intervalle. Les opérations "sadd" et "smult"
   doivent modifier des intervalles existantes. Considerez les choix
   de conception et concevez vôtre propre version du "bridge". *)
 