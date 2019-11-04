module type RANGE =
sig
  (* les types *)
  (* type de l'intervalle *)
  type t
  (* type des élements *)
  type e
    
  (* Constructeurs *)
  (* Construisez un intervalle d'un seul point *)
  val singleton : e -> t
  (* Construisez un intervalle qui a deux extrémités *)
  val range : e -> e -> t

  (* Modificateurs *)
  (* "scalar add range", par exemple si r est une intervalle de -4 à 6,
     sadd r 1 renvoie un intervalle de -3 à 7.
     Cette opération ne modifie pas la taille de l'intervalle. *)
  val sadd : t -> e -> t
  (* "scalar multiply range", par exemple si r est une intervalle de 2 à 4,
     smult r 3 renvoie un intervalle de 6 à 12.
     Cette opération peut changer la taille de l'intervalle. *)                        
  val smult : t -> e -> t
  (* Créez un nouvel intervalle qui couvre les deux intervalles
     donnés.  Par exemple, si un argument est un intervalle de -4 à 6
     et l'autre et un intervalle de 10 à 12, l'intervalle de -4 à 12
     est renvoyé. *)
  val bridge : t -> t -> t

  (* Observateurs *)
  (* Combien d'éléments sont dans l'intervalle? *)
  val size : t -> int
  (* Est-ce que l'intervalle t contient e? *)
  val contains : t -> e -> bool
  (* Est-ce qu'un élément arbitraire du premier intervalle est
     inférieur à un élément arbitraire du deuxième intervalle?  Si les
     intervalles se chevauchent, renvoyez None, parce que les réponses
     diffèrent selon l'élément choisi.  Sinon, renvoyez le résultat
     d'un test qui détermine si le maximum du premier intervalle est
     inférieur au minimum du deuxième intervalle *)
  val rless : t -> t -> bool option
      
end

module LoHiPairRange : RANGE with type e = int
module LoLenPairRange : RANGE with type e = int
module ListRange : RANGE with type e = int