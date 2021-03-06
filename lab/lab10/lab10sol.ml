(* Les pipelines en OCaml *)
(* Dans ce laboratoire, vous allez utiliser le traitement en pipeline
   pour calculer et afficher les notes finales des étudiants d'un
   cours. *)

(* Le type "marks" est un tuple de 6 nombres à virgule flottante. Les 3
   premières nombres sont des notes pour 3 devoirs. Les 2 nombres
   suivants sont des notes pour deux tests. Le dernier est la note
   pour l'examen final. *)
type marks = float * float * float * float * float * float


(* Le type "mark_triple" est un tuple de 3 nombres à virgule
   flottante. Le premier nombre est la note de devoirs pour le
   cours. La deuxième nombre est la note de test pour le cours. Le
   troisième est la note pour l'examen a final. *)
type mark_triple = float * float * float

(* Le type "final_grade" est la note finale de l'étudiant représentée
   sous la forme d'un pourcentage. *)
type final_grade = float

(* Le type "letter_grade" représente la note finale des étudiants
   telle qu'elle apparaîtra sur leurs relevés de notes (A+, A, ...) *)
type letter_grade = string

(* Les calculs utiliseront un "student_id" et 3 types de tuples qui
   représentent 3 formes différentes d'enregistrements contenant des
   informations sur les étudiants. *)
type student_id = int
type st_record1 = student_id * marks
type st_record2 = student_id * mark_triple
type st_record3 = student_id * final_grade * letter_grade

(* Le devoir 1 vaut 60 points au total, le devoir 2 vaut 75, et le
   devoir 3 vaut 40. Chaque test vaut 50 points et l'examen final vaut
   100 points. *)
let total_a1 : float = 60.
let total_a2 : float = 75.
let total_a3 : float = 40.
let total_t1 : float = 50.
let total_t2 : float = 50.
let total_exam : float = 100.
let perfect_score : marks = (total_a1,total_a2,total_a3,total_t1,total_t2,total_exam)

(* Le mode d’évaluation du cours est que les devoirs valent 33%, les
   tests valent 33% et l’examen final vaut 34%. *)
let assign_percent1 = 33.
let test_percent1 = 33.
let exam_percent1 = 34.

(* La fonction suivante peut être utile pour transformer une note en
   pourcentage. *)
let out_of_100 (max_marks:float) (actual_marks:float) : float =
  (actual_marks *. 100.) /. max_marks

(* QUESTION 1.  Ddéfinissez une fonction OCaml qui prend un
   st_record1, et utilise un pipeline pour effectuer les opérations
   suivantes:

(a) La première opération consiste à modifier la composante de
   l'examen de chaque tuple. Dans le tuple d'entrée, la note de chaque
   étudiant est sur 100 points. Le professeur a décidé de le marquer
   sur 95 points. Donc, si un étudiant a eu 95 à l'examen, sa note
   sera convertie à 100. Si l'étudiant en a eu 96, sa note sera
   convertie à 101,05. Si l'étudiant en a eu 94, leur note sera
   convertie à 98,95, etc.

(b) Ensuite, transformez chaque st_record1 en st_record2 en calculant
   le nombre total de points que l'étudiant a obtenus pour la partie
   "devoirs" du cours, la partie "test" et la partie "examen final".

(c) Ensuite, modifiez chacune des 3 composantes des notes dans
   st_record2 en les transformant en pourcentage.

(d) Ensuite, modifiez à nouveau chaque composant en le transformant en
   la proportion spécifiée par le mode d’évaluation.  Par exemple, si
   l'étudiant a obtenu 100% sur la partie "devoirs" du cours, le
   nombre 100 dans la position "devoirs" du tuple de type
   student_record2 doit être remplacé par 33, parce que la partie
   "devoirs" du cours représente 33% de la note totale . Si l'étudiant
   a obtenu 50%, cette valeur devrait être remplacée par la moitié de
   33, soit 16,5, etc.

(e) Transformez le st_record2 obtenu à l’étape (d) en un st_record3.
   Premièmement, additionnez les 3 composantes de marque de
   st_record2.  Deuxièmement, utilisez le résultat pour calculer la
   note alphabétique à l'aide de l'échelle de notation de l'Université
   d'Ottawa.  *)

(* Exemple de solution à 1(a) *)
let update_exam (sr:st_record1) : st_record1 =
  let (s_id,(a1,a2,a3,t1,t2,e)) = sr in
  let new_e = out_of_100 95. e in
  (s_id,(a1,a2,a3,t1,t2,new_e))

(* Exemple de solution à 1(b) *)
let assign_score (sr:st_record1) : float =
  let (_,(a1,a2,a3,_,_,_)) = sr in a1+.a2+.a3

let test_score (sr:st_record1) : float =
  let (_,(_,_,_,t1,t2,_)) = sr in t1+.t2

let exam_score (sr:st_record1) : float =
  let (_,(_,_,_,_,_,e)) = sr in e

let total_scores (sr:st_record1) : st_record2 =
  let (s_id,_) = sr in
  let a = assign_score sr in
  let t = test_score sr in
  let e = exam_score sr in
  (s_id,(a,t,e))

(* Exemple de solution à 1(c) *)
let percentages (sr:st_record2) : st_record2 =
  let (s_id,(a_mark,t_mark,e_mark)) = sr in
  let a_percent = out_of_100 (total_a1+.total_a2+.total_a3) a_mark in
  (s_id,(a_percent,t_mark,e_mark))

(* Exemple de solution à 1(d) *)
let raw_scores (sr:st_record2) : st_record2 =
  let (s_id,(a_mark,t_mark,e_mark)) = sr in
  let a_raw = (a_mark *. assign_percent1) /. 100. in
  let t_raw = (t_mark *. test_percent1) /. 100. in
  let e_raw = (e_mark *. exam_percent1) /. 100. in
  (s_id,(a_raw,t_raw,e_raw))

(* Exemple de solution à 1(e). Notez que cette solution n'est que
   partielle. Le calcul de la note finale alphabétique est
   incomplet. *)
let compute_final (sr:st_record2) : st_record3 =
  let (s_id,(a_raw,t_raw,e_raw)) = sr in
  let final_percent = a_raw +. t_raw +. e_raw in
  let final_letter = "TBD" in
  (s_id,final_percent,final_letter)

(* Solution à Question 1 *)
let compute_mark x =
  x |> update_exam
    |> total_scores
    |> percentages
    |> raw_scores
    |> compute_final

(* Quelques données de test *)
let example1 = (100000,(55.,74.,38.,45.,40.,75.))
let example2 = (100001,(45.,73.,30.,38.,37.,80.))
let examples = [example1;example2]

let _ = compute_mark example1
let _ = compute_mark example2

(* QUESTION 2 *)
(* Définissez une nouvelle version de la fonction "display" à la page
   18 des notes de cours sur les pipelines. La nouvelle version doit
   fonctionner sur les données de ce laboratoire.  Le type de
   l'argument d'entrée de votre version de "display" sera "st_record1
   list". Vous utiliserez votre solution pour la question 1 au lieu de
   "compute_score". Vous devrez également définir une nouvelle version
   de "compare_score" et "stringify".
 *)

let student_compare (_,score1,_) (_,score2,_) =
  if score1 < score2 then 1
  else if score1 > score2 then -1
  else 0

let stringify (s_id, final_percent, final_letter) = 
  string_of_int s_id ^ ": " ^ string_of_float final_percent ^ ", " ^ final_letter


let display (students : st_record1 list) =
  students |> List.map compute_mark
           |> List.sort student_compare
           |> List.map stringify
           |> List.iter print_endline
          
let _ = display examples
