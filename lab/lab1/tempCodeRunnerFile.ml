
let problem5 (x:int) (k:int) : int =
  match k=0, k mod 2=0 with
  true,_ = 0 -> 1
  | _,true -> (x * x) ** (k/2)
  | _,_ -> x * (x ** (k-1));;