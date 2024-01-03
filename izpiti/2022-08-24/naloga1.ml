(* 1. a) *)
let zamenjaj (a, b) (c, d) = ((a, c), (b, d))

(* 1. b) *)
let modus (a, b, c) = 
  if a <> b then
    if a <> c then
      if b <> c then
        None
      else Some b
    else Some a
  else Some a

(* 1. c) *)
let uncons = function
  | [] -> None
  | x :: xs -> Some (x, xs)

(* 1. d) *)
let rec vstavljaj x = function
  | [] -> []
  | [x] -> [x]
  | y :: rep -> y :: x :: vstavljaj x rep

(* 1. e) *)
let popolnoma_obrni sez = 
  let rec obrni acc = function
    | [] -> acc
    | x :: xs -> obrni (x :: acc) xs
  in
  let rec popolnoma_obrni_aux acc = function
    | [] -> acc
    | glava :: rep -> popolnoma_obrni_aux ((obrni [] glava) :: acc) rep
  in
  popolnoma_obrni_aux [] sez