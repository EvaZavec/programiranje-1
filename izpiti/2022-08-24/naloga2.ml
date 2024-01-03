type 'a tape = Tape of { left : 'a list; head : 'a; right : 'a list }

type 'a command = Left | Do of ('a -> 'a) | Right

let example = Tape { left = [ 3; 2; 1 ]; head = 4; right = [ 5; 6 ] }

(* 2. a) *)

let map (Tape { left = sez1; head = x; right = sez2 }) f = 
  let rec map_sez f = function
    | [] -> []
    | x :: xs -> (f x) :: map_sez f xs
  in
  Tape { left = (map_sez f sez1); head = (f x); right = (map_sez f sez2) }


(* 2. b) *)

let izvedi _ = failwith "TODO"

(* 2. c) *)

let izvedi_ukaze _ = failwith "TODO"

(* 2. d) *)

let naberi_in_pretvori _ = failwith "TODO"

(* 2. e) *)

let pripravi_ukaze _ = failwith "TODO"
