(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a drevo = 
     | Prazno 
     | Sestavljeno of 'a drevo * 'a * 'a drevo

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let list x = Sestavljeno(Prazno, x, Prazno)

let primer = 
     Sestavljeno(
          Sestavljeno(list 0, 2, Prazno), 
          5,
          Sestavljeno(list 6, 7, list 11)
     )

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
     | Prazno -> Prazno
     | Sestavljeno(levo, x, desno) -> Sestavljeno(mirror desno, x, mirror levo)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
     | Prazno -> 0
     | Sestavljeno(levo, x, desno) -> 1 + max (height levo) (height desno)

let rec size = function
     | Prazno -> 0
     | Sestavljeno(levo, x, desno) -> 1 + (size levo) + (size desno)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
     | Prazno -> Prazno
     | Sestavljeno(levo, x, desno) -> Sestavljeno(map_tree f levo, f x, map_tree f desno)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
     | Prazno -> []
     | Sestavljeno(levo, x, desno) -> list_of_tree levo @ [x] @ list_of_tree desno

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_bst drevo =
     let rec list_is_ordered = function
       | [] | _ :: [] -> true
       | x :: y :: rep -> if x <= y then list_is_ordered (y :: rep) else false
     in drevo |> list_of_tree |> list_is_ordered

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert x = function
     | Prazno -> list x
     | Sestavljeno(levo, y, desno) when x = y -> Sestavljeno(levo, y, desno)
     | Sestavljeno(levo, y, desno) when x < y -> Sestavljeno(insert x levo, y, desno)
     | Sestavljeno(levo, y, desno)  -> Sestavljeno(levo, y, insert x desno)

let rec member x = function
     | Prazno -> false
     | Sestavljeno(levo, y, desno) when x = y -> true
     | Sestavljeno(levo, y, desno) when x < y -> member x levo
     | Sestavljeno(levo, y, desno) -> member x desno

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let member3 x drevo = 
     let rec is_in x = function
     | [] -> false
     | y :: rep -> if x = y then true else is_in x rep
     in drevo |> list_of_tree |> is_in

let rec member2 x = function
     | Prazno -> false
     | Sestavljeno(levo, y, desno) -> x = y || (member2 x levo) || (member2 x desno)

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let succ drevo =
     let rec minimal = function
       | Prazno -> None
       | Sestavljeno(Prazno, x, _) -> Some x
       | Sestavljeno(l, _, _) -> minimal l
     in
     match drevo with
     | Prazno -> None
     | Sestavljeno(_, _, r) -> minimal r

let pred drevo =
     let rec maximal = function
       | Prazno -> None
       | Sestavljeno(_, x, Prazno) -> Some x
       | Sestavljeno(_, _, r) -> maximal r
     in
     match drevo with
     | Prazno -> None
     | Sestavljeno(l, _, _) -> maximal l
   
(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x = function
  | Prazno -> Prazno
  | Sestavljeno(l, y, r) when x > y -> Sestavljeno(l, y, delete x r)
  | Sestavljeno(l, y, r) when x < y -> Sestavljeno(delete x l, y, r)
  | Sestavljeno(l, y, r) as bst -> (
      (*Potrebno je izbrisati vozlišče.*)
      match succ bst with
      | None -> l (*To se zgodi le kadar je [r] enak [Empty].*)
      | Some s ->
        let clean_r = delete s r in
        Sestavljeno(l, s, clean_r))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) drevo

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict 
     : (string, int) dict
  = Sestavljeno (list ("a", 0), ("b", 1), Sestavljeno (list ("c", -2), ("d", 2), Prazno))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
     | Prazno -> None
     | Sestavljeno (l, (k, x), d) -> 
          if k = key then Some x
          else if k > key then dict_get key l
          else dict_get key d
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict = function
  | Prazno -> ()
  | Sestavljeno (d_l, (k, v), d_r) -> (
      print_dict d_l;
      print_string (k ^ " : "); print_int v; print_newline ();
      print_dict d_r)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert key value = function
     | Prazno -> list (key, value)
     | Sestavljeno (l, (k, v), d) -> 
          if k = key then Sestavljeno (l, (k, value), d)
          else if k < key then Sestavljeno (l, (k, v), dict_insert key value d)
          else Sestavljeno (dict_insert key value l, (k, v),  d)
