open PMap

(* wyjatek rzucany przy zacyklonym grafie *)
exception Cykliczne

(* FUNKCJA WYKONUJACA SORTOWANIE TOPOLOGICZNE *)
(* jako argument otrzymuje liste reprezentujaca graf *)
(* 'graf' to mapa trzymajaca wierzcholki wraz z listami
wierzcholkow do ktorych one prowadza *)
(* 'odw' to mapa przypisujaca wierzcholkowi 0 - gdy nie byl jeszcze przerabiany,
1 - gdy jest przerabiany, (-1) - gdy zostal juz przerobiony *)
(* 'wyn' to wynikowa lista przedstawiajaca wierzcholki posortowane topologicznie *)
(* funkcja 'tw_gr' tworzy mape 'graf' *)
(* funkcja 'dfs' przechodzi po grafie i tworzy liste 'wyn' *)
let topol list =
	let graf = ref empty and odw = ref empty and wyn = ref [] in
	let rec tw_gr lista kolejka =
		match lista with
		| [] -> kolejka
		| (w, list_w)::t -> 
			if mem w !graf = true then
				let st_lst = find w !graf in graf := add w (list_w @ st_lst) !graf
			else graf := add w list_w !graf;
			tw_gr t (w::kolejka)
	in
	let rec dfs lista =
		match lista with
		| [] -> ()
		| a::t ->
			if mem a !odw = true then 
				if find a !odw = 1 then raise Cykliczne
				else dfs t
			else 
				begin
					odw := add a 1 !odw;
					if mem a !graf = true then dfs (find a !graf);
					wyn := a::!wyn;
					odw := add a (-1) !odw;
					dfs t;
				end
	in dfs (tw_gr list []);
	!wyn


(* TESTY *)
(*
let a = [("a", ["b"]);("b", ["a"]);("c", ["a"])];;
let a = try topol a with 
	| Cykliczne -> [];;
let b = [("a", ["a"])];;
let b = try topol b with
	| Cykliczne -> [];;
let c = [(1, [4; 5]);(3, [2]);(2, [3])];;
let c = try topol c with
	| Cykliczne -> [];;
let d = [(1, [2]);(2, [3; 4]);(3, [5; 6]);(6, [2])];;
let d = try topol d with
	| Cykliczne -> [];;

assert (a = []);;
assert (b = []);;
assert (c = []);;
assert (d = []);;

let a = [("a", ["c"]);("e", ["g"]);("f", ["a";"e"]);("g", ["c";"a"])];;
let b = [(false, [true])];;
let c = [("z", ["c"; "f"; "a"]);("f", ["x"; "a"]);("g", ["h"])];;
let d = [("xx", ["aa"; "gg"]);("ab", ["uw"; "mim"]);("mim", ["uw";"xx"])];;
let e = [("d", ["c"]);("c", ["b"]);("b", ["a"])];;
let a = topol a;;
let b = topol b;;
let c = topol c;;
let d = topol d;;
let e = topol e;;

assert (a = ["f"; "e"; "g"; "a"; "c"]);;
assert (b = [false; true]);;
assert (c = ["z"; "c"; "f"; "a"; "x"; "g"; "h"]);;
assert (d = ["ab"; "mim"; "xx"; "gg"; "aa"; "uw"]);;
assert (e = ["d"; "c"; "b"; "a"]);;

let a = [];;
let b = [(1, [2;3]);(3, [6;7]);(6, [4;7]);(7, [5]);(2, [5]);(8, [])];;
let c = [(5, []);(2, [])];;
let d = [];;
let a = topol a;;
let b = topol b;;
let c = topol c;;
let d = topol d;;

assert (a = []);;
assert (b = [1; 3; 6; 4; 7; 2; 5; 8]);;
assert (c = [5; 2]);;
assert (d = []);;

let g = [
  ("1", ["2"; "3"]);
  ("3", ["2"]);
  ("4", ["3"; "2"])
];;
let g = topol g;;

assert (g = ["1"; "4"; "3"; "2"]);;

let g = [
  ("first", ["second"; "fourth"; "eighth"]);
  ("second", ["fourth"; "eighth"]);
  ("third", ["fourth"; "fifth"; "sixth"]);
  ("fourth", ["eighth"]);
  ("fifth", ["sixth"; "seventh"]);
  ("sixth", ["eighth"; "first"]);
  ("seventh", ["eighth"])
];;
let g = topol g;;

assert (g = ["third"; "fifth"; "sixth"; "first"; "second"; "fourth"; "seventh";
   "eighth"]);;

let g = [
  (1, [2; 3]);
  (2, [4]);
  (3, [4]);
  (4, [5; 6]);
  (5, [7]);
  (6, [7])
];;
let g = topol g;;

assert (g = [1; 2; 3; 4; 5; 6; 7]);;

let g = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;
let g = topol g;;

assert (g = [6; 3; 5; 4; 1; 7; 2]);;

let g = [
  (1, [2; 4; 8]);
  (2, [16; 32]);
  (4, [64; 128]);
  (8, [256; 512]);
  (16, [1024]);
  (32, [2048]);
  (64, [4096]);
  (128, [8192]);
  (256, [16384]);
  (512, [32768])
];;
let g = topol g;;

assert (g = [1; 2; 4; 8; 16; 1024; 32; 2048; 64; 4096; 128; 8192; 256; 16384; 512;
   32768]);;

let g = [
  ("Lorem", ["sit"]);
  ("ipsum", ["sit"; "amet"]);
  ("dolor", ["amet"; "elit"]);
  ("sit", ["consectetur"; "adipiscing"; "elit"])
];;
let g = topol g;;

assert (g = ["Lorem"; "ipsum"; "dolor"; "amet"; "sit"; "elit"; "adipiscing";
   "consectetur"]);;

let g = [
  ("through", ["the"; "gates"; "of"; "hell"]);
  ("hell", ["as"; "we"; "make"; "our"; "way"; "to"; "heaven"]);
  ("PRIMO", ["VICTORIA"])
];;
let g = topol g;;

assert (g = ["through"; "of"; "gates"; "the"; "hell"; "heaven"; "to"; "way"; "our";
   "make"; "we"; "as"; "PRIMO"; "VICTORIA"]);;

let g = [
  ("one", ["three"]);
  ("one", ["two"]);
  ("two", []);
  ("two", []);
  ("two", ["three"])
];;
let g = topol g;;

assert (g = ["one"; "two"; "three"]);;
*)

