(* Box office analysis *)

(* Contents:
    -- the movie type
    -- the studio_gross type
    -- functions for querying and transforming lists of movies
*)

(* a movie is a tuple of (title, studio, gross in millions, year) *)
type movie = string * string * float * int

(* a studio_gross is a pair of (studio, gross in millions) *)
type studio_gross = string * float

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

(* a useful debugging routine *)
let debug s = print_string s; flush_all()

(* *** DO NOT CHANGE DEFINITIONS ABOVE THIS LINE! *** *)

(* you may add "rec" after any of the let declarations below that you
 * wish if you find doing so useful. *)

let ( % ) f g x = f (g x)
let flip f x y = f y x
let equals x y = x = y
(* :: isn't a real function, so (::) doesn't work :-\ *)
let reverse xs = List.fold_left (fun acc x -> x::acc ) [] xs

let get_name   ((name,   _, _, _): movie): string = name
let get_studio ((_, studio, _, _): movie): string = studio
let get_gross  ((_, _, gross,  _): movie): float  = gross
let get_year   ((_, _, _, year  ): movie): int    = year

(* find the average gross of the movies in the list                  *)
(* return 0.0 if the list is empty                                   *)
(* hint: you may need to use functions float_of_int and int_of_float *)
(* hint: if you don't know what those functions do,                  *)
(*       type them in to ocaml toplevel                              *)
(* hint: recall the difference between +. and + also 0. and 0        *)

let sum_floats = List.fold_left (+.) 0.
let sum_gross: (movie list -> float) = sum_floats % (List.map get_gross)

let average (movies : movie list) : float = 
  (sum_gross movies) /. (float_of_int (List.length movies))

(* return a list containing only the movies from the given decade *)
(* call bad_arg if n is not 20, 30, ..., 90, 00, 10               *)
(* Treat 0 as 00 (this is unavoidable as 00 is not represented    *)
(*   differently from 0).                                         *)
(* Note: movies from any years outside the range 1920-2019 will   *)
(* always be discarded but should not raise an error condition    *)
let last_two: (int -> int) = flip (mod) 100
let last_one:   (int -> int) = flip (mod) 10
let get_decade_start (n: int): int = (last_two n) - (last_one n)

let bad_decade (n: int): bool = n < 0 || 90 < n
let bad_year   (n: int): bool = (n mod 10) != 0
let bad_decade_input (n: int): bool = bad_decade n || bad_year n

let decade (n:int) (ms:movie list) : movie list =
  if bad_decade_input n then
    bad_arg (string_of_int n)
  else
    List.filter (equals n % get_decade_start % get_year) ms

(* return the first n items from the list *)
(* if there are fewer than n items, return all of them *)
(* call bad_arg if n is negative *)
let rec take (n:int) (l:'a list)  : 'a list =
  match l with
  | [] -> []
  | hd::tl ->
    match n with
    | 0 -> []
    | n when n < 0 -> bad_arg (string_of_int n)
    | n -> hd :: take (n - 1) tl

(* return everything but the first n items from the list *)
(* if there are fewer than n items, return the empty list *)
(* call bad_arg if n is negative *)

let rec drop (n:int) (l:'a list)  : 'a list =
  match l with
  | [] -> []
  | hd::tl ->
    match n with
    | 0 -> l
    | n when n < 0 -> bad_arg (string_of_int n)
    | n -> drop (n - 1) tl

(* return a list [x1; x2; ...; xn] with the same elements as the input l
   and where:
     leq xn xn-1
     ...
     leq x3 x2
     leq x2 x1
     are all true
*)
(* hint: define an auxiliary function "select" *)
type 'a less = 'a -> 'a -> bool
let selection_sort (leq:'a less) (l:'a list) : 'a list =
  failwith "selection_sort unimplemented"


(* ASIDE:  Why does this assignment ask you to implement selection sort?
   Insertion sort is almost always preferable to selection sort,
   if you have to implement a quadratic-time sorting algorithm.
   Insertion sort is faster, it's simpler to implement, and it's
   easier to reason about.  For smallish inputs (less than 5 or 8),
   insertion sort is typically faster than quicksort or any
   other NlogN sorting algorithm.  So, why do we ask you to implement
   selection sort?  Answer: we already showed you insertion sort
   in the lecture notes.

   ASIDE 2: But at least selection sort is better than bubble sort.
   Even Barack Obama knows that. https://www.youtube.com/watch?v=k4RRi_ntQc8
*)

(* return list of movies sorted by gross (largest gross first) *)
let sort_by_gross (movies : movie list) : movie list = 
  failwith "sort_by_gross unimplemented"


(* return list of movies sorted by year produced (largest year first) *)
let sort_by_year (movies : movie list) : movie list = 
  failwith "sort_by_year unimplemented"


(* sort list of (studio, gross in millions) by gross in millions 
 * with the largest gross first *)
let sort_by_studio (studio_grosses : studio_gross list) : studio_gross list = 
  failwith "sort_by_studio unimplemented"


(* given list of movies,
 * return list of pairs (studio_name, total gross revenue for that studio)  *)
let by_studio (movies:movie list) : studio_gross list =
  failwith "by_studio unimplemented"


(***********)
(* Testing *)
(***********)

(* Augment the testing infrastructure below as you see fit *)

(* Test Data *)

let data1 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
]

let data2 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
]

let data3 : movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
]

let data4 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
]

(* Assertion Testing *)

(* Uncomment the following when you are ready to test your take routine *)
(*
let _ = assert(take 0 data4 = [])
let _ = assert(take 1 data1 = data1)
let _ = assert(take 2 data4 = data2)
let _ = assert(take 5 data2 = data2)
let _ = assert(take 2 data2 = data2)
*)

(* Additional Testing Infrastructure *)

let stests : (unit -> movie list) list = [
  (fun () -> sort_by_gross data1);
  (fun () -> sort_by_gross data2);
  (fun () -> sort_by_gross data3);
  (fun () -> sort_by_gross data4)
]

let check (i:int) (tests:(unit -> 'a) list) : 'a =
  if i < List.length tests && i >= 0 then
    List.nth tests i ()
  else
    failwith ("bad test" ^ string_of_int i)
