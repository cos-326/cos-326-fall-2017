open Memoizer
open Timing

type base = Base.base
type dna = Base.dna

(* slow lcs *)
let rec slow_lcs ((s1,s2) : dna * dna) : dna =
  match (s1,s2) with 
    ([], _) -> []
  | (_, []) -> []
  | (x :: xs, y :: ys) ->
    if Base.eq x y then
      x :: slow_lcs (xs, ys)
    else
      Base.longer_dna_of (slow_lcs (s1, ys)) (slow_lcs (xs, s2))


(* A potentially useful module *)
module DnaPairOrder : Map.OrderedType with type t = dna * dna =
struct
  type t = dna * dna

  let rec compare_dna x' y' : int = 
    match x',y' with 
      [],[] -> 0
    | [], xs -> -1
    | xs, [] -> 1
    | x::xs, y::ys -> 
      (match Base.compare x y with
         0 -> compare_dna xs ys
       | other -> other)


  (* implements a lexicographic ordering: 
   * compare the second components only if first components are equal *)
  let compare (a, b) (c, d) =
    match compare_dna a c with
      0 -> compare_dna b d
    | other -> other

end

(* Task 4.4 *)

(* implement fast_lcs using your automatic memoizer functor! 
 * doing so will of course require proper creation of modules and
 * use of functors *)
let fast_lcs (ds : dna * dna) : dna =  failwith "unimplemented"

(* Task 4.5 *)

(* Implement some experiment that shows performance difference
 * between slow_lcs and fast_lcs. (Print your results.)     
 * Explain in a brief comment what your experiment shows.        *)
let main () =
  print_string "nothing yet ...\n"


(*

(* uncomment this block to run your experiment, 
 * but please do not submit with it uncommented
 *)
let _ = main ()
*)

