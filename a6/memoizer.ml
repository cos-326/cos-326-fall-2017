(* We will be using OCaml's Map (aka dictionary) library.  
 * The documentation for OCaml Maps starts here:  
 * 
 * http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.html
 * 
 * You can see that Map.Make is a functor for creating maps.
 *
 * If you click on "S" it will lead you here:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.S.html
 *
 * and you can read about the operations created when you use the functor
 *
*)
module type DICT = Map.S

(********************************)
(* A MEMOIZER THAT DOESN'T WORK *)
(********************************)

module type POORMEMOIZER =
sig
  (* the type of the memoized function's argument *)
  type key

  (* given a function, returns a poorly memoized version of that function *)
  val memo :  (key -> 'a) -> (key -> 'a)
end

module PoorMemoizer (D : DICT) : (POORMEMOIZER with type key = D.key) =
struct
  type key = D.key

  let memo (f : key -> 'a) : key -> 'a =
    let f_memoed x =
      let history = ref (D.empty) in
      try D.find x (!history) with
        Not_found ->
        let result = f x in
        history := D.add x result (!history); 
        result
    in
    f_memoed
end

(**********************************)
(* END MEMOIZER THAT DOESN'T WORK *)
(**********************************)

(**********************************)
(* START MEMOIZER THAT DOES WORK! *)
(**********************************)

module type MEMOIZER =
sig
  type key

  val memo : ((key -> 'a) -> (key -> 'a)) -> (key -> 'a)
end

(* Task 4.2:  Finish the generic memoizer *)

module Memoizer (D : DICT) : MEMOIZER with type key = D.key =
struct
  type key = D.key

  let memo _ = failwith "unimplemented"
end
