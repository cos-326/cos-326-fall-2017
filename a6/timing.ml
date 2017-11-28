(* This file depends on OCaml Unix library.
 *
 * To load this file in the OCaml toplevel, you need to start Ocaml 
 * with unix.cma (the Unix library) as an argument: 
 *
 *   % ocaml unix.cma
 *            Objective Caml version 3.11.0
 *   # #use "timing.ml";;
 *
 * To use the ocamlc compiler or ocamlbuild, you need to include the 
 * Unix library as well.
 * For example:
 *
 * ocamlc -o queue unix.cma ...
 * ocamlbuild -libs unix ...
 *)

let time_fun f = 
  fun x -> 
    let t0 = Unix.gettimeofday() in 
    let _ = f x in 
    let t1 = Unix.gettimeofday() in 
      (t1 -. t0)


(* iterate is like reduce or fold_left for numbers -- we have
 * a function s that tells us what to do when we have a non-zero
 * number, a value z that tells us what to do when the number is
 * zero, and a number n which is how far we want to count. *)
let rec iterate s z n = 
  if n >= 0 then iterate s (s n z) (n-1) else z


(* (gen n) generates the list of numbers from 0 to n *)
let gen = iterate (fun i rest -> i::rest) []


let iter_time_fun f arg n = 
  let t = time_fun (iterate (fun i _ -> f arg; ()) ()) n in
    t /. (float_of_int n)


(* TESTING *)

(* A signature for a sorting module *)
module type SORT = 
sig 
  val sort : ('a->'a->bool) -> 'a list -> 'a list 
end 

(* Insertion Sort *)
module InsertSort : SORT = 
struct
  let rec insert lt xs x = 
    match xs with 
      | [] -> [x]
      | h::t -> if lt x h then x::xs else h::(insert lt t x)

  let sort lt = List.fold_left (insert lt) []
end

(* For example, we can now define isort to be 
 * functions which return the total time taken for 
 * merge-sort and insertion-sort, given a list. *)
let isort : int list -> float = time_fun (InsertSort.sort (<)) 
  
