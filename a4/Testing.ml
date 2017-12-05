(* Testing *)

(* In this file, we build abstract syntax trees representing various
   functions for the purpose of testing our evaluators.

   You will have to construct some additional functions yourself.
*)

open Syntax
open Printing

(************)
(* INTEGERS *)
(************)

(* Useful Constants *)
let zero = Constant (Int 0) 
let one = Constant (Int 1) 
let two = Constant (Int 2) 
let three = Constant (Int 3) 
let four = Constant (Int 4)

(***********************)
(* RECURSIVE FUNCTIONS *)
(***********************)

(* let z = 2 in (let x = 3 in fun y -> x + y + z) (let x = 4 in x + z) *)
let clo =  
  Let ("z", two,
       App (Let ("x", three, 
                 Rec ("f", "y", 
                      Op (Var "x", Plus, Op (Var "y", Plus, Var "z")))
                ),
            Let ("x", four, Op (Var "x", Plus, Var "z"))
           )
      )

(* rec fact n = if n < 1 then 1 else n * fact (n - 1) *)
let fact = 
  Rec ("fact", "n", 
       If (Op (Var "n", Less, one),
           one,
           Op (Var "n", Times, 
               App (Var "fact", 
                    Op (Var "n", Minus, one)))))

(* fact 4 *)
let fact4 = App (fact, four)

(*********)
(* PAIRS *)
(*********)

(* the pair (1,2) *)
let p1 = Pair (one, two)

(* the function swap below is equivalent to:
       let swap p = let (x,y) = p in (y,x)
*)
let swap = 
  Rec ("swap", "p",
       Let ("x", Fst (Var "p"),
            Let ("y", Snd (Var "p"),
                 Pair(Var "y", Var "x"))))

(* use swap to swap the elements of p1 *)
let swap_p1 = App (swap, p1)

(*********)
(* LISTS *)
(*********)

(* takes an OCaml list of expressions and generates a single expression
   representing the list
*)
let rec listify (l:exp list) : exp =
  match l with
    [] -> EmptyList
  | hd::tl -> Cons(hd,listify tl)

(* a list of 4 numbers *)
let list4 = listify [one;two;three;four] 

(* rec sumlist l = 
 *   match l with
 *     [] -> 0
 *   | hd::tl -> hd + sumlist tl *)
let sumlist = 
  Rec ("sumlist", "l", 
       Match (Var "l",
              zero,
              "hd", "tl", Op (Var "hd", Plus, 
                              App (Var "sumlist", Var "tl"))))

let sl4 = App (sumlist, list4)

(*******************************)
(* QUESTIONS FOR YOU TO ANSWER *)
(*******************************)

(* NOTE: NONE OF THE FUNCTIONS YOU WRITE BELOW SHOULD INCLUDE
 * Closure (env,f,x,e)
 *
 * Define recursive functions using the Rec(f,x,body) form
*)

(* Replace the constant "one" below with your implementation of 
   the function map : ('a -> 'b) -> 'a list -> 'b list 
   Note: do not implement this as map: (('a -> 'b)*'a list) -> 'b list
*)

let map = Rec ("map", "f",
            Rec ("loop", "l",
              Match (Var "l",
                     EmptyList,
                     "hd", "tl", Cons (App (Var "f", Var "hd"), App (Var "loop", Var "tl")))
            )
          )

(* Replace the constant "one" below with your implementation of 
   the function plus1 that adds one to an integer *)
let plus1 = Rec ("plus1", "n", (Op (Var "n", Plus, one)))

(* Use plus1 and map, defined above, to implement the function 
   incr_all, which adds 1 to every element of a list. Examples:

   incr_all [] == []
   incr_all [1;2;3] == [2;3;4]
*)
let incr_all = Rec ("incr_all", "l", (App (App (map, plus1), Var "l")))

let test_incr_all = App (incr_all, list4)

(* Replace the constant one below by implementing a function that 
 * takes a list of pairs of integers and returns a list of integers 
 * where each element of the list is the sum of the elements of the 
 * pairs.  Examples:

   sum_pairs [] == []
   sum_pairs [(1,2); (3,4)] == [3; 7]
*)

let sum_pair = Rec ("sum_pair", "p", Op (Fst (Var "p"), Plus, Snd (Var "p")))
let sum_pairs = Rec ("sum_pairs", "l", App (App (map, sum_pair), Var "l"))

(*
 * AUX TESTS
 *)
let id = Rec ("id", "x", Var "x")

let simple_if : exp = If (Constant (Bool false), one, two)
let simple_op : exp = Op (one, Plus, four)
let simple_let : exp = Let ("x", three, Var "x")
let simple_let_2 : exp = Let ("x", three, Op (two, Plus, Var "x"))
let simple_pair : exp = Pair (one, two)
let simple_pair_2 : exp = Pair (simple_let, two)
let simple_match : exp = Match (EmptyList, one, "hd", "tl", two)
let simple_match_2 : exp = Match (Cons (one, EmptyList), two, "hd", "tl", three)
let simple_match_3 : exp = Match (Cons (one, EmptyList), two, "hd", "tl", (Var "tl"))
let simple_f = App (id, Constant (Int 420))
let simple_map = App (App (map, id), list4)
let simple_sum_pairs = App (sum_pairs, Cons (Pair (one, two), Cons (Pair (three, four), EmptyList)))
let simple_semantic_fail = Let ("x",four,(If (one, two, three)))
let simple_semantic_fail_1 = Let ("x",four,(Op (one, Plus, Constant (Bool false))))

(*********)
(* TESTS *)
(*********)

(* Feel free to add many more tests of your own to the list *)
let tests = [
  zero;
  simple_if;
  simple_op;
  simple_let;
  simple_let_2;
  simple_pair;
  simple_pair_2;
  simple_match;
  simple_match_2;
  simple_match_3;
  simple_f;
  fact4;
  list4;
  sl4;
  clo;
  simple_map;
  test_incr_all;
  simple_sum_pairs;
  simple_semantic_fail_1;
  simple_semantic_fail;
]

let run_test eval exp =
  Printf.printf "========\n";
  Printf.printf "%s\n" (string_of_exp exp);
  Printf.printf "Evaluates to:\n";
  Printf.printf "%s\n" (string_of_exp (eval exp));
  Printf.printf "========\n"

let run_tests eval tests =
  List.iter (run_test eval) tests

let xPlus2 = Op (Var "x", Plus, two)
let xIs3InXPlus2 = Let ("x", three, xPlus2)
