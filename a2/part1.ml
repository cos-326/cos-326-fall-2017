(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type. 
   (Do not change the left-hand-side.)
*)

let exp1a : string = "Lists are delimited with ;"
let prob1a : int list = [1; 2; 3]

(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
*)
let exp1b : string = "list was modifying int, not the whole tuple"
let prob1b : (string * int) list = [("COS", 326); ("COS", 441)]

(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type. 
   (Do not change the left-hand-side.)  
*)
let exp1c : string = ":: cons a single value to the head, not a list"
let prob1c : float list = 2.0 :: 3.0 :: [4.0; 5.0]


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types: 
 *
 * NOTE: for option, list, and function types, you must 
 * provide a nontrivial answer. For a list that means a 
 * non-empty one, for an option type that means a Some 
 * construction, and for a function, that means using 
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int = 
 *         String.length  ((string_of_int x) ^ (string_of_int y))
*)

(*>* Problem 2a *>*)
let prob2a : (float * (string * int) option list) list = [(1., [Some ("foo", 2)])]


(*>* Problem 2b *>*)
(* a student is a (name, age option) pair *)

type student = string * int option

let prob2b : (student list option * int) list = 
  [(Some [("Jane", Some 90); ("Bob", None)], 1)]

(*>* Problem 2c *>*)
let prob2c : (int * int -> int) * (float -> float -> unit) * bool  = 
  (fun (x, y) -> x), (fun x y -> ()), true


(*>* Problem 2d *>*)
(* Fill in a valid function call to foo to make prob2d typecheck *)

let prob2d =
  let rec foo bar =
    match bar with
    | (a, (b, c)) :: xs -> if a then (b + c + (foo xs)) else foo xs
    | _ -> 0
  in foo [(true, (2, 3))]

(*************)
(* PROBLEM 3 *)
(*************)

(* Consider the following terribly written function: *)

let rec zardoz f ls acc =
  if (((List.length (ls@[])) = 1) = true) then (f (List.hd(ls)) (acc))
  else if (((List.length ls) = 0) = true) then acc
  else
    let hd = List.hd(ls) in
    let tl = List.tl(ls) in
    let ans = f (hd) (acc) in
    let ans = zardoz f tl ans in
    ans

(* Rewrite the code above so that it does the same thing
 * but style-wise is far superior.  
 * Be sure to provide types for the function's arguments and to 
 * call itself (not the original zardoz) recursively as needed.
 * You may want to write some assert statements
 * to check that your function is doing the same thing as zardoz.  
 * Use the COS 326 style guide. *)

let rec myzardoz (f: 'a -> 'b -> 'b) (ls: 'a list) (acc: 'b): 'b =
  match ls with
  | [] -> acc
  | hd :: tl -> myzardoz f tl (f hd acc)

(*************)
(* PROBLEM 4 *)
(*************)

(***************************************)
(* Conway's Lost Cosmological Theorem! *)
(***************************************)

(* 

If l is any list of integers, the look-and-say list of s is obtained by 
reading off adjacent groups of identical elements in s. For example, the 
look-and-say list of

l = [2; 2; 2]

is

[3; 2]

because l is exactly "three twos." Similarly, the look-and-say sequence of

l = [1; 2; 2]

is

[1; 1; 2; 2]

because l is exactly "one ones, then two twos."

You will now define a function look_and_say that computes the 
look-and-say sequence of its argument. look_and_say of an empty 
list is the empty list. 

For full credit your solution should be a linear time solution.

CULTURAL ASIDE:

The title of this problem comes from a theorem about the sequence generated 
by repeated applications of the "look and say" operation. As look and say 
has type int list -> int list, the function can be applied to its own result. 
For example, if we start with the list of length one consisting of just the 
number 1, we get the following first 6 elements of the sequence:

[1]
[1,1]
[2,1]
[1,2,1,1]
[1,1,1,2,2,1]
[3,1,2,2,1,1]

Conway's theorem states that any element of this sequence will "decay" 
(by repeated applications of look and say) into a "compound" made up of 
combinations of "primitive elements" (there are 92 of them, plus 2 
infinite families) in 24 steps. If you are interested in this sequence, 
you may wish to consult [Conway(1987)] or other papers about the 
"look and say" operation.

====== 

Progamming practice aside related to the "look and say" problem. You
may find this useful for constructing your solution to "look and say",
or you may not. 

Another interesting list problem is determining "runs"
in a list: maximal length sublists with all equal elements. For
example,

[1; 1; 1] and [5]

are both runs of the list

[1; 1; 1; 5; 2]

but

[1; 1] and [5; 2] and [1; 2]

are not: 

[1; 1] is not maximal
[5; 2] has unequal elements
[1; 2] is not a sublist.

*)

(* :: isn't a real function, so (::) doesn't work :-\ *)
let reverse xs = List.fold_left (fun acc x -> x::acc ) [] xs

let rec look_and_say' (xs: int list) (acc: int list) (current: int) (count: int) : int list = 
  match xs with
  | [] -> (current::count::acc)
  | hd::tl when hd = current -> look_and_say' tl acc current (count + 1)
  | hd::tl -> look_and_say' tl (current::count::acc) hd 1

let look_and_say (xs: int list) : int list = 
  match xs with
  | [] -> [] 
  | hd::tl -> reverse (look_and_say' tl [] hd 1)

(*************)
(* PROBLEM 5 *)
(*************)

(* Write a function that flattens a list of lists in to a single
 * list with all of the elements in the same order they appeared in 
 * the original list of lists. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6] 
 * flatten [[]; ['e';'d']; ['a';'b';'c']] = ['e';'d';'a';'b';'c'] 
*)

let rec flatten (xss:'a list list) : 'a list =
  match xss with 
  | [] -> []
  | hd::tl -> hd @ (flatten tl)

(*************************************)
(* PROBLEM 6 -- Warning: Challenging!*)
(*************************************)

(* Return the list of all permutations of the input list. eg: 
   perm [1;2;3] = [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] 
   The ordering of the permutations does not matter in your solution.
   We will accept either [[]] or [] as the result for perm [].
   NB: test this on small inputs - perm is ~n! which is approximately ~n^n.
*)

(* List.fold_left cycle [] items

   let cycle (items:'a list) (acc:'a list list) (items:'a list) : 'a list list =
   match cycles with
   0 -> 

   let perm (items:'a list) : 'a list list =
   let length = (List.length items) in
   [] *)
