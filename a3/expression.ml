(* 

Name:
Email:
Minutes Spent on Problem 2:

(You aren't in any way graded on the number of minutes spent; 
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

*)

open Ast 
open ExpressionLibrary 

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry 
 *    about expressionLibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
*)


(*>* Problem 2.1 *>*)

(* evaluate : evaluates an expression for a particular value of x. 
 *  Example : evaluate (parse "x*x + 3") 2.0 = 7.0 *)

let operator_of_binop = function
  | Add -> (+.)
  | Sub -> (-.)
  | Mul -> ( *. )

let rec evaluate (e:expression) (x:float) : float =
  match e with 
  | Num n -> n
  | Var -> x
  | Binop (binop, e1, e2) -> (operator_of_binop binop) (evaluate e1 x) (evaluate e2 x)

(*>* Problem 2.2 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with
  | Num n -> Num 0.
  | Var -> Num 1.
  | Binop (Add, e1, e2) -> Binop (Add, (derivative e1), (derivative e2))
  | Binop (Sub, e1, e2) -> Binop (Sub, (derivative e1), (derivative e2))
  | Binop (Mul, e1, e2) -> 
    Binop 
      (Add,
       Binop (Mul, (derivative e1), e2),
       Binop (Mul, e1, (derivative e2)))

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
    print_string "Result of evaluation: ";
    print_float  (evaluate parsed xval);
    print_endline " ";
    print_string "Result of derivative: ";
    print_endline " ";
    print_string (to_string (derivative parsed));
    print_endline " ")


(*>* Problem 2.3 *>*)
let within (y: float) (epsilon:float) (g: float): bool =
  y -. epsilon < g && g < y +. epsilon 

let near_zero = within 0.

let rec find_zero' (f: float -> float) (f': float -> float) (g:float) (epsilon:float) (lim:int): float option =
  if lim = 0 then None
  else
    match f g with
    | y when near_zero epsilon y ->
      Some g
    | y ->
      find_zero' f f' (g -. (y /. (f' g))) epsilon (lim - 1)

(* See writeup for instructions. *)
let find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
  : float option =
  find_zero' (evaluate e) (evaluate (derivative e)) g epsilon lim

(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
  failwith "Not implemented" 



(*>* Problem 2.5 *>*)

(* See writeup for instructions. This problem is completely optional.
 * Write code here to address the problem, and once finished, 
 * fill the variables below with your answers and uncomment them
*)
              (*
              let e_order_of_growth : string  = 
              let iterations_for_56_bits : int = 
             *)
