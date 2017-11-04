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

let evaluateBinop (b:binop) (o1:float) (o2:float) : float =
  match b with
    | Add -> o1 +. o2
    | Sub -> o1 -. o2
    | Mul -> o1 *. o2

(* evaluate : evaluates an expression for a particular value of x. 
 *  Example : evaluate (parse "x*x + 3") 2.0 = 7.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
    | Num n -> n
    | Var -> x
    | Binop (b,o1,o2) -> evaluateBinop b (evaluate o1 x) (evaluate o2 x)

(* derivative(f + g)(x) = derivative(f)(x) + derivative(g)(x)
 * derivative(f - g)(x) = derivative(f)(x) - derivative(g)(x)
 * derivative(f * g)(x) = derivative(f)(x) * g(x) + f(x) * derivative(g)(x)
 *)

(*>* Problem 2.2 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with
    | Num n -> Num 0.0
    | Var -> Num 1.0
    | Binop (b,o1,o2) -> match b with
                           | Add -> Binop (Add,(derivative o1),(derivative o2))
                           | Sub -> Binop (Sub,(derivative o1),(derivative o2))
                           | Mul -> Binop (Add,(Binop (Mul,(derivative o1), o2)),(Binop (Mul,o1,(derivative o2))))



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

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int) : float option =
  if lim == 0
    then None
    else
      let r = evaluate e g in
      if r < epsilon
        then Some g
        else
          let e' = derivative e in
          let g' = g -. ((evaluate e g) /. (evaluate e' g)) in
          find_zero e g' epsilon (lim-1)



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
