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
    | Binop (b, o1, o2) -> 
        let o1' = derivative o1 in
        let o2' = derivative o2 in
        match b with
          | Add -> Binop (Add, o1', o2')
          | Sub -> Binop (Sub, o1', o2')
          | Mul -> Binop (Add, (Binop (Mul, o1', o2)), (Binop (Mul, o1, o2')))



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

let rec distribute (e:expression) : expression =
  match e with
  (* m * (x + a) -> m * x + m * a *)
  | Binop (Mul, m, Binop (Add, x, a)) -> let m' = distribute m in Binop (Add, Binop (Mul, m', distribute x), Binop (Mul, m', distribute a))
  (* m * (x - a) -> m * x - m * a *)
  | Binop (Mul, m, Binop (Sub, x, a)) -> let m' = distribute m in Binop (Sub, Binop (Mul, m', distribute x), Binop (Mul, m', distribute a))
  (* distribute all that can be in this expression *)
  | Binop (op, e1, e2) -> Binop (op, distribute e1, distribute e2)
  | Var | Num _ -> e

let max (n1:int) (n2:int) = if n1 > n2 then n1 else n2

let rec degree (e:expression): int =
  match e with
  | Var -> 1
  | Binop (Mul, e1, e2) -> degree e1 + degree e2
  | Binop (_, e1, e2)   -> max (degree e1) (degree e2)
  | _ -> 0

let rec standard_form (e:expression) : expression =
  match e with
  | Binop (Mul, Binop (op, e1, e2), Num n) -> standard_form (Binop (Mul, Num n, Binop (op, standard_form e1, standard_form e2)))
  | Binop (Add, Num n, Binop (op, e1, e2)) -> standard_form (Binop (Add, Binop (op, standard_form e1, standard_form e2), Num n))
  | Binop (Add, e1, e2) ->
      let e1' = standard_form e1 in
      let e2' = standard_form e2 in
      if degree e1 > degree e2
      then Binop (Add, e1', e2')
      else Binop (Add, e2', e1')
  | _ -> e

let rec normalize (e:expression) : expression =
  match e with
  | Binop (Sub, e1, e2) -> Binop (Add, normalize e1, Binop (Mul, Num (~-. 1.), normalize e2))
  | Binop (op, e1, e2)  -> Binop (op, normalize e1, normalize e2)
  | _ -> e

let rec simplify (e:expression) : expression =
  match e with
  (* 
   * 1 + 3 -> 4
   * 2 - 1 -> 1
   * 3 * 4 -> 12
   *)
  | Binop (Add, Num n1, Num n2) -> Num (n1 +. n2)
  | Binop (Sub, Num n1, Num n2) -> Num (n1 -. n2)
  | Binop (Mul, Num n1, Num n2) -> Num (n1 *. n2)
  (* 
   * ((e + 1) + 3) -> e + 4
   * ((e - 3) + 2) -> e - 1
   * ((e - 4) - 5) -> e - 9
   * ((e + 3) - 2) -> e + 1
   *)
  | Binop (Add, Binop (Add, a, Num n1), Num n2) -> simplify (Binop (Add, simplify a, Num (n1 +.  n2)))
  | Binop (Add, Binop (Sub, a, Num n1), Num n2) -> simplify (Binop (Sub, simplify a, Num (n1 -.  n2)))
  | Binop (Sub, Binop (Sub, a, Num n1), Num n2) -> simplify (Binop (Sub, simplify a, Num (n1 +.  n2)))
  | Binop (Sub, Binop (Add, a, Num n1), Num n2) -> simplify (Binop (Add, simplify a, Num (n1 -.  n2)))
  (* 
   * 3 * x + 2 * x -> 5 * x
   * 3 * x - 2 * x -> 5 * x
   * 3 * x * 2 * x -> 6 * x * x
   *)
  | Binop (Add, Binop (Mul, Num n1, Var), Binop (Mul, Num n2, Var)) -> simplify (Binop (Mul, Num (n1 +. n2), Var))
  | Binop (Sub, Binop (Mul, Num n1, Var), Binop (Mul, Num n2, Var)) -> simplify (Binop (Mul, Num (n1 -. n2), Var))

  (* 
   * (3 * (5 * a)) -> 15 * a
   *
   * ((2*x) + 16) + (5*x)
   *)
  | Binop (Mul, Num n1, Binop (Mul, Num n2, a)) -> simplify (Binop (Mul, Num (n1 *. n2), simplify a))
  (* be recursive *)
  | Binop (op, e1, e2) -> Binop (op, simplify e1, simplify e2)
  | _ -> e

(*


insert (Num n1) (Num n2) = Binop (Add, Num n1, Num n2)
insert Var Num n = Binop (Add, Var, Num n)

5*x - 3 + 2*(x - 8) + 17*(3*x + 9)

   0
-> (+ ( * 5 x) 0)           when adding binop to num always add to left
-> (+ (+ ( * 5 x) 0) -3)    when adding num to binop always add to right

   0
-> 0 + (-3)                 when adding num to num always add to right

*)

let rec insert (into:expression) (e':expression) : expression =
  match (e', into) with
  | (Binop (Mul,_,_), Binop (Add,e1,e2)) ->
      if degree e' > degree e1
      then Binop (Add, e', into)
      else Binop (Add, insert e1 e', e2)
  | (Binop (Mul,_,_), Var)              -> Binop (Add, e', into)
  | (Binop (_,_,_), Num _)              -> Binop (Add, e', into)
  | (Num _, Binop (_,_,_))              -> Binop (Add, into, e')
  | (Num _, Num _)                      -> Binop (Add, into, e')
  | (Var, Var)                          -> Binop (Add, into, e')
  | (Var, Binop (Add, e1, e2))          -> Binop (Add, insert e1 e', e2)
  | (Var, Binop (Mul, e1, Var))         -> Binop (Mul, e1, (insert Var e'))
  | _                                   -> into

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
