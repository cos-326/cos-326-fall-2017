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
let near_zero (epsilon:float) (g: float): bool =
  (abs_float g) < epsilon

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
type standard =
  | Ax of float 
  | B of float
  | Exp of float

let combine_numbers op n1 n2: float =
  ((operator_of_binop op) n1 n2)

let rec zip_with' (f: 'a -> 'a -> 'a) (xs: 'a list) (ys: 'a list) (acc: 'a list): 'a list =
  match (xs, ys) with
  | ([], _) ->
    List.rev acc
  | (xhd::xtl, []) ->
    zip_with' f xtl [] (xhd::acc)
  | (xhd::xtl, yhd::ytl) ->
    zip_with' f xtl ytl ((f xhd yhd)::acc)

let zip_with (f: 'a -> 'a -> 'a) (xs: 'a list) (ys: 'a list): 'a list =
  if (List.length xs) >= (List.length ys) then
    zip_with' f xs ys []
  else
    zip_with' f ys xs []

let rec add_to_head (n: int) (pad: 'a) (xs: 'a list): 'a list =
  if n <= 0 then xs
  else add_to_head (n - 1) pad (pad::xs)

let a_mult (n: float) (xs: float list): float list =
  List.map (( *. ) n) xs

let x_mult (a: float) (exponent: int) (xs: float list): float list =
  let xs = a_mult a xs in
  add_to_head exponent 0. xs

let poly_add (xs: float list) (ys: float list): float list =
  zip_with (+.) xs ys

let poly_sub (xs: float list) (ys: float list): float list =
  zip_with (-.) xs ys

let rec poly_mul' (xs: float list) (ys: float list) (acc: float list list): float list =
  (* The current exponent is the number of exponents processed so far *)
  let exponent = List.length acc in
  match xs with 
  | [] -> 
    (* add the products together at the end *)
    List.fold_left poly_add [] acc 
  | hd::tl ->
    let multiplied = x_mult hd exponent ys in
    poly_mul' tl ys (multiplied::acc)

let poly_mul (xs: float list) (ys: float list): float list =
  poly_mul' xs ys []

let poly_op: (Ast.binop) -> (float list -> float list -> float list) = function
  | Add -> poly_add
  | Sub -> poly_sub
  | Mul -> poly_mul

let rec standardize: expression -> float list = function
  | Num n -> [n]
  | Var -> [0.; 1.]
  | Binop (op, e1, e2) -> (poly_op op) (standardize e1) (standardize e2)

let rec trim_head (empty: 'a) (xs: 'a list): 'a list =
  match xs with
  | hd::tl when hd = empty -> trim_head empty tl
  | _ -> xs

let trim_tail (empty: 'a) (xs: 'a list): 'a list =
  List.rev (trim_head empty (List.rev xs))

let rec find_zero_exact (e:expression) : expression option =
  match e |> standardize |> trim_tail 0. with
  | [b; a] when a > 0. -> Some (Num (((-1.) *. b) /. a))
  | _ -> None

(*>* Problem 2.5 *>*)

(* See writeup for instructions. This problem is completely optional.
 * Write code here to address the problem, and once finished, 
 * fill the variables below with your answers and uncomment them
*)
              (*
              let e_order_of_growth : string  = 
              let iterations_for_56_bits : int = 
             *)
