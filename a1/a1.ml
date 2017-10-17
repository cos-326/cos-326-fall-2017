(*** COS 326 Problem Set 1 ***)
(*** YOUR NAME HERE ***)
(*** YOUR LOGIN HERE ***)

let undefined : unit -> 'a = fun () -> failwith "undefined"

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by compiling with "ocamlbuild" in the terminal
 * emulator, or by using an evaluation plugin installed in your editor,
 * for example, Ctrl+c and then Ctrl+e in Emacs with Tuareg mode *)

(* 1.a. Create a string with your first name *)
let name : string = "Sarah"

(* 1.b. Use a string operator on the string from 1.a. to create
 * a string that contains both your first and last names. *)
let name : string = name ^ " Port"

(* 1.c. Create a string containing your email address *)
let email : string = "sarah@carbonfive.com"

(* 1.d. Replace (Other "...") in class_year with the appropriate item below *)
(* ie: replace (Other "...") with Sophomore or Junior for example *)
type year = Freshman | Sophomore | Junior | Senior | Other of string

let class_year : year = Other "I graduated!"

(* 1.e. Replace the .... with what you're excited about in this course *)
let exciting : string = "I'm excited about learning about ocaml, emacs, and functional programming!"

let print = Printf.printf

let print_survey () =
  let string_year =
    (match class_year with
       | Freshman -> "2021"
       | Sophomore -> "2020"
       | Junior -> "2019"
       | Senior -> "2018"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s\n" name;
     print "Email: %s\n" email;
     print "Year: %s\n" string_year;
     print "%s\n" exciting;
     print "----------------------------------------\n\n")

(* Problem 2 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission.
 * Note that the expressions might not do anything useful -- and in fact
 * might even display interesting problems! -- but all you should do is fill
 * in the ???s to make them type check. *)

(* Problem 2a *)
(*
let prob2a : string  = let greet y = "Hello " ^ y in greet "World!"
*)

(* Problem 2b *)
(*
let prob2b : float = float_of_int( int_of_float(2.2 +. 7.7))
*)

(*>* Problem 2c *>*)
(*
let rec prob2c (x : ???) : ??? =
  prob2c ( if true then prob2c x else 'h')

prob2c will never return 'h' because true is always true.
So prob2c will always return prob2c X
So x could be literally any type.

Does prob2c return a type string, because the theoretical terminating condition returns 'h':string
or does prob2c return a function of whatever type prob2c is, because that terminating condition is never reached?

*)

(*>* Problem 2d *>*)
(*
let rec prob2d (y:bool) (z:bool) : ??? =
   prob2d (prob2d z y) (not y)

y has to be a bool, because not is only an operator on bools.
so z must also be a bool, since we're calling prob2d with variables of type ??? and bool.
*)

(* Explain why each of 3a, 3b, 3c will not compile (use the strings
 * exp3{a,b,c} for your answers) and change the code in some small way
   so that it does, and leave prob3{a,b,c} uncommented. Do not change
   the top-level type associated with the expression. *)

(*>* Problem 3a *>*)
let exp3a : string = "4 is type int, expects float"
(*
let prob3a : bool =
  let compare x y = x < y in
  compare 3.9 4.
*)

(*>* Problem 3b *>*)
let exp3b : string = "aux expects 3 inputs of the same type, gets ints and functions."
(*
let prob3b : int =
  let fib n =
   let rec aux n y x =
    if n <= 0 then x
    else aux (n-1) (x+y) y
   in
   aux n 1 0
  in
  fib 10
*)


(*>* Problem 3c *>*)
let exp3c : string = "haven't defined sumTo as recursive"
(*
let prob3c : int =
  let rec sumTo (n:int) : int =
  if n <= 0 then 0
  else n + sumTo (n-1)
  in
  sumTo 10
*)

(*>* Problem 4 *>*)
(* 4a: Fill in the ??? with an expression that uses x and y and has
 * the right type.
 *
 * HAHA IT'S 42
*)

(*
let prob4a =
  let u = 32.0 in
  let v = 28.0 in
  let square w = w *. w in
  let boff (x) (y) = (square x) +. (square y) in
  let d = sqrt (boff u v) in
  int_of_float d
*)

(*
 * Also:  What warning message do you get if your ??? expression does not
 * use the function "square"?
 *
*)
let warn4a : string = "unused variable square"

(* 4b: Replace each ?? with the type of the corresponding expression,
 * and write a function f that has the correct type signature. Explain
 * in exp4b a problem that remains with the function prob4b *)

(*
let f (a:int) (b:int) : float = float_of_int (a + b)



let rec prob4b (x:float) (y:int) : ?? =
  prob4b (f y 4) (int_of_float x)


  * because int_of_float x returns an int, y must be an int
  * because int_of_float x takes a float, x must be a float
  * since y and 4 in f y 4 are ints, a and b must be ints
  * since f y 4 is being passed to prob4b which calls for a float there, f must return a float.
  * float_of_int takes an int and returns a float.
*)

let exp4b : string = "this recursion doesn't terminate"

(* 4c: Is it possible to find types for the argument and result that
 * make the function forever type check?
 *
 * Either give correct types or explain why it is impossible in the
 * string exp4c *)

(*
let rec forever (x:??) : ?? =
  forever forever

let exp4c : string = "not possible, because there will always be another function argument in the chain when it's being called"
*)

(*>* Problem 5 *>*)

exception BadDivisors of int * int
let bad_divisors n m = raise (BadDivisors (n,m))

(* Write the function few_divisors, which takes two parameters n and m,
 * and should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise: *)
(*
let _ = few_divisors 17 3 (* true -- 17 divides only 1 and 17 *)
let _ = few_divisors 4 3  (* false -- 4 divides 1, 4, and 2 *)
let _ = few_divisors 4 4  (* true -- 4 divides only 1, 4, and 2 *)
let _ = few_divisors 18 6 (* false -- 18 divides 1, 18, 2, 3, 6, and 9 *)
let _ = few_divisors 18 7 (* true -- 18 divides only 1, 18, 2, 3, 6, and 9 *)
 *)

(* The type signature for few_divisors is:
 * few_divisors : int -> int -> bool
 *)

(* few_divisors should call the function bad_divisors n m defined above
 * if n <= 0 or m < 0
 *)

let rec range (n:int): 'a list =
  if n = 0 then []
  else range (n-1) @ [n]

let number_of_divisors (n:int): int =
  List.length (List.filter (fun v -> (n mod v) = 0) (range n))

let few_divisors (n:int) (m:int): bool =
  if (n <= 0 || m < 0) then bad_divisors n m
  else if (number_of_divisors n) < m then true
  else false

(* After writing few_divisors above, uncomment the following lines to test your
 * code.  (Note: your code is not necessarily completely correct just because
 * it passes these 3 tests.)  *)

let _ = assert (few_divisors 17 3)
let _ = assert (not (few_divisors 4 3))
let _ = assert (few_divisors 4 4)


(* Problem 6 - Approximating Pi *)

exception BadPi of int
let bad_pi (n:int) = raise (BadPi n)

(*>* Problem 6a - Sinusoidal Approximation *>*)

(* Use the following equations to define a function sin_pi that returns
 * the ith approximation of pi.

 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))

 * Using this approximation, you will converge on many digits of pi very
 * fast.  The first few digits of pi are 3.14159 26535 89793 23846 26433.
 * Approximation 1 accurately predicts these digits:  3.141
 * Approximation 2 accurately predicts these digits:  3.14159 26535
 * Approximation 3 accurately predicts these digits:  3.14159 26535 89793
 *
 *)

(* The type signature for sin_pi is:
 *   sin_pi : int -> float
*)

(* sin_pi should call the function bad_pi i if its argument i is less than 0 *)

let rec sin_pi (n:int) : float =
  if n < 0 then bad_pi n
  else if n = 0 then 3.
  else (sin_pi (n-1)) +. (sin (sin_pi (n-1)))

(*>* Problem 6b - Monte Carlo Approximation*>*)
(*
 * A Monte Carlo method relies on repeated random sampling to simulate
 * some process or compute a value.  See Wikipedia:
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 *
 * Pi can be computed using Monte Carlo simulation through a series
 * of experiments.  Here is a single experiment:
 *
 *  -- choose a pair of random floating point numbers between 0 and 1
 *  -- call the numbers x and y
 *  -- think of (x,y) as a point on the plane in the unit square
 *  -- test whether the point falls within the unit circle by measuring
 *     the distance from the point to the origin:  x^2 + y^2 <= 1
 *
 * Now suppose you do m experiments and in n of those experiments, the
 * random point chosen falls within the upper right quarter of the unit circle.
 * Since the area of a circle is known to be pi * r^2 and the area of
 * a square is r^2 (and here we are dealing with a radius/square side
 * of length 1), the following equations hold:

  n    quarter of area of circle     1/4 * pi * r^2
 --- = -------------------------  =  -------------- = 1/4 * pi
  m        area of square                r^2

 * Use the above information to write the function monte_pi, which
 * takes a positive number indicating the number of random points n to
 * sample and approximates pi using that number of random points.
 *)

(* The type signature for monte_pi is:
 *   monte_pi : int -> float
 * monte_pi should call bad_arg i when its argument i is not positive.
 *)

(*
 * To compute some random numbers, use OCaml's Random library:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html
 *
 * We initialize the library below.
 *
 * Random.float f will return a random floating point number between 0.0 and f.
 *
 * Note: this estimation method will converge far more slowly than the
 * sinusoidal method (because the sin function already captures pi, so
 * that approximation was really cheating!).  I only had the first 2
 * digits after 5000 trials.
 * I estimated pi at 3.141628 after 1,000,000 trials (your result may
 * vary depending on exact details of your computation)

 * =======
 * WARNING:
 * =======
 * If you make too many recursive calls in a row, you may run in
 * to a stack overflow error that looks like this:

 * Stack overflow during evaluation (looping recursion)?

 * Do not worry about that message -- just try your code on fewer trials.
 * You don't necessarily have to be able to execute 1,000,000.  For example,
 * if you code works on 100 or 1000 recursive calls, that is just fine.
 *
 * "Too many calls" may vary on different machines. Later in the semester
 * we will discuss "tail recursion" and how to fix this problem.
 *
 *)

(* Don't remove these lines; Your code should follow after them  *)
let _ = Random.init 17
exception BadArg of int
let bad_arg (n:int) = raise (BadArg n)
let is_unit_circle (x:float)(y:float):int = 
  if x**2. +. y**2. <= 1. then 1
  else 0
let rec random_points (n:int): int =
  if n = 0 then 0
  else random_points (n-1) + is_unit_circle (Random.float 1.) (Random.float 1.)
let monte_pi (n:int): float =
  if n < 0 then bad_arg n
  else 4. *. (float_of_int (random_points n)) /. (float_of_int n)
let _ = print_float (monte_pi 5000)

(*************)
(* Problem 7 *)
(*************)

(* Look up another technique for approximating pi on the web.
 * As a starting point, see here:
 *
 * http://en.wikipedia.org/wiki/Approximations_of_%CF%80
 *
 * You might be able to find other interesting articles on the web too.
 *
 * The algorithm you choose must be capable of computing many digits of
 * pi.  Algorithms that compute just 1 approximation (such as 3 or
 * 3927/1250 or any other fixed fraction) are insufficient.  Choose
 * an algorithm that successively approximates pi in some manner.
 * Your algorithm may not use trigonometric functions such as sin,
 * cos, arctan, etc.
 *
 *)

(* 7a:  Explain your algorithm and your sources in the string exp7a: *)
let exp7a : string = ""

(* 7b:  Implement your algorithm here. *)
(*      Your algorithm should take a positive integer parameter
 *      which increases the precision of your approximation as it
 *      increases. You should call bad_arg when a non-positive argument is
 *      given. In comments, explain what the parameter is used for in your
 *      algorithm and show some tests.
 *      The signature for your function is: custom_pi : int -> float

 *      Again, don't worry about "stack overflow" errors for large values
 *      of the input to custom_pi.
*)
