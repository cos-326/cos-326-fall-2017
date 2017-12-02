let rec exp n =
  match n with
  | 0 -> 1
  | n -> 2 * exp (n-1)

(*

Theorem: For all natural numbers n, exp n == 2^n

Proof: By induction on natural numbers n.

exp n = 2^n

Case: n = 0

   exp 0
== match 0 with | 0 -> 1 | n -> 2 * exp (n-1)        (eval exp)
== 1                                                 (eval case)
== 2^0                                               (math)

Case: n = k+1

   exp (k+1)
== match (k+1) with | 0 -> 1 | n -> 2 * exp (n-1)    (eval exp)
== 2 * exp ((k+1)-1)                                 (eval case)
== 2 * exp k                                         (math)
== 2 * 2^k                                           (inductive hypothesis)
== 2^(k+1)                                           (math)

QED

*)

let rec even n =
  match n with
  | 0 -> true
  | 1 -> false
  | n -> even (n-2)

(*

Theorem: For all natural numbers n, even (2*n) == true

Proof: By induction on natural numbers n.

Case: n = 0

   even (2*0)
== even 0                                                           (math)
== match n with | 0 -> true | 1 -> false | n -> even (n-2)          (eval even)
== true                                                             (eval case)

Case: n = k+1

   even (2*(k+1))
== even (2k+2)                                                       (math)
== match (2k+2) with | 0 -> true | 1 -> false | n -> even (n-2)      (eval even)
== even ((2k+2)-2)                                                   (eval case)
== even 2k                                                           (math)
== true                                                              (inductive hypothesis)

QED

*)

let rec length xs =
  match xs with
  | [] -> 0
  | hd :: tl -> 1 + length tl

let rec cat xs ys =
  match xs with
  | [] -> ys
  | hd :: tl -> hd :: cat tl ys

(*

Theorem: For all lists xs and ys, length (cat xs ys) == length xs + length ys

Proof: By induction on the list xs.

case xs = []:
  length (cat [] ys)                        (LHS)
= length ys                                 (eval cat)
= 0 + length ys                             (math)
= (length []) + (length + ys)               (eval length)

case xs = hd::tl:
  length (cat (hd::tl) ys)                  (LHS)
= length (hd::(cat tl ys))                  (eval cat, 2nd branch)
= 1 + length (cat tl ys)                    (eval length, 2nd branch)
= 1 + (length tl + length ys)               (inductive hypothesis)
= length (hd::tl) + length ys               (reparenthesizing and eval length in reverse)

QED

*)

let rec add_all xs c =
  match xs with
  | [] -> []
  | hd::tl -> (hd+c)::add_all tl c

(*

Theorem: For all lists xs, add_all (add_all xs a) b == add_all xs (a+b)

Proof: By induction on xs.

case xs = []:
  add_all (add_all [] a) b            (LHS)
= add_all [] b                        (eval inner add_all)
= []                                  (eval add_all)
= add_all [] (a+b)                    (eval add_all)

case xs = hd::tl:
  add_all (add_all (hd::tl) a) b              (LHS)
= add_all ((hd+a)::add_all tl a) b            (eval inner add_all)
= (hd+a+b)::add_all (add_all tl a) b          (eval add_all)
= (hd+a+b)::add_all tl (a+b)                  (inductive hypothesis)
= (hd+(a+b))::add_all tl (a+b)                (math)
= add_all (hd::tl) (a+b)                      (reverse eval add_all)

QED

*)

let rec tm f t =
  match t with
  | Leaf -> Leaf
  | Node (x,l,r) -> Node (f x, tm f l, tm f r)

let (<>) f g =
  fun x -> f (g x)

(*

Theorem: For all trees t: a tree, tm f (tm g t) == tm (f<>g) t

Proof: By induction on t

case t = Leaf:
  tm f (tm g Leaf)
= tm f Leaf           (eval tm)
= Leaf                (eval tm)
= tm (f<>g) Leaf      (reverse eval tm)

case t = Node (v,l,r):
  tm f (tm g (Node (v,l,r))                       (LHS)
= tm f (Node (g v, tm g l, tm g r))               (eval inner tm)
= Node (f (g v), tm f (tm g l), tm f (tm g r))    (eval outer tm)
= Node ((f<>g) v), tm f (tm g l), tm f (tm g r))  (reverse eval <>)
= Node ((f<>g) v, tm (f<>g) l, tm (f<>g) r)       (inductive hypothesis)
= tm (f<>g) (Node (v,l,r))                        (reverse eval tm)

QED

*)

type id	=	string	
type exp = Int of	int	|	Add	of exp * exp | Var of	id		

type env
val lookup : env -> id -> int

let rec eval (env:env) (e:exp) : int =
  match e with
  | Int i -> i
  | Add (e1, e2) -> (eval e1) + (eval e2)
  | Var x -> lookup env x

let	rec	opt	(e:exp)	:	exp	=	
  | Int i	->	Int i
  |	Add	(Int 0, e) ->	opt	e	
  |	Add	(e,	Int	0) ->	opt	e	
  |	Add	(e1,e2)	-> Add(opt e1, opt e2)
  |	Var	x	-> Var x	

let	e1 = Add (Int	3, Var "x")	

(*

Theorem: For all e : exp, eval (opt e) == eval e

Proof: By induction on the structure of expressions e : exp.

case e = Int i:
  eval (opt (Int i))       (LHS)
= eval (Int i)             (eval opt, 1st branch)

case e = Add (Int 0, e2):

  eval (opt (Add (Int 0, e2)))      (LHS)
= eval (opt e2)                     (eval opt, 2nd branch)
= eval e2                           (inductive hypothesis)

  eval (Add (Int 0, e2))            (RHS)
= (eval (Int 0)) + (eval e2)        (eval eval, 2nd branch)
= 0 + (eval e2)                     (eval eval, 1st branch)
= eval e2                           (math)

case e = Add (e2, Int 0):

  eval (opt (Add (e2, Int 0))       (LHS)
= eval (opt e2)                     (eval opt, 3rd branch)
= eval e2                           (inductive hypothesis)

  eval (Add (e2, Int 0))            (RHS)
= (eval e2) + (eval (Int 0))        (eval eval, 2nd branch)
= (eval e2) + (Int 0)               (eval eval, 1st branch)
= eval e2                           (math)

case e = Add (e1, e2):
  eval (opt (Add (e1, e2)))              (LHS)
= eval (Add (opt e1, opt e2))            (eval opt, 4th branch)
= (eval (opt e1)) + (eval (opt e2))      (eval eval, 2nd branch)
= (eval e1) + (eval (opt e2))            (inductive hypothesis 1)
= (eval e1) + (eval e2)                  (inductive hypothesis 2)
= eval (Add (e1, e2))                    (reverse eval eval; RHS)

case e = Var x:
  eval (opt (Var x))                (LHS)
= eval (Var x)                      (eval opt, 5th branch; RHS)

QED

*)

type 'a tree =  Leaf of 'a | Node of 'a tree * 'a tree

let rec flip (t: 'a tree) =
  match t with
  | Leaf _ -> t
  | Node (a,b) -> Node (flip b, flip a)

(*

Theorem: flip (flip t) = t

Proof: By induction on the structure of trees t : 'a tree

case t = Leaf:
  flip (flip Leaf)              (LHS)
= flip (Leaf)                   (eval flip, 1st branch)
= Leaf                          (eval flip, 1st branch; RHS)

case t = Node (a, b):
  flip (flip (Node (a,b)))                  (LHS)
= flip (Node (flip b, flip a))              (eval flip, 2nd branch)
= Node (flip (flip a), flip (flip b))       (eval flip, 2nd branch)
= Node (a, flip (flip b))                   (inductive hypothesis 1)
= Node (a,b)                                (inductive hypothesis 2; RHS)

QED



Theorem: flip (flip (flip t)) = flip t

Proof: By induction on the structure of t : 'a tree

case t = Leaf:
  flip (flip (flip Leaf))           (LHS)
= flip (flip Leaf)                  (eval flip, 1st branch)
= flip Leaf                         (eval flip, 1st branch; RHS)

case t = Node (a,b):
  flip (flip (flip (Node (a,b)))                (LHS)
= flip (flip (Node (flip b, flip a)))           (eval flip, 2nd branch)
= flip (Node (flip (flip a), flip (flip b))     (eval flip, 2nd branch)
= flip (Node (a, flip (flip b)))                (flip (flip t) = t)               
= flip (Node (a, b))                            (flip (flip t) = t; RHS)
  
QED

*)
