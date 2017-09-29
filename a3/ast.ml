
(* ------ Type definitions for the abstract syntax tree ------- *)

(* Binary operators. *)
type binop = Add | Sub | Mul 

type expression =
  | Num of float
  | Var
  | Binop of binop * expression * expression
