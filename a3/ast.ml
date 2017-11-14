
(* ------ Type definitions for the abstract syntax tree ------- *)

(* Binary operators. *)
type binop = Add | Sub | Mul 

type expression =
  | Num of float
  | Var
  | Binop of binop * expression * expression

let binop_to_string (o:binop) : string =
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"

let rec expression_to_string (e:expression) : string =
  match e with
  | Num n -> string_of_float n
  | Var -> "x"
  | Binop (op, e1, e2) -> "(" ^ (binop_to_string op) ^ " " ^ (expression_to_string e1) ^ " " ^ (expression_to_string e2) ^ ")"
