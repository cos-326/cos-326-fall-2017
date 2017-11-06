type variable = string

type op = Plus | Minus | Times

type exp =
  | Int_e of int
  | Op_e of exp * op * exp
  | Var_e of variable
  | Let_e of variable * exp * exp

type value = exp

let is_value (e:exp) : bool =
  match e with
  | Int_e _ -> true
  | Op_e _ | Var_e _ | Let_e _ -> false

let e1 = Int_e 3
let e2 = Int_e 17

let e3 = Op_e (e1, Plus, e2)

let e4 = Let_e ("x", Int_e 30,
 Let_e ("y",
  Let_e ("z", Int_e 3,
   Op_e (Var_e "z", Times, Int_e 4)),
    Op_e (Var_e "y", Plus, Var_e "y"))) 
