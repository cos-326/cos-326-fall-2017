(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Syntax
open Printing
open EvalUtil

(* Defines the subset of expressions considered values
   Notice that closures are values but the rec form is not -- this is
   slightly different from the way values are defined in the 
   substitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
*) 
let rec is_value (e:exp) : bool = 
  match e with
    Constant _ -> true  
  | Pair (e1, e2) -> is_value e1 && is_value e2
  | EmptyList -> true
  | Cons (e1, e2) -> is_value e1 && is_value e2
  | Closure _ -> true
  | _ -> false

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  let eval_loop' = eval_loop env in
  let update_env' = update_env env in
  match e with
  | Var x -> 
    (match lookup_env env x with 
     | None -> raise (UnboundVariable x)
     | Some v -> v)

  | Let (v,e1,e2) ->
      let env_with_v = update_env' v (eval_loop' e1) in
      eval_loop env_with_v e2

  | If (c,e1,e2) ->
      (match eval_loop' c with
       | Constant (Bool b) -> if b then eval_loop' e1 else eval_loop' e2
       | _ -> failwith "If: Invalid code path")

  | Op (e1,op,e2) ->
    (match (eval_loop' e1,eval_loop' e2) with
     | (Constant (Int op1), Constant (Int op2)) ->
         Constant (match op with
          | Plus   -> Int  (op1 + op2)
          | Minus  -> Int  (op1 - op2)
          | Times  -> Int  (op1 * op2)
          | Div    -> Int  (op1 / op2)
          | Less   -> Bool (op1 < op2)
          | LessEq -> Bool (op1 <= op2))
      | _ -> failwith "Op: Invalid code path")

  | Pair (e1,e2) -> Pair (eval_loop' e1, eval_loop' e2)
  | Fst e' ->
      (match eval_loop' e' with
       | Pair (e1,_) -> e1
       | _ -> failwith "Fst: Invalid code path")
  | Snd e' ->
      (match eval_loop' e' with
       | Pair (_,e2) -> e2
       | _ -> failwith "Snd: Invalid code path")

  | Cons (e1,e2) -> Cons (eval_loop' e1, eval_loop' e2)
  | Match (e1, e2, hd, tl, e3) ->
      (match (eval_loop' e1) with
       | EmptyList ->
           eval_loop' e2
       | Cons (x,xs) ->
           let x' = eval_loop' x in
           let xs' = eval_loop' xs in

           let env_with_hd = update_env' hd x' in
           let env_with_hd_and_tl = update_env env_with_hd tl xs' in

           eval_loop env_with_hd_and_tl e3
       | _ ->
           failwith "Match: Invalid code path")

  | Rec (f,x,b) -> Closure ((update_env' f e), f, x, b)
  | App (c,p) ->
      let c' = eval_loop' c in
      (match c' with
       | Closure (env',f,x,b) ->
           let p' = eval_loop' p in

           let env_with_x = update_env env' x p' in
           let env_with_x_and_f = update_env env_with_x f c' in

           eval_loop env_with_x_and_f b
       | _ ->
           failwith "App: Invalid code path")

  | _ ->
      if is_value e
      then e
      else failwith "Final: Invalid code path"

(* evaluate closed, top-level expression e *)

let eval e =
  let rec loop env e = eval_body env loop e in
  loop empty_env e


(* print out subexpression after each step of evaluation *)
let debug_eval e = 
  let rec loop env e =
    if is_value e then e  (* don't print values *)
    else 
      begin
        Printf.printf "Evaluating %s\n" (string_of_exp e); 
        let v = eval_body env loop e in 
        Printf.printf 
          "%s evaluated to %s\n" (string_of_exp e) (string_of_exp v); 
        v
      end
  in
  loop empty_env e
