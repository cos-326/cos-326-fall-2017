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

let cons_if_not_exists (x:'a) (xs:'a list) : 'a list =
  if not (List.exists (fun x' -> x' == x) xs)
  then x::xs
  else xs

let rec free_variables (e:exp) (vs:variable list) (env:env) : variable list =
  let free_variables' (es:exp list) (vs':variable list) (env':env) =
    List.fold_left (fun acc e' -> free_variables e' acc env') vs es
  in
  (match e with
   | Var v                  -> if lookup_env env v != None
                               then cons_if_not_exists v vs
                               else vs
   | Op (e1,_,e2)           -> free_variables' [e1;e2]    vs env 
   | If (e1,e2,e3)          -> free_variables' [e1;e2;e3] vs env 
   | Let (_,e1,e2)          -> free_variables' [e1;e2]    vs env 
   | Pair (e1,e2)           -> free_variables' [e1;e2]    vs env 
   | Fst e1                 -> free_variables' [e1]       vs env 
   | Snd e1                 -> free_variables' [e1]       vs env 
   | Cons (e1,e2)           -> free_variables' [e1;e2]    vs env 
   | Match (e1,e2,_,_,e3)   -> free_variables' [e1;e2;e3] vs env 
   | Rec (_,_,e1)           -> free_variables' [e1]       vs env 
   | Closure (envc,_,_,e1)  -> free_variables' [e1]       vs (List.append env envc)
   | App (e1,e2)            -> free_variables' [e1;e2]    vs env
   | _                      -> vs)

let update_env' (vs:(variable * exp) list) (env':env) =
  List.fold_left (fun acc (v,e') -> update_env acc v e') env' vs

let prune_env (free_vars:variable list) (calling_env:env) : env =
  List.fold_left
    (fun acc v ->
      match lookup_env calling_env v with
      | Some ev -> update_env' [(v,ev)] acc
      | None -> acc)
    []
    free_vars

(*

Two debugging things I had to do:

  1. Write small tests that only operated on part of an expression
  2. Specify in evaluation where failure occurred

I had to do (2) because sometimes the expressions can't be broken up into
small enough tests to make it obvious which part of the expression is failing.

*)

let semantic_fail (e:exp) = failwith (Printf.sprintf "SemanticError: %s" (string_of_exp e))

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  let eval_loop' = eval_loop env in
  match e with
  | Var x -> 
    (match lookup_env env x with 
     | None -> raise (UnboundVariable x)
     | Some v -> v)
  | Let (v,e1,e2) ->
      let env_with_v = update_env' [(v,(eval_loop' e1))] env in
      eval_loop env_with_v e2
  | If (c,e1,e2) ->
      (match eval_loop' c with
       | Constant (Bool b) -> if b
                              then eval_loop' e1
                              else eval_loop' e2
       | _ -> semantic_fail e)
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
      | _ -> semantic_fail e)
  | Pair (e1,e2) ->
      Pair (eval_loop' e1, eval_loop' e2)
  | Fst e' ->
      (match eval_loop' e' with
       | Pair (e1,_) -> e1
       | _ -> semantic_fail e)
  | Snd e' ->
      (match eval_loop' e' with
       | Pair (_,e2) -> e2
       | _ -> semantic_fail e)
  | Cons (e1,e2) ->
      Cons (eval_loop' e1, eval_loop' e2)
  | Match (e1, e2, hd, tl, e3) ->
      (match (eval_loop' e1) with
       | EmptyList ->
           eval_loop' e2
       | Cons (x,xs) ->
           let x' = eval_loop' x in
           let xs' = eval_loop' xs in
           let env_with_hd_and_tl = update_env' [(hd,x');(tl,xs')] env in
           eval_loop env_with_hd_and_tl e3
       | _ -> semantic_fail e)
  | Rec (f,x,b) ->
      let free_vars = free_variables b [] env in
      let pruned_env = prune_env free_vars env in
      Closure (pruned_env, f, x, b)
  | App (c,p) ->
      let c' = eval_loop' c in
      (match c' with
       | Closure (env',f,x,b) ->
           let p' = eval_loop' p in
           let env_with_x_and_f = update_env' [(x,p');(f,c')] env' in
           eval_loop env_with_x_and_f b
       | _ -> semantic_fail e)
  | _ ->
      if is_value e
      then e
      else semantic_fail e

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
