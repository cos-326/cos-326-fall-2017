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
  match e with
  | Var x -> 
    (match lookup_env env x with 
     | None -> raise (UnboundVariable x)
     | Some v -> v)
  | Constant _ -> e
  | Op (x, op, y) ->
    let v1 = eval_loop env x in 
    let v2 = eval_loop env y in 
    apply_op v1 op v2 
  | If (condition, yes, no) ->
    (match eval_loop env condition with
     | Constant (Bool true) -> eval_loop env yes
     | Constant (Bool false) -> eval_loop env no
     | v1 -> raise (BadIf v1))
  | Let (var, x, expression) ->
    eval_loop (update_env env var x) expression
  | Pair(x, y) -> Pair(eval_loop env x, eval_loop env y)
  | Fst p -> 
    (match eval_loop env p with
     | Pair(x, _) -> eval_loop env x
     | p -> raise (BadPair p))
  | Snd p -> 
    (match eval_loop env p with
     | Pair(_, y) -> eval_loop env y
     | p -> raise (BadPair p))
  | EmptyList -> EmptyList
  | Cons (hd, tl)-> 
    let hd = eval_loop env hd in
    (match eval_loop env tl with
     | EmptyList -> Cons(hd, EmptyList)
     | Cons(x,y) -> Cons(hd, Cons(x,y))
     | _ -> raise (BadMatch tl))
  | Match (matcher, if_empty, hd, tl, if_full)-> 
    (match matcher with
     | EmptyList -> eval_loop env if_empty
     | Cons(h, t) ->
       let env = update_env env hd h in
       let env = update_env env tl t in
       eval_loop env if_full
     | _ -> raise (BadMatch matcher))
  | Rec(name, param, body)->
    Closure(env, name, param, body)
  | Closure _-> e
  | App(f, arg)->
    (match f with
     | Closure(env, name, param, body) ->
       eval_loop (update_env env param arg) body
     | _ -> raise (BadApplication f))

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
