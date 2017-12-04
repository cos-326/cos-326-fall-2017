open OUnit2;;

open Syntax
open Printing
open Testing 

let eval (e: exp) : string =
  e
  |> EvalEnv.eval
  |> string_of_exp

let identity x = x
let assert_eq_string = assert_equal ~printer:identity

(* let z = 2 in (let f = (fun x -> z + x) in f 3) *)
let clo2 =
  Let ("z", two,
       Let ("f",
            Rec ("_closure", "x", Op(Var "z", Plus, (Var "x"))),
            App (Var "f", three)))

(* let make-adder = (fun x -> (fun arg -> x + arg)) in (let add_three (make_adder 3) in add_three 4) *)
let clo3 =
  Let ("make_adder",
       Rec ("_make_adder", "x",
            Rec ("_adder", "arg",
                 Op(Var "x", Plus, (Var "arg")))),
       Let("add_three", App((Var "make_adder"), three),
           App((Var "add_three"), four)))

let suite =
  "A4" >::: [
    "zero" >:: (fun _ -> 
        assert_eq_string (eval Testing.zero) "0";
      );

    "fact4" >:: (fun _ -> 
        assert_eq_string (eval Testing.fact4) "24";
      );

    "list4" >:: (fun _ -> 
        assert_eq_string (eval Testing.list4) "1::2::3::4::[]";
      );

    "sl4" >:: (fun _ -> 
        assert_eq_string (eval Testing.sl4) "10";
      );

    "clo" >:: (fun _ -> 
        assert_eq_string (eval Testing.clo) "11";
      );

    "clo2" >:: (fun _ ->
        assert_eq_string (eval clo2) "5");

    "clo3" >:: (fun _ ->
        assert_eq_string (eval clo3) "7");

    "incr_all: empty" >:: (fun _ -> 
        let exp = App(incr_all, EmptyList) in

        assert_eq_string (eval exp) "[]";
      );

    "incr_all" >:: (fun _ -> 
        let list3 = listify [one;two;three] in
        let exp = App(incr_all, list3) in

        assert_eq_string (eval exp) "2::3::4::[]";
      );

    "sum_pairs: empty" >:: (fun _ -> 
        let exp = App(sum_pairs, EmptyList) in

        assert_eq_string (eval exp) "[]";
      );

    "sum_pairs" >:: (fun _ -> 
        let pairs = 
          Cons (Pair (one, two), Cons( Pair (three, four), EmptyList)) in
        let exp = App(sum_pairs, pairs) in

        assert_eq_string (eval exp) "3::7::[]";
      );
  ]

let () =
  run_test_tt_main suite
;;
