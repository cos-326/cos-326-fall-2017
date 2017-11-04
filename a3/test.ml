open OUnit2;;

let test_find_zero_exact (exp:string) : unit =
  let e = ExpressionLibrary.parse exp in
  (* For some reason, cmp_float is failing here. Rather than debug too much, use this. *)
  let my_cmp_float epsilon a b : bool = ((abs_float (a -. b)) < epsilon) in
  match Expression.find_zero_exact e with
  | Some solution ->
    let s = (Expression.evaluate solution 123.) in
    let zero = (Expression.evaluate e s) in

    assert_equal ~cmp:(my_cmp_float 0.001) ~printer:string_of_float 0. zero
  | None -> assert false

let suite =
  "A3" >::: [
    "negate_all" >:: (fun _ -> 
        assert_equal (Mapreduce.negate_all [1; -2; 0]) [-1; 2; 0]
      );

    "sum_rows" >:: (fun _ -> 
        assert_equal (Mapreduce.sum_rows [[1;2]; [3;4]]) [3; 7]
      );

    "num_occurs" >:: (fun _ -> 
        assert_equal (Mapreduce.num_occurs 4 [1;3;4;5;4]) 2
      );

    "super_sum" >:: (fun _ -> 
        assert_equal (Mapreduce.super_sum [[1;2;3];[];[5]]) 11 
      );

    "consec_dedupe" >:: (fun _ -> 
        let nocase_eq (s1:string) (s2:string) : bool =
          (String.uppercase_ascii s1) = (String.uppercase_ascii s2) in

        let output = (Mapreduce.consec_dedupe nocase_eq ["hi"; "HI"; "bi"]) in

        assert_bool "not equal" (output = ["hi"; "bi"] || output = ["HI"; "bi"])
      );

    "prefixes" >:: (fun _ -> 
        assert_equal (Mapreduce.length [1;2;3;4;5]) 5;
        assert_equal (Mapreduce.indexes [10;20;30;40]) [0;1;2;3];
        assert_equal (Mapreduce.zip [0;1;2;3] [10;20;30;40]) [(0,10);(1,20);(2,30);(3,40)];
        assert_equal (Mapreduce.indexed [10;20;30;40]) [(0,10);(1,20);(2,30);(3,40)];
        assert_equal (Mapreduce.take [1;2;3;4] 0) [];
        assert_equal (Mapreduce.prefixes [1;2;3;4]) [[1]; [1;2]; [1;2;3]; [1;2;3;4]];
      );

    "flatten" >:: (fun _ -> 
        assert_equal (Mapreduce.flatten [[1;2;3]; []; [0]; [4;5]]) [1;2;3;0;4;5]
      );

    "evaluate: simple" >:: (fun _ -> 
        assert_equal (Expression.evaluate (ExpressionLibrary.parse "x*x + 3") 2.) 7.
      );

    "evaluate: complex" >:: (fun _ -> 
        assert_equal (Expression.evaluate (ExpressionLibrary.parse "x*x + (3 + x + (x * 2))") 2.) 13.
      );

    "derivative: simple" >:: (fun _ -> 
        let f = (ExpressionLibrary.parse "x * x - 1") in
        let f' = Expression.derivative f in

        let output = Expression.evaluate f' 2. in

        assert_equal ~printer:string_of_float 4. output
      );

    "find_zero: simple" >:: (fun _ -> 
        let f = (ExpressionLibrary.parse "x * x - 1") in

        match Expression.find_zero f 2. 0.01 50 with
        | Some x ->
          assert_equal ~cmp:(cmp_float ~epsilon:0.001) ~printer:string_of_float 1. x
        | None ->
          assert_failure "find_zero: None"
      );

    "find_zero: no zero" >:: (fun _ -> 
        let f = (ExpressionLibrary.parse "x * x + 1") in
        let output = Expression.find_zero f 2. 0.01 50 in

        match output with
        | Some x ->
          (Printf.printf "\nfind_zero: Some %f" x);
          assert_failure "find_zero should not have found a 0"
        | None ->
          assert_bool "not equal" true
      );

    "find_zero_exact" >:: (fun _ -> 
        skip_if true "skip";

        assert_bool "should be None" ((Expression.find_zero_exact (ExpressionLibrary.parse "9")) = None);
        assert_bool "should be None" ((Expression.find_zero_exact (ExpressionLibrary.parse "x * 0")) = None);
        assert_bool "should be None" ((Expression.find_zero_exact
                                         (ExpressionLibrary.parse "(x * 2) + (x * (0-2)) + 7")) = None);
        assert_bool "should be None" ((Expression.find_zero_exact
                                         (ExpressionLibrary.parse "(x * x) + 9")) = None);
        test_find_zero_exact "3 * x + 2";
        test_find_zero_exact "5*x - 3 + 2*(x - 8)";
        test_find_zero_exact "5*x - 3 + 2*(x - 8) + 17*(3*x + 9)";
        test_find_zero_exact "(x * x * 0) + (x * 0.2) + 3";
      );
  ]

let () =
  run_test_tt_main suite
;;
