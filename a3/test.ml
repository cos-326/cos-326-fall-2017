open OUnit2;;

let suite =
  "A3" >::: [
    "negate_all" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Mapreduce.negate_all [1; -2; 0]) [-1; 2; 0]
      );

    "sum_rows" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Mapreduce.sum_rows [[1;2]; [3;4]]) [3; 7]
      );

    "num_occurs" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Mapreduce.num_occurs 4 [1;3;4;5;4]) 2
      );

    "super_sum" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Mapreduce.super_sum [[1;2;3];[];[5]]) 11 
      );

    "consec_dedupe" >:: (fun _ -> 
        skip_if true "skip";
        let nocase_eq (s1:string) (s2:string) : bool =
          (String.uppercase_ascii s1) = (String.uppercase_ascii s2) in

        let output = (Mapreduce.consec_dedupe nocase_eq ["hi"; "HI"; "bi"]) in

        assert_bool "not equal" (output = ["hi"; "bi"] || output = ["HI"; "bi"])
      );

    "prefixes" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Mapreduce.prefixes [1;2;3;4]) [[1]; [1;2]; [1;2;3]; [1;2;3;4]]
      );

    "flatten" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Mapreduce.flatten [[1;2;3]; []; [0]; [4;5]]) [1;2;3;0;4;5]
      );

    "evaluate: simple" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Expression.evaluate (ExpressionLibrary.parse "x*x + 3") 2.) 7.
      );

    "evaluate: complex" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Expression.evaluate (ExpressionLibrary.parse "x*x + (3 + x + (x * 2))") 2.) 13.
      );

    "derivative: simple" >:: (fun _ -> 
        skip_if true "skip";
        let f = (ExpressionLibrary.parse "x * x - 1") in
        let f' = Expression.derivative f in

        let output = Expression.evaluate f' 2. in

        Printf.printf "\nderivative: %f" output;
        assert_equal output 4.
      );

    "find_zero: simple" >:: (fun _ -> 
        skip_if true "skip";
        let f = (ExpressionLibrary.parse "x * x - 1") in
        let output = Expression.find_zero f 2. 0.01 50 in

        match output with
        | Some x ->
          (Printf.printf "\nfind_zero: Some %f" x);
          assert_bool "not equal" (cmp_float ~epsilon:0.001 x 1.)
        | None ->
          assert_failure "find_zero: None"
      );

    "find_zero: no zero" >:: (fun _ -> 
        skip_if true "skip";
        let f = (ExpressionLibrary.parse "x * x + 1") in
        let output = Expression.find_zero f 2. 0.01 50 in

        match output with
        | Some x ->
          (Printf.printf "\nfind_zero: Some %f" x);
          assert_failure "find_zero should not have found a 0"
        | None ->
          assert_bool "not equal" true
      );
  ]

let () =
  run_test_tt_main suite
;;

