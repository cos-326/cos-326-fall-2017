open OUnit2;;

let suite =
  "A3" >::: [
    "negate_all" >:: (fun _ -> 
        assert_equal (Mapreduce.negate_all [1; -2; 0]) [-1; 2; 0]
      );
  ]

let () =
  run_test_tt_main suite
;;

