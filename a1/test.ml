open OUnit2;;

let suite =
  "A1" >::: [
    "name" >:: (fun _ -> 

        assert_bool "empty name" ((String.length A1.name) > 1 )
      );

    "email" >:: (fun _ -> 

        assert_bool "email is not longer than name" ((String.length A1.name) > 1 )
      );

    "few_divisors 1"  >:: (fun _ -> 

        assert_bool "bad divisors" (A1.few_divisors 17 3)
      );

    "few_divisors 2"  >:: (fun _ -> 

        assert_bool "bad divisors" (not (A1.few_divisors 4 3))
      );

    "few_divisors 3"  >:: (fun _ -> 

        assert_bool "bad divisors" (A1.few_divisors 4 4)
      );

    "few_divisors 4"  >:: (fun _ -> 

        assert_bool "bad divisors" (not (A1.few_divisors 18 6))
      );

    "few_divisors 5"  >:: (fun _ -> 

        assert_bool "bad divisors" (A1.few_divisors 18 7)
      );

    "sin_pi round 0"  >:: (fun _ -> 

        assert_equal (A1.sin_pi 0) 3.
      );

    "sin_pi round 1"  >:: (fun _ -> 

        assert_bool "wrong pi" (cmp_float ~epsilon:0.001 (A1.sin_pi 1) 3.141)
      );

    "sin_pi round 2"  >:: (fun _ -> 

        assert_bool "wrong pi" (cmp_float ~epsilon:0.0000000001 (A1.sin_pi 2) 3.1415926535)
      );

    "sin_pi round 3"  >:: (fun _ -> 

        assert_bool "wrong pi" (cmp_float ~epsilon:0.000000000000001 (A1.sin_pi 3) 3.141592653589793)
      );

    "monte_pi"  >:: (fun _ -> 

        Printf.printf "\nmonte_pi answer: %F\n" (A1.monte_pi 10000);
        assert_bool "wrong Pi" (cmp_float ~epsilon:0.1 (A1.monte_pi 10000) 3.1)
      );

    (* Feel free to change the argument to other_pi, depending on your formula *)
    "other_pi"  >:: (fun _ -> 
        skip_if true "skip";
        Printf.printf "\nother_pi answer: %F\n" (A1.other_pi 9);
        assert_bool "wrong Pi" (cmp_float ~epsilon:0.1 (A1.other_pi 9) 3.1)
      );
  ]

let () =
  run_test_tt_main suite
;;

