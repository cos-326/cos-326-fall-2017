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

    "sin_pi 0 rounds"  >:: (fun _ -> 
        assert_equal (A1.sin_pi 0) 3.
      );

    "sin_pi 1 round"  >:: (fun _ -> 
        assert_equal (int_of_float ((A1.sin_pi 1) *. 1000.)) 3141
      );

    "monte_pi"  >:: (fun test_ctxt -> 
        let _ = logf test_ctxt `Info "monte_pi: %F" (A1.monte_pi 10000) in
        assert_equal (int_of_float ((A1.monte_pi 10000) *. 10.)) 31
      );
  ]

let () = 
  run_test_tt_main suite
;;
