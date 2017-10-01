open OUnit2;;

let test1 test_name = assert_bool "empty name" ((String.length A1.name) > 1 )

let test2 test_email = assert_bool "email is not longer than name" ((String.length A1.name) > 1 )

let test3 test_few_divisors = assert_bool "bad divisors" (A1.few_divisors 17 3) 
let test4 test_few_divisors = assert_bool "bad divisors" (not (A1.few_divisors 4 3))
let test4 test_few_divisors = assert_bool "bad divisors" (A1.few_divisors 4 4)
let test5 test_few_divisors = assert_bool "bad divisors" (not (A1.few_divisors 18 6))
let test6 test_few_divisors = assert_bool "bad divisors" (A1.few_divisors 18 7)

let suite =
  "suite">:::
  ["test1">:: test1;
   "test2">:: test2;
   "test3">:: test3;
   "test4">:: test4;
   "test5">:: test5;
   "test6">:: test6;
  ]
;;

let () = 
  run_test_tt_main suite
;;
