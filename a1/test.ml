open OUnit2;;

let test1 test_name = assert_bool "empty name" ((String.length A1.name) > 1 )

let test2 test_email = assert_bool "email is not longer than name" ((String.length A1.name) > 1 )

let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2
 ]
;;

let () =
  run_test_tt_main suite
;;