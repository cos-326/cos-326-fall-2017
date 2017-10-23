open OUnit2;;

let data1 : Query.movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
]

let data2 : Query.movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
]

let data3 : Query.movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
]

let data4 : Query.movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
]

let assert_set_equal (a:'a list) (b:'a list) =
  assert_equal (List.sort compare a) (List.sort compare b)

let suite =
  "A2" >::: [
    "zardoz" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal 7 (Part1.zardoz (+) [1;2;3] 1)
      );

    "myzardoz" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal 7 (Part1.myzardoz (+) [1;2;3] 1)
      );

    "look_and_say" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Part1.look_and_say [1; 1; 1; 3; 2; 2; 4]) [3; 1; 1; 3; 2; 2; 1; 4]
      );

    "flatten: integers" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Part1.flatten [[1;2;3]; []; [4]; [5;6]]) [1;2;3;4;5;6]
      );

    "flatten: characters" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Part1.flatten [[]; ['e';'d']; ['a';'b';'c']]) ['e';'d';'a';'b';'c'] 
      );

    "perm: 1" >:: (fun _ ->
        skip_if true "skip";
        assert_set_equal (Part1.perm [1]) [[1;]] 
      );

    "perm: 2" >:: (fun _ -> 
        skip_if true "skip";
        assert_set_equal (Part1.perm [1;2]) [[1;2]; [2;1]] 
      );

    "perm: 3" >:: (fun _ -> 
        skip_if true "skip";
        assert_set_equal (Part1.perm [1;2;3]) [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] 
      );

    "perm: 4" >:: (fun _ ->
        skip_if true "skip";
        assert_set_equal (Part1.perm [1;2;2;3]) [[1; 2; 2; 3]; [1; 2; 3; 2]; [1; 3; 2; 2]; [2; 1; 2; 3];
                                                 [2; 1; 3; 2]; [2; 2; 1; 3]; [2; 2; 3; 1]; [2; 3; 1; 2];
                                                 [2; 3; 2; 1]; [3; 1; 2; 2]; [3; 2; 1; 2]; [3; 2; 2; 1]]
      );

    (* Bonus test -- note this may take a long time, and you will get a stack overflow if your implementation
       grows the stack (i.e. is not tail recursive). *)
    "perm: 5" >:: (fun _ ->
        skip_if true "skip";
        assert_equal (List.length (Part1.perm [1;2;3;4;5;6;7;8;9;10])) 3628800
      );

    "average 1" >:: (fun _ -> 
        skip_if true "skip";
        assert_bool "bad average" (cmp_float ~epsilon:0.01 377.85 (Query.average data1) )
      );

    "average 4" >:: (fun _ -> 
        skip_if true "skip";
        assert_bool "bad average" (cmp_float ~epsilon:0.01 416.63 (Query.average data4) )
      );

    "decade: good" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal [
          ("The Hunger Games","LGF",374.32,2012);
          ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011);
        ] (Query.decade 10 data4)
      );

    "decade: bad decade" >:: (fun _ -> 
        skip_if true "skip";
        assert_raises (Query.Bad_arg "100") (fun () -> (Query.decade 100 data4))
      );

    "decade: bad year" >:: (fun _ -> 
        skip_if true "skip";
        assert_raises (Query.Bad_arg "17") (fun () -> (Query.decade 17 data4))
      );

    "take: empty list returns an empty list" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.take 5 []) []
      );

    "take: negative n throws an error" >:: (fun _ ->
        skip_if true "skip";
        assert_raises (Query.Bad_arg "-1") (fun () -> (Query.take (-1) [1]))
      );

    "take: large n returns the list" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.take 5 [1; 2]) [1; 2]
      );

    "take: small n returns start of the list" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.take 2 [1; 2; 3; 4]) [1; 2]
      );

    "drop: empty list returns an empty list" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.drop 5 []) []
      );

    "drop: negative n throws an error" >:: (fun _ -> 
        skip_if true "skip";
        assert_raises (Query.Bad_arg "-1") (fun () -> (Query.drop (-1) [1]))
      );

    "drop: large n returns []" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.drop 5 [1; 2]) []
      );

    "drop: small n returns end of the list" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.drop 2 [1; 2; 3; 4]) [3; 4]
      );

    "selection_sort" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.selection_sort (<=) [11; 25; 12; 22; 64])
          [11; 12; 22; 25; 64]
      );

    "sort_by_gross" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.sort_by_gross data4) [
          ("The Dark Knight","WB",533.34,2008);
          ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011);
          ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
          ("The Hunger Games","LGF",374.32,2012);
        ]
      );

    "sort_by_year" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.sort_by_year data4) [
          ("The Hunger Games","LGF",374.32,2012);
          ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011);
          ("The Dark Knight","WB",533.34,2008);
          ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
        ]
      );

    "sort_by_studio" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.sort_by_studio (Query.by_studio data4)) [
          ("WB", 914.35);
          ("NL", 377.85);
          ("LGF", 374.32);
        ]
      );

    "by_studio" >:: (fun _ -> 
        skip_if true "skip";
        assert_equal (Query.by_studio data4) [
          ("LGF", 374.32);
          ("WB", 914.35);
          ("NL", 377.85);
        ]
      );
  ]

let () =
  run_test_tt_main suite
;;
