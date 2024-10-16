open Qol

let check_empty_list (testable : 'a Alcotest.testable) (actual : 'a list) =
  Alcotest.check (Alcotest.list testable) "Expected empty list" [] actual
;;

let not_none_empty =
  ( "not_none on empty list is identity"
  , fun () -> Alcotest.(check (list unit)) "Expected empty list" [] (List.not_none []) )
;;

let not_none_only_none =
  ( "not_none on only None is empty list"
  , fun () -> check_empty_list Alcotest.unit (List.not_none [ None; None; None ]) )
;;

let not_none_order =
  ( "not_none preserves original list order"
  , fun () ->
      Alcotest.(check (list int))
        "Expected list in same order"
        [ 1; 2; 3 ]
        (List.not_none [ None; Some 1; Some 2; None; Some 3 ]) )
;;

let sublist_empty =
  ( "sub list of an empty list is empty"
  , fun () ->
      List.iter
        (fun n -> check_empty_list Alcotest.int (List.sublist n []))
        [ 0; 1; 2; 3; 4; 5 ] )
;;

let check_sublist tag expected n l =
  ( tag
  , fun () ->
      Alcotest.check
        (Alcotest.list Alcotest.int)
        "Sublist does not match expected"
        expected
        (List.sublist n l) )
;;

let check_at_most tag expected n l =
  ( tag
  , fun () ->
      Alcotest.check
        (Alcotest.list Alcotest.int)
        "Sublist does not match expected"
        expected
        (List.at_most n l) )
;;

let opt_default_none =
  ( "use default if None"
  , fun () ->
      let default = "Default" in
      let actual = None |?? lazy default in
      Alcotest.(check string) "Expected default to be used" default actual )
;;

let opt_default_laziness =
  ( "do not force default if Some"
  , fun () ->
      let updated = ref false in
      let () = Some () |?? lazy (updated := true) in
      Alcotest.(check bool) "Expected updated not to have been executed" false !updated )
;;

let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let () =
  Alcotest.run
    "Quality Of Life functions"
    [ "List.not_none", quick_tests [ not_none_empty; not_none_order; not_none_only_none ]
    ; ( "List.sublist"
      , quick_tests
          [ sublist_empty
          ; check_sublist "sub list within bound" [ 2; 3 ] 1 [ 1; 2; 3 ]
          ; check_sublist "sub list beyond bound" [] 3 [ 1; 2 ]
          ; check_sublist "sub list 0 equal list" [ 1; 2 ] 0 [ 1; 2 ]
          ] )
    ; ( "List.at_most"
      , quick_tests
          [ check_at_most "at most on empty list" [] 1 []
          ; check_at_most "at most length" [ 1; 2; 3 ] 3 [ 1; 2; 3 ]
          ; check_at_most "at most more than length" [ 1; 2 ] 5 [ 1; 2 ]
          ; check_at_most "at most less than length" [ 1; 2 ] 2 [ 1; 2; 3 ]
          ] )
    ; "Option operators", quick_tests [ opt_default_none; opt_default_laziness ]
    ]
;;
