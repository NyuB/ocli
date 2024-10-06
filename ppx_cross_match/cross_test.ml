let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let test_0x2 =
  ( "0x2"
  , fun () ->
      Alcotest.check
        (Alcotest.list (Alcotest.list Alcotest.int))
        "Ok"
        [ [ 1 ]; [ 2 ] ]
        (Cross.cross [] [ 1; 2 ]) )
;;

let test_1x2 =
  ( "1x2"
  , fun () ->
      Alcotest.check
        (Alcotest.list (Alcotest.list Alcotest.int))
        "Ok"
        [ [ 1; 2 ]; [ 1; 3 ] ]
        (Cross.cross [ 1 ] [ 2; 3 ]) )
;;

let test_2x1 =
  ( "2x1"
  , fun () ->
      Alcotest.check
        (Alcotest.list (Alcotest.list Alcotest.int))
        "Ok"
        [ [ 1; 2; 3 ] ]
        (Cross.cross [ 1; 2 ] [ 3 ]) )
;;

let match_with_cross a b =
  match a, b with
  | [%cross_match ("A", "B"), (Some 1, Some 2, Some 3)] -> true
  | _ -> false
;;

let match_with_cross_any a b =
  match a, b with
  | [%cross_match ("A", "B"), Some [%cross_any]] -> true
  | _ -> false
;;

let do_match f a b =
  "Match", fun () -> Alcotest.check Alcotest.bool "Expected a match" true (f a b)
;;

let do_not_match f a b =
  "No Match", fun () -> Alcotest.check Alcotest.bool "Expected no match" false (f a b)
;;

let () =
  Alcotest.run
    "Cross products"
    [ "Cross", quick_tests [ test_0x2; test_1x2; test_2x1 ]
    ; ( "Ppx cross match with constant"
      , let do_match = do_match match_with_cross
        and do_not_match = do_not_match match_with_cross in
        quick_tests
          [ do_match "A" (Some 1)
          ; do_match "B" (Some 1)
          ; do_match "A" (Some 2)
          ; do_match "B" (Some 2)
          ; do_match "A" (Some 3)
          ; do_match "B" (Some 3)
          ; do_not_match "A" None
          ; do_not_match "B" None
          ; do_not_match "A" (Some 4)
          ; do_not_match "C" (Some 1)
          ] )
    ; ( "Ppx cross match with any"
      , let do_match = do_match match_with_cross_any
        and do_not_match = do_not_match match_with_cross_any in
        quick_tests
          [ do_match "A" (Some 3); do_match "A" (Some 1); do_not_match "C" (Some 1) ] )
    ]
;;
