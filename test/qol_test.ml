open Qol

let not_none_empty =
  ( "not_none on empty list is identity"
  , fun () -> Alcotest.(check (list unit)) "Expected empty list" [] (List.not_none []) )
;;

let not_none_only_none =
  ( "not_none on only None is empty list"
  , fun () ->
      Alcotest.(check (list unit))
        "Expected empty list"
        []
        (List.not_none [ None; None; None ]) )
;;

let not_none_order =
  ( "not_none preserves original list order"
  , fun () ->
      Alcotest.(check (list int))
        "Expected list in same order"
        [ 1; 2; 3 ]
        (List.not_none [ None; Some 1; Some 2; None; Some 3 ]) )
;;

let opt_default_none =
  ( "use default if None"
  , fun () ->
      let default = "Default" in
      let actual = None |?: lazy default in
      Alcotest.(check string) "Expected default to be used" default actual )
;;

let opt_default_laziness =
  ( "do not force default if Some"
  , fun () ->
      let updated = ref false in
      let () = Some () |?: lazy (updated := true) in
      Alcotest.(check bool) "Expected updated not to have been executed" false !updated )
;;

let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let () =
  Alcotest.run
    "Quality Of Life functions"
    [ "List.not_none", quick_tests [ not_none_empty; not_none_order; not_none_only_none ]
    ; "Option operators", quick_tests [ opt_default_none; opt_default_laziness ]
    ]
;;
