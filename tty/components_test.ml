module Editing_line = Components.Editing_line

let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests
let chars s = String.to_seq s |> Seq.map (fun c -> Editing_line.Char c) |> List.of_seq
let play_events e s = List.fold_left Editing_line.update s e
let check_string = Alcotest.check Alcotest.string "Expected equal strings"
let check_int = Alcotest.check Alcotest.int "Expected equal integers"

let test_empty_type_chars =
  ( "Type three characters from empty"
  , fun () ->
      let edited = Editing_line.empty |> play_events (chars "abc") in
      check_string "abc" (Editing_line.to_string edited);
      check_int 3 (Editing_line.edition_index edited) )
;;

let test_del_from_empty =
  ( "Delete from empty string"
  , fun () ->
      let edited = play_events [ Del ] Editing_line.empty in
      check_string "" (Editing_line.to_string edited);
      check_int 0 (Editing_line.edition_index edited) )
;;

let test_del_after_three_chars =
  ( "Type three characters then delete"
  , fun () ->
      let edited =
        Editing_line.empty |> play_events (chars "abc") |> play_events [ Del ]
      in
      check_string "ab" (Editing_line.to_string edited);
      check_int 2 (Editing_line.edition_index edited) )
;;

let test_right_then_char_after_three_chars =
  ( "Right from end of string"
  , fun () ->
      let edited =
        Editing_line.empty |> play_events (chars "abc") |> play_events [ Right; Char 'd' ]
      in
      check_string "abcd" (Editing_line.to_string edited);
      check_int 4 (Editing_line.edition_index edited) )
;;

let test_left_del_after_three_chars =
  ( "Type three characters, move left, then delete"
  , fun () ->
      let edited =
        Editing_line.empty |> play_events (chars "abc") |> play_events [ Left; Del ]
      in
      check_string "ac" (Editing_line.to_string edited);
      check_int 1 (Editing_line.edition_index edited) )
;;

let test_left_char_after_three_chars =
  ( "Type three characters, move left, then type one more character"
  , fun () ->
      let edited =
        Editing_line.empty |> play_events (chars "abc") |> play_events [ Left; Char 'd' ]
      in
      check_string "abdc" (Editing_line.to_string edited);
      check_int 3 (Editing_line.edition_index edited) )
;;

let test_style = Tty.Default_style.default_style

let view row_start col_start width t =
  let component =
    Editing_line.component t |> Components.positioned_to_ansi_view_component test_style
  in
  let v, _ = component { row_start; col_start; height = 1; width } in
  v
;;

let view_item_kind_testable : Tty.ansi_view_item_kind Alcotest.testable =
  Alcotest.testable
    (fun fmt item ->
      match item with
      | Tty.Cursor -> Format.pp_print_string fmt "Cursor"
      | Tty.Text s -> Format.pp_print_string fmt (Printf.sprintf "Text \"%s\"" s))
    ( = )
;;

let position_testable : Tty.position Alcotest.testable =
  Alcotest.testable
    (fun fmt Tty.{ row; col } ->
      Format.pp_print_string fmt (Printf.sprintf "{ row = %d; col = %d }" row col))
    ( = )
;;

let style_testable : Tty.style Alcotest.testable =
  Alcotest.testable (fun fmt _ -> Format.pp_print_string fmt "test_style") ( = )
;;

let ansi_view_item_testable =
  Alcotest.triple position_testable style_testable view_item_kind_testable
;;

let ansi_view_testable = Alcotest.list ansi_view_item_testable
let check_view = Alcotest.check ansi_view_testable "Expected equal views"

let test_view_full =
  ( "Display full text when enough space available"
  , fun () ->
      let edited = Editing_line.empty |> play_events (chars "abcde") in
      let edited_view = view 1 1 5 edited in
      check_view
        [ { row = 1; col = 1 }, test_style, Text "abcde"
        ; { row = 1; col = 6 }, test_style, Cursor
        ]
        edited_view )
;;

let test_view_partial =
  ( "Display only a portion centered on cursor when not enough space available"
  , fun () ->
      let edited =
        Editing_line.empty
        |> play_events (chars "abcde")
        |> play_events [ Left; Left; Left ]
      in
      let edited_view = view 1 1 3 edited in
      check_view
        [ { row = 1; col = 1 }, test_style, Text "bcd"
        ; { row = 1; col = 2 }, test_style, Cursor
        ]
        edited_view )
;;

let test_view_partial_left =
  ( "Display only a portion starting with cursor when not enough space available and \
     cursor is on the first letter"
  , fun () ->
      let edited =
        Editing_line.empty
        |> play_events (chars "abcde")
        |> play_events [ Left; Left; Left; Left; Left ]
      in
      let edited_view = view 1 1 3 edited in
      check_view
        [ { row = 1; col = 1 }, test_style, Text "abc"
        ; { row = 1; col = 1 }, test_style, Cursor
        ]
        edited_view )
;;

let () =
  Alcotest.run
    "Components"
    [ ( "Editing line (model)"
      , quick_tests
          [ test_empty_type_chars
          ; test_del_from_empty
          ; test_del_after_three_chars
          ; test_left_del_after_three_chars
          ; test_left_char_after_three_chars
          ; test_right_then_char_after_three_chars
          ] )
    ; ( "Editing line (view)"
      , quick_tests [ test_view_full; test_view_partial; test_view_partial_left ] )
    ]
;;
