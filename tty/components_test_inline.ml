let print_render view = Tty_testing.print_render_app Fun.id view
let print_render_and_cursor view = Tty_testing.print_render_and_cursor_app Fun.id view

let top_left_constraint =
  Components.Constraints.{ row_start = 1; col_start = 1; width = 100; height = 100 }
;;

let view component constraints =
  let v, _ = component constraints in
  v
;;

let print_render_to_left component = print_render @@ view component top_left_constraint

let default_styled component =
  Components.to_ansi_view_component Tty.Default_style.default_style component
;;

module Row = Components.Row (Components.Merge_ansi_views)
module Row_divided = Components.Row_divided (Components.Merge_ansi_views)
module Col = Components.Column (Components.Merge_ansi_views)

let%expect_test "Row and column components" =
  let line_a = Components.Text_line.component "AAA" |> default_styled
  and line_b = Components.Text_line.component "BBB" |> default_styled in
  print_render_to_left line_a;
  [%expect {| AAA |}];
  let row = [ line_a; line_a; line_a ] |> Row.component in
  print_render_to_left row;
  [%expect {| AAAAAAAAA |}];
  let column = [ line_b; line_b; line_b ] |> Col.component in
  print_render_to_left column;
  [%expect {|
    BBB
    BBB
    BBB
    |}];
  let row_of_row_col = Row.component [ row; column ]
  and row_of_col_row = Row.component [ column; row ]
  and col_of_row_col = Col.component [ row; column ]
  and col_of_col_row = Col.component [ column; row ] in
  print_render_to_left row_of_row_col;
  [%expect {|
    AAAAAAAAABBB
             BBB
             BBB
    |}];
  print_render_to_left row_of_col_row;
  [%expect {|
    BBBAAAAAAAAA
    BBB
    BBB
    |}];
  print_render_to_left col_of_row_col;
  [%expect {|
    AAAAAAAAA
    BBB
    BBB
    BBB
    |}];
  print_render_to_left col_of_col_row;
  [%expect {|
    BBB
    BBB
    BBB
    AAAAAAAAA
    |}]
;;

let%expect_test "Divided row" =
  let only_nine_width =
    Components.Constraints.{ col_start = 1; row_start = 1; width = 9; height = 1 }
  in
  let print_render component = print_render @@ view component only_nine_width in
  let line_a = Components.Text_line.component "AAA" |> default_styled
  and line_b = Components.Text_line.component "BBB" |> default_styled
  and line_c = Components.Text_line.component "CCC" |> default_styled in
  print_render @@ Row_divided.component [ line_a, 1; line_b, 1; line_c, 1 ];
  [%expect {| AAABBBCCC |}];
  print_render @@ Row_divided.component [ line_a, 1; line_b, 3; line_c, 3 ];
  [%expect {| ABBBCCC |}];
  print_render @@ Row_divided.component [ line_a, 3; line_b, 1; line_c, 3 ];
  [%expect {| AAABCCC |}];
  print_render @@ Row_divided.component [ line_a, 3; line_b, 3; line_c, 1 ];
  [%expect {| AAABBBC |}]
;;

let play_events editing events =
  List.fold_left Components.Editing_line.update editing events
;;

let%expect_test "Editing line" =
  let only_nine_width =
    Components.Constraints.{ col_start = 1; row_start = 1; width = 9; height = 1 }
  in
  let print_render line =
    let component =
      Components.Editing_line.component line
      |> Components.positioned_to_ansi_view_component Tty.Default_style.default_style
    in
    print_render_and_cursor @@ view component only_nine_width
  in
  let chars s =
    String.to_seq s |> Seq.map (fun c -> Components.Editing_line.Char c) |> List.of_seq
  in
  let editing = Components.Editing_line.init "" in
  print_render editing;
  [%expect {| _ |}];
  let typed = play_events editing (chars "abc") in
  print_render typed;
  [%expect {| abc_ |}];
  let del = play_events typed [ Del ] in
  print_render del;
  [%expect {| ab_ |}];
  let left_left = play_events typed [ Left; Left ] in
  print_render left_left;
  [%expect {| a_c |}];
  let cropped = play_events editing (chars "123456789a") in
  print_render cropped;
  [%expect {| 3456789a_ |}];
  let cropped_left = play_events cropped [ Left; Left ] in
  print_render cropped_left;
  [%expect {| 2345678_a |}]
;;
