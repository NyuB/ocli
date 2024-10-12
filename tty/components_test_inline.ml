let print_render view = Tty_testing.print_render_app Fun.id view

let top_left_constraint =
  Components.{ row_start = 1; col_start = 1; width = 100; height = 100 }
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
  let row = [ line_a; line_a; line_a ] |> Row.make in
  print_render_to_left row;
  [%expect {| AAAAAAAAA |}];
  let column = [ line_b; line_b; line_b ] |> Col.make in
  print_render_to_left column;
  [%expect {|
    BBB
    BBB
    BBB
    |}];
  let row_of_row_col = Row.make [ row; column ]
  and row_of_col_row = Row.make [ column; row ]
  and col_of_row_col = Col.make [ row; column ]
  and col_of_col_row = Col.make [ column; row ] in
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
    Components.{ col_start = 1; row_start = 1; width = 9; height = 1 }
  in
  let print_render component = print_render @@ view component only_nine_width in
  let line_a = Components.Text_line.component "AAA" |> default_styled
  and line_b = Components.Text_line.component "BBB" |> default_styled
  and line_c = Components.Text_line.component "CCC" |> default_styled in
  print_render @@ Row_divided.make [ line_a, 1; line_b, 1; line_c, 1 ];
  [%expect {| AAABBBCCC |}];
  print_render @@ Row_divided.make [ line_a, 1; line_b, 3; line_c, 3 ];
  [%expect {| ABBBCCC |}];
  print_render @@ Row_divided.make [ line_a, 3; line_b, 1; line_c, 3 ];
  [%expect {| AAABCCC |}];
  print_render @@ Row_divided.make [ line_a, 3; line_b, 3; line_c, 1 ];
  [%expect {| AAABBBC |}]
;;
