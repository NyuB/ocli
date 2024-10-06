open Rebase

let test_entries = [ "pick 1a A"; "pick 2b B"; "pick 3c C"; "pick 4d D" ] |> parse_entries

module Test_Info = struct
  let entries = test_entries
  let modified_files _ = []
end

module Test_App = App (Test_Info)

module Test_Info_with_modified (M : sig
    val modified : (string * string list) list
  end) =
struct
  let entries = test_entries

  let modified_files sha1 =
    List.find_map
      (fun (ref, files) -> if String.equal sha1 ref then Some files else None)
      M.modified
    |> Option.value ~default:[]
  ;;
end

let play_events_app update events model =
  List.fold_left (fun m e -> Qol.first @@ update m e) model events
;;

let play_events = play_events_app Test_App.update

let print_render_app view model =
  Tty_testing.Test_Platform.render @@ view model;
  List.iter print_endline (Tty_testing.Test_Platform.lines ())
;;

let print_render = print_render_app Test_App.view

let%expect_test "Navigate between commits" =
  let down_right = play_events [ Down; Right ] Test_App.init in
  print_render down_right;
  [%expect
    {|
    pick: 1a 'A'
    ^v pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let up = play_events [ Up ] down_right in
  print_render up;
  [%expect
    {|
    ^v pick: 2b 'B'
    pick: 1a 'A'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let down_down_left = play_events [ Down; Down; Left ] up in
  print_render down_down_left;
  [%expect {|
    pick: 1a 'A'
    pick: 3c 'C'
    pick: 2b 'B'
    pick: 4d 'D'
    |}]
;;

let chars s = s |> String.to_seq |> Seq.map (fun c -> Tty.Char c) |> List.of_seq

let%expect_test "Renaming" =
  let renaming = play_events [ Right; Right ] Test_App.init in
  print_render renaming;
  [%expect
    {|
    pick: 1a 'A'(renaming)
    pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let type_letters = play_events (chars "wesome Message!") renaming in
  print_render type_letters;
  [%expect
    {|
    pick: 1a 'Awesome Message!'(renaming)
    pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let validate_typing = play_events [ Enter ] type_letters in
  print_render validate_typing;
  [%expect
    {|
    pick: 1a 'Awesome Message!'(renamed)
    pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let cancel_typing = play_events [ Esc ] type_letters in
  print_render cancel_typing;
  [%expect {|
    pick: 1a 'A'
    pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}]
;;

let%expect_test "Fixup" =
  let fixup_second_commit = play_events [ Down; Char 'F' ] Test_App.init in
  print_render fixup_second_commit;
  [%expect
    {|
    pick: 1a 'A'
       fixup: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let fixup_second_commit = play_events [ Down; Char 'F'; Right ] Test_App.init in
  print_render fixup_second_commit;
  [%expect
    {|
    pick: 1a 'A'
    ^v fixup: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}]
;;

let%expect_test "Cannot fixup root entry" =
  let try_to_fixup_root = play_events [ Char 'F' ] Test_App.init in
  print_render try_to_fixup_root;
  [%expect {|
    pick: 1a 'A'
    pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let try_to_move_fixup_to_root =
    play_events [ Down; Char 'F'; Right; Up ] Test_App.init
  in
  print_render try_to_move_fixup_to_root;
  [%expect
    {|
    pick: 1a 'A'
    ^v fixup: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let try_to_move_root_after_fixup =
    play_events [ Down; Char 'F'; Up; Right; Down ] Test_App.init
  in
  print_render try_to_move_root_after_fixup;
  [%expect
    {|
    ^v pick: 1a 'A'
       fixup: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}]
;;

let%expect_test "Display modified files along entries" =
  let module Info : Rebase_info_external =
    Test_Info_with_modified (struct
      let modified =
        [ "1a", [ "a.txt"; "a/" ]
        ; "3c", [ "1.c"; "2.c"; "3.c"; "4.c"; "5.c"; "6.c"; "7.c" ]
        ]
      ;;
    end)
  in
  let module A = App (Info) in
  let print_render = print_render_app A.view
  and play_events = play_events_app A.update in
  print_render A.init;
  [%expect
    {|
    pick: 1a 'A' | a.txt
    pick: 2b 'B' | a/
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let move_down = play_events [ Down ] A.init in
  print_render move_down;
  [%expect {|
    pick: 1a 'A'
    pick: 2b 'B'
    pick: 3c 'C'
    pick: 4d 'D'
    |}];
  let move_down_again = play_events [ Down ] move_down in
  print_render move_down_again;
  [%expect
    {|
    pick: 1a 'A' | 1.c
    pick: 2b 'B' | 2.c
    pick: 3c 'C' | 3.c
    pick: 4d 'D' | 4.c
                 | 5.c
                 | 6.c
                 | 7.c
    |}]
;;

let%expect_test "Crop commit messages and file names" =
  let module I = struct
    include Test_Info

    let entries =
      test_entries
      |> List.map (fun e ->
        { e with message = e.message ^ " 123456789123456789123456789" })
    ;;

    let modified_files sha1 =
      List.find_map
        (fun (s, fs) -> if String.equal sha1 s then Some fs else None)
        [ "2b", [ "com/compagny/root/package/AbstractEntityBuilderFactoryVisitor.java" ]
        ; "3c", [ "ok/ok.ml" ]
        ]
      |> Option.value ~default:[]
    ;;
  end
  in
  let module A = App (I) in
  let print_render = print_render_app A.view
  and play_events = play_events_app A.update in
  let size = Tty.{ col = 50; row = 999 } in
  Tty_testing.Test_Platform.set_dimensions size;
  print_render (play_events [ Size size ] A.init);
  [%expect
    {|
    pick: 1a 'A 123456789123456789...
    pick: 2b 'B 123456789123456789...
    pick: 3c 'C 123456789123456789...
    pick: 4d 'D 123456789123456789...
    |}];
  print_render (play_events [ Size size; Down ] A.init);
  [%expect
    {|
    pick: 1a 'A 123456789123456789... | com/compagn...
    pick: 2b 'B 123456789123456789...
    pick: 3c 'C 123456789123456789...
    pick: 4d 'D 123456789123456789...
    |}];
  print_render (play_events [ Size size; Down; Down ] A.init);
  [%expect
    {|
    pick: 1a 'A 123456789123456789... | ok/ok.ml
    pick: 2b 'B 123456789123456789...
    pick: 3c 'C 123456789123456789...
    pick: 4d 'D 123456789123456789...
    |}]
;;

let%expect_test "Slide entry list to fit terminal rows" =
  let module I = struct
    include Test_Info

    let entries =
      List.init 2 (fun i ->
        List.map
          (fun e -> { e with message = Printf.sprintf "(%d)%s" i e.message })
          test_entries)
      |> List.concat
    ;;
  end
  in
  let module A = App (I) in
  let size = Tty.{ row = 5; col = 80 } in
  Tty_testing.Test_Platform.set_dimensions size;
  let print_render = print_render_app A.view
  and play_events = play_events_app A.update in
  let resized = play_events [ Size size ] A.init in
  print_render resized;
  [%expect {|
    pick: 1a '(0)A'
    pick: 2b '(0)B'
    pick: 3c '(0)C'
    |}];
  (*End with a Right to make the current selected entry clear *)
  print_render (play_events [ Down; Right ] resized);
  [%expect {|
    pick: 1a '(0)A'
    ^v pick: 2b '(0)B'
    pick: 3c '(0)C'
    |}];
  print_render (play_events [ Down; Down; Right ] resized);
  [%expect {|
    pick: 2b '(0)B'
    ^v pick: 3c '(0)C'
    pick: 4d '(0)D'
    |}];
  print_render (play_events [ Down; Down; Down; Right ] resized);
  [%expect {|
    pick: 3c '(0)C'
    ^v pick: 4d '(0)D'
    pick: 1a '(1)A'
    |}];
  print_render (play_events [ Down; Down; Down; Down; Down; Down; Right ] resized);
  [%expect {|
    pick: 2b '(1)B'
    ^v pick: 3c '(1)C'
    pick: 4d '(1)D'
    |}]
;;

let%expect_test "Crop modified list to fit terminal rows" =
  let module Info =
    Test_Info_with_modified (struct
      let many_files = List.init 20 (fun i -> Printf.sprintf "file_at_row_%02d" (i + 1))
      let modified = List.map (fun e -> e.sha1, many_files) test_entries
    end)
  in
  let module A = App (Info) in
  let print_render = print_render_app A.view
  and play_events = play_events_app A.update in
  let size = Tty.{ col = 999; row = 15 } in
  Tty_testing.Test_Platform.set_dimensions size;
  let resized = play_events [ Size size ] A.init in
  print_render resized;
  [%expect
    {|
    pick: 1a 'A' | file_at_row_01
    pick: 2b 'B' | file_at_row_02
    pick: 3c 'C' | file_at_row_03
    pick: 4d 'D' | file_at_row_04
                 | file_at_row_05
                 | file_at_row_06
                 | file_at_row_07
                 | file_at_row_08
                 | file_at_row_09
                 | file_at_row_10
                 | file_at_row_11
                 | file_at_row_12
                 | file_at_row_13
    |}];
  Tty_testing.Test_Platform.set_dimensions { size with row = 20 };
  let expanded = play_events [ Size { size with row = 20 } ] resized in
  print_render expanded;
  [%expect
    {|
    pick: 1a 'A' | file_at_row_01
    pick: 2b 'B' | file_at_row_02
    pick: 3c 'C' | file_at_row_03
    pick: 4d 'D' | file_at_row_04
                 | file_at_row_05
                 | file_at_row_06
                 | file_at_row_07
                 | file_at_row_08
                 | file_at_row_09
                 | file_at_row_10
                 | file_at_row_11
                 | file_at_row_12
                 | file_at_row_13
                 | file_at_row_14
                 | file_at_row_15
                 | file_at_row_16
                 | file_at_row_17
                 | file_at_row_18
    |}]
;;
