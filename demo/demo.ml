module Progress : sig
  type t

  val init : char -> t
  val is_full : t -> bool
  val is_empty : t -> bool
  val to_string : t -> string
  val handle_command : Tty.ansi_event -> t -> t
end = struct
  type t = int * char

  let init c = 0, c
  let is_full (i, _) = i = 10
  let is_empty (i, _) = i = 0

  let to_string (i, c) =
    let left = String.make i c
    and right = String.make (10 - i) '_' in
    Printf.sprintf "[%s%s]" left right
  ;;

  let handle_command cmd (i, c) =
    let open Tty in
    let next_i =
      match cmd with
      | Left -> if i > 0 then i - 1 else 0
      | Right -> if i < 10 then i + 1 else 10
      | Del -> 0
      | _ -> i
    in
    next_i, c
  ;;

  module Tests = struct
    let progress_after t events =
      List.fold_left (fun acc i -> handle_command i acc) t events
    ;;

    let print_t t = print_endline (to_string t)
    let empty = init '@'
    let repeat n (event : Tty.ansi_event) = List.init n (fun _ -> event)

    let%expect_test "Empty" =
      print_t (progress_after empty []);
      [%expect {| [__________] |}]
    ;;

    let%expect_test "Right increment the bar" =
      let one_right = progress_after empty [ Right ] in
      print_t one_right;
      [%expect {| [@_________] |}];
      let nine_more = progress_after one_right (repeat 9 Right) in
      print_t nine_more;
      [%expect {| [@@@@@@@@@@] |}]
    ;;

    let%expect_test "Left decrement the bar, del empty the bar" =
      let mid_fill = progress_after empty (repeat 5 Right) in
      print_t mid_fill;
      [%expect {| [@@@@@_____] |}];
      let minus_one = progress_after mid_fill [ Left ] in
      print_t minus_one;
      [%expect {| [@@@@______] |}];
      let deleted = progress_after minus_one [ Del ] in
      print_t deleted;
      [%expect {| [__________] |}]
    ;;
  end
end

module App : Tty.Ansi_App with type command = Tea.no_command = struct
  include Tty.Ansi_Tea_Base

  type command = Tea.no_command

  type phase =
    | Hello
    | Display_check
    | Progress_bar of Progress.t
    | End

  type model = Tty.position * phase

  let init = Tty.{ row = 1; col = 1 }, Hello

  let dimension_info_line ({ row; col } : Tty.position) =
    ( Tty.{ row; col = 1 }
    , Tty.Default_style.default_style
    , Tty.text @@ Printf.sprintf "(detected dimensions: %dx%d)" row col )
  ;;

  let display_check_lines (pos : Tty.position) =
    dimension_info_line pos
    :: ([ ( Tty.Default_style.default_style
          , "The following lines surrounded by vvv and ^^^ are here to verify the \
             terminal display has the expected behavior" )
        ; Tty.Default_style.default_style, String.make 80 'v'
        ; ( { Tty.Default_style.default_style with fg_color = Some Red }
          , "This should appear Red" )
        ; ( { Tty.Default_style.default_style with fg_color = Some Green }
          , "This should appear Green" )
        ; ( { Tty.Default_style.default_style with fg_color = Some Blue }
          , "This should appear Blue" )
        ; ( { Tty.Default_style.default_style with fg_color = Some Yellow }
          , "This should appear Yellow" )
        ; ( { Tty.Default_style.default_style with fg_color = Some Magenta }
          , "This should appear Magenta" )
        ; ( { Tty.Default_style.default_style with fg_color = Some Cyan }
          , "This should appear Cyan" )
        ; ( { Tty.Default_style.default_style with bg_color = Some Red }
          , "This should appear Red" )
        ; ( { Tty.Default_style.default_style with bg_color = Some Green }
          , "This should appear Green" )
        ; ( { Tty.Default_style.default_style with bg_color = Some Blue }
          , "This should appear Blue" )
        ; ( { Tty.Default_style.default_style with bg_color = Some Yellow }
          , "This should appear Yellow" )
        ; ( { Tty.Default_style.default_style with bg_color = Some Magenta }
          , "This should appear Magenta" )
        ; ( { Tty.Default_style.default_style with bg_color = Some Cyan }
          , "This should appear Cyan" )
        ; ( { Tty.Default_style.default_style with underlined = true }
          , "This should be underlined" )
        ; ( { Tty.Default_style.default_style with striked = true }
          , "This should be striked out" )
        ; { Tty.Default_style.default_style with bold = true }, "This should be bold"
        ; ( { Tty.Default_style.default_style with underlined = true; bold = true }
          , "This should be bold and underlined" )
        ; ( { bg_color = Some Yellow
            ; fg_color = None
            ; underlined = true
            ; bold = false
            ; striked = false
            }
          , "This should be yellow and underlined" )
        ; Tty.Default_style.default_style, String.make 80 '^'
        ; ( Tty.Default_style.default_style
          , "Press Enter to proceed to the next phase of the demo" )
        ]
        |> List.mapi (fun i (s, str) -> Tty.{ row = i + 1; col = 1 }, s, Tty.text str))
  ;;

  let simply_lines lines =
    List.mapi
      (fun i l ->
        Tty.{ row = i + 1; col = 1 }, Tty.Default_style.default_style, Tty.text l)
      lines
  ;;

  let hello =
    [ "            #####      ####"
    ; "          ########    ######        #####"
    ; "         #########    #######      #      #"
    ; "         #       #   #       #    #    o   ##"
    ; "        #  Hell'O # #  Caml  #   #        ###"
    ; "       ##          #         #####     #####"
    ; "      #  This is a demo/test program  #"
    ; "     ##                              ##"
    ; "     #   Press Enter to proceed     ##"
    ; "     #                          ####"
    ; "    /###########################"
    ; "   /     ####             #  ##"
    ; "  |      ##  #            #    #"
    ; " /      ##   #            #     #"
    ; "*      ##    ##           #     ##"
    ; "      ##      #          ##      ##"
    ; "    ##        #          #        #"
    ; "    #         ##        ##        ##"
    ]
  ;;

  let progress_bar_lines pos bar =
    dimension_info_line pos
    :: simply_lines
         [ "Now testing the terminal I/O interactivity behavior"
         ; "A progress bar will appear. You can:"
         ; (if not @@ Progress.is_full bar
            then "\t\xE2\x87\x92   increase it by pressing the right  arrow key,"
            else "")
         ; (if not @@ Progress.is_empty bar
            then "\t\xE2\x87\x90   decrease it by pressing the left arrow key,"
            else "")
         ; (if not @@ Progress.is_empty bar
            then "\tDEL clear it by pressing the delete key,"
            else "")
         ; "Press Enter to proceed to the next phase of the demo"
         ; Progress.to_string bar
         ]
  ;;

  let view (pos, model) =
    match model with
    | Hello -> simply_lines hello
    | Display_check -> display_check_lines pos
    | Progress_bar b -> progress_bar_lines pos b
    | End -> simply_lines [ "End of Demo, hit Ctrl+C to exit he program" ]
  ;;

  let update (pos, model) message =
    let next_model =
      match model, message with
      | m, Tty.Size p -> p, m
      | Hello, Tty.Enter -> pos, Display_check
      | Display_check, Tty.Enter -> pos, Progress_bar (Progress.init '@')
      | Progress_bar _, Tty.Enter -> pos, End
      | Progress_bar b, msg -> pos, Progress_bar (Progress.handle_command msg b)
      | m, _ -> pos, m
    in
    next_model, []
  ;;

  module Tests = struct
    let repr_state = function
      | Hello -> "Hello"
      | Display_check -> "Display Check"
      | Progress_bar b -> Printf.sprintf "Progress Bar %s" (Progress.to_string b)
      | End -> "End"
    ;;

    let repr_model ({ row; col } : Tty.position) m =
      Printf.sprintf "(%dx%d) %s" row col (repr_state m)
    ;;

    let print_t (pos, m) = print_endline (repr_model pos m)

    let play_events model events =
      List.fold_left (fun m e -> Qol.first @@ update m e) model events
    ;;

    let%expect_test "Hello then Display check then Progress bar then End" =
      let enter m = play_events m [ Enter ] in
      let m = init in
      print_t m;
      [%expect {| (1x1) Hello |}];
      let m = enter m in
      print_t m;
      [%expect {| (1x1) Display Check |}];
      let m = enter m in
      print_t m;
      [%expect {| (1x1) Progress Bar [__________] |}];
      let m = enter m in
      print_t m;
      [%expect {| (1x1) End |}]
    ;;

    let%expect_test "Update size" =
      let resized = play_events init [ Size { row = 50; col = 80 } ] in
      print_t resized;
      [%expect {| (50x80) Hello |}]
    ;;

    let%expect_test "Update progress bar" =
      let progress =
        play_events
          init
          [ Size { row = 50; col = 80 }; Enter; Enter; Right; Right; Right ]
      in
      print_t progress;
      [%expect {| (50x80) Progress Bar [@@@_______] |}];
      let progress = play_events progress [ Left ] in
      print_t progress;
      [%expect {| (50x80) Progress Bar [@@________] |}];
      let progress = play_events progress [ Del ] in
      print_t progress;
      [%expect {| (50x80) Progress Bar [__________] |}]
    ;;
  end
end
