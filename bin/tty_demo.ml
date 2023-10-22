open Qol

module Term_colors = struct
  let default_background_color = Tty.Default
  let default_foreground_color = Tty.Default
end

module Term_style = Tty.Style (Term_colors)

module Progress : sig
  type t

  val init : char -> t
  val is_full : t -> bool
  val is_empty : t -> bool
  val to_string : t -> string
  val handle_command : Tty.command -> t -> t
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
end

module Demo : Tty.App = struct
  type phase =
    | Hello
    | Display_check
    | Progress_bar of Progress.t
    | End

  type model = Tty.position * phase

  let init = Tty.{ row = 1; col = 1 }, Hello

  let dimension_info_line ({ row; col } : Tty.position) =
    ( Tty.{ row; col = 1 }
    , Term_style.default_style
    , Printf.sprintf "(detected dimensions: %dx%d)" row col )
  ;;

  let display_check_lines (pos : Tty.position) =
    dimension_info_line pos
    :: ([ ( Term_style.default_style
          , "The following lines surrounded by vvv and ^^^ are here to verify the \
             terminal display has the expected behavior" )
        ; Term_style.default_style, String.make 80 'v'
        ; { Term_style.default_style with fg_color = Some Red }, "This should appear Red"
        ; ( { Term_style.default_style with fg_color = Some Green }
          , "This should appear Green" )
        ; ( { Term_style.default_style with fg_color = Some Blue }
          , "This should appear Blue" )
        ; ( { Term_style.default_style with fg_color = Some Yellow }
          , "This should appear Yellow" )
        ; ( { Term_style.default_style with fg_color = Some Magenta }
          , "This should appear Magenta" )
        ; ( { Term_style.default_style with fg_color = Some Cyan }
          , "This should appear Cyan" )
        ; { Term_style.default_style with bg_color = Some Red }, "This should appear Red"
        ; ( { Term_style.default_style with bg_color = Some Green }
          , "This should appear Green" )
        ; ( { Term_style.default_style with bg_color = Some Blue }
          , "This should appear Blue" )
        ; ( { Term_style.default_style with bg_color = Some Yellow }
          , "This should appear Yellow" )
        ; ( { Term_style.default_style with bg_color = Some Magenta }
          , "This should appear Magenta" )
        ; ( { Term_style.default_style with bg_color = Some Cyan }
          , "This should appear Cyan" )
        ; { Term_style.default_style with underlined = true }, "This should be underlined"
        ; { Term_style.default_style with bold = true }, "This should be bold"
        ; ( { Term_style.default_style with underlined = true; bold = true }
          , "This should be bold and underlined" )
        ; ( { bg_color = Some Yellow; fg_color = None; underlined = true; bold = false }
          , "This should be yellow and underlined" )
        ; Term_style.default_style, String.make 80 '^'
        ; Term_style.default_style, "Press Enter to proceed to the next phase of the demo"
        ]
        |> List.mapi (fun i (s, str) -> Tty.{ row = i + 1; col = 1 }, s, str))
  ;;

  let simply_lines lines =
    List.mapi (fun i l -> Tty.{ row = i + 1; col = 1 }, Term_style.default_style, l) lines
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
    match model, message with
    | m, Tty.Size p -> p, m
    | Hello, Tty.Enter -> pos, Display_check
    | Display_check, Tty.Enter -> pos, Progress_bar (Progress.init '@')
    | Progress_bar _, Tty.Enter -> pos, End
    | Progress_bar b, msg -> pos, Progress_bar (Progress.handle_command msg b)
    | m, _ -> pos, m
  ;;
end

let () =
  let term = Unix.stdin in
  let info = Unix.tcgetattr term in
  Unix.tcsetattr term Unix.TCSANOW (Tty.disable_default_terminal_behavior info);
  let out = Out_channel.stdout in
  Tty.loop_app (module Demo) (module Term_style) term out
;;
