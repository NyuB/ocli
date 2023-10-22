open Qol

module Term_colors = struct
  let default_background_color = Tty.Default
  let default_foreground_color = Tty.Default
end

module Term_style = Tty.Style (Term_colors)

module Progress : sig
  type t

  val init : char -> t
  val to_string : t -> string
  val handle_command : Tty.command -> t -> t
end = struct
  type t = int * char

  let init c = 0, c

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

  type model = phase

  let init = Hello

  let display_check_lines =
    [ ( Term_style.default_style
      , "The following lines surrounded by vvv and ^^^ are here to verify the terminal \
         display has the expected behavior" )
    ; Term_style.default_style, String.make 80 'v'
    ; { Term_style.default_style with fg_color = Some Red }, "This should appear Red"
    ; { Term_style.default_style with fg_color = Some Green }, "This should appear Green"
    ; { Term_style.default_style with fg_color = Some Blue }, "This should appear Blue"
    ; ( { Term_style.default_style with fg_color = Some Yellow }
      , "This should appear Yellow" )
    ; ( { Term_style.default_style with fg_color = Some Magenta }
      , "This should appear Magenta" )
    ; { Term_style.default_style with fg_color = Some Cyan }, "This should appear Cyan"
    ; { Term_style.default_style with bg_color = Some Red }, "This should appear Red"
    ; { Term_style.default_style with bg_color = Some Green }, "This should appear Green"
    ; { Term_style.default_style with bg_color = Some Blue }, "This should appear Blue"
    ; ( { Term_style.default_style with bg_color = Some Yellow }
      , "This should appear Yellow" )
    ; ( { Term_style.default_style with bg_color = Some Magenta }
      , "This should appear Magenta" )
    ; { Term_style.default_style with bg_color = Some Cyan }, "This should appear Cyan"
    ; { Term_style.default_style with underlined = true }, "This should be underlined"
    ; { Term_style.default_style with bold = true }, "This should be bold"
    ; ( { Term_style.default_style with underlined = true; bold = true }
      , "This should be bold and underlined" )
    ; ( { bg_color = Some Yellow; fg_color = None; underlined = true; bold = false }
      , "This should be yellow and underlined" )
    ; Term_style.default_style, String.make 80 '^'
    ; Term_style.default_style, "Press Enter to proceed to the next phase of the demo"
    ]
    |> List.mapi (fun i (s, str) -> Tty.{ row = i + 1; col = 1 }, s, str)
  ;;

  let simply_lines lines =
    List.mapi (fun i l -> Tty.{ row = i + 1; col = 1 }, Term_style.default_style, l) lines
  ;;

  let view : model -> Tty.view_item list = function
    | Hello ->
      simply_lines
        [ "Hell'OCaml"
        ; "This is a demo/test program"
        ; "Press Enter to proceed to the next phase of the demo"
        ]
    | Display_check -> display_check_lines
    | Progress_bar b ->
      simply_lines
        [ "Now testing the terminal I/O interactivity behavior"
        ; "A progress bar will appear. You can:"
        ; "\tincrease it by pressing the right arrow key,"
        ; "\tdecrease it by pressing the left arrow key,"
        ; "\tclear it by pressing the delete key,"
        ; "Press Enter to proceed to the next phase of the demo"
        ; Progress.to_string b
        ]
    | End -> simply_lines [ "End of Demo, hit Ctrl+C to exit he program" ]
  ;;

  let update model message =
    match model, message with
    | Hello, Tty.Enter -> Display_check
    | Display_check, Tty.Enter -> Progress_bar (Progress.init '@')
    | Progress_bar _, Tty.Enter -> End
    | Progress_bar b, msg -> Progress_bar (Progress.handle_command msg b)
    | m, _ -> m
  ;;
end

let () =
  let term = Unix.stdin in
  let info = Unix.tcgetattr term in
  Unix.tcsetattr term Unix.TCSANOW (Tty.disable_default_terminal_behavior info);
  let out = Out_channel.stdout in
  Tty.loop_app (module Demo) (module Term_style) term out
;;
