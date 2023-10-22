open Qol

module Term_color = Tty.Style (struct
    let default_foreground_color = Tty.Default
    let default_background_color = Tty.Default
  end)

module type App = sig
  type model

  val init : model
  val view : model -> string list
  val update : model -> Tty.command -> model
end

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

let disable_default_terminal_behavior term_info =
  (* Disable canonical character pre-processing (buffer flush trigger by \n instead of key by key)
     and input echo (printing of user input on the terminal output) *)
  Unix.{ term_info with c_icanon = false; c_echo = false }
;;

let ( ~! ) out = Out_channel.flush out

let rec read_terminal_input_loop terminal =
  match Tty.read_terminal_input terminal with
  | [] -> read_terminal_input_loop terminal
  | cmds -> cmds
;;

let loop_app (module A : App) terminal out =
  let tty_out_chars = Tty.send_chars out
  and tty_out_line s =
    Tty.send_string out s;
    Tty.send_chars out [ '\n' ]
  in
  let rec loop model =
    tty_out_chars Tty.clear_screen;
    List.iter tty_out_line (A.view model);
    ~!out;
    let cmds = read_terminal_input_loop terminal in
    let updated = List.fold_left A.update model cmds in
    loop updated
  in
  loop A.init
;;

module Demo : App = struct
  type phase =
    | Hello
    | Display_check
    | Progress_bar of Progress.t
    | End

  type model = phase

  let init = Hello

  let display_check_lines =
    [ "The following lines surrounded by vvv and ^^^ are here to verify the terminal \
       display has the expected behavior"
    ; String.make 80 'v'
    ; Term_color.styled
        { Term_color.default_style with fg_color = Some Red }
        "This should appear Red"
    ; Term_color.styled
        { Term_color.default_style with fg_color = Some Green }
        "This should appear Green"
    ; Term_color.styled
        { Term_color.default_style with fg_color = Some Blue }
        "This should appear Blue"
    ; Term_color.styled
        { Term_color.default_style with fg_color = Some Yellow }
        "This should appear Yellow"
    ; Term_color.styled
        { Term_color.default_style with fg_color = Some Magenta }
        "This should appear Magenta"
    ; Term_color.styled
        { Term_color.default_style with fg_color = Some Cyan }
        "This should appear Cyan"
    ; Term_color.styled
        { Term_color.default_style with bg_color = Some Red }
        "This should appear Red"
    ; Term_color.styled
        { Term_color.default_style with bg_color = Some Green }
        "This should appear Green"
    ; Term_color.styled
        { Term_color.default_style with bg_color = Some Blue }
        "This should appear Blue"
    ; Term_color.styled
        { Term_color.default_style with bg_color = Some Yellow }
        "This should appear Yellow"
    ; Term_color.styled
        { Term_color.default_style with bg_color = Some Magenta }
        "This should appear Magenta"
    ; Term_color.styled
        { Term_color.default_style with bg_color = Some Cyan }
        "This should appear Cyan"
    ; Term_color.styled
        { Term_color.default_style with underlined = true }
        "This should be underlined"
    ; Term_color.styled
        { Term_color.default_style with bold = true }
        "This should be bold"
    ; Term_color.styled
        { Term_color.default_style with underlined = true; bold = true }
        "This should be bold and underlined"
    ; Term_color.styled
        { bg_color = Some Yellow; fg_color = None; underlined = true; bold = false }
        "This should be yellow and underlined"
    ; String.make 80 '^'
    ; "Press Enter to proceed to the next phase of the demo"
    ]
  ;;

  let view = function
    | Hello ->
      [ "Hell'OCaml"
      ; "This is a demo/test program"
      ; "This is a demo/test program"
      ; "Press Enter to proceed to the next phase of the demo"
      ]
    | Display_check -> display_check_lines
    | Progress_bar b ->
      [ "Now testing the terminal I/O interactivity behavior"
      ; "A progress bar will appear. You can:"
      ; "\tincrease it by pressing the right arrow key,"
      ; "\tdecrease it by pressing the left arrow key,"
      ; "\tclear it by pressing the delete key,"
      ; "Press Enter to proceed to the next phase of the demo"
      ; Progress.to_string b
      ]
    | End -> [ "End of Demo, hit Ctrl+C to exit he program" ]
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
  Unix.tcsetattr term Unix.TCSANOW (disable_default_terminal_behavior info);
  let out = Out_channel.stdout in
  loop_app (module Demo) term out
;;
