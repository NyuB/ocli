open Qol

module Term_color = Tty.Style (struct
    let default_foreground_color = Tty.Default
    let default_background_color = Tty.Default
  end)

module Progress : sig
  type t

  val init : char -> t
  val to_string : t -> string
  val handle_command : Tty.command -> t -> t
  val equal : t -> t -> bool
end = struct
  type t = int * char

  let equal = ( = )
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

let () =
  let term = Unix.stdin in
  let info = Unix.tcgetattr term in
  Unix.tcsetattr term Unix.TCSANOW (disable_default_terminal_behavior info);
  let bar = ref (Progress.init '@') in
  let out = Out_channel.stdout in
  let tty_out_chars = Tty.send_chars out
  and tty_out_line s =
    Tty.send_string out s;
    Tty.send_chars out [ '\n' ]
  in
  tty_out_line "Hell'OCaml";
  tty_out_line "This is a demo/test program";
  tty_out_line
    "The following lines surrounded by vvv and ^^^ are here to verify the terminal \
     display has the expected behavior";
  tty_out_line (String.make 80 'v');
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with fg_color = Some Red }
       "This should appear Red");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with fg_color = Some Green }
       "This should appear Green");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with fg_color = Some Blue }
       "This should appear Blue");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with fg_color = Some Yellow }
       "This should appear Yellow");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with fg_color = Some Magenta }
       "This should appear Magenta");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with fg_color = Some Cyan }
       "This should appear Cyan");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bg_color = Some Red }
       "This should appear Red");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bg_color = Some Green }
       "This should appear Green");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bg_color = Some Blue }
       "This should appear Blue");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bg_color = Some Yellow }
       "This should appear Yellow");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bg_color = Some Magenta }
       "This should appear Magenta");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bg_color = Some Cyan }
       "This should appear Cyan");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with underlined = true }
       "This should be underlined");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with bold = true }
       "This should be bold");
  tty_out_line
    (Term_color.styled
       { Term_color.default_style with underlined = true; bold = true }
       "This should be bold and underlined");
  tty_out_line
    (Term_color.styled
       { bg_color = Some Yellow; fg_color = None; underlined = true; bold = false }
       "This should be yellow and underlined");
  tty_out_line (String.make 80 '^');
  tty_out_line "Now testing the terminal I/O interactivity behavior";
  tty_out_line "A progress bar will appear. You can:";
  tty_out_line "\tincrease it by pressing the right arrow key,";
  tty_out_line "\tdecrease it by pressing the left arrow key,";
  tty_out_line "\tclear it by pressing the delete key,";
  tty_out_line "End the program by hitting Ctrl+C";
  Out_channel.output_line out (Progress.to_string !bar);
  Out_channel.flush out;
  while true do
    let cmds = Tty.read_terminal_input term in
    let next_bar =
      List.fold_left (fun b cmd -> Progress.handle_command cmd b) !bar cmds
    in
    if not (Progress.equal !bar next_bar)
    then (
      bar := next_bar;
      tty_out_chars Tty.previous_line;
      Out_channel.flush out;
      Out_channel.output_line out (Progress.to_string !bar);
      Out_channel.flush out)
  done
;;
