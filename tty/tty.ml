open Qol

type command =
  | Up
  | Right
  | Down
  | Left
  | Del
  | Enter
  | Esc
  | Unknown
  | Char of char

let string_of_command = function
  | Up -> "Up"
  | Right -> "Right"
  | Down -> "Down"
  | Left -> "Left"
  | Del -> "Del"
  | Enter -> "Enter"
  | Esc -> "Esc"
  | Unknown -> "Unknown"
  | Char c -> Printf.sprintf "Char %c" c
;;

let commands_of_bytes bytes =
  let rec aux acc = function
    | [] -> List.rev acc
    | '\027' :: '\091' :: '\065' :: t -> aux (Up :: acc) t
    | '\027' :: '\091' :: '\066' :: t -> aux (Down :: acc) t
    | '\027' :: '\091' :: '\067' :: t -> aux (Right :: acc) t
    | '\027' :: '\091' :: '\068' :: t -> aux (Left :: acc) t
    | '\027' :: '\091' :: _ :: t -> aux (Unknown :: acc) t
    | '\027' :: t -> aux (Esc :: acc) t
    | '\127' :: t -> aux (Del :: acc) t
    | '\r' :: '\n' :: t | '\n' :: t | '\r' :: t -> aux (Enter :: acc) t
    | c :: t -> aux (Char c :: acc) t
  in
  aux [] bytes
;;

let csi_seq = [ '\x1B'; '[' ]
let csi chars = csi_seq @ chars
let csi_str s = Printf.sprintf "\x1b[%s" s

let csis count cmd_char =
  let rec aux acc n =
    if n <= 0
    then List.rev acc
    else if n < 10
    then (
      let with_csi = List.rev_append csi_seq acc in
      let code = Char.chr n in
      aux (cmd_char :: code :: with_csi) 0)
    else (
      let ni = n - 9 in
      let with_csi = List.rev_append csi_seq acc in
      let code = Char.chr 9 in
      aux (cmd_char :: code :: with_csi) ni)
  in
  aux [] count
;;

let clear_screen = csi [ '1'; ';'; '1'; 'H' ] @ csi [ '0'; 'J' ]

type color =
  | Black
  | White
  | Red
  | Green
  | Blue
  | Yellow
  | Magenta
  | Cyan
  | Default

type style =
  { fg_color : color option
  ; bg_color : color option
  ; underlined : bool
  ; bold : bool
  }

module type Style_Default = sig
  val default_foreground_color : color
  val default_background_color : color
end

module Style (D : Style_Default) : sig
  val default_style : style
  val styled : style -> string -> string
end = struct
  let default_style =
    { fg_color = None; bg_color = None; underlined = false; bold = false }
  ;;

  let fg_code = function
    | Black -> 30
    | Red -> 31
    | Green -> 32
    | Yellow -> 33
    | Blue -> 34
    | Magenta -> 35
    | Cyan -> 36
    | White -> 37
    | Default -> 39
  ;;

  let bg_code = function
    | Black -> 40
    | Red -> 41
    | Green -> 42
    | Yellow -> 43
    | Blue -> 44
    | Magenta -> 45
    | Cyan -> 46
    | White -> 47
    | Default -> 49
  ;;

  let bg_str c = Printf.sprintf "%d" (bg_code c)
  let fg_str c = Printf.sprintf "%d" (fg_code c)
  let ul_str b = if b then Some "4" else None
  let bold_str b = if b then Some "1" else None

  let csi_defaults =
    csi_str
    @@ Printf.sprintf
         "%d;%d;0m"
         (fg_code D.default_foreground_color)
         (bg_code D.default_background_color)
  ;;

  let styled style str =
    let commands =
      [ ul_str style.underlined
      ; bold_str style.bold
      ; style.bg_color |? bg_str
      ; style.fg_color |? fg_str
      ]
      |> List.not_none
    in
    if commands = []
    then str
    else csi_str (Printf.sprintf "%sm%s%s" (String.concat ";" commands) str csi_defaults)
  ;;
end

let send_chars out cmds = List.iter (Out_channel.output_char out) cmds
let send_string out s = Out_channel.output_string out s

let disable_default_terminal_behavior term_info =
  (* Disable canonical character pre-processing (buffer flush trigger by \n instead of key by key)
     and input echo (printing of user input on the terminal output) *)
  Unix.{ term_info with c_icanon = false; c_echo = false }
;;

let read_terminal_input terminal_fd =
  let ready, _, _ = Unix.select [ terminal_fd ] [] [] 0.01 in
  let buf = Bytes.create 48 in
  match List.exists (( = ) terminal_fd) ready with
  | true ->
    let read = Unix.read terminal_fd buf 0 48 in
    Bytes.sub buf 0 read |> Bytes.to_seq |> List.of_seq |> commands_of_bytes
  | false -> []
;;

let rec read_terminal_input_loop terminal =
  match read_terminal_input terminal with
  | [] -> read_terminal_input_loop terminal
  | cmds -> cmds
;;

module type App = sig
  type model

  val init : model
  val view : model -> string list
  val update : model -> command -> model
end

let loop_app (module A : App) terminal out =
  let tty_out_chars = send_chars out
  and tty_out_line s =
    send_string out s;
    send_chars out [ '\n' ]
  in
  let rec loop model =
    tty_out_chars clear_screen;
    List.iter tty_out_line (A.view model);
    Out_channel.flush out;
    let cmds = read_terminal_input_loop terminal in
    let updated = List.fold_left A.update model cmds in
    loop updated
  in
  loop A.init
;;
