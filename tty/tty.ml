open Qol

type position =
  { row : int
  ; col : int
  }

type ansi_event =
  | Up
  | Right
  | Down
  | Left
  | Del
  | Enter
  | Esc
  | Size of position
  | Unknown
  | Char of char

let string_of_ansi_event = function
  | Up -> "Up"
  | Right -> "Right"
  | Down -> "Down"
  | Left -> "Left"
  | Del -> "Del"
  | Enter -> "Enter"
  | Esc -> "Esc"
  | Size { row; col } -> Printf.sprintf "Size %dx%d" row col
  | Unknown -> "Unknown"
  | Char c -> Printf.sprintf "Char %c" c
;;

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
  ; striked : bool
  }

type ansi_view_item_kind =
  | Text of string
  | Cursor

let text s = Text s

type ansi_view_item = position * style * ansi_view_item_kind

module type Ansi_App =
  Tea.App with type event = ansi_event and type view = ansi_view_item list

module type Ansi_Platform =
  Tea.Platform with type event = ansi_event and type view = ansi_view_item list

module Ansi_Tea_Base = struct
  type event = ansi_event
  type view = ansi_view_item list
end

let csi_seq = [ '\x1B'; '[' ]
let csi chars = csi_seq @ chars
let csi_str s = Printf.sprintf "\x1b[%s" s

let parse_terminated_num terminator char_list =
  let rec aux acc = function
    | c :: t when c = terminator -> if acc = "" then None else Some (int_of_string acc, t)
    | ('0' .. '9' as d) :: t -> aux (acc ^ String.make 1 d) t
    | _ -> None
  in
  aux "" char_list
;;

(* match the n; part of ESC[n;mR sequence *)
let parse_resize_row char_list = parse_terminated_num ';' char_list

(* match the mR part of ESC[n;mR sequence *)
let parse_resize_col char_list = parse_terminated_num 'R' char_list

let parse_resize char_list =
  parse_resize_row char_list
  |?* fun (r, t) -> parse_resize_col t |? fun (c, t) -> { row = r; col = c }, t
;;

let events_of_bytes bytes =
  let rec aux acc = function
    | [] -> List.rev acc
    | '\027' :: '\091' :: '\065' :: t -> aux (Up :: acc) t
    | '\027' :: '\091' :: '\066' :: t -> aux (Down :: acc) t
    | '\027' :: '\091' :: '\067' :: t -> aux (Right :: acc) t
    | '\027' :: '\091' :: '\068' :: t -> aux (Left :: acc) t
    | '\x1B' :: '[' :: t ->
      (match parse_resize t with
       | Some (pos, rest) -> aux (Size pos :: acc) rest
       | None -> aux (Unknown :: acc) t)
    | '\027' :: t -> aux (Esc :: acc) t
    | '\127' :: t -> aux (Del :: acc) t
    | '\r' :: '\n' :: t | '\n' :: t | '\r' :: t -> aux (Enter :: acc) t
    | c :: t -> aux (Char c :: acc) t
  in
  aux [] bytes
;;

let move { row; col } = csi_str @@ Printf.sprintf "%d;%dH" row col
let csi_comma = ';'

(* Move cursor to the top left then clear screen after the cursor *)
let clear_screen = csi [ '1'; csi_comma; '1'; 'H' ] @ csi [ '0'; 'J' ]

(* Try to move cursor to the bottom-rightest possible then query the actual cursor position *)
let ask_dimensions =
  csi [ '9'; '9'; '9'; csi_comma; '9'; '9'; '9'; 'H' ] @ csi [ '6'; 'n' ]
;;

let show_cursor = csi [ '?'; '2'; '5'; 'h' ]
let hide_cursor = csi [ '?'; '2'; '5'; 'l' ]

module type Style_Default = sig
  val default_foreground_color : color
  val default_background_color : color
end

module type Styling = sig
  val default_style : style
  val styled : style -> string -> string
end

module Posix_style (D : Style_Default) : Styling = struct
  let default_style =
    { fg_color = None
    ; bg_color = None
    ; underlined = false
    ; bold = false
    ; striked = false
    }
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
  let striked_str b = if b then Some "9" else None

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
      ; striked_str style.striked
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

module Default_style = Posix_style (struct
    let default_background_color = Default
    let default_foreground_color = Default
  end)

let send_chars out cmds = List.iter (Out_channel.output_char out) cmds
let send_string out s = Out_channel.output_string out s

let read_terminal_input terminal_fd =
  let ready, _, _ = Unix.select [ terminal_fd ] [] [] 0.01 in
  let buf = Bytes.create 48 in
  match List.exists (( = ) terminal_fd) ready with
  | true ->
    let read = Unix.read terminal_fd buf 0 48 in
    Bytes.sub buf 0 read |> Bytes.to_seq |> List.of_seq |> events_of_bytes
  | false -> []
;;

let read_terminal_input_loop terminal out =
  let tty_out_chars = send_chars out in
  let ask_resize_freq = 50 in
  let rec aux counter =
    match read_terminal_input terminal with
    | [] ->
      if counter = 0
      then (
        tty_out_chars ask_dimensions;
        Out_channel.flush out;
        aux ask_resize_freq)
      else aux (counter - 1)
    | cmds -> cmds
  in
  aux ask_resize_freq
;;

module type Posix_terminal = sig
  val terminal_in : Unix.file_descr
  val terminal_out : Out_channel.t

  module Style : Styling
end

module Posix_terminal_platform (T : Posix_terminal) : sig
  include Ansi_Platform with type command = Tea.no_command

  val restore_terminal_state : unit -> unit
end = struct
  include Ansi_Tea_Base

  type command = Tea.no_command

  let default_behavior_disabled term_info =
    (*
       c_icanon = false => disable special input pre processing
       echo     = false => disable auto-printing of inputs to output
    *)
    Unix.{ term_info with c_icanon = false; c_echo = false }
  ;;

  let default_behavior_enabled term_info =
    Unix.{ term_info with c_icanon = true; c_echo = true }
  ;;

  let tty_out_chars = send_chars T.terminal_out
  and tty_out_line s = send_string T.terminal_out s

  let show_cursor_if_present = function
    | Some pos ->
      tty_out_line (move pos);
      tty_out_line (show_cursor |> List.to_seq |> String.of_seq)
    | None -> ()
  ;;

  let setup () =
    let info = Unix.tcgetattr T.terminal_in in
    Unix.tcsetattr T.terminal_in Unix.TCSANOW (default_behavior_disabled info)
  ;;

  let render_item style position final_cursor_position = function
    | Text txt -> tty_out_line (T.Style.styled style txt)
    | Cursor -> final_cursor_position := Some position
  ;;

  let render view =
    tty_out_chars hide_cursor;
    tty_out_chars clear_screen;
    let cursor_position = ref None in
    List.iter
      (fun (p, s, item) ->
        tty_out_line (move p);
        render_item s p cursor_position item)
      view;
    show_cursor_if_present !cursor_position;
    Out_channel.flush T.terminal_out
  ;;

  let poll_events () = read_terminal_input_loop T.terminal_in T.terminal_out
  let handle_commands _ = ()

  let restore_terminal_state () =
    tty_out_chars clear_screen;
    tty_out_line (move { row = 1; col = 1 });
    let info = Unix.tcgetattr T.terminal_in in
    Unix.tcsetattr T.terminal_in Unix.TCSANOW (default_behavior_enabled info);
    tty_out_chars show_cursor
  ;;
end
