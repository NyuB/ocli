type position =
  { row : int
  ; col : int
  }

type command =
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

val string_of_command : command -> string

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

module type Styling = sig
  val default_style : style
  val styled : style -> string -> string
end

module Style : functor (_ : Style_Default) -> Styling

val send_chars : out_channel -> char list -> unit
val send_string : out_channel -> string -> unit
val disable_default_terminal_behavior : Unix.terminal_io -> Unix.terminal_io
val read_terminal_input : Unix.file_descr -> command list
val read_terminal_input_loop : Unix.file_descr -> out_channel -> command list

type view_item = position * style * string

module type App = sig
  type model

  val init : model
  val view : model -> view_item list
  val update : model -> command -> model
end

val loop_app : (module App) -> (module Styling) -> Unix.file_descr -> out_channel -> unit
