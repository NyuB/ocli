type position =
  { row : int
  ; col : int
  }

type event =
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

val string_of_event : event -> string

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

val default_behavior_disabled : Unix.terminal_io -> Unix.terminal_io

type view_item = position * style * string

module type App = sig
  type model

  val init : model
  val view : model -> view_item list
  val update : model -> event -> model
end

val loop_app : (module App) -> (module Styling) -> Unix.file_descr -> out_channel -> unit
