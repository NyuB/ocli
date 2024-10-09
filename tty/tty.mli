type position =
  { row : int
  ; col : int
  }

(** Terminal events
    - [Size position] -> signals that the surrouding window has been resized to [position.row] x [position.col] *)
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

val string_of_ansi_event : ansi_event -> string

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

val text : string -> ansi_view_item_kind

type ansi_view_item = position * style * ansi_view_item_kind

module type Ansi_App =
  Tea.App with type event = ansi_event and type view = ansi_view_item list

module type Ansi_Platform =
  Tea.Platform with type event = ansi_event and type view = ansi_view_item list

module Ansi_Tea_Base : sig
  type event = ansi_event
  type view = ansi_view_item list
end

module type Style_Default = sig
  val default_foreground_color : color
  val default_background_color : color
end

(** Surround strings with control sequences to render the given style *)
module type Styling = sig
  val default_style : style
  val styled : style -> string -> string
end

module Posix_style : functor (_ : Style_Default) -> Styling
module Default_style : Styling

module type Posix_terminal = sig
  val terminal_in : Unix.file_descr
  val terminal_out : Out_channel.t

  module Style : Styling
end

module Posix_terminal_platform (_ : Posix_terminal) : sig
  include Ansi_Platform with type command = Tea.no_command

  (** Reset terminal display modifications such as cursor position or visibility that may have been altered for rendering *)
  val restore_terminal_state : unit -> unit
end
