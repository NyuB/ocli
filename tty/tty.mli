type position =
  { row : int
  ; col : int
  }

(** Terminal events
    - [Size position] -> signals that the surrouding window has been resized to [position.row] x [position.col] *)
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

type view_item = position * style * string

(** Elm-like application :
    - [model] represents the application state, [init] being the initial state
    - [view] is the graphical projection of this model, here as strings positioned on a window
    - [update] is the pure function computing the updated version of a model in response to a message (here terminal [events])
      The actual tasks of rendering the view and implementing the event loop is left to the platform (see [loop_app]) *)
module type App = sig
  type model

  val init : model
  val view : model -> view_item list
  val update : model -> event -> model
end

(** Represents the actual, potentially impure, engine used to render an application and implement the events poling and distribution *)
module type Platform = sig
  val render : view_item list -> unit
  val poll_events : unit -> event list
end

(** [ loop_app (module A) (module P) terminal out ] loops indefinitely over the sequence:
    + Compute the current view of the current [model: A.model] value (starting with [A.init])
    + Render the view on [out] using [P.render]
    + Read [events] from [P.poll_events]
    + Compute the new model from the current one, feeding events to [A.update] *)
val loop_app : (module App) -> (module Platform) -> unit

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

module type Posix_terminal = sig
  val terminal_in : Unix.file_descr
  val terminal_out : Out_channel.t

  module Style : Styling
end

module Posix_terminal_platform (_ : Posix_terminal) : Platform

(** Disable canonical character pre-processing (buffer flush trigger by \n instead of key by key)
    and input echo (printing of user input on the terminal output) *)
val default_behavior_disabled : Unix.terminal_io -> Unix.terminal_io
