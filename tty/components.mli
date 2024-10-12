type constraints =
  { col_start : int
  ; width : int
  ; row_start : int
  ; height : int
  }

type 'view component = constraints -> 'view * constraints

val map
  :  ('a -> constraints -> 'b * constraints)
  -> 'a component
  -> constraints
  -> 'b * constraints

val to_ansi_view_component
  :  Tty.style
  -> Tty.ansi_view_item_kind component
  -> Tty.ansi_view_item list component

val positioned_to_ansi_view_component
  :  Tty.style
  -> (Tty.position * Tty.ansi_view_item_kind) list component
  -> Tty.ansi_view_item list component

val styled_to_ansi_view_item_component
  :  (Tty.style * Tty.ansi_view_item_kind) component
  -> Tty.ansi_view_item list component

module type Merge_views = sig
  type view

  val empty : view
  val merge : view -> view -> view
end

module Merge_ansi_views : Merge_views with type view = Tty.ansi_view_item list

module Row (M : Merge_views) : sig
  val component : M.view component list -> M.view component
end

module Row_divided (M : Merge_views) : sig
  type 'view fraction_component = 'view component * int

  val component : M.view fraction_component list -> M.view component
end

module Column (M : Merge_views) : sig
  val component : M.view component list -> M.view component
end

module Text_line : sig
  val component : string -> constraints -> Tty.ansi_view_item_kind * constraints
end

module Editing_line : sig
  type t

  type event =
    | Del
    | Char of char
    | Left
    | Right

  val update : t -> event -> t
  val append_char : char -> t -> t
  val del : t -> t
  val left : t -> t
  val right : t -> t
  val empty : t
  val init : string -> t
  val to_string : t -> string
  val edition_index : t -> int

  val component
    :  t
    -> constraints
    -> (Tty.position * Tty.ansi_view_item_kind) list * constraints
end
