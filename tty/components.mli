module Constraints : sig
  (** Represents a box area available to display a given view *)
  type t =
    { col_start : int
    ; width : int
    ; row_start : int
    ; height : int
    }
end

module Space : sig
  (** Represents a box area required to draw a given view *)
  type t =
    { col_start : int
    ; width : int
    ; row_start : int
    ; height : int
    }
end

(** A component is a function from an available box area (the [constraints]) to a view and the box area actually required to display this view

    The component is reponsible to return a view that actually fits within the given constraints *)
type 'view component = Constraints.t -> 'view * Space.t

val map : ('a -> Space.t -> 'b * Space.t) -> 'a component -> 'b component

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

(** Monoid, intended to combine views in layout components *)
module type Merge_views = sig
  type view

  val empty : view
  val merge : view -> view -> view
end

module Merge_ansi_views : Merge_views with type view = Tty.ansi_view_item list

module Row (M : Merge_views) : sig
  (** [component components] arrange the given [components] in a row.

      Placement is eager and the first components are given the full space as constraints,
      use [Row_divided] to control each component available space more precisely. *)
  val component : M.view component list -> M.view component
end

module Row_divided (M : Merge_views) : sig
  type 'view fraction_component = 'view component * int

  (** [component components] arrange the given [components] in a row, giving each component its fraction of the available area as constraint.

      Examples:
      - [component [a, 1; b, 1] constraints] will layout a and b within at most one half of the available [constraints] each.
      - [component [a, 2; b, 1; c, 2] constraints] will layout a and c within at most 2/5th of the available [constraints] each, and b with 1/5. *)
  val component : M.view fraction_component list -> M.view component
end

module Column (M : Merge_views) : sig
  (** [component components] arrange the given [components] in a column.

      Placement is eager and the first components are given the full space as constraints. *)
  val component : M.view component list -> M.view component
end

module Text_line : sig
  (** [component s] display the string s, cropping it if not enough width is available *)
  val component : string -> Tty.ansi_view_item_kind component
end

module Column_divided (M : Merge_views) : sig
  type 'view fraction_component = 'view component * int

  val component : M.view fraction_component list -> M.view component
end

(** Handle a line being 'edited', with current typed text and an edition cursor *)
module Editing_line : sig
  type t

  (** *)
  type event =
    | Del (** Delete the character before cursor *)
    | Suppr (** Delete the character under cursor *)
    | Char of char (** Type a character at cursor *)
    | Left (** Move cursor left *)
    | Right (** Move cursor right *)

  val update : t -> event -> t

  (** [append_char c t] = [update t (Char c)] *)
  val append_char : char -> t -> t

  (** [del t] = [update t Del] *)
  val del : t -> t

  (** [suppr t] = [update t Suppr] *)
  val suppr : t -> t

  (** [left t] = [update t Left] *)
  val left : t -> t

  (** [right t] = [update t Right] *)
  val right : t -> t

  (** Start editing with no typed character *)
  val empty : t

  (** [init s] Start editing with [s] typed and the cursor at the end of the line *)
  val init : string -> t

  (** The typed characters *)
  val to_string : t -> string

  (** Current position of the cursor. [edition_index t] may be be equal to [String.length (to_string t)] if the cursor is at the end of the line *)
  val edition_index : t -> int

  (** Display the edited line, cropping it around cursor if needed, and the cursor *)
  val component : t -> (Tty.position * Tty.ansi_view_item_kind) list component
end

module Column_sliding (M : Merge_views) : sig
  val component
    :  ?height_per_entry:int
    -> (int -> 'a -> M.view component)
    -> 'a array
    -> int
    -> M.view component
end
