type constraints =
  { col_start : int
  ; width : int
  ; row_start : int
  ; height : int
  }

let next_col_start { col_start; width; _ } = col_start + width
let next_row_start { row_start; height; _ } = row_start + height

type 'view component = constraints -> 'view * constraints

let map
  (f : 'a -> constraints -> 'b * constraints)
  (component : 'a component)
  (constraints : constraints)
  : 'b * constraints
  =
  let v, c = component constraints in
  f v c
;;

let to_ansi_view_component style (component : Tty.ansi_view_item_kind component)
  : Tty.ansi_view_item list component
  =
  map
    (fun item ({ col_start; row_start; _ } as constraints) ->
      [ Tty.{ col = col_start; row = row_start }, style, item ], constraints)
    component
;;

let styled_to_ansi_view_item_component
  (component : (Tty.style * Tty.ansi_view_item_kind) component)
  : Tty.ansi_view_item list component
  =
  map
    (fun (style, item) ({ col_start; row_start; _ } as constraints) ->
      [ Tty.{ col = col_start; row = row_start }, style, item ], constraints)
    component
;;

module type Merge_views = sig
  type view

  val empty : view
  val merge : view -> view -> view
end

module Merge_ansi_views : Merge_views with type view = Tty.ansi_view_item list = struct
  type view = Tty.ansi_view_item list

  let empty = []
  let merge a b = a @ b
end

module Row (M : Merge_views) = struct
  let shift_constraints constraints component_rendering =
    { constraints with
      col_start = next_col_start component_rendering
    ; width = constraints.width - component_rendering.width
    }
  ;;

  let add_column (view, constraints, max_height) item =
    let item_view, actual_item_constraints = item constraints in
    ( M.merge view item_view
    , shift_constraints constraints actual_item_constraints
    , max max_height actual_item_constraints.height )
  ;;

  let make (components : 'view component list) : 'view component =
    fun ({ width; _ } as constraints) ->
    let v, { width = remaining_width; _ }, max_height =
      List.fold_left add_column (M.empty, constraints, 0) components
    in
    v, { constraints with height = max_height; width = width - remaining_width }
  ;;
end

module Row_divided (M : Merge_views) = struct
  type 'view fraction_component = 'view component * int

  let make (components : 'view fraction_component list) : 'view component =
    fun constraints ->
    let total = List.fold_left (fun n (_, f) -> n + f) 0 components in
    let v, col, max_height =
      List.fold_left
        (fun (v, col, max_height) (component, fraction) ->
          let available = constraints.width * fraction / total in
          let component_view, { height; width; _ } =
            component
              { col_start = col
              ; row_start = constraints.row_start
              ; width = available
              ; height = constraints.height
              }
          in
          M.merge v component_view, col + width, max max_height height)
        (M.empty, constraints.col_start, 0)
        components
    in
    ( v
    , { col_start = constraints.col_start
      ; row_start = constraints.row_start
      ; height = max_height
      ; width = col - constraints.col_start
      } )
  ;;
end

module Column (M : Merge_views) = struct
  let shift_constraints constraints component_rendering =
    { constraints with
      row_start = next_row_start component_rendering
    ; height = constraints.height - component_rendering.height
    }
  ;;

  let add_row (view, constraints, max_width) item =
    let item_view, actual_item_constraints = item constraints in
    ( M.merge view item_view
    , shift_constraints constraints actual_item_constraints
    , max max_width actual_item_constraints.width )
  ;;

  let make (components : 'view component list) : 'view component =
    fun ({ height; _ } as constraints) ->
    let v, { height = remaining_height; _ }, max_width =
      List.fold_left add_row (M.empty, constraints, 0) components
    in
    v, { constraints with height = height - remaining_height; width = max_width }
  ;;
end

module Text_line = struct
  let crop_to_size max_size s =
    if String.length s <= max_size
    then s
    else if max_size >= 3
    then String.sub s 0 (max_size - 3) ^ "..."
    else String.sub s 0 max_size
  ;;

  let component line ({ width; height; _ } as constraints) =
    if height <= 0
    then Tty.Text "", { constraints with width = 0; height = 0 }
    else (
      let cropped = crop_to_size width line in
      Tty.Text cropped, { constraints with width = String.length cropped; height = 1 })
  ;;
end
