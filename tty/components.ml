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

let positioned_to_ansi_view_component
  style
  (component : (Tty.position * Tty.ansi_view_item_kind) list component)
  : Tty.ansi_view_item list component
  =
  map
    (fun items constraints ->
      List.map (fun (pos, kind) -> pos, style, kind) items, constraints)
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

module Editing_line = struct
  type t =
    { s : string
    ; cursor : int
    }

  type event =
    | Del
    | Char of char
    | Left
    | Right

  let update ({ s; cursor } as t) (e : event) : t =
    match e with
    | Del ->
      if cursor = 0
      then t
      else (
        let deleted = cursor - 1 in
        let left = String.sub s 0 deleted in
        let right_from = cursor
        and len = String.length s in
        if right_from = len
        then { s = left; cursor = cursor - 1 }
        else (
          let right = String.sub s right_from (String.length s - right_from) in
          { s = left ^ right; cursor = cursor - 1 }))
    | Char c ->
      let left = String.sub s 0 cursor
      and right =
        if String.length s = cursor
        then ""
        else String.sub s cursor (String.length s - cursor)
      in
      { s = Printf.sprintf "%s%c%s" left c right; cursor = cursor + 1 }
    | Left -> { t with cursor = max 0 (cursor - 1) }
    | Right -> { t with cursor = min (String.length s) (cursor + 1) }
  ;;

  let append_char c t = update t (Char c)
  let del t = update t Del
  let left t = update t Left
  let right t = update t Right
  let empty = { s = ""; cursor = 0 }
  let init s = { s; cursor = String.length s }
  let to_string t = t.s
  let edition_index t = t.cursor

  let make { s; cursor } { col_start; width; row_start; height } =
    if height <= 0 || width <= 0
    then [], { row_start; col_start; width = 0; height = 0 }
    else (
      let len = String.length s in
      if len <= width
      then
        ( [ Tty.{ row = row_start; col = col_start }, Tty.text s
          ; Tty.{ row = row_start; col = col_start + cursor }, Tty.Cursor
          ]
        , { row_start; col_start; height = 1; width = max len (cursor + 1) } )
      else (
        let left = width / 2 in
        let left_start = max 0 (cursor - left) in
        let cropped = String.sub s left_start width in
        ( [ Tty.{ row = row_start; col = col_start }, Tty.text @@ cropped
          ; Tty.{ row = row_start; col = cursor - left_start + 1 }, Tty.Cursor
          ]
        , { row_start; col_start; height = 1; width } )))
  ;;
end
