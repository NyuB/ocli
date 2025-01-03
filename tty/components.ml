module Constraints = struct
  type t =
    { col_start : int
    ; width : int
    ; row_start : int
    ; height : int
    }
end

module Space = struct
  type t =
    { col_start : int
    ; width : int
    ; row_start : int
    ; height : int
    }
end

let next_col_start Space.{ col_start; width; _ } = col_start + width
let next_row_start Space.{ row_start; height; _ } = row_start + height

type 'view component = Constraints.t -> 'view * Space.t

let map
  (f : 'a -> Space.t -> 'b * Space.t)
  (component : 'a component)
  (constraints : Constraints.t)
  : 'b * Space.t
  =
  let v, c = component constraints in
  f v c
;;

let to_ansi_view_component style (component : Tty.ansi_view_item_kind component)
  : Tty.ansi_view_item list component
  =
  map
    (fun item ({ col_start; row_start; _ } as size) ->
      [ Tty.{ col = col_start; row = row_start }, style, item ], size)
    component
;;

let positioned_to_ansi_view_component
  style
  (component : (Tty.position * Tty.ansi_view_item_kind) list component)
  : Tty.ansi_view_item list component
  =
  map
    (fun items size -> List.map (fun (pos, kind) -> pos, style, kind) items, size)
    component
;;

let styled_to_ansi_view_item_component
  (component : (Tty.style * Tty.ansi_view_item_kind) component)
  : Tty.ansi_view_item list component
  =
  map
    (fun (style, item) ({ col_start; row_start; _ } as size) ->
      [ Tty.{ col = col_start; row = row_start }, style, item ], size)
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
    Constraints.
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

  let component (components : 'view component list) : 'view component =
    fun ({ width; col_start; row_start; _ } as constraints) ->
    let v, Constraints.{ width = remaining_width; _ }, max_height =
      List.fold_left add_column (M.empty, constraints, 0) components
    in
    ( v
    , Space.{ col_start; row_start; height = max_height; width = width - remaining_width }
    )
  ;;
end

module Row_divided (M : Merge_views) = struct
  type 'view fraction_component = 'view component * int

  let component (components : 'view fraction_component list) : 'view component =
    fun constraints ->
    let total = List.fold_left (fun n (_, f) -> n + f) 0 components in
    let v, col, max_height =
      List.fold_left
        (fun (v, col, max_height) (component, fraction) ->
          let available = constraints.width * fraction / total in
          let component_view, Space.{ height; width; _ } =
            component
              Constraints.
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
    Constraints.
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

  let component (components : 'view component list) : 'view component =
    fun ({ height; col_start; row_start; _ } as constraints) ->
    let v, Constraints.{ height = remaining_height; _ }, max_width =
      List.fold_left add_row (M.empty, constraints, 0) components
    in
    ( v
    , Space.
        { col_start; row_start; height = height - remaining_height; width = max_width } )
  ;;
end

module Column_divided (M : Merge_views) = struct
  type 'view fraction_component = 'view component * int

  let component (components : 'view fraction_component list) : 'view component =
    fun constraints ->
    let total = List.fold_left (fun n (_, f) -> n + f) 0 components in
    let v, row, max_width =
      List.fold_left
        (fun (v, row, max_width) (component, fraction) ->
          let available = constraints.height * fraction / total in
          let component_view, Space.{ height; width; _ } =
            component
              Constraints.
                { col_start = constraints.col_start
                ; row_start = row
                ; width = constraints.width
                ; height = available
                }
          in
          M.merge v component_view, row + height, max max_width width)
        (M.empty, constraints.row_start, 0)
        components
    in
    ( v
    , { col_start = constraints.col_start
      ; row_start = constraints.row_start
      ; height = row - constraints.row_start
      ; width = max_width
      } )
  ;;
end

module Text_line = struct
  let crop_to_size max_size s =
    if String.length s <= max_size
    then s
    else if max_size >= 3
    then String.sub s 0 (max_size - 3) ^ "..."
    else if max_size >= 0
    then String.sub s 0 max_size
    else ""
  ;;

  let component line Constraints.{ width; height; col_start; row_start } =
    if height <= 0
    then Tty.Text "", Space.{ col_start; row_start; width = 0; height = 0 }
    else (
      let cropped = crop_to_size width line in
      ( Tty.Text cropped
      , Space.{ col_start; row_start; width = String.length cropped; height = 1 } ))
  ;;
end

module Editing_line = struct
  type t =
    { s : string
    ; cursor : int
    }

  type event =
    | Del
    | Suppr
    | Char of char
    | Left
    | Right

  let update ({ s; cursor } as t) (e : event) : t =
    let len = String.length s in
    match e with
    | Del ->
      if cursor = 0
      then t
      else (
        let deleted = cursor - 1 in
        let left = String.sub s 0 deleted in
        let right_from = cursor in
        if right_from = len
        then { s = left; cursor = cursor - 1 }
        else (
          let right = String.sub s right_from (len - right_from) in
          { s = left ^ right; cursor = cursor - 1 }))
    | Suppr ->
      if cursor = len
      then t
      else (
        let right =
          if cursor >= len - 1 then "" else String.sub s (cursor + 1) (len - cursor - 1)
        in
        { s = String.sub s 0 cursor ^ right; cursor })
    | Char c ->
      let left = String.sub s 0 cursor
      and right = if len = cursor then "" else String.sub s cursor (len - cursor) in
      { s = Printf.sprintf "%s%c%s" left c right; cursor = cursor + 1 }
    | Left -> { t with cursor = max 0 (cursor - 1) }
    | Right -> { t with cursor = min len (cursor + 1) }
  ;;

  let append_char c t = update t (Char c)
  let del t = update t Del
  let suppr t = update t Suppr
  let left t = update t Left
  let right t = update t Right
  let empty = { s = ""; cursor = 0 }
  let init s = { s; cursor = String.length s }
  let to_string t = t.s
  let edition_index t = t.cursor

  let around_cursor ~width ~cursor ~len =
    if cursor = len
    then cursor - (width - 1), cursor
    else if width = 1
    then cursor, cursor
    else (
      let left = ref cursor
      and right = ref (cursor + 1)
      and available = ref (width - 1) in
      let updated = ref true in
      while !available > 0 && !updated do
        updated := false;
        if !left > 0
        then (
          left := !left - 1;
          available := !available - 1;
          updated := true);
        if !available > 0 && !right < len
        then (
          right := !right + 1;
          available := !available - 1;
          updated := true)
      done;
      !left, !right)
  ;;

  let component { s; cursor } Constraints.{ col_start; width; row_start; height } =
    if height <= 0 || width <= 0
    then [], Space.{ row_start; col_start; width = 0; height = 0 }
    else (
      let len = String.length s in
      if len <= width
      then
        ( [ Tty.{ row = row_start; col = col_start }, Tty.text s
          ; Tty.{ row = row_start; col = col_start + cursor }, Tty.Cursor
          ]
        , { row_start; col_start; height = 1; width = max len (cursor + 1) } )
      else (
        let left, right = around_cursor ~width ~cursor ~len in
        let size = right - left in
        let cropped = String.sub s left size in
        ( [ Tty.{ row = row_start; col = col_start }, Tty.text @@ cropped
          ; Tty.{ row = row_start; col = col_start + cursor - left }, Tty.Cursor
          ]
        , { row_start; col_start; height = 1; width } )))
  ;;
end

module Column_sliding (M : Merge_views) = struct
  module Column = Column_divided (M)

  let slice ~first ~last arr =
    if first < 0 || first > last || last >= Array.length arr
    then [||]
    else (
      let res = Array.make (last - first + 1) arr.(first) in
      for i = first to last do
        res.(i - first) <- arr.(i)
      done;
      res)
  ;;

  let first_last cursor len available =
    let first = max 0 (cursor - (available / 2)) in
    let last = min (len - 1) (first + available - 1) in
    let diff = available - (last - first + 1) in
    max 0 (first - diff), last
  ;;

  let component
    ?(height_per_entry = 1)
    (entry_component : int -> 'a -> M.view component)
    (entries : 'a array)
    cursor
    (constraints : Constraints.t)
    =
    let components = Array.mapi entry_component entries in
    let count = constraints.height / height_per_entry in
    let first, last = first_last cursor (Array.length entries) count in
    let evenly_divided_entries =
      slice ~first ~last components
      |> Array.map (fun cmp -> cmp, 1)
      |> Array.to_list
      |> Column.component
    in
    evenly_divided_entries constraints
  ;;
end
