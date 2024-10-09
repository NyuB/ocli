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

let string_view style row col_start available { s; cursor } : Tty.ansi_view_item =
  let len = String.length s in
  let pos = Tty.{ row; col = col_start } in
  if len <= available
  then pos, style, Tty.text s
  else (
    let left = available / 2 in
    let left_start = cursor - left in
    pos, style, Tty.text @@ String.sub s left_start available)
;;

let cursor_view style row col_start available { s; cursor } : Tty.ansi_view_item =
  let len = String.length s in
  if len <= available
  then Tty.{ row; col = col_start + cursor }, style, Cursor
  else (
    let left = available / 2 in
    Tty.{ row; col = col_start + left }, style, Cursor)
;;

let view style row col_start available t : Tty.ansi_view_item list =
  [ string_view style row col_start available t
  ; cursor_view style row col_start available t
  ]
;;
