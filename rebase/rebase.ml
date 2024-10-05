(** Application logic of a custom rebase editor *)

type rebase_command =
  | Pick
  | Edit
  | Fixup
  | Squash
  | Reword
  | Exec
  | Break
  | Drop
  | Label of string
  | Reset of string
  | Merge
  | Update of string

let string_of_rebase_command = function
  | Pick -> "pick"
  | Edit -> "edit"
  | Fixup -> "fixup"
  | Squash -> "squash"
  | Reword -> "reword"
  | Exec -> "exec"
  | Break -> "break"
  | Drop -> "drop"
  | Label label -> Printf.sprintf "label <%s>" label
  | Reset label -> Printf.sprintf "reset <%s>" label
  | Merge -> "merge"
  | Update git_ref -> Printf.sprintf "update <%s>" git_ref
;;

type rebase_entry =
  { command : rebase_command
  ; sha1 : string
  ; message : string
  ; renamed : bool
  }

let string_of_rebase_entry { command; sha1; message; renamed } =
  Printf.sprintf
    "%s: %s '%s'%s"
    (string_of_rebase_command command)
    sha1
    message
    (if renamed then "(renamed)" else "")
;;

let git_todo_of_rebase_entry { command; sha1; message; renamed } : string list =
  let base = Printf.sprintf "%s %s %s" (string_of_rebase_command command) sha1 message
  and exec_rename = Printf.sprintf "exec git commit --amend -m '%s'" message in
  if renamed then [ base; exec_rename ] else [ base ]
;;

let git_todo_of_rebase_entries (entries : rebase_entry list) : string list =
  List.concat_map git_todo_of_rebase_entry entries
;;

let rec sublist n l =
  match n, l with
  | over, res when over <= 0 -> res
  | _, [] -> []
  | pos, _ :: t -> sublist (pos - 1) t
;;

let parse_entry (line : string) : rebase_entry option =
  let line = String.trim line in
  let parts = String.split_on_char ' ' line in
  if List.length parts >= 3 && String.equal (List.hd parts) "pick"
  then
    Some
      { command = Pick
      ; sha1 = List.nth parts 1
      ; message = String.concat " " (sublist 2 parts)
      ; renamed = false
      }
  else None
;;

let parse_entries lines = List.filter_map parse_entry lines

let parse_rebase_file f =
  let ic = open_in f in
  let rec aux acc =
    try
      let l = input_line ic in
      aux (l :: acc)
    with
    | End_of_file -> List.rev acc
  in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> aux [] |> parse_entries)
;;

module type Rebase_info_external = sig
  val entries : rebase_entry list
  val modified_files : string -> string list
end

type rebase_app_command = Exit_with of rebase_entry list

module App (Info : Rebase_info_external) :
  Tty.Ansi_App with type command = rebase_app_command = struct
  include Tty.Ansi_Tea_Base

  type command = rebase_app_command

  type mode =
    | Navigate
    | Move
    | Rename of string

  type model =
    { entries : rebase_entry array
    ; cursor : int
    ; mode : mode
    ; dimensions : Tty.position
    }

  let init =
    { entries = Array.of_list Info.entries
    ; cursor = 0
    ; mode = Navigate
    ; dimensions = { row = 25; col = 80 }
    }
  ;;

  let string_of_renaming_entry { command; sha1; _ } rename =
    Printf.sprintf "%s: %s '%s'(renaming)" (string_of_rebase_command command) sha1 rename
  ;;

  let highlight_entry i e model =
    let base_style =
      { Tty.Default_style.default_style with striked = e.command = Drop }
    in
    let style =
      if i = model.cursor
      then { base_style with bg_color = Some Tty.Cyan }
      else base_style
    in
    let repr =
      match model.mode with
      | Navigate ->
        let prefix = if e.command = Fixup then "   " else "" in
        prefix ^ string_of_rebase_entry e
      | Move when model.cursor <> i -> string_of_rebase_entry e
      | Rename _ when model.cursor <> i -> string_of_rebase_entry e
      | Move -> "^v " ^ string_of_rebase_entry e
      | Rename s -> string_of_renaming_entry e s
    in
    style, repr
  ;;

  let max_or_zero ~by l = Array.fold_left (fun acc item -> max acc (by item)) 0 l
  let current_entry model = model.entries.(model.cursor)
  let current_sha1 model = (current_entry model).sha1

  let max_entry_full_length ({ entries; _ } as model) =
    Array.mapi (fun i e -> highlight_entry i e model) entries
    |> max_or_zero ~by:(fun (_, s) -> String.length s)
  ;;

  let panels_widths model =
    (* 2/3 for left panel and 1/3 for right panel *)
    let left = min (model.dimensions.col * 2 / 3) (max_entry_full_length model) in
    let right = model.dimensions.col - left in
    left, right
  ;;

  let entry_count model = Array.length model.entries

  let start_dest model =
    let before = model.dimensions.row / 2 in
    let start = max 0 (model.cursor - before) in
    let dest = min (entry_count model - 1) (start + model.dimensions.row - 1) in
    start, dest
  ;;

  let slice start dest arr = Array.init (dest - start + 1) (fun i -> arr.(i + start))

  let crop_to_size max_size s =
    if String.length s <= max_size
    then s
    else if max_size >= 3
    then String.sub s 0 (max_size - 3) ^ "..."
    else String.sub s 0 max_size
  ;;

  let at_most n l =
    let rec aux acc n = function
      | [] -> List.rev acc
      | h :: t -> if n > 0 then aux (h :: acc) (n - 1) t else List.rev acc
    in
    aux [] n l
  ;;

  let right_panel_view model =
    let left_width, max_width = panels_widths model in
    let files = Info.modified_files (current_sha1 model) in
    List.mapi
      (fun i f ->
        ( Tty.{ row = i + 1; col = left_width + 1 }
        , Tty.Default_style.default_style
        , crop_to_size max_width (" | " ^ f) ))
      files
    |> at_most model.dimensions.row
  ;;

  let mapped_i_inplace f arr =
    Array.mapi_inplace f arr;
    arr
  ;;

  let left_panel_view model =
    let max_width, _ = panels_widths model
    and start, dest = start_dest model in
    Array.mapi
      (fun i e ->
        let style, repr = highlight_entry i e model in
        Tty.{ row = i + 1; col = 1 }, style, crop_to_size max_width repr)
      model.entries
    |> slice start dest
    |> mapped_i_inplace (fun i (pos, style, repr) ->
      Tty.{ pos with row = i + 1 }, style, repr)
    |> Array.to_list
  ;;

  let view model : Tty.ansi_view_item list =
    left_panel_view model @ right_panel_view model
  ;;

  let swap arr a b =
    let copy = Array.copy arr in
    let a_copy = arr.(a) in
    copy.(a) <- arr.(b);
    copy.(b) <- a_copy;
    copy
  ;;

  let swap_entries model ~source_cursor ~target_cursor =
    { model with
      cursor = target_cursor
    ; entries = swap model.entries source_cursor target_cursor
    }
  ;;

  let entry_at_cursor model cursor = model.entries.(cursor)

  let entry_at_cursor_is_fixup model cursor =
    (entry_at_cursor model cursor).command = Fixup
  ;;

  let move_up ({ cursor; _ } as model) =
    if cursor <= 0 || (cursor = 1 && entry_at_cursor_is_fixup model cursor)
    then model
    else swap_entries model ~source_cursor:cursor ~target_cursor:(cursor - 1)
  ;;

  let move_down ({ cursor; entries; _ } as model) =
    if cursor >= Array.length entries - 1
       || (cursor = 0 && entry_at_cursor_is_fixup model (cursor + 1))
    then model
    else swap_entries model ~source_cursor:cursor ~target_cursor:(cursor + 1)
  ;;

  let set_rebase_command ({ cursor; entries; _ } as model) cmd =
    let current = entries.(cursor) in
    let copy = Array.copy entries in
    copy.(cursor) <- { current with command = cmd };
    { model with entries = copy }
  ;;

  let set_fixup model = if model.cursor = 0 then model else set_rebase_command model Fixup

  let set_name ({ cursor; entries; _ } as model) name =
    let current = entries.(cursor) in
    let copy = Array.copy entries in
    copy.(cursor) <- { current with message = name; renamed = true };
    { model with entries = copy; mode = Navigate }
  ;;

  let del_rename s =
    if String.length s = 0 then s else String.sub s 0 (String.length s - 1)
  ;;

  let update model (event : Tty.ansi_event) =
    match model.mode, event with
    | Navigate, Up -> { model with cursor = max 0 (model.cursor - 1) }, []
    | Navigate, Down ->
      { model with cursor = min (Array.length model.entries - 1) (model.cursor + 1) }, []
    | Navigate, Right -> { model with mode = Move }, []
    | Navigate, Char 'd' | Navigate, Char 'D' | Move, Del ->
      set_rebase_command model Drop, []
    | Navigate, Char 'f' | Navigate, Char 'F' -> set_fixup model, []
    | Navigate, Char 'p' | Navigate, Char 'P' -> set_rebase_command model Pick, []
    | Move, Up -> move_up model, []
    | Move, Down -> move_down model, []
    | Move, Left -> { model with mode = Navigate }, []
    | Move, Right -> { model with mode = Rename "" }, []
    | Move, Char 'd' | Move, Char 'D' | Navigate, Del -> set_rebase_command model Drop, []
    | Move, Char 'f' | Move, Char 'F' -> set_fixup model, []
    | Move, Char 'p' | Move, Char 'P' -> set_rebase_command model Pick, []
    | Rename name, Enter -> set_name model name, []
    | Rename _, Left -> { model with mode = Navigate }, []
    | Rename s, Char c -> { model with mode = Rename (Printf.sprintf "%s%c" s c) }, []
    | Rename s, Del -> { model with mode = Rename (del_rename s) }, []
    | _, Esc -> model, [ Exit_with (Array.to_list model.entries) ]
    | _, Size dimensions -> { model with dimensions }, []
    | _ -> model, []
  ;;
end
