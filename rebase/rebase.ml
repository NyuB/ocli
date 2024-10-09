(** Application logic of a custom rebase editor *)
open Qol

type fixup_kind =
  | Discard_message
  | Keep_message

type rebase_command =
  | Pick
  | Edit
  | Fixup of fixup_kind
  | Squash
  | Reword
  | Exec
  | Break
  | Drop
  | Label of string
  | Reset of string
  | Merge
  | Update of string

type custom_command =
  | Rename of string
  (** Replace the message of a commit. Differs from git [Reword] in that it will be applied without further user actions. *)
  | Explode
  (** Split a single commit into one commit by modified file in the original commit *)
  | Nothing

type rebase_entry =
  { command : rebase_command
  ; sha1 : string
  ; message : string
  ; custom : custom_command
  }

let is_fixup = function
  | Fixup _ -> true
  | _ -> false
;;

let string_of_rebase_command = function
  | Pick -> "pick"
  | Edit -> "edit"
  | Fixup Discard_message -> "fixup"
  | Fixup Keep_message -> "fixup -C"
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

let message_of_rebase_entry entry =
  match entry.custom with
  | Rename n -> n
  | Nothing | Explode -> entry.message
;;

let custom_info entry =
  match entry.custom with
  | Rename _ -> "(renamed)"
  | Explode -> "(explode)"
  | _ -> ""
;;

let string_of_rebase_entry ({ command; sha1; _ } as entry) =
  Printf.sprintf
    "%s: %s '%s'%s"
    (string_of_rebase_command command)
    sha1
    (message_of_rebase_entry entry)
    (custom_info entry)
;;

let git_todo_of_exploded_entry modified_files base sha1 =
  let modified = modified_files sha1 in
  if List.is_empty modified
  then [ base ]
  else (
    let exec_each =
      List.concat_map
        (fun f ->
          [ Printf.sprintf "git add %s" f
          ; Printf.sprintf "git commit -m '(Exploded) %s'" f
          ])
        modified
    in
    let exec =
      Printf.sprintf "exec %s" (String.concat " && " ("git reset HEAD~" :: exec_each))
    in
    [ base; exec ])
;;

(** [git_todo_of_rebase_entry modified_files entry] returns the git rebase command line to execute to apply [entry], in git execution order. These lines are meant to be written to the rebase file handled to git to proceed with the rebase *)
let git_todo_of_rebase_entry
  (modified_files : string -> string list)
  { command; sha1; message; custom }
  : string list
  =
  let base = Printf.sprintf "%s %s %s" (string_of_rebase_command command) sha1 message in
  match custom with
  | Rename new_name ->
    let exec_rename = Printf.sprintf "exec git commit --amend -m '%s'" new_name in
    [ base; exec_rename ]
  | Explode -> git_todo_of_exploded_entry modified_files base sha1
  | Nothing -> [ base ]
;;

(** [git_todo_of_rebase_entries modified_files entries] returns the git rebase command line to execute to apply [entries], in git execution order. These lines are meant to be written to the rebase file handled to git to proceed with the rebase.

    Note that it will not necessarily return 1 line for 1 entry, since a single entry can be translated to multiple git commands, such as intermediary {b exec}s. *)
let git_todo_of_rebase_entries
  (modified_files : string -> string list)
  (entries : rebase_entry list)
  : string list
  =
  List.concat_map (git_todo_of_rebase_entry modified_files) entries
;;

let parse_entry (line : string) : rebase_entry option =
  let line = String.trim line in
  let parts = String.split_on_char ' ' line in
  if List.length parts >= 3 && String.equal (List.hd parts) "pick"
  then
    Some
      { command = Pick
      ; sha1 = List.nth parts 1
      ; message = String.concat " " (List.sublist 2 parts)
      ; custom = Nothing
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

(** External information about git status that should be provided by the platform *)
module type Rebase_info_external = sig
  (** The content of the initial rebase file *)
  val entries : rebase_entry list

  (** [modified_files sha1] should return all the files modified by commit with ref [sha1] *)
  val modified_files : string -> string list
end

(** Commands required by [App] *)
type rebase_app_command =
  | Exit_with of string list
  (** [Exit_with lines] Signals the end of the rebase entries processing, platform should handle back control to git after writing [lines] to the rebase file *)

module App (Info : Rebase_info_external) :
  Tty.Ansi_App with type command = rebase_app_command = struct
  include Tty.Ansi_Tea_Base

  type command = rebase_app_command

  type mode =
    | Navigate (** Navigating between rebase entries *)
    | Move (** Moving a single rebase entry up & down *)
    | Rename of string
    (** [Rename new_msg] represents an ongoing renaming with message [new_msg] a given rebase entry, differs from [Reword] in that it will actually rename the commit without requiring further user action. *)
    | Cli of string

  type model =
    { entries : rebase_entry array
    ; cursor : int (** The current selected entry index within [entries] *)
    ; mode : mode (** Crurrent [mode] *)
    ; dimensions : Tty.position (** Current dimensions of the display *)
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
    let prefix = if is_fixup e.command then "   " else "" in
    let repr =
      match model.mode with
      | Navigate | Cli _ -> prefix ^ string_of_rebase_entry e
      | Move when model.cursor <> i -> prefix ^ string_of_rebase_entry e
      | Rename _ when model.cursor <> i -> prefix ^ string_of_rebase_entry e
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
  let cli_line_count = 2

  let cli_view model =
    let style = Tty.Default_style.default_style in
    let content_line =
      match model.mode with
      | Cli s -> s
      | _ -> ""
    in
    Tty.
      [ ( { row = min model.dimensions.row (entry_count model + 2); col = 1 }
        , style
        , content_line )
      ]
  ;;

  let start_dest model =
    let available = model.dimensions.row - cli_line_count in
    let full = entry_count model in
    if full <= available
    then 0, full - 1
    else (
      let before = available / 2 in
      let start = max 0 (model.cursor - before) in
      let dest = min (entry_count model - 1) (start + available - 1) in
      start, dest)
  ;;

  let slice start dest arr = Array.init (dest - start + 1) (fun i -> arr.(i + start))

  let crop_to_size max_size s =
    if String.length s <= max_size
    then s
    else if max_size >= 3
    then String.sub s 0 (max_size - 3) ^ "..."
    else String.sub s 0 max_size
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
    |> List.at_most (model.dimensions.row - cli_line_count)
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
    left_panel_view model @ right_panel_view model @ cli_view model
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
    is_fixup (entry_at_cursor model cursor).command
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

  let set_fixup model fixup_kind =
    if model.cursor = 0 then model else set_rebase_command model (Fixup fixup_kind)
  ;;

  let set_name ({ cursor; entries; _ } as model) name =
    let current = entries.(cursor) in
    let copy = Array.copy entries in
    copy.(cursor) <- { current with custom = Rename name };
    { model with entries = copy; mode = Navigate }
  ;;

  let switch_explode ({ cursor; entries; _ } as model) =
    let current = entries.(cursor) in
    let copy = Array.copy entries in
    copy.(cursor)
    <- { current with custom = (if current.custom = Explode then Nothing else Explode) };
    { model with entries = copy; mode = Navigate }
  ;;

  let del_last_char s =
    if String.length s = 0 then s else String.sub s 0 (String.length s - 1)
  ;;

  let append_char s c = Printf.sprintf "%s%c" s c

  let exit_with model =
    ( model
    , [ Exit_with
          (Array.to_list model.entries |> git_todo_of_rebase_entries Info.modified_files)
      ] )
  ;;

  let switch_mode model mode = { model with mode }
  let renaming_with model s = { model with mode = Rename s }

  let current_message model =
    match current_entry model with
    | { custom = Rename s; _ } -> s
    | { message; _ } -> message
  ;;

  let update model (event : Tty.ansi_event) =
    match model.mode, event with
    | Navigate, Up -> { model with cursor = max 0 (model.cursor - 1) }, []
    | Navigate, Down ->
      { model with cursor = min (Array.length model.entries - 1) (model.cursor + 1) }, []
    | Navigate, Right -> switch_mode model Move, []
    | Move, Up -> move_up model, []
    | Move, Down -> move_down model, []
    | Move, Left -> switch_mode model Navigate, []
    | Rename name, Enter -> set_name model name, []
    | Rename s, Char c -> renaming_with model (append_char s c), []
    | Rename s, Del -> renaming_with model (del_last_char s), []
    | Cli s, Char c -> { model with mode = Cli (append_char s c) }, []
    | Cli s, Del -> { model with mode = Cli (del_last_char s) }, []
    | Cli ":q", Enter -> exit_with model
    | Cli _, Enter -> switch_mode model Navigate, []
    | [%cross_match (Navigate, Move), Char ':'] -> switch_mode model (Cli ":"), []
    | [%cross_match (Rename [%cross_any], Cli [%cross_any]), Esc] ->
      switch_mode model Navigate, []
    | [%cross_match (Navigate, Move), (Char 'd', Char 'D', Del)] ->
      set_rebase_command model Drop, []
    | [%cross_match (Navigate, Move), Char 'f'] -> set_fixup model Discard_message, []
    | [%cross_match (Navigate, Move), Char 'F'] -> set_fixup model Keep_message, []
    | [%cross_match (Navigate, Move), (Char 'p', Char 'P')] ->
      set_rebase_command model Pick, []
    | [%cross_match (Navigate, Move), Char 'r'] ->
      renaming_with model (current_message model), []
    | [%cross_match (Navigate, Move), Char 'R'] -> renaming_with model "", []
    | [%cross_match (Navigate, Move), (Char 'x', Char 'X')] -> switch_explode model, []
    | _, Size dimensions -> { model with dimensions }, []
    | _ -> model, []
  ;;
end
