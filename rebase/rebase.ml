(** Application logic of a custom rebase editor *)
open Qol

module Column = Components.Column (Components.Merge_ansi_views)
module Row = Components.Row (Components.Merge_ansi_views)
module Row_divided = Components.Row_divided (Components.Merge_ansi_views)
module Editing_line = Components.Editing_line

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

let git_todo_base { command; sha1; message; _ } =
  Printf.sprintf "%s %s %s" (string_of_rebase_command command) sha1 message
;;

let git_todo_of_exploded_entry modified_files ({ sha1; message; _ } as entry) =
  let modified = modified_files sha1 in
  if List.is_empty modified
  then [ git_todo_base entry ]
  else (
    let exec_each =
      List.concat_map
        (fun f ->
          [ Printf.sprintf "git add %s" f
          ; Printf.sprintf "git commit -m '%s (Exploded from '%s')'" f message
          ])
        modified
    in
    let exec =
      Printf.sprintf "exec %s" (String.concat " && " ("git reset HEAD~" :: exec_each))
    in
    [ git_todo_base entry; exec ])
;;

(** [git_todo_of_rebase_entry modified_files entry] returns the git rebase command line to execute to apply [entry], in git execution order. These lines are meant to be written to the rebase file handled to git to proceed with the rebase *)
let git_todo_of_rebase_entry
  (modified_files : string -> string list)
  ({ custom; _ } as entry)
  : string list
  =
  match custom with
  | Rename new_name ->
    let exec_rename = Printf.sprintf "exec git commit --amend -m '%s'" new_name in
    [ git_todo_base entry; exec_rename ]
  | Explode -> git_todo_of_exploded_entry modified_files entry
  | Nothing -> [ git_todo_base entry ]
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
  let concat = String.concat " "
  and custom = Nothing in
  match parts with
  | "pick" :: sha1 :: rest -> Some { command = Pick; sha1; custom; message = concat rest }
  | "edit" :: sha1 :: rest -> Some { command = Edit; sha1; custom; message = concat rest }
  | "reword" :: sha1 :: rest ->
    Some { command = Reword; sha1; custom; message = concat rest }
  | "squash" :: sha1 :: rest ->
    Some { command = Squash; sha1; custom; message = concat rest }
  | "drop" :: sha1 :: rest -> Some { command = Drop; sha1; custom; message = concat rest }
  | "fixup" :: "-C" :: sha1 :: rest | "fixup" :: "-c" :: sha1 :: rest ->
    Some { command = Fixup Keep_message; sha1; custom; message = concat rest }
  | "fixup" :: sha1 :: rest ->
    Some { command = Fixup Discard_message; sha1; custom; message = concat rest }
  | "exec" :: cmd -> Some { command = Exec; sha1 = ""; custom; message = concat cmd }
  | _ -> None
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
    | Cli of Editing_line.t

  type symbols =
    { up_arrow_prefix : string
    ; down_arrow_prefix : string
    ; up_and_down_arrow_prefix : string
    ; fixup_prefix : string
    ; panel_separator : string
    ; panel_bot_left_corner : string
    }

  type model =
    { entries : rebase_entry array
    ; cursor : int (** The current selected entry index within [entries] *)
    ; mode : mode (** Crurrent [mode] *)
    ; dimensions : Tty.position (** Current dimensions of the display *)
    ; symbols : symbols (** Special symbol representation *)
    }

  let pretty_symbols : symbols =
    { up_arrow_prefix = "▲  "
    ; down_arrow_prefix = " ▼ "
    ; up_and_down_arrow_prefix = "▲▼ "
    ; fixup_prefix = " ∟ "
    ; panel_separator = " │ "
    ; panel_bot_left_corner = " └ "
    }
  ;;

  let raw_symbols : symbols =
    { up_arrow_prefix = "^  "
    ; down_arrow_prefix = " v "
    ; up_and_down_arrow_prefix = "^v "
    ; fixup_prefix = " |_"
    ; panel_separator = " | "
    ; panel_bot_left_corner = " |_"
    }
  ;;

  let init =
    { entries = Array.of_list Info.entries
    ; cursor = 0
    ; mode = Navigate
    ; dimensions = { row = 25; col = 80 }
    ; symbols = pretty_symbols
    }
  ;;

  let string_of_renaming_entry { command; sha1; _ } (rename : string) : string =
    Printf.sprintf "%s: %s '%s'(renaming)" (string_of_rebase_command command) sha1 rename
  ;;

  let entry_count model = Array.length model.entries

  let move_prefix model =
    let is_first = model.cursor = 0
    and is_last = model.cursor = entry_count model - 1 in
    let symbols = model.symbols in
    if is_first && is_last
    then "   "
    else if is_first
    then symbols.down_arrow_prefix
    else if is_last
    then symbols.up_arrow_prefix
    else symbols.up_and_down_arrow_prefix
  ;;

  let fixup_prefix model = model.symbols.fixup_prefix

  let highlight_entry (i : int) (e : rebase_entry) (model : model) : Tty.style * string =
    let base_style =
      { Tty.Default_style.default_style with striked = e.command = Drop }
    in
    let style =
      if i = model.cursor
      then { base_style with bg_color = Some Tty.Cyan }
      else base_style
    in
    let prefix = if is_fixup e.command then fixup_prefix model else "" in
    let repr =
      match model.mode with
      | Navigate | Cli _ -> prefix ^ string_of_rebase_entry e
      | Move when model.cursor <> i -> prefix ^ string_of_rebase_entry e
      | Rename _ when model.cursor <> i -> prefix ^ string_of_rebase_entry e
      | Move -> move_prefix model ^ string_of_rebase_entry e
      | Rename s -> string_of_renaming_entry e s
    in
    style, repr
  ;;

  let current_entry model = model.entries.(model.cursor)
  let current_sha1 model = (current_entry model).sha1
  let cli_line_count = 2

  let cli_view model : Tty.ansi_view_item list Components.component =
    let style = Tty.Default_style.default_style in
    match model.mode with
    | Cli s -> Editing_line.make s |> Components.positioned_to_ansi_view_component style
    | _ -> Components.Text_line.component "" |> Components.to_ansi_view_component style
  ;;

  let cli_separator =
    Components.Text_line.component ""
    |> Components.to_ansi_view_component Tty.Default_style.default_style
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

  let panel_separator model =
    let files = Info.modified_files (current_sha1 model) in
    let files_count = List.length files in
    let symbols = model.symbols in
    if files_count = 0
    then Column.make []
    else
      List.init files_count (fun _ -> symbols.panel_separator)
      @ [ symbols.panel_bot_left_corner ]
      |> List.map (fun l ->
        Components.Text_line.component l
        |> Components.to_ansi_view_component Tty.Default_style.default_style)
      |> Column.make
  ;;

  let right_panel_view model =
    Info.modified_files (current_sha1 model)
    |> List.map Components.Text_line.component
    |> List.map (Components.to_ansi_view_component Tty.Default_style.default_style)
    |> Column.make
  ;;

  let left_panel_view model =
    let start, dest = start_dest model in
    Array.mapi
      (fun i e ->
        let style, repr = highlight_entry i e model in
        Components.Text_line.component repr |> Components.to_ansi_view_component style)
      model.entries
    |> slice start dest
    |> Array.to_list
    |> Column.make
  ;;

  let view model : Tty.ansi_view_item list =
    let constraints =
      Components.
        { col_start = 1
        ; row_start = 1
        ; width = model.dimensions.col
        ; height = model.dimensions.row - cli_line_count
        }
    in
    let left_right_panel =
      Row_divided.make
        [ left_panel_view model, 6; panel_separator model, 1; right_panel_view model, 2 ]
    in
    let full_screen = Column.make [ left_right_panel; cli_separator; cli_view model ] in
    let v, _ = full_screen constraints in
    v
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

  let switch_mode mode model = { model with mode }
  let renaming_with model s = { model with mode = Rename s }

  let current_message model =
    match current_entry model with
    | { custom = Rename s; _ } -> s
    | { message; _ } -> message
  ;;

  let inline model =
    let lines =
      git_todo_of_rebase_entries Info.modified_files (Array.to_list model.entries)
    in
    let new_entries = parse_entries lines |> Array.of_list in
    { model with mode = Navigate; entries = new_entries }
  ;;

  let update_cli_command cmd model =
    match cmd with
    | ":q" -> exit_with model
    | ":abort" -> exit_with init
    | ":inline" -> inline model, []
    | ":pretty" -> { model with symbols = pretty_symbols } |> switch_mode Navigate, []
    | ":raw" -> { model with symbols = raw_symbols } |> switch_mode Navigate, []
    | _ -> switch_mode Navigate model, []
  ;;

  let update model (event : Tty.ansi_event) =
    match model.mode, event with
    | Navigate, Up -> { model with cursor = max 0 (model.cursor - 1) }, []
    | Navigate, Down ->
      { model with cursor = min (Array.length model.entries - 1) (model.cursor + 1) }, []
    | Navigate, Right -> switch_mode Move model, []
    | Move, Up -> move_up model, []
    | Move, Down -> move_down model, []
    | Move, Left -> switch_mode Navigate model, []
    | Rename name, Enter -> set_name model name, []
    | Rename s, Char c -> renaming_with model (append_char s c), []
    | Rename s, Del -> renaming_with model (del_last_char s), []
    | Cli s, Char c -> { model with mode = Cli (Editing_line.append_char c s) }, []
    | Cli s, Del -> { model with mode = Cli (Editing_line.del s) }, []
    | Cli s, Left -> { model with mode = Cli (Editing_line.left s) }, []
    | Cli s, Right -> { model with mode = Cli (Editing_line.right s) }, []
    | Cli s, Enter -> update_cli_command (Editing_line.to_string s) model
    | [%cross_match (Navigate, Move), Char ':'] ->
      switch_mode (Cli (Editing_line.init ":")) model, []
    | [%cross_match (Rename [%cross_any], Cli [%cross_any]), Esc] ->
      switch_mode Navigate model, []
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
