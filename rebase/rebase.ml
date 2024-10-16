(** Application logic of a custom rebase editor *)
open Qol

module Column = Components.Column (Components.Merge_ansi_views)
module Column_divided = Components.Column_divided (Components.Merge_ansi_views)
module Column_sliding = Components.Column_sliding (Components.Merge_ansi_views)
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

  module Appearance = struct
    type symbols =
      { up_arrow_prefix : string
      ; down_arrow_prefix : string
      ; up_and_down_arrow_prefix : string
      ; fixup_prefix : string
      ; panel_separator : string
      ; panel_bot_left_corner : string
      }

    type t =
      { symbols : symbols
      ; selection_color : Tty.color
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

    let with_selection_color selection_color t = { t with selection_color }
  end

  type mode =
    | Navigate (** Navigating between rebase entries *)
    | Navigate_files of int (** Navigating between modified files *)
    | Move (** Moving a single rebase entry up & down *)
    | Rename of Editing_line.t
    (** [Rename new_msg] represents an ongoing renaming with message [new_msg] a given rebase entry, differs from [Reword] in that it will actually rename the commit without requiring further user action. *)
    | Cli of Editing_line.t (** [Cli s] represents the ongoing typing of a command *)

  module Model : sig
    type t =
      { entries : rebase_entry array
      ; cursor : int
      ; mode : mode
      ; dimensions : Tty.position
      ; appearance : Appearance.t
      }

    val entry_count : t -> int
    val is_navigate_files : t -> bool
    val current_sha1 : t -> string
    val move_up : t -> t
    val move_down : t -> t
    val set_fixup : t -> fixup_kind -> t
    val set_drop : t -> t
    val set_pick : t -> t
    val set_rename : t -> string -> t
    val switch_explode : t -> t
    val switch_mode : mode -> t -> t
    val with_appearance : Appearance.t -> t -> t
    val renaming : t -> Editing_line.t -> t
    val current_message : t -> string
  end = struct
    type t =
      { entries : rebase_entry array
      ; cursor : int (** The current selected entry index within [entries] *)
      ; mode : mode (** Crurrent [mode] *)
      ; dimensions : Tty.position (** Current dimensions of the display *)
      ; appearance : Appearance.t (** Appearance config *)
      }

    let entry_count model = Array.length model.entries

    let is_navigate_files model =
      match model.mode with
      | Navigate_files _ -> true
      | _ -> false
    ;;

    let current_entry model = model.entries.(model.cursor)
    let current_sha1 model = (current_entry model).sha1

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

    let set_drop model = set_rebase_command model Drop
    let set_pick model = set_rebase_command model Drop

    let set_rename ({ cursor; entries; _ } as model) (name : string) =
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

    let switch_mode mode model = { model with mode }
    let with_appearance appearance model = { model with appearance }
    let renaming model s = switch_mode (Rename s) model

    let current_message model =
      match current_entry model with
      | { custom = Rename s; _ } -> s
      | { message; _ } -> message
    ;;
  end

  type model = Model.t

  let modified_count model = List.length (Info.modified_files (Model.current_sha1 model))

  module View : sig
    val view : model -> Tty.ansi_view_item list
  end = struct
    let move_prefix (model : model) =
      let is_first = model.cursor = 0
      and is_last = model.cursor = Model.entry_count model - 1 in
      let symbols = model.appearance.symbols in
      if is_first && is_last
      then "   "
      else if is_first
      then symbols.down_arrow_prefix
      else if is_last
      then symbols.up_arrow_prefix
      else symbols.up_and_down_arrow_prefix
    ;;

    let fixup_prefix (model : model) = model.appearance.symbols.fixup_prefix

    let renaming_entry_component { command; sha1; _ } editing =
      let style = Tty.Default_style.default_style in
      let left =
        Printf.sprintf "%s: %s '" (string_of_rebase_command command) sha1
        |> Components.Text_line.component
        |> Components.to_ansi_view_component style
      and right =
        "'(renaming)"
        |> Components.Text_line.component
        |> Components.to_ansi_view_component style
      and editing_component =
        Editing_line.component editing
        |> Components.positioned_to_ansi_view_component Tty.Default_style.default_style
      in
      Row.component [ left; editing_component; right ]
    ;;

    let rebase_entry_component (model : model) (i : int) (e : rebase_entry)
      : Tty.ansi_view_item list Components.component
      =
      let base_style =
        { Tty.Default_style.default_style with striked = e.command = Drop }
      in
      let style =
        if i = model.cursor
        then { base_style with bg_color = Some model.appearance.selection_color }
        else base_style
      in
      let prefix = if is_fixup e.command then fixup_prefix model else "" in
      match model.mode with
      | Navigate | Cli _ | Navigate_files _ ->
        Components.Text_line.component (prefix ^ string_of_rebase_entry e)
        |> Components.to_ansi_view_component style
      | Move when model.cursor <> i ->
        Components.Text_line.component (prefix ^ string_of_rebase_entry e)
        |> Components.to_ansi_view_component style
      | Rename _ when model.cursor <> i ->
        Components.Text_line.component (prefix ^ string_of_rebase_entry e)
        |> Components.to_ansi_view_component style
      | Move ->
        Components.Text_line.component (move_prefix model ^ string_of_rebase_entry e)
        |> Components.to_ansi_view_component style
      | Rename s -> renaming_entry_component e s
    ;;

    let cli_view (model : model) : Tty.ansi_view_item list Components.component =
      let style = Tty.Default_style.default_style in
      match model.mode with
      | Cli s ->
        Editing_line.component s |> Components.positioned_to_ansi_view_component style
      | _ -> Components.Text_line.component "" |> Components.to_ansi_view_component style
    ;;

    let cli_separator =
      Components.Text_line.component ""
      |> Components.to_ansi_view_component Tty.Default_style.default_style
    ;;

    let panel_separator (model : model) =
      let files_count = modified_count model in
      let symbols = model.appearance.symbols in
      if files_count = 0
      then Column.component []
      else
        List.init files_count (fun _ -> symbols.panel_separator)
        @ [ symbols.panel_bot_left_corner ]
        |> List.map (fun l ->
          Components.Text_line.component l
          |> Components.to_ansi_view_component Tty.Default_style.default_style)
        |> Column.component
    ;;

    let right_panel_view (model : model) =
      let selected_index =
        match model.mode with
        | Navigate_files i -> Some i
        | _ -> None
      in
      let style i =
        if selected_index = Some i
        then
          { Tty.Default_style.default_style with
            bg_color = Some model.appearance.selection_color
          }
        else Tty.Default_style.default_style
      in
      let file_entries =
        Info.modified_files (Model.current_sha1 model)
        |> Array.of_list
        |> Array.map Components.Text_line.component
      in
      Column_sliding.component
        (fun i e -> e |> Components.to_ansi_view_component (style i))
        file_entries
        (Option.value ~default:0 selected_index)
    ;;

    let left_panel_view model =
      Column_sliding.component (rebase_entry_component model) model.entries model.cursor
    ;;

    let view (model : model) : Tty.ansi_view_item list =
      let constraints =
        Components.Constraints.
          { col_start = 1
          ; row_start = 1
          ; width = model.dimensions.col
          ; height = model.dimensions.row
          }
      in
      let left_portion, right_portion =
        if Model.is_navigate_files model then 1, 7 else 6, 2
      in
      let left_right_panel =
        Row_divided.component
          [ left_panel_view model, left_portion
          ; panel_separator model, 1
          ; right_panel_view model, right_portion
          ]
      in
      let full_screen =
        Column_divided.component
          [ left_right_panel, model.dimensions.row - 2
          ; cli_separator, 1
          ; cli_view model, 1
          ]
      in
      let v, _ = full_screen constraints in
      v
    ;;
  end

  let view = View.view

  let init =
    Model.
      { entries = Array.of_list Info.entries
      ; cursor = 0
      ; mode = Navigate
      ; dimensions = { row = 25; col = 80 }
      ; appearance = { symbols = Appearance.pretty_symbols; selection_color = Tty.Cyan }
      }
  ;;

  let exit_with (model : model) =
    ( model
    , [ Exit_with
          (Array.to_list model.entries |> git_todo_of_rebase_entries Info.modified_files)
      ] )
  ;;

  let inline (model : model) =
    let lines =
      git_todo_of_rebase_entries Info.modified_files (Array.to_list model.entries)
    in
    let new_entries = parse_entries lines |> Array.of_list in
    { model with mode = Navigate; entries = new_entries }
  ;;

  let navigate_files (model : model) =
    let files = Info.modified_files (Model.current_sha1 model) in
    if List.is_empty files then model else { model with mode = Navigate_files 0 }
  ;;

  let update_cli_command cmd (Model.{ appearance; _ } as model) =
    let with_selection_color color model =
      Model.with_appearance (Appearance.with_selection_color color appearance) model
    in
    match String.trim cmd |> String.split_on_char ' ' with
    | [ ":q" ] -> exit_with model
    | [ ":abort" ] -> exit_with init
    | [ ":inline" ] -> inline model, []
    | [ ":pretty" ] ->
      ( { model with
          appearance = { model.appearance with symbols = Appearance.pretty_symbols }
        }
        |> Model.switch_mode Navigate
      , [] )
    | [ ":raw" ] ->
      ( { model with
          appearance = { model.appearance with symbols = Appearance.raw_symbols }
        }
        |> Model.switch_mode Navigate
      , [] )
    | [ ":color"; "red" ] -> with_selection_color Red model, []
    | [ ":color"; "green" ] -> with_selection_color Green model, []
    | [ ":color"; "blue" ] -> with_selection_color Blue model, []
    | [ ":color"; "cyan" ] -> with_selection_color Cyan model, []
    | [ ":color"; "magenta" ] -> with_selection_color Magenta model, []
    | [ ":color"; "yellow" ] -> with_selection_color Yellow model, []
    | [ ":color"; "white" ] -> with_selection_color White model, []
    | _ -> Model.switch_mode Navigate model, []
  ;;

  let update (model : model) (event : Tty.ansi_event) =
    match model.mode, event with
    | Navigate, Up -> { model with cursor = max 0 (model.cursor - 1) }, []
    | Navigate, Down ->
      { model with cursor = min (Model.entry_count model - 1) (model.cursor + 1) }, []
    | Navigate_files i, Up -> Model.switch_mode (Navigate_files (max 0 (i - 1))) model, []
    | Navigate_files i, Down ->
      ( Model.switch_mode (Navigate_files (min (modified_count model - 1) (i + 1))) model
      , [] )
    | Navigate_files _, Char '\t' -> Model.switch_mode Navigate model, []
    | Navigate, Right -> Model.switch_mode Move model, []
    | Move, Up -> Model.move_up model, []
    | Move, Down -> Model.move_down model, []
    | Move, Left -> Model.switch_mode Navigate model, []
    | Rename editing_name, Enter ->
      Model.set_rename model (Editing_line.to_string editing_name), []
    | Rename s, Char c -> Model.renaming model (Editing_line.append_char c s), []
    | Rename s, Del -> Model.renaming model (Editing_line.del s), []
    | Rename s, Suppr -> Model.renaming model (Editing_line.suppr s), []
    | Rename s, Left -> Model.renaming model (Editing_line.left s), []
    | Rename s, Right -> Model.renaming model (Editing_line.right s), []
    | Cli s, Char c -> { model with mode = Cli (Editing_line.append_char c s) }, []
    | Cli s, Del -> { model with mode = Cli (Editing_line.del s) }, []
    | Cli s, Suppr -> { model with mode = Cli (Editing_line.suppr s) }, []
    | Cli s, Left -> { model with mode = Cli (Editing_line.left s) }, []
    | Cli s, Right -> { model with mode = Cli (Editing_line.right s) }, []
    | Cli s, Enter -> update_cli_command (Editing_line.to_string s) model
    | [%cross_match (Navigate, Move), Char '\t'] -> navigate_files model, []
    | [%cross_match (Navigate, Move, Navigate_files [%cross_any]), Char ':'] ->
      Model.switch_mode (Cli (Editing_line.init ":")) model, []
    | [%cross_match
        (Rename [%cross_any], Cli [%cross_any], Navigate_files [%cross_any]), Esc] ->
      Model.switch_mode Navigate model, []
    | [%cross_match (Navigate, Move), (Char 'd', Char 'D', Del, Suppr)] ->
      Model.set_drop model, []
    | [%cross_match (Navigate, Move), Char 'f'] ->
      Model.set_fixup model Discard_message, []
    | [%cross_match (Navigate, Move), Char 'F'] -> Model.set_fixup model Keep_message, []
    | [%cross_match (Navigate, Move), (Char 'p', Char 'P')] -> Model.set_pick model, []
    | [%cross_match (Navigate, Move), Char 'r'] ->
      Model.renaming model (Editing_line.init @@ Model.current_message model), []
    | [%cross_match (Navigate, Move), Char 'R'] ->
      Model.renaming model @@ Editing_line.empty, []
    | [%cross_match (Navigate, Move), (Char 'x', Char 'X')] ->
      Model.switch_explode model, []
    | _, Size dimensions -> { model with dimensions }, []
    | _ -> model, []
  ;;
end
