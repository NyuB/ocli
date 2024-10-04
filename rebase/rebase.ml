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
    }

  let init = { entries = Array.of_list Info.entries; cursor = 0; mode = Navigate }

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
      | Navigate -> string_of_rebase_entry e
      | Move when model.cursor <> i -> string_of_rebase_entry e
      | Rename _ when model.cursor <> i -> string_of_rebase_entry e
      | Move -> "^v " ^ string_of_rebase_entry e
      | Rename s -> string_of_renaming_entry e s
    in
    style, repr
  ;;

  let max_or_zero ~by l = Array.fold_left (fun acc item -> max acc (by item)) 0 l

  let right_panel_col_start ({ entries; _ } as model) =
    Array.mapi (fun i e -> highlight_entry i e model) entries
    |> max_or_zero ~by:(fun (_, s) -> String.length s + 1)
  ;;

  let entry_count model = Array.length model.entries
  let current_entry model = model.entries.(model.cursor)
  let current_sha1 model = (current_entry model).sha1

  let right_panel_border model =
    let col = right_panel_col_start model in
    List.init (entry_count model) (fun i ->
      Tty.{ row = i + 1; col = col + 1 }, Tty.Default_style.default_style, "|")
  ;;

  let right_panel_content model =
    let files = Info.modified_files (current_sha1 model)
    and col = right_panel_col_start model in
    List.mapi
      (fun i f -> Tty.{ row = i + 1; col = col + 3 }, Tty.Default_style.default_style, f)
      files
  ;;

  let right_panel_view model = right_panel_border model @ right_panel_content model

  let view ({ entries; _ } as model) : Tty.ansi_view_item list =
    let entries =
      Array.mapi
        (fun i e ->
          let style, repr = highlight_entry i e model in
          Tty.{ row = i + 1; col = 1 }, style, repr)
        entries
      |> Array.to_list
    and right_panel = right_panel_view model in
    entries @ right_panel
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

  let move_up ({ cursor; _ } as model) =
    if cursor <= 0
    then model
    else swap_entries model ~source_cursor:cursor ~target_cursor:(cursor - 1)
  ;;

  let move_down ({ cursor; entries; _ } as model) =
    if cursor >= Array.length entries - 1
    then model
    else swap_entries model ~source_cursor:cursor ~target_cursor:(cursor + 1)
  ;;

  let set_rebase_command ({ cursor; entries; _ } as model) cmd =
    let current = entries.(cursor) in
    let copy = Array.copy entries in
    copy.(cursor) <- { current with command = cmd };
    { model with entries = copy }
  ;;

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
    match event, model.mode with
    | Up, Navigate -> { model with cursor = max 0 (model.cursor - 1) }, []
    | Down, Navigate ->
      { model with cursor = min (Array.length model.entries - 1) (model.cursor + 1) }, []
    | Up, Move -> move_up model, []
    | Down, Move -> move_down model, []
    | Left, Move -> { model with mode = Navigate }, []
    | Right, Navigate -> { model with mode = Move }, []
    | Esc, _ -> model, [ Exit_with (Array.to_list model.entries) ]
    | Enter, Rename name -> set_name model name, []
    | Left, Rename _ -> { model with mode = Navigate }, []
    | Right, Move -> { model with mode = Rename "" }, []
    | Char c, Rename s -> { model with mode = Rename (Printf.sprintf "%s%c" s c) }, []
    | Del, Rename s -> { model with mode = Rename (del_rename s) }, []
    | Char 'f', _ | Char 'F', _ -> set_rebase_command model Fixup, []
    | Char 'p', _ | Char 'P', _ -> set_rebase_command model Pick, []
    | Char 'd', _ | Char 'D', _ | Del, _ -> set_rebase_command model Drop, []
    | _ -> model, []
  ;;
end

module Tests = struct
  let%expect_test "Parse rebase file" =
    let lines =
      [ "pick 8e46867 Add default style to Tty module"
      ; "pick ee88f85 Make test output more readable"
      ; "pick e24e6e4 Move setup logic to Platform modules"
      ; "pick 8a6ece0 wip"
      ; " "
      ; "# Rebase fabb4d9..8a6ece0 onto fabb4d9 (4 commands)"
      ; "#"
      ; "# Commands:"
      ; "# p, pick <commit> = use commit"
      ; "# r, reword <commit> = use commit, but edit the commit message"
      ; "# e, edit <commit> = use commit, but stop for amending"
      ; "# s, squash <commit> = use commit, but meld into previous commit"
      ; "# f, fixup [-C | -c] <commit> = like 'squash' but keep only the previous"
      ; "#                    commit's log message, unless -C is used, in which case"
      ; "#                    keep only this commit's message; -c is same as -C but"
      ; "#                    opens the editor"
      ; "# x, exec <command> = run command (the rest of the line) using shell"
      ; "# b, break = stop here (continue rebase later with 'git rebase --continue')"
      ; "# d, drop <commit> = remove commit"
      ; "# l, label <label> = label current HEAD with a name"
      ; "# t, reset <label> = reset HEAD to a label"
      ; "# m, merge [-C <commit> | -c <commit>] <label> [# <oneline>]"
      ; "#         create a merge commit using the original merge commit's"
      ; "#         message (or the oneline, if no original merge commit was"
      ; "#         specified); use -c <commit> to reword the commit message"
      ; "# u, update-ref <ref> = track a placeholder for the <ref> to be updated"
      ; "#                       to this position in the new commits. The <ref> is"
      ; "#                       updated at the end of the rebase"
      ; "#"
      ; "# These lines can be re-ordered; they are executed from top to bottom."
      ; "#"
      ; "# If you remove a line here THAT COMMIT WILL BE LOST."
      ; "#"
      ; "# However, if you remove everything, the rebase will be aborted."
      ; "#"
      ; "Successfully rebased and updated refs/heads/main."
      ]
    in
    let entries = parse_entries lines in
    List.iter (fun e -> print_endline @@ string_of_rebase_entry e) entries;
    [%expect
      {|
      pick: 8e46867 'Add default style to Tty module'
      pick: ee88f85 'Make test output more readable'
      pick: e24e6e4 'Move setup logic to Platform modules'
      pick: 8a6ece0 'wip'
      |}]
  ;;

  let test_entries =
    [ "pick 1a A"; "pick 2b B"; "pick 3c C"; "pick 4d D" ] |> parse_entries
  ;;

  module Test_Info = App (struct
      let entries = test_entries
      let modified_files _ = []
    end)

  let play_events events model =
    List.fold_left (fun m e -> Qol.first @@ Test_Info.update m e) model events
  ;;

  let print_render model =
    Tty_testing.Test_Platform.render @@ Test_Info.view model;
    List.iter print_endline (Tty_testing.Test_Platform.lines ())
  ;;

  let%expect_test "Navigate between commits" =
    let down_right = play_events [ Down; Right ] Test_Info.init in
    print_render down_right;
    [%expect
      {|
      pick: 1a 'A'    |
      ^v pick: 2b 'B' |
      pick: 3c 'C'    |
      pick: 4d 'D'    |
      |}];
    let up = play_events [ Up ] down_right in
    print_render up;
    [%expect
      {|
      ^v pick: 2b 'B' |
      pick: 1a 'A'    |
      pick: 3c 'C'    |
      pick: 4d 'D'    |
      |}];
    let down_down_left = play_events [ Down; Down; Left ] up in
    print_render down_down_left;
    [%expect
      {|
      pick: 1a 'A' |
      pick: 3c 'C' |
      pick: 2b 'B' |
      pick: 4d 'D' |
      |}]
  ;;

  let chars s = s |> String.to_seq |> Seq.map (fun c -> Tty.Char c) |> List.of_seq

  let%expect_test "Rename a commit" =
    let right_right = play_events [ Right; Right ] Test_Info.init in
    print_render right_right;
    [%expect
      {|
      pick: 1a ''(renaming) |
      pick: 2b 'B'          |
      pick: 3c 'C'          |
      pick: 4d 'D'          |
      |}];
    let renamed =
      play_events (chars "Awesome name!") right_right |> play_events [ Enter ]
    in
    print_render renamed;
    [%expect
      {|
      pick: 1a 'Awesome name!'(renamed) |
      pick: 2b 'B'                      |
      pick: 3c 'C'                      |
      pick: 4d 'D'                      |
      |}]
  ;;

  let%expect_test "Renaming a commit adds an exec entry" =
    let entries =
      [ { command = Pick; sha1 = "A"; message = "aaa"; renamed = false }
      ; { command = Pick; sha1 = "B"; message = "RENAMED"; renamed = true }
      ; { command = Pick; sha1 = "C"; message = "ccc"; renamed = false }
      ]
    in
    let to_git = git_todo_of_rebase_entries entries in
    List.iter print_endline to_git;
    [%expect
      {|
      pick A aaa
      pick B RENAMED
      exec git commit --amend -m 'RENAMED'
      pick C ccc
      |}]
  ;;
end
