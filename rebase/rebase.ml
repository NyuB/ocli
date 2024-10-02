(*
   pick 8e46867 Add default style to Tty module
   pick ee88f85 Make test output more readable
   pick e24e6e4 Move setup logic to Platform modules
   pick 7b62a6d wip

   # Rebase fabb4d9..7b62a6d onto fabb4d9 (4 commands)
   #
   # Commands:
   # p, pick <commit> = use commit
   # r, reword <commit> = use commit, but edit the commit message
   # e, edit <commit> = use commit, but stop for amending
   # s, squash <commit> = use commit, but meld into previous commit
   # f, fixup [-C | -c] <commit> = like "squash" but keep only the previous
   #                    commit's log message, unless -C is used, in which case
   #                    keep only this commit's message; -c is same as -C but
   #                    opens the editor
   # x, exec <command> = run command (the rest of the line) using shell
   # b, break = stop here (continue rebase later with 'git rebase --continue')
   # d, drop <commit> = remove commit
   # l, label <label> = label current HEAD with a name
   # t, reset <label> = reset HEAD to a label
   # m, merge [-C <commit> | -c <commit>] <label> [# <oneline>]
   #         create a merge commit using the original merge commit's
   #         message (or the oneline, if no original merge commit was
   #         specified); use -c <commit> to reword the commit message
   # u, update-ref <ref> = track a placeholder for the <ref> to be updated
   #                       to this position in the new commits. The <ref> is
   #                       updated at the end of the rebase
   #
   # These lines can be re-ordered; they are executed from top to bottom.
   #
   # If you remove a line here THAT COMMIT WILL BE LOST.
   #
   # However, if you remove everything, the rebase will be aborted.
   #
*)

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
  }

let string_of_rebase_entry { command; sha1; message } =
  Printf.sprintf "%s: %s '%s'" (string_of_rebase_command command) sha1 message
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
  if List.length parts > 3 && String.equal (List.hd parts) "pick"
  then
    Some
      { command = Pick
      ; sha1 = List.nth parts 1
      ; message = String.concat " " (sublist 2 parts)
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

module type Entries = sig
  val entries : rebase_entry list
end

type rebase_app_command = Exit_with of rebase_entry list

module App (E : Entries) : Tty.Ansi_App with type command = rebase_app_command = struct
  include Tty.Ansi_Tea_Base

  type command = rebase_app_command

  type move_mode =
    | Navigate
    | Move

  type model =
    { entries : rebase_entry array
    ; cursor : int
    ; mode : move_mode
    }

  let init = { entries = Array.of_list E.entries; cursor = 0; mode = Navigate }

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
      | Move ->
        let prefix = if model.cursor = i then "^v " else "" in
        prefix ^ string_of_rebase_entry e
    in
    style, repr
  ;;

  let view ({ entries; _ } as model) : Tty.ansi_view_item list =
    Array.mapi
      (fun i e ->
        let style, repr = highlight_entry i e model in
        Tty.{ row = i + 1; col = 1 }, style, repr)
      entries
    |> Array.to_list
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
      |}]
  ;;
end
