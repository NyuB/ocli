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
    | Char 'f', _ | Char 'F', _ -> set_fixup model, []
    | Char 'p', _ | Char 'P', _ -> set_rebase_command model Pick, []
    | Char 'd', _ | Char 'D', _ | Del, _ -> set_rebase_command model Drop, []
    | Size dimensions, _ -> { model with dimensions }, []
    | _ -> model, []
  ;;
end

module Tests = struct
  let test_entries =
    [ "pick 1a A"; "pick 2b B"; "pick 3c C"; "pick 4d D" ] |> parse_entries
  ;;

  module Test_Info = struct
    let entries = test_entries
    let modified_files _ = []
  end

  module Test_App = App (Test_Info)

  let play_events_app update events model =
    List.fold_left (fun m e -> Qol.first @@ update m e) model events
  ;;

  let play_events = play_events_app Test_App.update

  let print_render_app view model =
    Tty_testing.Test_Platform.render @@ view model;
    List.iter print_endline (Tty_testing.Test_Platform.lines ())
  ;;

  let print_render = print_render_app Test_App.view

  let%expect_test "Navigate between commits" =
    let down_right = play_events [ Down; Right ] Test_App.init in
    print_render down_right;
    [%expect
      {|
      pick: 1a 'A'
      ^v pick: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let up = play_events [ Up ] down_right in
    print_render up;
    [%expect
      {|
      ^v pick: 2b 'B'
      pick: 1a 'A'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let down_down_left = play_events [ Down; Down; Left ] up in
    print_render down_down_left;
    [%expect
      {|
      pick: 1a 'A'
      pick: 3c 'C'
      pick: 2b 'B'
      pick: 4d 'D'
      |}]
  ;;

  let chars s = s |> String.to_seq |> Seq.map (fun c -> Tty.Char c) |> List.of_seq

  let%expect_test "Rename a commit" =
    let right_right = play_events [ Right; Right ] Test_App.init in
    print_render right_right;
    [%expect
      {|
      pick: 1a ''(renaming)
      pick: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let renamed =
      play_events (chars "Awesome name!") right_right |> play_events [ Enter ]
    in
    print_render renamed;
    [%expect
      {|
      pick: 1a 'Awesome name!'(renamed)
      pick: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
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

  let%expect_test "Fixup" =
    let fixup_second_commit = play_events [ Down; Char 'F' ] Test_App.init in
    print_render fixup_second_commit;
    [%expect
      {|
      pick: 1a 'A'
         fixup: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let fixup_second_commit = play_events [ Down; Char 'F'; Right ] Test_App.init in
    print_render fixup_second_commit;
    [%expect
      {|
      pick: 1a 'A'
      ^v fixup: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}]
  ;;

  let%expect_test "Cannot fixup root entry" =
    let try_to_fixup_root = play_events [ Char 'F' ] Test_App.init in
    print_render try_to_fixup_root;
    [%expect
      {|
      pick: 1a 'A'
      pick: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let try_to_move_fixup_to_root =
      play_events [ Down; Char 'F'; Right; Up ] Test_App.init
    in
    print_render try_to_move_fixup_to_root;
    [%expect
      {|
      pick: 1a 'A'
      ^v fixup: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let try_to_move_root_after_fixup =
      play_events [ Down; Char 'F'; Up; Right; Down ] Test_App.init
    in
    print_render try_to_move_root_after_fixup;
    [%expect
      {|
      ^v pick: 1a 'A'
      fixup: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}]
  ;;

  module Test_Info_with_modified (M : sig
      val modified : (string * string list) list
    end) =
  struct
    let entries = test_entries

    let modified_files sha1 =
      List.find_map
        (fun (ref, files) -> if String.equal sha1 ref then Some files else None)
        M.modified
      |> Option.value ~default:[]
    ;;
  end

  let%expect_test "Display modified files along entries" =
    let module Info : Rebase_info_external =
      Test_Info_with_modified (struct
        let modified =
          [ "1a", [ "a.txt"; "a/" ]
          ; "3c", [ "1.c"; "2.c"; "3.c"; "4.c"; "5.c"; "6.c"; "7.c" ]
          ]
        ;;
      end)
    in
    let module A = App (Info) in
    let print_render = print_render_app A.view
    and play_events = play_events_app A.update in
    print_render A.init;
    [%expect
      {|
      pick: 1a 'A' | a.txt
      pick: 2b 'B' | a/
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let move_down = play_events [ Down ] A.init in
    print_render move_down;
    [%expect
      {|
      pick: 1a 'A'
      pick: 2b 'B'
      pick: 3c 'C'
      pick: 4d 'D'
      |}];
    let move_down_again = play_events [ Down ] move_down in
    print_render move_down_again;
    [%expect
      {|
      pick: 1a 'A' | 1.c
      pick: 2b 'B' | 2.c
      pick: 3c 'C' | 3.c
      pick: 4d 'D' | 4.c
                   | 5.c
                   | 6.c
                   | 7.c
      |}]
  ;;

  let%expect_test "Crop commit messages and file names" =
    let module I = struct
      include Test_Info

      let entries =
        test_entries
        |> List.map (fun e ->
          { e with message = e.message ^ " 123456789123456789123456789" })
      ;;

      let modified_files sha1 =
        List.find_map
          (fun (s, fs) -> if String.equal sha1 s then Some fs else None)
          [ "2b", [ "com/compagny/root/package/AbstractEntityBuilderFactoryVisitor.java" ]
          ; "3c", [ "ok/ok.ml" ]
          ]
        |> Option.value ~default:[]
      ;;
    end
    in
    let module A = App (I) in
    let print_render = print_render_app A.view
    and play_events = play_events_app A.update in
    let size = Tty.{ col = 50; row = 999 } in
    Tty_testing.Test_Platform.set_dimensions size;
    print_render (play_events [ Size size ] A.init);
    [%expect
      {|
      pick: 1a 'A 123456789123456789...
      pick: 2b 'B 123456789123456789...
      pick: 3c 'C 123456789123456789...
      pick: 4d 'D 123456789123456789...
      |}];
    print_render (play_events [ Size size; Down ] A.init);
    [%expect
      {|
      pick: 1a 'A 123456789123456789... | com/compagn...
      pick: 2b 'B 123456789123456789...
      pick: 3c 'C 123456789123456789...
      pick: 4d 'D 123456789123456789...
      |}];
    print_render (play_events [ Size size; Down; Down ] A.init);
    [%expect
      {|
      pick: 1a 'A 123456789123456789... | ok/ok.ml
      pick: 2b 'B 123456789123456789...
      pick: 3c 'C 123456789123456789...
      pick: 4d 'D 123456789123456789...
      |}]
  ;;

  let%expect_test "Slide entry list to fit terminal rows" =
    let module I = struct
      include Test_Info

      let entries =
        List.init 2 (fun i ->
          List.map
            (fun e -> { e with message = Printf.sprintf "(%d)%s" i e.message })
            test_entries)
        |> List.concat
      ;;
    end
    in
    let module A = App (I) in
    let size = Tty.{ row = 5; col = 80 } in
    Tty_testing.Test_Platform.set_dimensions size;
    let print_render = print_render_app A.view
    and play_events = play_events_app A.update in
    let resized = play_events [ Size size ] A.init in
    print_render resized;
    [%expect
      {|
      pick: 1a '(0)A'
      pick: 2b '(0)B'
      pick: 3c '(0)C'
      pick: 4d '(0)D'
      pick: 1a '(1)A'
      |}];
    (*End with a Right to make the current selected entry clear *)
    print_render (play_events [ Down; Right ] resized);
    [%expect
      {|
      pick: 1a '(0)A'
      ^v pick: 2b '(0)B'
      pick: 3c '(0)C'
      pick: 4d '(0)D'
      pick: 1a '(1)A'
      |}];
    print_render (play_events [ Down; Down; Right ] resized);
    [%expect
      {|
      pick: 1a '(0)A'
      pick: 2b '(0)B'
      ^v pick: 3c '(0)C'
      pick: 4d '(0)D'
      pick: 1a '(1)A'
      |}];
    print_render (play_events [ Down; Down; Down; Right ] resized);
    [%expect
      {|
      pick: 2b '(0)B'
      pick: 3c '(0)C'
      ^v pick: 4d '(0)D'
      pick: 1a '(1)A'
      pick: 2b '(1)B'
      |}];
    print_render (play_events [ Down; Down; Down; Down; Down; Down; Right ] resized);
    [%expect
      {|
      pick: 1a '(1)A'
      pick: 2b '(1)B'
      ^v pick: 3c '(1)C'
      pick: 4d '(1)D'
      |}]
  ;;

  let%expect_test "Crop modified list to fit terminal rows" =
    let module Info =
      Test_Info_with_modified (struct
        let many_files = List.init 20 (fun i -> Printf.sprintf "file_at_row_%02d" (i + 1))
        let modified = List.map (fun e -> e.sha1, many_files) test_entries
      end)
    in
    let module A = App (Info) in
    let print_render = print_render_app A.view
    and play_events = play_events_app A.update in
    let size = Tty.{ col = 999; row = 15 } in
    Tty_testing.Test_Platform.set_dimensions size;
    let resized = play_events [ Size size ] A.init in
    print_render resized;
    [%expect
      {|
      pick: 1a 'A' | file_at_row_01
      pick: 2b 'B' | file_at_row_02
      pick: 3c 'C' | file_at_row_03
      pick: 4d 'D' | file_at_row_04
                   | file_at_row_05
                   | file_at_row_06
                   | file_at_row_07
                   | file_at_row_08
                   | file_at_row_09
                   | file_at_row_10
                   | file_at_row_11
                   | file_at_row_12
                   | file_at_row_13
                   | file_at_row_14
                   | file_at_row_15
      |}];
    Tty_testing.Test_Platform.set_dimensions { size with row = 20 };
    let expanded = play_events [ Size { size with row = 20 } ] resized in
    print_render expanded;
    [%expect
      {|
      pick: 1a 'A' | file_at_row_01
      pick: 2b 'B' | file_at_row_02
      pick: 3c 'C' | file_at_row_03
      pick: 4d 'D' | file_at_row_04
                   | file_at_row_05
                   | file_at_row_06
                   | file_at_row_07
                   | file_at_row_08
                   | file_at_row_09
                   | file_at_row_10
                   | file_at_row_11
                   | file_at_row_12
                   | file_at_row_13
                   | file_at_row_14
                   | file_at_row_15
                   | file_at_row_16
                   | file_at_row_17
                   | file_at_row_18
                   | file_at_row_19
                   | file_at_row_20
      |}]
  ;;
end
