open Qol

let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

module StringSet = Set.Make (String)

let string_of_explode s = Rebase.ExplodeCommit.exploded_list s |> String.concat "; "

let string_of_custom_command Rebase.{ rename; explode } =
  let rename_string = rename |?: "None" in
  let explode_string = string_of_explode explode in
  Printf.sprintf "{ rename = %s; explode = { %s }}" rename_string explode_string
;;

let rebase_entry_testable : Rebase.rebase_entry Alcotest.testable =
  Alcotest.testable
    (fun fmt Rebase.{ command; message; sha1; custom } ->
      Format.pp_print_string
        fmt
        (Printf.sprintf
           "{ command = %s; message = \"%s\";  sha1 = \"%s\"; custom = %s }"
           (Rebase.string_of_rebase_command command |> String.capitalize_ascii)
           message
           sha1
           (string_of_custom_command custom)))
    ( = )
;;

let check_rebase_entry_list =
  Alcotest.check (Alcotest.list rebase_entry_testable) "Expected equal entry lists"
;;

let check_string_list =
  Alcotest.check (Alcotest.list Alcotest.string) "Expected equal string lists"
;;

let no_modified _ = []
let nothing_custom = Rebase.{ rename = None; explode = Rebase.ExplodeCommit.init_nothing }

let only_exploded exploded =
  Rebase.{ rename = None; explode = ExplodeCommit.init_all exploded }
;;

let only_renamed name =
  Rebase.{ rename = Some name; explode = ExplodeCommit.init_nothing }
;;

let test_renamed_entries =
  ( "Renamed entries are translated to execs"
  , fun () ->
      let entry =
        [ Rebase.
            { command = Pick
            ; sha1 = "sha1"
            ; message = "message"
            ; custom = only_renamed "rename"
            }
        ]
      in
      check_string_list
        [ "pick sha1 message"; "exec git commit --amend -m 'rename'" ]
        (Rebase.git_todo_of_rebase_entries no_modified entry) )
;;

let test_exploded_entries =
  ( "Exploded entries are translated to one commit for each file"
  , fun () ->
      let modif s =
        if String.equal s "Target" then [ "a.txt"; "b.txt"; "c.txt" ] else []
      in
      let entry =
        [ Rebase.
            { command = Pick
            ; sha1 = "Target"
            ; message = "message"
            ; custom = only_exploded [ "a.txt"; "b.txt"; "c.txt" ]
            }
        ]
      in
      check_string_list
        [ "pick Target message"
        ; "exec git reset HEAD~ && git add a.txt && git commit -m 'a.txt (Exploded from \
           'message')' && git add b.txt && git commit -m 'b.txt (Exploded from \
           'message')' && git add c.txt && git commit -m 'c.txt (Exploded from \
           'message')'"
        ]
        (Rebase.git_todo_of_rebase_entries modif entry) )
;;

let test_exploded_entries_some_kept =
  ( "Exploded entries are translated to one commit for each file"
  , fun () ->
      let modif s =
        if String.equal s "Target" then [ "a.txt"; "b.txt"; "c.txt" ] else []
      in
      let entry =
        [ Rebase.
            { command = Pick
            ; sha1 = "Target"
            ; message = "message"
            ; custom = only_exploded [ "b.txt"; "c.txt" ]
            }
        ]
      in
      check_string_list
        [ "pick Target message"
        ; "exec git reset HEAD~ && git add a.txt && git commit -m 'message' && git add \
           b.txt && git commit -m 'b.txt (Exploded from 'message')' && git add c.txt && \
           git commit -m 'c.txt (Exploded from 'message')'"
        ]
        (Rebase.git_todo_of_rebase_entries modif entry) )
;;

let test_exploded_entry_no_modified =
  ( "If  there is no modified file, no exec is generated for an exploded entry"
  , fun () ->
      let entry =
        [ Rebase.
            { command = Pick
            ; sha1 = "SHA1"
            ; message = "message"
            ; custom = nothing_custom
            }
        ]
      in
      check_string_list
        [ "pick SHA1 message" ]
        (Rebase.git_todo_of_rebase_entries no_modified entry) )
;;

let test_exploded_and_renamed =
  ( "Entries are renamed first then exploded"
  , fun () ->
      let modif s =
        if String.equal s "Target" then [ "a.txt"; "b.txt"; "c.txt" ] else []
      in
      let entry =
        [ Rebase.
            { command = Pick
            ; sha1 = "Target"
            ; message = "message"
            ; custom =
                { rename = Some "Renamed"
                ; explode = ExplodeCommit.init_all [ "b.txt"; "c.txt" ]
                }
            }
        ]
      in
      check_string_list
        [ "pick Target message"
        ; "exec git commit --amend -m 'Renamed'"
        ; "exec git reset HEAD~ && git add a.txt && git commit -m 'Renamed' && git add \
           b.txt && git commit -m 'b.txt (Exploded from 'Renamed')' && git add c.txt && \
           git commit -m 'c.txt (Exploded from 'Renamed')'"
        ]
        (Rebase.git_todo_of_rebase_entries modif entry) )
;;

let test_parse_git_entry_file =
  ( "Parse rebase file"
  , fun () ->
      let entries = Rebase.parse_rebase_file "rebase_file_sample.txt" in
      check_rebase_entry_list
        [ { command = Pick
          ; message = "Add default style to Tty module"
          ; sha1 = "8e46867"
          ; custom = nothing_custom
          }
        ; { command = Pick
          ; message = "Make test output more readable"
          ; sha1 = "ee88f85"
          ; custom = nothing_custom
          }
        ; { command = Pick
          ; message = "Move setup logic to Platform modules"
          ; sha1 = "e24e6e4"
          ; custom = nothing_custom
          }
        ; { command = Pick; message = "wip"; sha1 = "8a6ece0"; custom = nothing_custom }
        ]
        entries )
;;

let test_fixup =
  ( "Fixup translation"
  , fun () ->
      check_string_list
        [ "pick a Pick_A"; "fixup f Discard"; "pick b Pick_B"; "fixup -C F Keep" ]
        (Rebase.git_todo_of_rebase_entries
           no_modified
           Rebase.
             [ { command = Pick; message = "Pick_A"; sha1 = "a"; custom = nothing_custom }
             ; { command = Fixup Discard_message
               ; message = "Discard"
               ; sha1 = "f"
               ; custom = nothing_custom
               }
             ; { command = Pick; message = "Pick_B"; sha1 = "b"; custom = nothing_custom }
             ; { command = Fixup Keep_message
               ; message = "Keep"
               ; sha1 = "F"
               ; custom = nothing_custom
               }
             ]) )
;;

let () =
  Alcotest.run
    "Rebase"
    [ ( "Rebase -> git todo"
      , quick_tests
          [ test_renamed_entries
          ; test_exploded_entries
          ; test_exploded_entries_some_kept
          ; test_exploded_entry_no_modified
          ; test_exploded_and_renamed
          ; test_fixup
          ] )
    ; "Rebase file parsing", quick_tests [ test_parse_git_entry_file ]
    ]
;;
