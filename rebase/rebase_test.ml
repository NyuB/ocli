let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let string_of_custom_command = function
  | Rebase.Rename new_name -> Printf.sprintf "Rename %s" new_name
  | Explode -> "Explode"
  | Nothing -> "Nothing"
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

let test_renamed_entries =
  ( "Renamed entries are translated to execs"
  , fun () ->
      let entry =
        [ Rebase.
            { command = Pick
            ; sha1 = "sha1"
            ; message = "message"
            ; custom = Rename "rename"
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
            { command = Pick; sha1 = "Target"; message = "message"; custom = Explode }
        ]
      in
      check_string_list
        [ "pick Target message"
        ; "exec git reset HEAD~ && git add a.txt && git commit -m '(Exploded) a.txt' && \
           git add b.txt && git commit -m '(Exploded) b.txt' && git add c.txt && git \
           commit -m '(Exploded) c.txt'"
        ]
        (Rebase.git_todo_of_rebase_entries modif entry) )
;;

let test_exploded_entriy_no_modified =
  ( "If  there is no modified file, no exec is generated for an exploded entry"
  , fun () ->
      let entry =
        [ Rebase.{ command = Pick; sha1 = "SHA1"; message = "message"; custom = Explode }
        ]
      in
      check_string_list
        [ "pick SHA1 message" ]
        (Rebase.git_todo_of_rebase_entries no_modified entry) )
;;

let test_parse_git_entry_file =
  ( "Parse rebase file"
  , fun () ->
      let entries = Rebase.parse_rebase_file "rebase_file_sample.txt" in
      check_rebase_entry_list
        [ { command = Pick
          ; message = "Add default style to Tty module"
          ; sha1 = "8e46867"
          ; custom = Nothing
          }
        ; { command = Pick
          ; message = "Make test output more readable"
          ; sha1 = "ee88f85"
          ; custom = Nothing
          }
        ; { command = Pick
          ; message = "Move setup logic to Platform modules"
          ; sha1 = "e24e6e4"
          ; custom = Nothing
          }
        ; { command = Pick; message = "wip"; sha1 = "8a6ece0"; custom = Nothing }
        ]
        entries )
;;

let test_fixup =
  ( "Fixup translation"
  , fun () ->
      check_string_list
        [ "pick a A"; "fixup f F" ]
        (Rebase.git_todo_of_rebase_entries
           no_modified
           Rebase.
             [ { command = Pick; message = "A"; sha1 = "a"; custom = Nothing }
             ; { command = Fixup; message = "F"; sha1 = "f"; custom = Nothing }
             ]) )
;;

let () =
  Alcotest.run
    "Rebase"
    [ ( "Rebase -> git todo"
      , quick_tests
          [ test_renamed_entries
          ; test_exploded_entries
          ; test_exploded_entriy_no_modified
          ; test_fixup
          ] )
    ; "Rebase file parsing", quick_tests [ test_parse_git_entry_file ]
    ]
;;
