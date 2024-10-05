let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let rebase_entry_testable : Rebase.rebase_entry Alcotest.testable =
  Alcotest.testable
    (fun fmt Rebase.{ command; message; sha1; renamed } ->
      Format.pp_print_string
        fmt
        (Printf.sprintf
           "{ command = %s; message = \"%s\";  sha1 = \"%s\"; renamed = %s }"
           (Rebase.string_of_rebase_command command |> String.capitalize_ascii)
           message
           sha1
           (string_of_bool renamed)))
    ( = )
;;

let check_rebase_entry_list =
  Alcotest.check (Alcotest.list rebase_entry_testable) "Expected equal entry lists"
;;

let check_string_list =
  Alcotest.check (Alcotest.list Alcotest.string) "Expected equal string lists"
;;

let test_renamed_entries =
  ( "Renamed entries are translated to execs"
  , fun () ->
      let entry =
        [ Rebase.{ command = Pick; sha1 = "sha1"; message = "message"; renamed = true } ]
      in
      check_string_list
        [ "pick sha1 message"; "exec git commit --amend -m 'message'" ]
        (Rebase.git_todo_of_rebase_entries entry) )
;;

let test_parse_git_entry_file =
  ( "Parse rebase file"
  , fun () ->
      let entries = Rebase.parse_rebase_file "rebase_file_sample.txt" in
      check_rebase_entry_list
        [ { command = Pick
          ; message = "Add default style to Tty module"
          ; sha1 = "8e46867"
          ; renamed = false
          }
        ; { command = Pick
          ; message = "Make test output more readable"
          ; sha1 = "ee88f85"
          ; renamed = false
          }
        ; { command = Pick
          ; message = "Move setup logic to Platform modules"
          ; sha1 = "e24e6e4"
          ; renamed = false
          }
        ; { command = Pick; message = "wip"; sha1 = "8a6ece0"; renamed = false }
        ]
        entries )
;;

let () =
  Alcotest.run
    "Rebase"
    [ "Rebase -> git todo", quick_tests [ test_renamed_entries ]
    ; "Rebase file parsing", quick_tests [ test_parse_git_entry_file ]
    ]
;;
