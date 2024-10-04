module Terminal_platform_with_exit
    (Terminal : Tty.Posix_terminal)
    (Target : sig
       val file : string
     end) : Tty.Ansi_Platform with type command = Rebase.rebase_app_command = struct
  include Tty.Posix_terminal_platform (Terminal)

  type command = Rebase.rebase_app_command

  let write_git_entries f content =
    let oc = open_out f in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        Rebase.git_todo_of_rebase_entries content
        |> List.iter (Qol.Out_channel.output_line oc))
  ;;

  let handle_commands = function
    | Rebase.Exit_with entries :: _ ->
      write_git_entries Target.file entries;
      restore_terminal_state ();
      exit 0
    | _ -> ()
  ;;
end

let () =
  let module Terminal = struct
    let terminal_in = Unix.stdin
    let terminal_out = Out_channel.stdout

    module Style = Tty.Default_style
  end
  in
  let file = Sys.argv.(1) in
  let module Entries = struct
    module Cache = Hashtbl.Make (String)

    type cache = string list Cache.t

    let cache : cache = Cache.create 50

    let cached sha1 lazy_files_result =
      match Cache.find_opt cache sha1 with
      | Some v -> v
      | None ->
        let v = Lazy.force lazy_files_result in
        Cache.add cache sha1 v;
        v
    ;;

    let entries = Rebase.parse_rebase_file file

    let modified_files sha1 =
      cached sha1
      @@ lazy
           (Qol_unix.command
              "git"
              [| "diff-tree"; "--no-commit-id"; "--name-only"; "-r"; "--root"; sha1 |])
    ;;
  end
  in
  let module Terminal_platform =
    Terminal_platform_with_exit
      (Terminal)
      (struct
        let file = file
      end)
  in
  Tea.loop_app (module Rebase.App (Entries)) (module Terminal_platform)
;;
