module Terminal_platform_with_exit (Terminal : Tty.Posix_terminal) :
  Tty.Ansi_Platform with type command = Rebase.rebase_app_command = struct
  include Tty.Posix_terminal_platform (Terminal)

  type command = Rebase.rebase_app_command

  let handle_commands = function
    | Rebase.Exit_with _ :: _ -> exit 0
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
    let entries = Rebase.parse_rebase_file file
  end
  in
  let module Terminal_platform = Terminal_platform_with_exit (Terminal) in
  Tea.loop_app (module Rebase.App (Entries)) (module Terminal_platform)
;;
