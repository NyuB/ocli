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
  let module Terminal_platform = Tty.Posix_terminal_platform (Terminal) in
  Sys.catch_break true;
  try Tea.loop_app (module Rebase.App (Entries)) (module Terminal_platform) with
  | Sys.Break -> exit 0
;;
