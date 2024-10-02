open Qol

let () =
  let module Terminal = struct
    let terminal_in = Unix.stdin
    let terminal_out = Out_channel.stdout

    module Style = Tty.Default_style
  end
  in
  let module Terminal_platform = Tty.Posix_terminal_platform (Terminal) in
  Tea.loop_app (module Demo.App) (module Terminal_platform)
;;
