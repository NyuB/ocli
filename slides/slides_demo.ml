open Qol

let () =
  let term = Unix.stdin in
  let info = Unix.tcgetattr term in
  Unix.tcsetattr term Unix.TCSANOW (Tty.default_behavior_disabled info);
  let out = Out_channel.stdout in
  let module Terminal = struct
    let terminal_in = term
    let terminal_out = out

    module Style = Tty.Default_style
  end
  in
  let module Terminal_platform : Tty.Platform = Tty.Posix_terminal_platform (Terminal) in
  Tty.loop_app (module Slides.Boiling.App) (module Terminal_platform)
;;
