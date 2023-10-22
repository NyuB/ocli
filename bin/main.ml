open Qol

let () =
  let term = Unix.stdin in
  let info = Unix.tcgetattr term in
  Unix.tcsetattr term Unix.TCSANOW (Tty.disable_default_terminal_behavior info);
  let out = Out_channel.stdout in
  Tty.loop_app (module Demo.App) (module Demo.Term_style) term out
;;
