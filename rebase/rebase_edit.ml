(** Custom git-rebase editor *)

module Terminal_platform_with_exit
    (Terminal : Tty.Posix_terminal)
    (Target : sig
       val file : string
     end) : Tty.Ansi_Platform with type command = Rebase.rebase_app_command = struct
  include Tty.Posix_terminal_platform (Terminal)

  type command = Rebase.rebase_app_command

  let write_git_entries (f : string) (content : string list) =
    let oc = open_out f in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () -> List.iter (Qol.Out_channel.output_line oc) content)
  ;;

  (** Handle application exit by writing the current application rebase entries to the target rebase file and exiting the program *)
  let handle_commands = function
    | Rebase.Exit_with entries :: _ ->
      write_git_entries Target.file entries;
      restore_terminal_state ();
      exit 0
    | _ -> ()
  ;;
end

module Rebase_info_of_file (F : sig
    val file : string
  end) =
struct
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

  let entries = Rebase.parse_rebase_file F.file

  (** Give modified files for a given commit by actually calling git. Cached to avoid redundant program calls *)
  let modified_files sha1 =
    cached sha1
    @@ lazy
         (Qol_unix.command
            "git"
            [| "diff-tree"; "--no-commit-id"; "--name-only"; "-r"; "--root"; sha1 |])
  ;;
end

let () =
  (* The rebase file created by git and passed as first argument to the editor *)
  let rebase_file = Sys.argv.(1) in
  let module Rebase_file = struct
    let file = rebase_file
  end
  in
  let module Info = Rebase_info_of_file (Rebase_file) in
  let module Terminal = struct
    let terminal_in = Unix.stdin
    let terminal_out = Out_channel.stdout

    module Style = Tty.Default_style
  end
  in
  let module Terminal_platform = Terminal_platform_with_exit (Terminal) (Rebase_file) in
  Tea.loop_app (module Rebase.App (Info)) (module Terminal_platform)
;;
