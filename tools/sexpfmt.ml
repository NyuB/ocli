let protected_out filename f =
  let oc = open_out filename in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)
;;

let write_lines filename lines =
  protected_out filename (fun oc ->
    Out_channel.output_string oc (String.concat "\n" lines))
;;

let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let input_file_name = args.(0)
  and output_dir_name = args.(1) in
  let output_file_name =
    Filename.concat output_dir_name (Filename.basename input_file_name ^ ".formatted")
  in
  let content =
    input_file_name
    |> Sexplib.Sexp.load_sexps
    |> List.map (Sexplib.Sexp.to_string_hum ~indent:2)
  in
  write_lines output_file_name content
;;
