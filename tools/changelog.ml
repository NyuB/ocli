let read_lines f =
  let ic = open_in f in
  let rec aux acc =
    try
      let l = input_line ic in
      aux (l :: acc)
    with
    | End_of_file -> List.rev acc
  in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> aux [])
;;

let write_string f s =
  let oc = open_out f in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc s)
;;

let starts_with_header_higher_than header s =
  String.starts_with ~prefix:"#" s && (not @@ String.starts_with ~prefix:header s)
;;

let current_section lines =
  let rec aux acc lines =
    match acc, lines with
    | None, [] -> []
    | None, header :: t when String.starts_with ~prefix:"## " header -> aux (Some []) t
    | None, _ :: t -> aux None t
    | Some section, header :: _
      when String.trim header |> starts_with_header_higher_than "###" -> List.rev section
    | Some section, line :: t -> aux (Some (line :: section)) t
    | Some section, [] -> List.rev section
  in
  aux None lines
;;

let () =
  let changelog = read_lines Sys.argv.(1)
  and concise = Sys.argv.(2) in
  let concise_lines = current_section changelog in
  write_string concise (String.concat "\n" concise_lines)
;;
