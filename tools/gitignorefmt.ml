let protected_in filename f =
  let ic = open_in filename in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)
;;

let read_lines filename =
  protected_in filename (fun ic ->
    let rec aux acc =
      match In_channel.input_line ic with
      | None -> List.rev acc
      | Some s -> aux (s :: acc)
    in
    aux [])
;;

let protected_out filename f =
  let oc = open_out filename in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)
;;

let write_lines filename lines =
  protected_out filename (fun oc ->
    Out_channel.output_string oc (String.concat "\n" lines))
;;

let prepend_if_not_empty list list_of_list =
  match list with
  | [] -> list_of_list
  | l -> l :: list_of_list
;;

let split_in_sections lines =
  let rec aux sections current = function
    | [] -> List.rev (prepend_if_not_empty (List.rev current) sections)
    | s :: t when String.starts_with ~prefix:"# " s ->
      aux (prepend_if_not_empty (List.rev current) sections) [ s ] t
    | s :: t when String.starts_with ~prefix:"#" s ->
      aux
        (prepend_if_not_empty (List.rev current) sections)
        [ "# " ^ String.sub s 1 (String.length s - 1) ]
        t
    | s :: t when String.equal s String.empty -> aux sections current t
    | s :: t -> aux sections (s :: current) t
  in
  aux [] [] lines
;;

let inter_lines sections =
  let rec aux acc = function
    | [] -> List.rev acc
    | section :: t -> aux ((section @ [ "" ]) :: acc) t
  in
  aux [] sections |> List.flatten
;;

let () =
  let input_file = Sys.argv.(1)
  and output_file = Sys.argv.(2) in
  let lines =
    read_lines input_file |> List.map String.trim |> split_in_sections |> inter_lines
  in
  write_lines output_file lines
;;
