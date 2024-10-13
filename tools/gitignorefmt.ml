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

let comment_prefix = "#"
let is_comment s = String.starts_with ~prefix:comment_prefix s
let spaced_comment_prefix = "# "

let split_in_sections lines =
  let rec aux sections current = function
    | [] -> List.rev (prepend_if_not_empty (List.rev current) sections)
    | s :: t when String.starts_with ~prefix:spaced_comment_prefix s ->
      aux (prepend_if_not_empty (List.rev current) sections) [ s ] t
    | s :: t when String.starts_with ~prefix:comment_prefix s ->
      aux
        (prepend_if_not_empty (List.rev current) sections)
        [ spaced_comment_prefix ^ String.sub s 1 (String.length s - 1) ]
        t
    | s :: t when String.equal s String.empty -> aux sections current t
    | s :: t -> aux sections (s :: current) t
  in
  aux [] [] lines
;;

let sort_section =
  let sort = List.sort String.compare in
  function
  | s :: t when is_comment s -> s :: sort t
  | l -> sort l
;;

let compare_sections_header sa sb =
  match sa, sb with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | a :: _, b :: _ when is_comment a && is_comment b -> String.compare a b
  | a :: _, _ :: _ when is_comment a -> 1
  | _ :: _, b :: _ when is_comment b -> -1
  | a :: _, b :: _ -> String.compare a b
;;

let sort_sections = List.sort compare_sections_header

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
    read_lines input_file
    |> List.map String.trim
    |> split_in_sections
    |> List.map sort_section
    |> sort_sections
    |> inter_lines
  in
  write_lines output_file lines
;;
