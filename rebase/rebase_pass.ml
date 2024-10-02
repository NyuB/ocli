let () =
  let f = Sys.argv.(1) in
  let ic = open_in f in
  let rec aux acc =
    try
      let l = input_line ic in
      aux (l :: acc)
    with
    | End_of_file -> List.rev acc
  in
  let lines = Fun.protect ~finally:(fun () -> close_in ic) (fun () -> aux []) in
  List.iter print_endline lines
;;
