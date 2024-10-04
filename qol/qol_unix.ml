(** [command "prog" [|"a"; "b"|]] executes 'prog a b' and returns its standard output *)
let command program args =
  let out, process_in, err =
    Unix.open_process_args_full program (Array.concat [ [| program |]; args ]) [||]
  in
  let rec aux acc =
    try
      let line = input_line out in
      aux (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  Fun.protect ~finally:(fun () ->
    ignore @@ Unix.close_process_full (out, process_in, err))
  @@ fun () -> aux []
;;
