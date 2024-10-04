(** A pass-through program that outputs the content of its single argument to test git behaviour and wrappers around custom rebase editors *)

let () =
  let f = Sys.argv.(1) in
  let ic = open_in f in
  let rec aux () =
    try
      let l = input_line ic in
      print_endline l;
      aux ()
    with
    | End_of_file -> ()
  in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> aux ())
;;
