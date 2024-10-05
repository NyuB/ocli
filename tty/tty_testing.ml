module Test_Platform : sig
  include Tty.Ansi_Platform with type command = Tea.no_command

  val set_dimensions : Tty.position -> unit
  val lines : unit -> string list
end = struct
  include Tty.Ansi_Tea_Base

  type command = Tea.no_command

  let rows = ref 999
  let cols = ref 999
  let current_rendering = ref (Array.init !rows (fun _ -> Array.make !cols ' '))
  let error_records : string list ref = ref []

  let set_dimensions (size : Tty.position) =
    rows := size.row;
    cols := size.col;
    let rendering =
      Array.init !rows (fun r -> Array.init !cols (fun c -> !current_rendering.(r).(c)))
    in
    current_rendering := rendering
  ;;

  let clean () =
    for i = 0 to !rows - 1 do
      for j = 0 to !cols - 1 do
        !current_rendering.(i).(j) <- ' '
      done
    done;
    error_records := []
  ;;

  let poll_events () = [ Tty.Size { row = !rows; col = !cols } ]

  let render_string_at (pos : Tty.position) s =
    if pos.row > !rows
    then
      error_records
      := Printf.sprintf "Trying to write string '%s' at OOB row %d" s pos.row
         :: !error_records
    else (
      let len = String.length s in
      if len + pos.col > !cols + 1
      then
        error_records
        := Printf.sprintf
             "Trying to write string '%s' of length %d from column %d (which would \
              expand to column %d) but terminal is only %d columns large"
             s
             len
             pos.col
             (pos.col + len - 1)
             !cols
           :: !error_records;
      for j = 0 to min (len - 1) (!cols - 1) do
        !current_rendering.(pos.row - 1).(j + pos.col - 1) <- String.get s j
      done)
  ;;

  let setup () = ()

  let render view =
    clean ();
    List.iter (fun ((pos : Tty.position), _, s) -> render_string_at pos s) view
  ;;

  let lines () =
    List.map (fun e -> "[Rendering error] " ^ e) !error_records
    @ (Array.to_list !current_rendering
       |> List.map (fun c_arr -> Array.to_seq c_arr |> String.of_seq))
  ;;

  let handle_commands _ = ()
end

module Tests = struct
  module Test_App : Tty.Ansi_App with type command = Tea.no_command = struct
    include Tty.Ansi_Tea_Base

    type command = Tea.no_command

    type model =
      | Padd_even
      | Padd_odd

    let init = Padd_even

    let padd model row line =
      let style = Tty.Default_style.default_style in
      match model with
      | Padd_even ->
        if row mod 2 = 0
        then Tty.{ row; col = 3 }, style, line
        else Tty.{ row; col = 1 }, style, line
      | Padd_odd ->
        if row mod 2 = 1
        then Tty.{ row; col = 3 }, style, line
        else Tty.{ row; col = 1 }, style, line
    ;;

    let view model =
      [ "1 2 3"; "4 5 6"; "7 8 9" ] |> List.mapi (fun i s -> padd model (i + 1) s)
    ;;

    let update model _ =
      let next_model =
        match model with
        | Padd_even -> Padd_odd
        | Padd_odd -> Padd_even
      in
      next_model, []
    ;;
  end

  let%expect_test _ =
    let model = Test_App.init in
    Test_Platform.render (Test_App.view model);
    List.iter print_endline (Test_Platform.lines ());
    [%expect {|
      1 2 3
        4 5 6
      7 8 9 |}];
    Test_Platform.render (Test_App.view (Qol.first @@ Test_App.update model Tty.Enter));
    List.iter print_endline (Test_Platform.lines ());
    [%expect {|
        1 2 3
      4 5 6
        7 8 9 |}]
  ;;
end
