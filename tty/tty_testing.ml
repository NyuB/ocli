module Test_Platform : sig
  include Tty.Ansi_Platform with type command = Tea.no_command

  val lines : unit -> string list
end = struct
  include Tty.Ansi_Tea_Base

  type command = Tea.no_command

  let rows = 50
  let cols = 100
  let current_rendering = Array.init rows (fun _ -> Array.make cols ' ')

  let clean () =
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        current_rendering.(i).(j) <- ' '
      done
    done
  ;;

  let poll_events () = [ Tty.Size { row = rows; col = 999 } ]

  let render_string_at (pos : Tty.position) s =
    for j = 0 to String.length s - 1 do
      current_rendering.(pos.row - 1).(j + pos.col - 1) <- String.get s j
    done
  ;;

  let setup () = ()

  let render view =
    clean ();
    List.iter (fun ((pos : Tty.position), _, s) -> render_string_at pos s) view
  ;;

  let lines () =
    Array.to_list current_rendering
    |> List.map (fun c_arr -> Array.to_seq c_arr |> String.of_seq)
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
