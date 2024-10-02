module Boiling = struct
  type boiling_level =
    | No_Fire
    | One_Fire
    | Two_Fires
    | Three_Fires

  type t =
    { level : boiling_level
    ; tick : bool
    }

  let increase_level t =
    let level =
      match t.level with
      | No_Fire -> One_Fire
      | One_Fire -> Two_Fires
      | Two_Fires -> Three_Fires
      | Three_Fires -> Three_Fires
    in
    { t with level }
  ;;

  let decrease_level t =
    let level =
      match t.level with
      | No_Fire -> No_Fire
      | One_Fire -> No_Fire
      | Two_Fires -> One_Fire
      | Three_Fires -> Two_Fires
    in
    { t with level }
  ;;

  let tick t = { t with tick = not t.tick }

  let boiling_base =
    {||-----------|
|           |========
|           |
|           |
|           |
\___________/|}
  ;;

  let fire = {|\|/|}
  let no_fire = ""
  let fire_one = {|     \|/     |}
  let fire_two = {| \|/     \|/ |}
  let fire_three = {| \|/ \|/ \|/ |}
  let no_smoke = ""

  let smoke_one = {|
    (
      )|}

  let smoke_two = {|      )   
     (    (
    )    )
    (     )|}

  let smoke_three =
    {|        (
      )
    )
   (
   (      )
    )    )
   (    )
   )    )
   (    (|}
  ;;

  let reverse_smoke = function
    | '(' -> ')'
    | ')' -> '('
    | c -> c
  ;;

  let smoke t s = String.map (if t.tick then reverse_smoke else Fun.id) s

  let lines s =
    if s = ""
    then [ "" ]
    else (
      let rec aux acc current char_seq =
        match char_seq () with
        | Seq.Nil -> List.rev (current :: acc)
        | Seq.Cons ('\n', next) -> aux (current :: acc) "" next
        | Seq.Cons (c, next) -> aux acc (Printf.sprintf "%s%c" current c) next
      in
      aux [] "" (String.to_seq s))
  ;;

  let array_of_line l = l |> String.to_seq |> Array.of_seq
  let array_of_lines lines = lines |> List.map array_of_line |> Array.of_list

  let lines_of_t t =
    match t.level with
    | No_Fire -> lines (smoke t no_smoke) @ lines boiling_base @ [ no_fire ]
    | One_Fire -> lines (smoke t smoke_one) @ lines boiling_base @ [ fire_one ]
    | Two_Fires -> lines (smoke t smoke_two) @ lines boiling_base @ [ fire_two ]
    | Three_Fires -> lines (smoke t smoke_three) @ lines boiling_base @ [ fire_three ]
  ;;

  module App : Tty.Ansi_App = struct
    include Tty.Ansi_Tea_Base

    module S = Tty.Posix_style (struct
        let default_foreground_color = Tty.Default
        let default_background_color = Tty.Default
      end)

    type model =
      { boiling : t
      ; dim : Tty.position
      }

    let init = { boiling = { level = No_Fire; tick = false }; dim = { row = 1; col = 1 } }

    let view model =
      let base_row = 20 in
      if model.dim.row < base_row
      then
        [ Tty.{ row = 1; col = 1 }, S.default_style, "Insufficient terminal dimensions" ]
      else
        List.rev (lines_of_t model.boiling)
        |> List.mapi (fun i l -> Tty.{ row = base_row - i; col = 1 }, S.default_style, l)
    ;;

    let update model msg =
      let open Tty in
      match msg with
      | Size dim -> { model with dim }
      | Right | Left -> { model with boiling = tick model.boiling }
      | Char '+' -> { model with boiling = increase_level model.boiling }
      | Char '-' -> { model with boiling = decrease_level model.boiling }
      | _ -> model
    ;;
  end

  module Tests = struct
    let print_lines l = l |> List.iter print_endline

    let%expect_test "No fire" =
      print_lines (lines_of_t { level = No_Fire; tick = false });
      [%expect
        {|
        |-----------|
        |           |========
        |           |
        |           |
        |           |
        \___________/|}]
    ;;

    let%expect_test "One fire" =
      print_lines (lines_of_t { level = One_Fire; tick = false });
      [%expect
        {|
            (
              )
        |-----------|
        |           |========
        |           |
        |           |
        |           |
        \___________/
             \|/ |}]
    ;;

    let%expect_test _ =
      print_lines (lines_of_t { level = Two_Fires; tick = false });
      [%expect
        {|
            )
           (    (
          )    )
          (     )
      |-----------|
      |           |========
      |           |
      |           |
      |           |
      \___________/
       \|/     \|/ |}]
    ;;

    let%expect_test _ =
      print_lines (lines_of_t { level = Three_Fires; tick = false });
      print_endline "\n======== Next =======\n";
      print_lines (lines_of_t { level = Three_Fires; tick = true });
      [%expect
        {|
              (
            )
          )
         (
         (      )
          )    )
         (    )
         )    )
         (    (
      |-----------|
      |           |========
      |           |
      |           |
      |           |
      \___________/
       \|/ \|/ \|/

      ======== Next =======

              )
            (
          (
         )
         )      (
          (    (
         )    (
         (    (
         )    )
      |-----------|
      |           |========
      |           |
      |           |
      |           |
      \___________/
       \|/ \|/ \|/ |}]
    ;;
  end
end
