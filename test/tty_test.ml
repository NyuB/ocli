let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

module Test_Style = Tty.Posix_style (struct
    let default_foreground_color = Tty.Black
    let default_background_color = Tty.White
  end)

let fg_red =
  ( "Foreground red"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground red coloring"
        "\027[31mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Red }
           "Content") )
;;

let fg_green =
  ( "Foreground green"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground green coloring"
        "\027[32mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Green }
           "Content") )
;;

let fg_blue =
  ( "Foreground blue"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground blue coloring"
        "\027[34mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Blue }
           "Content") )
;;

let fg_yellow =
  ( "Foreground yellow"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground yellow coloring"
        "\027[33mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Yellow }
           "Content") )
;;

let fg_magenta =
  ( "Foreground magenta"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground magenta coloring"
        "\027[35mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Magenta }
           "Content") )
;;

let fg_cyan =
  ( "Foreground cyan"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground cyan coloring"
        "\027[36mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Cyan }
           "Content") )
;;

let fg_black =
  ( "Foreground black"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground black coloring"
        "\027[30mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Black }
           "Content") )
;;

let fg_white =
  ( "Foreground white"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground white coloring"
        "\027[37mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some White }
           "Content") )
;;

let fg_default =
  ( "Foreground default"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for foreground default coloring"
        "\027[39mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with fg_color = Some Default }
           "Content") )
;;

let bg_red =
  ( "Background red"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background red coloring"
        "\027[41mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Red }
           "Content") )
;;

let bg_green =
  ( "Background green"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background green coloring"
        "\027[42mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Green }
           "Content") )
;;

let bg_blue =
  ( "Background blue"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background blue coloring"
        "\027[44mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Blue }
           "Content") )
;;

let bg_yellow =
  ( "Background yellow"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background yellow coloring"
        "\027[43mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Yellow }
           "Content") )
;;

let bg_magenta =
  ( "Background magenta"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background magenta coloring"
        "\027[45mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Magenta }
           "Content") )
;;

let bg_cyan =
  ( "Background cyan"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background cyan coloring"
        "\027[46mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Cyan }
           "Content") )
;;

let bg_black =
  ( "Background black"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background black coloring"
        "\027[40mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Black }
           "Content") )
;;

let bg_white =
  ( "Background white"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background white coloring"
        "\027[47mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some White }
           "Content") )
;;

let bg_default =
  ( "Background default"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for background default coloring"
        "\027[49mContent\027[30;47;0m"
        (Test_Style.styled
           { Test_Style.default_style with bg_color = Some Default }
           "Content") )
;;

let mixed_styling =
  ( "Mixed background and underline"
  , fun () ->
      Alcotest.(check string)
        "Invalid tty sequence for underlined yelow background"
        "\027[4;43mContent\027[30;47;0m"
        (Test_Style.styled
           { bg_color = Some Yellow; fg_color = None; underlined = true; bold = false }
           "Content") )
;;

let no_styling =
  ( "No styling"
  , fun () ->
      Alcotest.(check string)
        "Should return the string as is if no styling is applied"
        "Content"
        (Test_Style.styled
           { bg_color = None; fg_color = None; underlined = false; bold = false }
           "Content") )
;;

let () =
  Alcotest.run
    "Tty"
    [ ( "Style foreground"
      , quick_tests
          [ fg_red
          ; fg_green
          ; fg_blue
          ; fg_yellow
          ; fg_magenta
          ; fg_cyan
          ; fg_black
          ; fg_white
          ; fg_default
          ] )
    ; ( "Style background"
      , quick_tests
          [ bg_red
          ; bg_green
          ; bg_blue
          ; bg_yellow
          ; bg_magenta
          ; bg_cyan
          ; bg_black
          ; bg_white
          ; bg_default
          ] )
    ; "Style", quick_tests [ mixed_styling; no_styling ]
    ]
;;
