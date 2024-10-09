type t

type event =
  | Del
  | Char of char
  | Left
  | Right

val update : t -> event -> t
val empty : t
val init : string -> t
val to_string : t -> string
val edition_index : t -> int
val view : Tty.style -> int -> int -> int -> t -> Tty.ansi_view_item list
val append_char : char -> t -> t
val del : t -> t
val left : t -> t
val right : t -> t
